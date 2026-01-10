library(R6)


ContingencyBase <- R6::R6Class(
    
    "ContingencyBase",
    # base attributes
    public = list(
      ref_labels = NULL,
      test_labels = NULL,
      contingency_table = NULL,

      initialize = function(ref_labels, test_labels){
        self$ref_labels = ref_labels
        self$test_labels = test_labels
      },

      # abstract methods
      get_contingency_values = function(){},
      classification_metrics = function(){},
      get_contingency_counts_df = function(){}
    )
)


# Static list of metrics definitions
BinaryContingency_metrics <- list(
    Sensitivity = function(cv) {
        k <- cv$TP
        n <- cv$TP + cv$FN
        if (n == 0) {
            return(list(value = NA, success = k, total = n))
        }
        list(value = k / n, success = k, total = n)
    },
    Specificity = function(cv) {
        k <- cv$TN
        n <- cv$FP + cv$TN
        if (n == 0) {
            return(list(value = NA, success = k, total = n))
        }
        list(value = k / n, success = k, total = n)
    },
    Accuracy = function(cv) {
        k <- cv$TP + cv$TN
        n <- cv$TP + cv$TN + cv$FP + cv$FN
        if (n == 0) {
            return(list(value = NA, success = k, total = n))
        }
        list(value = k / n, success = k, total = n)
    },
    # Precision = function(cv) {
    #     k <- cv$TP
    #     n <- cv$TP + cv$FP
    #     if (n == 0) {
    #         return(list(value = NA, success = k, total = n))
    #     }
    #     list(value = k / n, success = k, total = n)
    # },
    NPV = function(cv) {
        k <- cv$TN
        n <- cv$TN + cv$FN
        if (n == 0) {
            return(list(value = NA, success = k, total = n))
        }
        list(value = k / n, success = k, total = n)
    },
    PPV = function(cv) {
        k <- cv$TP
        n <- cv$TP + cv$FP
        if (n == 0) {
            return(list(value = NA, success = k, total = n))
        }
        list(value = k / n, success = k, total = n)
    },
    F1_Score = function(cv) {
        # F1 Score = 2 * (Precision * Recall) / (Precision + Recall)
        prec <- if ((cv$TP + cv$FP) == 0) 0 else cv$TP / (cv$TP + cv$FP)
        rec <- if ((cv$TP + cv$FN) == 0) 0 else cv$TP / (cv$TP + cv$FN)

        if (prec + rec == 0) {
            return(list(value = 0, success = 0, total = 0))
        } # Edge case

        val <- 2 * (prec * rec) / (prec + rec)
        # Return generic structure to avoid breaking downstream table
        list(value = val, success = NA, total = NA)
    }
    # FDR = function(cv) { # False Discovery Rate = 1 - PPV
    #     k <- cv$FP
    #     n <- cv$TP + cv$FP
    #     if (n == 0) {
    #         return(list(value = NA, success = k, total = n))
    #     }
    #     list(value = k / n, success = k, total = n)
    # }
)




# main object for computation
BinaryContingency <- R6::R6Class(
    "BinaryContingency",
    inherit = ContingencyBase,

    # setting up hiden methods
    private = list(
      calculate_contingency = function(){
          TP <- sum(self$ref_labels & self$test_labels)
          FP <- sum(!self$ref_labels & self$test_labels)
          TN <- sum(!self$ref_labels & !self$test_labels)
          FN <- sum(self$ref_labels & !self$test_labels)
          return(list(TP = TP, FP = FP, TN = TN, FN = FN, TOTAL = TP + FP + TN + FN))
      },
      
      get_totals = function(contingency) {
            TP <- contingency$TP
            FP <- contingency$FP
            TN <- contingency$TN
            FN <- contingency$FN

            return(list(
                total = TP + FP + TN + FN,
                pos_ref = TP + FN,
                neg_ref = FP + TN,
                pos_test = TP + FP,
                neg_test = FN + TN
            ))
        }
    ),

    # setting up accessible methods
    public = list(

      # dictionary for all metrics defined above
      metrics_registry = BinaryContingency_metrics,
      
      # fetch contingency values
      get_contingency_values = function(){
        return( private$calculate_contingency())
      }, 

      # returns total contingency and total counts
      get_basic_counts = function() {
        contingency <- private$calculate_contingency()
        totals <- private$get_totals(contingency)
        return(c(contingency, totals))
      },

      # this is the contingency table
      get_contingency_matrix = function() {
        counts <- self$get_basic_counts()

        # Create 2x2 Contingency Table
        table_2x2 <- data.frame(
            Reference = c("Ref Positive", "Ref Negative", "Total"),
            "Test_Positive" = c(counts$TP, counts$FP, counts$pos_test),
            "Test_Negative" = c(counts$FN, counts$TN, counts$neg_test),
            "Total" = c(counts$pos_ref, counts$neg_ref, counts$total),
            stringsAsFactors = FALSE,
            check.names = FALSE
        )

        names(table_2x2) <- c(" ", "Test Positive", "Test Negative", "Total")
        return(as.data.frame(table_2x2))
        },

        # Get the count dataframe
        get_contingency_counts_df = function() {
            counts <- self$get_basic_counts()

            # Create Cell Counts table
            cell_counts <- data.frame(
                Category = c(
                    "True Positive (TP)",
                    "False Positive (FP)",
                    "False Negative (FN)",
                    "True Negative (TN)",
                    "Total"
                ),
                Count = c(counts$TP, counts$FP, counts$FN, counts$TN, counts$TOTAL),
                Description = c(
                    "Reference +, Test +",
                    "Reference -, Test +",
                    "Reference +, Test -",
                    "Reference -, Test -",
                    "All samples"
                ),
                stringsAsFactors = FALSE
            )
            return(cell_counts)
        },

        # calculates classification metrics based on registry
        calculate_classification_metrics = function() {
            contingency_values <- self$get_contingency_values()
            result <- lapply(self$metrics_registry, function(f) f(contingency_values))
            return(result)
        },

        # gets the classification metrics df
        get_classification_metrics_df = function() {
            metric_results <- self$calculate_classification_metrics()

            # Convert list of lists to data frame
            result <- do.call(rbind, lapply(names(metric_results), function(m) {
                res <- metric_results[[m]] # extract metric value = accuracy = list(prop, value, total)
                data.frame(
                    Metric = m,
                    Value = res$value,
                    Success = res$success,
                    Total = res$total,
                    stringsAsFactors = FALSE
                )
            }))
            rownames(result) <- NULL
            return(result)
        },

        # A clean summary for download
        get_clean_summary = function(ref_info, test_info) {
            class_metrics <- self$get_classification_metrics_df()

            # Extract key metrics values safely
            sens_val <- class_metrics$Value[class_metrics$Metric == "Sensitivity"]
            spec_val <- class_metrics$Value[class_metrics$Metric == "Specificity"]
            ppv_val <- class_metrics$Value[class_metrics$Metric == "PPV"]
            npv_val <- class_metrics$Value[class_metrics$Metric == "NPV"]

            data.frame(
                Metric = c(
                    "Reference Column",
                    "Reference Threshold",
                    "Test Column",
                    "Test Threshold",
                    "Sensitivity",
                    "Specificity",
                    "PPV",
                    "NPV"
                ),
                Value = c(
                    ref_info$var,
                    as.character(ref_info$value),
                    test_info$var,
                    as.character(test_info$value),
                    sprintf("%.3f", if (length(sens_val)) sens_val else NA),
                    sprintf("%.3f", if (length(spec_val)) spec_val else NA),
                    sprintf("%.3f", if (length(ppv_val)) ppv_val else NA),
                    sprintf("%.3f", if (length(npv_val)) npv_val else NA)
                ),
                stringsAsFactors = FALSE
            )
        }

    )

)



