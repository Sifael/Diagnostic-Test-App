library(R6)

CIMethods <- R6::R6Class(
  "CIMethods",
  private = list(
    # makes sure interval is within range - applied to all methods
    clip_interval = function(ci) {
      c(max(0, ci[1]), min(1, ci[2]))
    },

    # clooper perason interval (x = successes, n = trials)
    clopper_pearson_interval = function(x, n, alpha) {
      if (x == 0) {
        return(c(0, 1 - (alpha / 2)^(1 / n)))
      }
      if (x == n) {
        return(c((alpha / 2)^(1 / n), 1))
      }

      lower <- qbeta(alpha / 2, x, n - x + 1)
      upper <- qbeta(1 - alpha / 2, x + 1, n - x)
      c(lower, upper)
    },

    # Defining the confidence interval
    wald_interval = function(x, n, alpha) {
      if (n == 0) {
        return(c(0, 1))
      }
      p <- x / n
      z <- qnorm(1 - alpha / 2)
      margin <- z * sqrt((p * (1 - p)) / n)
      private$clip_interval(c(p - margin, p + margin))
    },

    # Define Wilson interval
    wilson_interval = function(x, n, alpha) {
      if (n == 0) {
        return(c(0, 1))
      }
      p <- x / n
      z <- qnorm(1 - alpha / 2)

      denom <- 1 + z^2 / n
      center <- (p + z^2 / (2 * n)) / denom
      margin <- (z / denom) * sqrt(p * (1 - p) / n + z^2 / (4 * n^2))

      private$clip_interval(c(center - margin, center + margin))
    },

    # Define agresti_coull
    agresti_coull_interval = function(x, n, alpha) {
      if (n == 0) {
        return(c(0, 1))
      }
      z <- qnorm(1 - alpha / 2)
      n_tilde <- n + z^2
      x_tilde <- x + (z^2) / 2
      p_tilde <- x_tilde / n_tilde

      margin <- z * sqrt((p_tilde * (1 - p_tilde)) / n_tilde)
      private$clip_interval(c(p_tilde - margin, p_tilde + margin))
    },

    # Validate method existence
    validate_method = function(method) {
      if (!method %in% names(self$methods)) {
        stop(sprintf(
          "Method '%s' not found. Available methods: %s",
          method,
          paste(names(self$methods), collapse = ", ")
        ))
      }
    },

    # Validate dataframe columns
    validate_dataframe = function(metrics_df) {
      required_cols <- c("Metric", "Success", "Total")
      if (!all(required_cols %in% names(metrics_df))) {
        stop(
          "Input dataframe must contain 'Metric', 'Success', and 'Total' columns."
        )
      }
    },

    # Compute CI for a single row
    compute_row_ci = function(x, n, alpha, method_fn) {
      # Handle edge cases (n=0)
      if (is.na(n) || n == 0) {
        return(data.frame(
          Lower_CI = NA,
          Upper_CI = NA,
          stringsAsFactors = FALSE
        ))
      }

      # Calculate CI
      ci <- method_fn(x, n, alpha)

      data.frame(
        Lower_CI = ci[1],
        Upper_CI = ci[2],
        stringsAsFactors = FALSE
      )
    }
  ),
  public = list(
    # setting up confidence interval and methos
    confidence_level = NULL,
    methods = NULL,

    # intializes methods names list/dictionary object
    initialize = function(conf_level = .95) {
      self$confidence_level <- conf_level
      self$methods[["wald"]] <- private$wald_interval
      self$methods[["wilson"]] <- private$wilson_interval
      self$methods[["clopper_pearson"]] <- private$clopper_pearson_interval
      self$methods[["agresti_coull"]] <- private$agresti_coull_interval
    },

    # ability to call calculate CI for any method
    calculate_ci = function(metrics_df, method = "wilson") {
      private$validate_method(method)
      private$validate_dataframe(metrics_df)

      alpha <- 1 - self$confidence_level
      method_fn <- self$methods[[method]]

      # Process each row
      ci_results <- lapply(1:nrow(metrics_df), function(i) {
        row <- metrics_df[i, ]
        private$compute_row_ci(row$Success, row$Total, alpha, method_fn)
      })

      # Combine results with original data
      result_ci <- do.call(rbind, ci_results)
      final_df <- cbind(metrics_df, result_ci)
      return(final_df)
    }
  )
)
