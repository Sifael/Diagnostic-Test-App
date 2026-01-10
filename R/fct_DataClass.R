library(R6)
library(janitor)

DataClass <- R6::R6Class(
  "Dataset",
  
  private = list(
    # clean names to remove spaces
    clean_colnames = function(df) {
      janitor::clean_names(df)
    }
  ),
  public = list(
    # initialize dataframe
    df = NULL,
    initialize = function(df = NULL) {
      if (is.null(df)) {
        df <- data.frame()
      } else {
        df <- private$clean_colnames(df)
      }
      self$df <- df
    },

    # generate load demo dataset if a user wishes
    generate_demo_data = function(n = 40) {
      demo_df <- data.frame(
        id = sprintf("ID%03d", 1:n),
        treatment_a = round(c(rnorm(n / 2, 34, 2.1), rnorm(n / 2, 44, 2.1)), 1),
        treatment_b = round(c(rnorm(n / 2, 30, 1.1), rnorm(n / 2, 36, 3.7)), 1),
        treatment_c = round(c(rnorm(n / 2, 54, 2.1), rnorm(n / 2, 64, 3.1)), 1),
        age = round(round(rnorm(n, 50, 12))),
        bmi = round(rnorm(n, 26, 4.5), 1)
      )
      self$df <- private$clean_colnames(demo_df)
    },

    # read_excel
    read_csv = function(file) {
      raw_df <- readr::read_csv(file$datapath)
      private$clean_colnames(raw_df)
    },

    # read_excel
    read_excel = function(file) {
      raw_df <- readxl::read_excel(file$datapath)
      private$clean_colnames(raw_df)
    },

    # validate empty files
    validate_empty_file = function(file) {
      if (file$size == 0) {
        stop("The uploaded file is empty. Please upload a valid file.")
      }
    },

    # get numeric columns
    get_numeric_columns = function(df = NULL) {
      if (is.null(df)) df <- self$df # in case we are using in server and there is
      if (is.null(df)) {
        return(character(0))
      }
      names(df)[sapply(df, is.numeric)]
    },

    # read and validate file
    read_validated_file = function(file) {
      file_ext <- tools::file_ext(file$name)

      if (!file_ext %in% c("csv", "xls", "xlsx")) {
        stop("Unsupported file type. Please upload a CSV or Excel file.")
      }

      # validate empty files
      self$validate_empty_file(file)

      temp_df <- NULL
      if (file_ext == "csv") {
        temp_df <- self$read_csv(file)
      } else {
        temp_df <- self$read_excel(file)
      }

      if (length(self$get_numeric_columns(temp_df)) < 2) {
        stop("The uploaded file must contain at least two numeric variables.")
      }

      self$df <- temp_df
    }
  )
)

# Statick classmethod to summarize column
DataClass$summarize_column <- function(x) {
  if (is.numeric(x)) {
    return(c(
      Type = "Numeric",
      Mean = round(mean(x, na.rm = TRUE), 2),
      SD = round(sd(x, na.rm = TRUE), 2),
      Min = round(min(x, na.rm = TRUE), 2),
      Max = round(max(x, na.rm = TRUE), 2),
      Missing = sum(is.na(x))
    ))
  }
  if (is.factor(x) || is.character(x) || is.logical(x)) {
    return(c(
      Type = class(x)[1],
      Unique = length(unique(x)),
      Missing = sum(is.na(x))
    ))
  }
  # fallback
  c(Type = class(x)[1], Missing = sum(is.na(x)))
}


# Static Method to summary dataset
DataClass$summarize_dataset <- function(df) {
  stopifnot(is.data.frame(df))

  summary_list <- lapply(df, DataClass$summarize_column)

  summary_df <- do.call(rbind, summary_list) |>
    as.data.frame(stringsAsFactors = FALSE)

  summary_df$Variable <- rownames(summary_df)
  rownames(summary_df) <- NULL

  summary_df <- summary_df[, c(
    "Variable",
    setdiff(names(summary_df), "Variable")
  )]

  #round numeric columns
  dplyr::mutate(
    summary_df,
    dplyr::across(where(is.numeric), round, 2)
  )
}


# round numeric dataframes
DataClass$round_numeric_columns <- function(df) {
  stopifnot(is.data.frame(df))

  df[] <- lapply(df, function(x) {
    if (is.numeric(x)) {
      round(x, 3)
    } else {
      x
    }
  })

  return(df)
}
