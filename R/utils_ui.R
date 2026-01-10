#' ui 
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd
get_operators <- function() {
  c(
    "Less than (<)" = "<",
    "Less than or equal to (≤)" = "<=",
    "Greater than or equal to (≥)" = ">=",
    "Greater than (>)" = ">"
  )
}

apply_threshold <- function(column, threshold, operator) {
  switch(operator,
    ">" = column > threshold,
    "<" = column < threshold,
    ">=" = column >= threshold,
    "<=" = column <= threshold,
    stop("Invalid operator. Must be one of: '<', '>', '<=', '>='")
  )
}

#' ci_methods
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
get_available_ci_methods <- function() {
  c(
    "Clopper-Pearson (Exact)" = "clopper_pearson",
    "Wilson Score" = "wilson",
    "Wald (Normal Approximation)" = "wald",
    "Agresti-Coull" = "agresti_coull"
  )
}