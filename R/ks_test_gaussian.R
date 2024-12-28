#' Kolmogorov-Smirnov Test for Normality
#'
#' This function performs a Kolmogorov-Smirnov test to assess whether a given 
#' numeric column in a data frame follows a normal distribution.
#'
#' @param data A data frame or tibble containing the data.
#' @param column A character string specifying the column name in `data` to be tested.
#' @param dist A character string specifying the theoretical distribution to test against. Defaults to "normal".
#' @param tidy_output Logical. If `TRUE`, returns a tidy data frame. If `FALSE`, returns the raw `ks.test` object. Defaults to `TRUE`.
#' @param verbose Logical. If `TRUE`, prints the estimated parameters of the fitted distribution. Defaults to `FALSE`.
#'
#' @return A tidy data frame (default) or the raw `ks.test` object, depending on `tidy_output`.
#'
#' @details The function uses the `fitdistr` function from the `MASS` package 
#'   to estimate the parameters of the specified distribution for the column. 
#'   It then applies the `ks.test` function to compare the empirical distribution 
#'   of the column to the fitted theoretical distribution.
#'
#' @examples
#' # Example usage
#' library(dplyr)
#' set.seed(123)
#' data <- data.frame(values = rnorm(100, mean = 5, sd = 2))
#' ks_test_gaussian(data, "values")
#'
#' @importFrom MASS fitdistr
#' @importFrom broom tidy
#' @export
ks_test_gaussian <- function(data, column, dist = "normal", tidy_output = TRUE, verbose = FALSE) {
  # Validate inputs
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame or tibble.")
  }
  if (!column %in% names(data)) {
    stop(sprintf("Column '%s' not found in the data.", column))
  }
  if (!is.numeric(data[[column]])) {
    stop(sprintf("Column '%s' must contain numeric values.", column))
  }
  if (anyNA(data[[column]])) {
    stop(sprintf("Column '%s' contains missing values. Please remove or impute them before running the test.", column))
  }
  
  # Fit the specified distribution to the column
  fit <- tryCatch(
    MASS::fitdistr(data[[column]], dist),
    error = function(e) stop("Failed to fit the distribution: ", e$message)
  )
  
  if (verbose) {
    message(sprintf("Fitted parameters: mean = %.2f, sd = %.2f", fit$estimate[1], fit$estimate[2]))
  }
  
  # Perform Kolmogorov-Smirnov test
  ks_result <- stats::ks.test(
    data[[column]], 
    paste0("p", dist), 
    mean = fit$estimate[1], 
    sd = fit$estimate[2]
  )
  
  # Return results
  if (tidy_output) {
    return(broom::tidy(ks_result))
  } else {
    return(ks_result)
  }
}
