#' Kolmogorov-Smirnov Test for a Theoretical Distribution
#'
#' This function performs a Kolmogorov-Smirnov test to assess whether a given 
#' numeric column in a data frame follows a specified theoretical distribution.
#'
#' @param data A data frame or tibble containing the data.
#' @param column A character string specifying the column name in `data` to be tested.
#' @param dist A character string specifying the theoretical distribution to test against. Defaults to "normal".
#' @param tidy_output Logical. If `TRUE`, returns a tidy data frame. If `FALSE`, returns the raw `ks.test` object. Defaults to `TRUE`.
#' @param verbose Logical. If `TRUE`, prints the estimated parameters of the fitted distribution. Defaults to `FALSE`.
#' @param signif_digits Integer or `NA`. The number of significant figures to round the results to. Defaults to 3. If `NA`, no rounding is applied.
#'
#' @return A tidy data frame (default) or the raw `ks.test` object, depending on `tidy_output`.
#'
#' @details The function uses the `fitdistr` function from the `MASS` package 
#'   to estimate the parameters of the specified distribution for the column. 
#'   It then applies the `ks.test` function to compare the empirical distribution 
#'   of the column to the fitted theoretical distribution. Results can be rounded 
#'   to a specified number of significant figures.
#'
#' @examples
#' # Example usage
#' library(dplyr)
#' set.seed(123)
#' data <- data.frame(values = rnorm(100, mean = 5, sd = 2))
#' ks_test_tidy(data, "values", signif_digits = 3)
#'
#' @importFrom MASS fitdistr
#' @importFrom broom tidy
#' @export
ks_test_tidy <- function(data, column, dist = "normal", tidy_output = TRUE, verbose = FALSE, signif_digits = 3) {
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
  
  # Map supported distributions to their CDF functions
  supported_distributions <- list(
    normal = "pnorm",
    exp = "pexp",
    poisson = "ppois",
    gamma = "pgamma",
    beta = "pbeta"
  )
  
  # Validate distribution
  if (!dist %in% names(supported_distributions)) {
    stop(sprintf("The distribution '%s' is not supported. Supported distributions: %s", 
                 dist, paste(names(supported_distributions), collapse = ", ")))
  }
  
  # Resolve the CDF function
  cdf_function <- match.fun(supported_distributions[[dist]])
  
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
    cdf_function, 
    mean = fit$estimate[1], 
    sd = fit$estimate[2]
  )
  
  # Convert to tidy output if required
  if (tidy_output) {
    tidy_result <- broom::tidy(ks_result)
    
    # Apply rounding if specified
    if (!is.na(signif_digits)) {
      tidy_result[] <- lapply(tidy_result, function(x) {
        if (is.numeric(x)) signif(x, signif_digits) else x
      })
    }
    
    return(tidy_result)
  } else {
    # Raw output without rounding
    return(ks_result)
  }
}
