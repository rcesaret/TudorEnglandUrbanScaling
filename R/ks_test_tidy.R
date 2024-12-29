#' Perform Kolmogorov-Smirnov Tests on Multiple Dataframe Columns
#'
#' This function performs Kolmogorov-Smirnov (KS) tests on one or more numeric columns of a dataframe.
#' It fits a specified distribution to each column and tests the goodness-of-fit using the KS test.
#' The results can be returned as a list of KS test objects, a tidy tibble, or a formatted gt table.
#'
#' @param data A dataframe or tibble containing the data.
#' @param columns A single column name or a vector of column names to test. Columns must be numeric.
#' @param dist A character string specifying the distribution to fit. Supported distributions are
#'   `"normal"`, `"exp"`, `"poisson"`, `"gamma"`, and `"beta"`. Default is `"normal"`.
#' @param alpha A numeric value specifying the significance level for the test. Default is `0.05`.
#' @param output A character string specifying the type of output. Options are `"ks"`, `"tibble"`, and `"gt"`.
#'   - `"ks"`: Returns a named list of KS test results.
#'   - `"tibble"`: Returns a tidy tibble with test results.
#'   - `"gt"`: Returns a formatted gt table with test results.
#'   Default is `"gt"`.
#' @param verbose A logical indicating whether to print fitted parameters. Default is `FALSE`.
#' @param signif_digits An integer specifying the number of significant digits for numerical outputs. Default is `3`.
#'
#' @return Depending on the `output` parameter:
#'   - `"ks"`: A named list where each element is a `htest` object from the KS test.
#'   - `"tibble"`: A tibble containing the test results for each variable.
#'   - `"gt"`: A gt table containing the formatted test results for each variable.
#'
#' @examples
#' \dontrun{
#' # Example with multiple columns
#' ks_test_tidy(data = mtcars, 
#'             columns = c("mpg", "hp"), 
#'             dist = "normal", 
#'             output = "tibble", 
#'             verbose = TRUE)
#' }
#'
#' @importFrom MASS fitdistr
#' @importFrom broom tidy
#' @importFrom dplyr rename mutate case_when select bind_rows if_else
#' @importFrom gt gt cols_label fmt_number cols_align tab_style cells_column_labels cell_text
#' @export
ks_test_tidy <- function(data, 
                         columns, 
                         dist = "normal", 
                         alpha = 0.05,
                         output = "gt", 
                         verbose = FALSE, 
                         signif_digits = 3) {
  
  # Check for required packages
  required_packages <- c("MASS", "broom", "dplyr", "gt", "stats")
  missing_packages <- required_packages[!sapply(required_packages, requireNamespace, quietly = TRUE)]
  if(length(missing_packages)) {
    stop("The following packages are required but not installed: ",
         paste(missing_packages, collapse = ", "), 
         ". Please install them before running this function.")
  }
  
  # Validate inputs
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame or tibble.")
  }
  
  if (!all(columns %in% names(data))) {
    missing_cols <- columns[!columns %in% names(data)]
    stop(sprintf("The following columns are not found in the data: %s.", 
                 paste(missing_cols, collapse = ", ")))
  }
  
  non_numeric_cols <- columns[!sapply(data[columns], is.numeric)]
  if (length(non_numeric_cols) > 0) {
    stop(sprintf("The following columns are not numeric: %s.", 
                 paste(non_numeric_cols, collapse = ", ")))
  }
  
  cols_with_na <- columns[sapply(data[columns], function(col) any(is.na(col)))]
  if (length(cols_with_na) > 0) {
    stop(sprintf("The following columns contain missing values. Please remove or impute them before running the test: %s.", 
                 paste(cols_with_na, collapse = ", ")))
  }
  
  ok_values <- c("tibble", "gt", "ks")
  if (!output %in% ok_values) {
    stop(sprintf("Output type '%s' is not supported. Supported types: %s", 
                 output, paste(ok_values, collapse = ", ")))
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
  
  # Initialize containers for results
  ks_list <- list()
  tidy_results <- dplyr::tibble()
  
  # Iterate over each column
  for(col in columns) {
    # Fit the specified distribution to the column
    fit <- tryCatch(
      MASS::fitdistr(data[[col]], dist),
      error = function(e) stop(sprintf("Failed to fit the distribution for column '%s': %s", col, e$message))
    )
    
    if (verbose) {
      message(sprintf("Fitted parameters for '%s': ", col))
      print(fit$estimate)
    }
    
    # Prepare parameters for ks.test
    params <- as.list(fit$estimate)
    
    # Perform Kolmogorov-Smirnov test using do.call
    ks_result <- tryCatch(
      do.call(stats::ks.test, c(list(x = data[[col]], y = cdf_function), params)),
      error = function(e) stop(sprintf("KS test failed for column '%s': %s", col, e$message))
    )
    
    # Store KS result
    ks_list[[col]] <- ks_result
    
    # Tidy the KS result
    a <- alpha
    tidy_result <- broom::tidy(ks_result) %>% 
      dplyr::rename(alpha = method) %>%
      dplyr::mutate(n = length(data[[col]]),
                    alpha = a,
                    dist = dist,
                    dist = dplyr::case_when(
                      dist == "normal" ~ "Gaussian",
                      dist == "exp" ~ "Exponential",
                      dist == "poisson" ~ "Poisson",
                      dist == "gamma" ~ "Gamma",
                      dist == "beta" ~ "Beta"
                    ),
                    result = dplyr::if_else(p.value < alpha, "Reject", "Fail to Reject"),
                    alternative = dplyr::if_else(alternative == "two-sided", "Two-sided", "One-sided"),
                    var = col) %>% 
      dplyr::select(var, n, dist, statistic, p.value, alpha, alternative, result)
    
    tidy_results <- dplyr::bind_rows(tidy_results, tidy_result)
  }
  
  # Generate output based on the 'output' parameter
  if (output == "ks") {
    return(ks_list)
  }
  
  if (output == "tibble") {
    return(tidy_results)
  }
  
  if (output == "gt") {
    gt_result <- tidy_results %>%
      gt::gt() %>%
      gt::cols_label(
        var = "Variable",
        n = html("Sample<br>Size"),
        dist = "Distribution",
        statistic = "D Statistic",
        p.value = "P-Value",
        alpha = "Alpha",
        alternative = html("Alternative<br>Hypothesis"),
        result = "Result"
      ) %>%
      gt::fmt_number(
        columns = c("statistic", "p.value"),
        decimals = signif_digits
      ) %>%
      gt::cols_align(
        align = "center",
        columns = c("var", "n", "dist", "statistic", "p.value", "alpha", "alternative", "result")
      ) %>%
      gt::tab_style(
        style = gt::cell_text(weight = "bold", align = "center", v_align = "middle"),
        locations = gt::cells_column_labels(
          columns = c("var", "n", "dist", "statistic", "p.value", "alpha", "alternative", "result")
        )
      )
    
    return(gt_result)
  }
}
