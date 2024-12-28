#' Perform Regression Analysis on Subsets of Data
#'
#' This function executes a regression analysis across subsets of a dataset
#' defined by specific conditions, calculates robust standard errors, 
#' and compiles results into summary tables.
#'
#' @param dataset A data frame containing the variables for analysis.
#' @param response A character string specifying the response variable.
#' @param predictors A character string or vector specifying the predictor variables.
#' @param conditions A matrix defining the conditions for subsetting the data. 
#'   Each column represents a condition.
#' @param group_columns A character vector specifying columns to group by for 
#'   the second analysis.
#'
#' @return A list containing:
#' \item{Table1}{Summary table for the first analysis.}
#' \item{WCS_Table}{Summary table for the second analysis.}
#'
#' @details This function runs linear regressions on subsets of the dataset,
#'   computes robust standard errors using the `sandwich` package, and calculates
#'   95% confidence intervals. It generates two summary tables analogous to the 
#'   outputs of the original snippets.
#'
#' @importFrom broom tidy
#' @importFrom lmtest coeftest
#' @importFrom sandwich vcovHC
#' @importFrom stats lm qt
#' @export
regression_analysis <- function(dataset, response, predictors, conditions, group_columns) {
  if (!is.data.frame(dataset)) stop("`dataset` must be a data frame.")
  if (!is.character(response) || !response %in% names(dataset)) stop("`response` must be a column name in `dataset`.")
  if (!is.character(predictors) || !all(predictors %in% names(dataset))) stop("`predictors` must be column names in `dataset`.")
  if (!is.matrix(conditions)) stop("`conditions` must be a matrix.")
  
  # Initialize results
  out <- data.frame()
  WCS_Table <- data.frame()
  
  # First analysis
  for (i in seq_len(ncol(conditions))) {
    subset_data <- subset(dataset, dataset$Problem < conditions[1, i] & dataset$Rural < conditions[2, i])
    x <- subset_data[[predictors[1]]]
    y <- subset_data[[response]]
    fm <- lm(y ~ x)
    
    coeftest_res <- lmtest::coeftest(fm, vcov = sandwich::vcovHC(fm, "HC1"))
    tidy_coeftest <- broom::tidy(coeftest_res)
    tidy_coeftest$term <- c("alpha", "beta")
    tidy_coeftest$estimate[1] <- exp(tidy_coeftest$estimate[1])
    
    analysis_label <- switch(
      as.character(c(conditions[1, i], conditions[2, i])),
      "2 2" = "All",
      "2 1" = "No Rural",
      "1 2" = "No Problem",
      "1 1" = "No Rural, No Problem"
    )
    
    tidy_coeftest$analysis <- rep(analysis_label, 2)
    tidy_coeftest$n <- rep(length(x), 2)
    tidy_coeftest$rsquared <- rep(summary(fm)$r.squared, 2)
    
    # Confidence intervals
    se <- sqrt(diag(sandwich::vcovHC(fm, "HC1")))
    tt <- qt(c(0.025, 0.975), df = summary(fm)$df[2])
    ci <- coef(fm) + outer(se, tt)
    tidy_coeftest$ci0.025 <- c(exp(ci[1, 1]), ci[2, 1])
    tidy_coeftest$ci0.975 <- c(exp(ci[1, 2]), ci[2, 2])
    
    out <- rbind(out, tidy_coeftest)
  }
  
  # Summary Table1
  analysis_num <- rep(1:ncol(conditions), each = 2)
  Table1 <- data.frame(
    analysis_num = analysis_num[c(TRUE, FALSE)],
    Subset = out$analysis[c(TRUE, FALSE)],
    n = out$n[c(TRUE, FALSE)],
    R_squared = out$rsquared[c(TRUE, FALSE)],
    beta_estimate = out$estimate[c(FALSE, TRUE)],
    beta_ci_0.025 = out$ci0.025[c(FALSE, TRUE)],
    beta_ci_0.975 = out$ci0.975[c(FALSE, TRUE)],
    alpha_estimate = out$estimate[c(TRUE, FALSE)],
    alpha_ci_0.025 = out$ci0.025[c(TRUE, FALSE)],
    alpha_ci_0.975 = out$ci0.975[c(TRUE, FALSE)]
  )
  
  # Second analysis
  for (j in seq_along(group_columns)) {
    group <- group_columns[j]
    for (i in seq_len(ncol(conditions))) {
      subset_data <- subset(dataset, dataset$Problem < conditions[1, i] & dataset$Rural < conditions[2, i])
      x <- log(subset_data[[group]])
      y <- log(subset_data[[response]])
      
      fm <- lm(y ~ x)
      coeftest_res <- lmtest::coeftest(fm, vcov = sandwich::vcovHC(fm, "HC1"))
      tidy_coeftest <- broom::tidy(coeftest_res)
      
      tidy_coeftest$term <- c("alpha", "beta")
      tidy_coeftest$estimate[1] <- exp(tidy_coeftest$estimate[1])
      
      analysis_label <- switch(
        as.character(c(conditions[1, i], conditions[2, i])),
        "2 2" = "All",
        "2 1" = "No Rural",
        "1 2" = "No Problem",
        "1 1" = "No Rural, No Problem"
      )
      
      tidy_coeftest$analysis <- rep(analysis_label, 2)
      tidy_coeftest$n <- rep(length(x), 2)
      tidy_coeftest$rsquared <- rep(summary(fm)$r.squared, 2)
      
      # Confidence intervals
      se <- sqrt(diag(sandwich::vcovHC(fm, "HC1")))
      tt <- qt(c(0.025, 0.975), df = summary(fm)$df[2])
      ci <- coef(fm) + outer(se, tt)
      tidy_coeftest$ci0.025 <- c(exp(ci[1, 1]), ci[2, 1])
      tidy_coeftest$ci0.975 <- c(exp(ci[1, 2]), ci[2, 2])
      
      tidy_coeftest$WCS_analysis <- group
      WCS_Table <- rbind(WCS_Table, tidy_coeftest)
    }
  }
  
  return(list(Table1 = Table1, WCS_Table = WCS_Table))
}
