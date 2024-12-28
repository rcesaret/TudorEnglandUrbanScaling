# Function for Kolmogorov-Smirnov Test
run_ks_test <- function(data, column) {
  fit <- fitdistr(data[[column]], "normal")$estimate
  ks_result <- ks.test(data[[column]], "pnorm", mean = fit[1], sd = fit[2])
  tidy(ks_result)
}