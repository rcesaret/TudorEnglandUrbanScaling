#' ECDF with Fitted Normal Overlay using ggplot2
#'
#' This function creates an ECDF plot with an overlay of the ECDF for a normal distribution
#' fitted to the data, including options for customizing colors, labels, and themes.
#'
#' @param data A numeric vector. The dataset column for which the ECDF plot is generated.
#' @param x_label A character string. The label for the x-axis.
#' @param y_label A character string. The label for the y-axis.
#' @param main_title A character string. The main title of the plot.
#' @param line_color A character string. The line color for the fitted normal ECDF. Defaults to `"blue"`.
#'
#' @return A `ggplot` object representing the ECDF with a normal overlay.
#'
#' @details The function uses `ggplot2` to create an ECDF plot and overlay the ECDF
#' of a normal distribution fitted to the data.
#'
#' @examples
#' # Example usage
#' set.seed(123)
#' test_data <- rnorm(100, mean = 5, sd = 1)
#' gg_ks_ecdf_ci_fit(
#'   data = test_data,
#'   x_label = "Values",
#'   y_label = "ECDF",
#'   main_title = "ECDF with Fitted Normal Overlay"
#' )
#'
#' @importFrom ggplot2 ggplot stat_ecdf geom_line aes labs theme_minimal theme element_text
#' @importFrom MASS fitdistr
#' @export
gg_ks_ecdf_ci_fit <- function(data, x_label = "X-axis", y_label = "ECDF", 
                              main_title = "ECDF with Fitted Normal Overlay", 
                              line_color = "blue") {
  # Validate inputs
  if (!is.numeric(data)) {
    stop("`data` must be a numeric vector.")
  }
  
  # Fit normal distribution
  fit <- MASS::fitdistr(data, "normal")$estimate
  norm_samples <- rnorm(10000, mean = fit[1], sd = fit[2])
  
  # Create ggplot
  p <- ggplot(data = data.frame(x = data), aes(x = x)) +
    stat_ecdf(geom = "step", size = 1, aes(color = "Empirical")) +
    geom_line(data = data.frame(x = sort(norm_samples), y = seq(0, 1, length.out = length(norm_samples))), 
              aes(x = x, y = y, color = "Fitted Normal"), size = 1) +
    labs(title = main_title, x = x_label, y = y_label, color = "") +
    scale_color_manual(values = c("Empirical" = "black", "Fitted Normal" = line_color)) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      axis.title = element_text(size = 12),
      legend.position = "top"
    )
  
  return(p)
}
