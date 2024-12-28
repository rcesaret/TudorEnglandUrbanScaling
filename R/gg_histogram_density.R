#' Histogram with Density Overlay using ggplot2
#'
#' This function creates a histogram with an overlaid density plot, including
#' options for customizing colors, labels, and themes.
#'
#' @param data A numeric vector. The dataset column for which the histogram and density are generated.
#' @param bins Number of bins for the histogram. Defaults to 30.
#' @param x_label A character string. The label for the x-axis.
#' @param y_label A character string. The label for the y-axis.
#' @param main_title A character string. The main title of the plot.
#' @param fill_color A character string. The fill color of the histogram. Defaults to `"gray70"`.
#' @param density_color A character string. The fill color of the density plot. Defaults to `"blue"`.
#' @param line_color A character string. The line color of the fitted normal curve. Defaults to `"red"`.
#'
#' @return A `ggplot` object representing the histogram with a density overlay.
#'
#' @details The function uses `ggplot2` to create a histogram, overlay a density plot,
#' and optionally include a fitted normal curve.
#'
#' @examples
#' # Example usage
#' set.seed(123)
#' test_data <- rnorm(100, mean = 5, sd = 1)
#' gg_histogram_density(
#'   data = test_data,
#'   bins = 20,
#'   x_label = "Values",
#'   y_label = "Density",
#'   main_title = "Histogram with Density"
#' )
#'
#' @importFrom ggplot2 ggplot geom_histogram aes geom_density geom_line labs theme_minimal theme element_text
#' @importFrom MASS fitdistr
#' @export
gg_histogram_density <- function(data, bins = 30, x_label = "X-axis", y_label = "Density", 
                                 main_title = "Histogram with Density", 
                                 fill_color = "gray70", density_color = "blue", line_color = "red") {
  # Validate inputs
  if (!is.numeric(data)) {
    stop("`data` must be a numeric vector.")
  }
  
  # Fit normal distribution
  fit <- MASS::fitdistr(data, "normal")$estimate
  
  # Create ggplot
  p <- ggplot(data = data.frame(x = data), aes(x = x)) +
    geom_histogram(aes(y = ..density..), bins = bins, fill = fill_color, color = "black", alpha = 0.7) +
    geom_density(color = density_color, fill = density_color, alpha = 0.3) +
    geom_line(aes(x = seq(min(data), max(data), length.out = 100), 
                  y = dnorm(seq(min(data), max(data), length.out = 100), mean = fit[1], sd = fit[2])),
              color = line_color, size = 1) +
    labs(title = main_title, x = x_label, y = y_label) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      axis.title = element_text(size = 12)
    )
  
  return(p)
}
