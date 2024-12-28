#' Generate a Custom Histogram with Density Plot
#'
#' This function creates a histogram with an overlaid density plot, including
#' labeled density and frequency axes, and optionally saves the plot to a specified file.
#'
#' @param data A numeric vector. The dataset column for which the histogram and density are generated.
#' @param breaks A numeric vector specifying the breakpoints for the histogram bins.
#' @param main_title A character string. The main title of the plot.
#' @param x_label A character string. The label for the x-axis.
#' @param y_label A character string. The label for the y-axis.
#' @param file_name A character string specifying the file path to save the plot, or `NULL` to plot directly.
#' @param fig_letter A character string. A label for the figure (e.g., "(a)" or "(b)").
#'
#' @return If `file_name` is specified, the plot is saved to the file. Otherwise, it is plotted directly.
#'
#' @details The function uses the base R graphics engine to generate a histogram
#' and overlays a density plot and a normal distribution curve fitted to the data.
#'
#' @examples
#' # Example usage
#' set.seed(123)
#' test_data <- rnorm(100, mean = 5, sd = 1)
#' histogram_density(
#'   data = test_data,
#'   breaks = seq(2, 8, 0.5),
#'   main_title = "(a)",
#'   x_label = "Values",
#'   y_label = "Frequency",
#'   file_name = NULL, # Plot directly
#'   fig_letter = "(a)"
#' )
#'
#' @importFrom stats optim
#' @importFrom MASS fitdistr
#' @export
histogram_density <- function(data, breaks, main_title, x_label, y_label, file_name = NULL, fig_letter) {
  # Validate inputs
  if (!is.numeric(data)) {
    stop("`data` must be a numeric vector.")
  }
  
  # Create the histogram
  hist_data <- hist(data, breaks = breaks, plot = FALSE)
  freq <- round(pretty(hist_data$density, n = 6) * length(data) * diff(hist_data$breaks[1:2]))
  
  if (!is.null(file_name)) {
    if (!is.character(file_name) || nchar(file_name) == 0) {
      stop("`file_name` must be a non-empty character string if specified.")
    }
    # Open a graphics device
    png(file_name, width = 800, height = 600)
  }
  
  # Plot the histogram
  graphics:::plot.histogram(hist_data, freq = FALSE, col = "gray70", family = "sans",
                            main = main_title, cex.main = 2, font.main = 2, font.lab = 2,
                            xlab = x_label, ylab = y_label, border = "black", yaxt = "n")
  
  # Add axes and labels
  Axis(side = 2, at = pretty(hist_data$density, n = 6), labels = freq)
  Axis(side = 4, at = pretty(hist_data$density, n = 6), labels = pretty(hist_data$density, n = 6))
  mtext("Density", side = 4, line = 3, family = "sans", font = 2)
  
  # Overlay density plot and fitted normal curve
  polygon(density(data), col = rgb(0, 0, 1, 0.3))
  fit <- MASS::fitdistr(data, "normal")$estimate
  x_fit <- seq(min(data) - 1, max(data) + 1, length.out = 50)
  y_fit <- dnorm(x_fit, mean = fit["mean"], sd = fit["sd"])
  lines(x_fit, y_fit, col = "red", lwd = 2)
  
  # Close the graphics device if file_name is specified
  if (!is.null(file_name)) {
    dev.off()
  }
}
