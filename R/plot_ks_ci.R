#' Generate a Custom ECDF Plot with Normal Comparison
#'
#' This function creates an ECDF plot and overlays it with the ECDF of a normal
#' distribution fitted to the data. The plot can be saved to a specified file or 
#' plotted directly to an active graphics device.
#'
#' @param data A numeric vector for which the ECDF plot is generated.
#' @param main_title A character string. The main title of the plot. Defaults to "K-S Test for Normality".
#' @param x_label A character string. The label for the x-axis. Defaults to "X"
#' @param y_label A character string. The label for the y-axis. Defaults to "ECDF(X)"
#' @param file_name A character string specifying the file path to save the plot, or `NULL` to plot directly.
#'
#' @return If `file_name` is specified, the plot is saved to the file. Otherwise, it is plotted directly.
#'
#' @details The function uses the base R graphics engine to generate an ECDF plot
#' and overlays it with the ECDF of a normal distribution fitted to the data.
#'
#' @examples
#' # Example usage
#' set.seed(123)
#' test_data <- rnorm(100, mean = 5, sd = 1)
#' plot_ks_ci(
#'   data = test_data,
#'   main_title = "(b)",
#'   x_label = "Values",
#'   y_label = "ECDF",
#'   file_name = NULL # Plot directly
#' )
#' @importFrom stats optim
#' @importFrom MASS fitdistr
#' @importFrom NSM3 ecdf.ks.CI
#' 
#' @export
plot_ks_ci <- function(data, 
                       main_title = "K-S Test for Normality", 
                       x_label = "X", 
                       y_label = "ECDF(X)",
                       file_name = NULL) {
  
  # Validate inputs
  if (!is.numeric(data)) {
    stop("`data` must be a numeric vector.")
  }
  
  if (!is.null(file_name)) {
    # Open a graphics device if file_name is specified
    if (!is.character(file_name) || nchar(file_name) == 0) {
      stop("`file_name` must be a non-empty character string if specified.")
    }
    png(file_name, width = 800, height = 600)
    on.exit(dev.off()) # Close the device when finished
  }
  
  # Set graphical parameters to decrease distance of axis titles and labels from plot
  par(mgp = c(2, 0.75, 0))
  
  # Create the ECDF plot without altering global `par` settings
  NSM3::ecdf.ks.CI(data, main = main_title, cex.main = 2, font.main = 2, 
                   font.lab = 2, xlab = x_label, ylab = y_label)
  
  # Overlay ECDF of a fitted normal distribution
  fit <- MASS::fitdistr(data, "normal")$estimate
  norm_samples <- rnorm(10000, mean = fit["mean"], sd = fit["sd"])
  lines(ecdf(norm_samples), do.points = FALSE, verticals = TRUE, col = "blue", lwd = 2)
  
  invisible(NULL)
  
  # Reset graphical parameters to default
  par(mgp = c(3, 1, 0))
}
