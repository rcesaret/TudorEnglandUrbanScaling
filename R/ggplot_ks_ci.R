# Load necessary libraries
library(ggplot2)
library(MASS)

#' Perform K-S Test for Normality and Generate ECDF Plot with Fitted Normal Overlay
#'
#' This function performs the Kolmogorov-Smirnov (K-S) test to assess the normality of a numeric dataset.
#' It generates a plot displaying the Empirical Cumulative Distribution Function (ECDF), the fitted normal
#' distribution, and confidence intervals based on the specified confidence level.
#'
#' @param x A numeric vector. The dataset to be tested for normality.
#' @param CI A numeric value between 0 and 1. The confidence level for the confidence intervals. Defaults to `0.95`.
#' @param x_label A character string. The label for the x-axis. Defaults to `"Value"`.
#' @param y_label A character string. The label for the y-axis. If `NULL`, defaults to `"Cumulative P(Value)"`. Defaults to `NULL`.
#' @param title_lab A character string. The main title of the plot. Defaults to `"K-S Test for Normality"`.
#' @param subtitle_lab A character string. The subtitle of the plot. If `NULL`, defaults to `"ECDF Plot with {CI*100}% C.I."`. Defaults to `NULL`.
#' @param title_size A numeric value. Font size for the main title. Defaults to `14`.
#' @param title_face A character string. Font face for the main title (e.g., `"bold"`, `"plain"`). Defaults to `"bold"`.
#' @param subtitle_size A numeric value. Font size for the subtitle. Defaults to `12`.
#' @param subtitle_face A character string. Font face for the subtitle (e.g., `"bold"`, `"plain"`). Defaults to `"bold"`.
#' @param subtitle_justification A numeric value between 0 and 1. Horizontal justification for the subtitle. Defaults to `0`.
#' @param title_justification A numeric value between 0 and 1. Horizontal justification for the title. Defaults to `0`.
#' @param line_color_normal A character string. Color for the fitted normal distribution line. Defaults to `"blue"`.
#' @param line_color_ecdf A character string. Color for the empirical ECDF line. Defaults to `"black"`.
#' @param line_color_CI A character string. Color for the confidence interval lines. Defaults to `"red"`.
#' @param line_width_normal A positive numeric value. Line width for the fitted normal distribution. Defaults to `1`.
#' @param line_width_ecdf A positive numeric value. Line width for the empirical ECDF. Defaults to `1`.
#' @param line_width_CI A positive numeric value. Line width for the confidence interval lines. Defaults to `1`.
#' @param legend_position A string or numeric vector. Position of the legend. Options include `"left"`, `"right"`, `"bottom"`, `"top"`,`"none"`, or `"inside"`. Defaults to `"inside"`.
#' @param legend_coord If `legend_position == "inside"`, then the legend location must by specify its relative interior coordinates by specifying `legend_coord` as a numeric vector of length 2. Defaults to `c(0.8, 0.3)` (lower right quadrant).
#' @param legend_box A logical value. Whether to display the legend box. Defaults to `TRUE`.
#' @param legend_background A character string. Background color for the legend. Defaults to `"white"`.
#' @param legend_box_outline A logical value. Whether to display the legend box outline. Defaults to `TRUE`.
#' @param legend_text_size A positive numeric value. Font size for the legend text. Defaults to `11`.
#' @param legend_text_face A character string. Font face for the legend text (e.g., `"bold"`, `"plain"`). Defaults to `"bold"`.
#' @param annotation_x A numeric value. X-coordinate for the annotation text. Defaults to the minimum of `x`.
#' @param annotation_y A numeric value. Y-coordinate for the annotation text. Defaults to `0.7`.
#' @param annotation_size A positive numeric value. Font size for the annotation text. Defaults to `3.5`.
#' @param annotation_font_face A character string. Font face for the annotation text (e.g., `"plain"`, `"bold"`). Defaults to `"plain"`.
#'
#' @return A `ggplot` object representing the ECDF with a fitted normal distribution overlay and confidence intervals.
#'
#' @details The function conducts a K-S test to evaluate whether the input data `x` follows a normal distribution.
#' It then visualizes the empirical distribution alongside the theoretical normal distribution and highlights
#' the confidence intervals. The plot includes detailed annotations of the test statistics and p-value.
#'
#' @examples
#' \dontrun{
#' # Example usage
#' set.seed(123)
#' test_data <- rnorm(100, mean = 5, sd = 1)
#' gg_ksci(
#'   x = test_data,
#'   CI = 0.95,
#'   x_label = "Test Values",
#'   y_label = "Cumulative Probability",
#'   title_lab = "K-S Test for Normality",
#'   subtitle_lab = "Empirical vs. Theoretical Distribution",
#'   title_size = 16,
#'   title_face = "bold",
#'   subtitle_size = 13,
#'   subtitle_face = "bold",
#'   subtitle_justification = 0,
#'   title_justification = 0,
#'   legend_position = "inside",
#'   legend_coord = c(0.8, 0.3),
#'   legend_box = TRUE,
#'   legend_background = "white",
#'   legend_box_outline = TRUE,
#'   legend_text_size = 11,
#'   legend_text_face = "bold",
#'   annotation_x = min(test_data),
#'   annotation_y = 0.95,
#'   annotation_size = 4,
#'   annotation_font_face = "italic"
#' )
#' }
#'
#' @importFrom ggplot2 ggplot geom_line stat_ecdf geom_step annotate labs scale_color_manual theme_minimal theme element_text element_rect
#' @importFrom MASS fitdistr
#' @export
ggplot_ks_ci <- function(x, 
                         CI = 0.95, 
                         x_label = "Value", 
                         y_label = NULL,
                         title_lab = "K-S Test for Normality",
                         subtitle_lab = NULL,
                         title_size = 14,
                         title_face = "bold",
                         subtitle_size = 12,
                         subtitle_face = "bold",
                         subtitle_justification = 0,
                         title_justification = 0,
                         line_color_normal = "blue",
                         line_color_ecdf = "black",
                         line_color_CI = "red",
                         line_width_normal = 1,
                         line_width_ecdf = 1,
                         line_width_CI = 1,
                         legend_position = "inside",
                         legend_coord = c(0.8, 0.3),
                         legend_box = TRUE,
                         legend_background = "white",
                         legend_box_outline = TRUE,
                         legend_text_size = 11,
                         legend_text_face = "bold",
                         annotation_x = NULL,
                         annotation_y = 0.7,
                         annotation_size = 3.5,
                         annotation_font_face = "bold") {
  
  # ----------------------------#
  # 1. Input Validations         #
  # ----------------------------#
  
  # Check if x is a numeric vector
  if (!is.numeric(x)) {
    stop("`x` must be a numeric vector.")
  }
  
  # Check if CI is numeric and between 0 and 1
  if (!is.numeric(CI) || length(CI) != 1 || CI <= 0 || CI >= 1) {
    stop("`CI` must be a single numeric value between 0 and 1.")
  }
  
  # Check if labels are character strings
  if (!is.character(x_label) || length(x_label) != 1) {
    stop("`x_label` must be a single character string.")
  }
  
  if (!is.null(y_label) && (!is.character(y_label) || length(y_label) != 1)) {
    stop("`y_label` must be a single character string or NULL.")
  }
  
  if (!is.character(title_lab) || length(title_lab) != 1) {
    stop("`title_lab` must be a single character string.")
  }
  
  if (!is.null(subtitle_lab) && (!is.character(subtitle_lab) || length(subtitle_lab) != 1)) {
    stop("`subtitle_lab` must be a single character string or NULL.")
  }
  
  # Check if font sizes are positive numerics
  if (!is.numeric(title_size) || title_size <= 0) {
    stop("`title_size` must be a positive numeric value.")
  }
  
  if (!is.numeric(subtitle_size) || subtitle_size <= 0) {
    stop("`subtitle_size` must be a positive numeric value.")
  }
  
  if (!is.numeric(line_width_normal) || line_width_normal <= 0) {
    stop("`line_width_normal` must be a positive numeric value.")
  }
  
  if (!is.numeric(line_width_ecdf) || line_width_ecdf <= 0) {
    stop("`line_width_ecdf` must be a positive numeric value.")
  }
  
  if (!is.numeric(line_width_CI) || line_width_CI <= 0) {
    stop("`line_width_CI` must be a positive numeric value.")
  }
  
  if (!is.numeric(annotation_size) || annotation_size <= 0) {
    stop("`annotation_size` must be a positive numeric value.")
  }
  
  # Check if legend_position is valid
  valid_positions <- c("left", "right", "bottom", "top", "inside", "none")
  if (!(all(legend_position %in% valid_positions))) {
    stop("`legend_position` must be one of 'left', 'right', 'bottom', 'top', 'inside', or 'none'")
  }
  
  # Check if legend_coord is valid
  if (legend_position == "inside" && (!is.numeric(legend_coord) || length(legend_coord) != 2)) {
    stop("if `legend_position` == 'inside' then legend_coord must be a numeric vector of length 2.")
  }
    
  # Check if legend_box is logical
  if (!is.logical(legend_box) || length(legend_box) != 1) {
    stop("`legend_box` must be a single logical value (`TRUE` or `FALSE`).")
  }
  
  # Check if legend_box_outline is logical
  if (!is.logical(legend_box_outline) || length(legend_box_outline) != 1) {
    stop("`legend_box_outline` must be a single logical value (`TRUE` or `FALSE`).")
  }
  
  # Check if legend_text_size is a positive numeric
  if (!is.numeric(legend_text_size) || legend_text_size <= 0) {
    stop("`legend_text_size` must be a positive numeric value.")
  }
  
  # Check if legend_text_face is a character string
  if (!is.character(legend_text_face) || length(legend_text_face) != 1) {
    stop("`legend_text_face` must be a single character string.")
  }
  
  # Check if subtitle_justification and title_justification are between 0 and 1
  if (!is.numeric(subtitle_justification) || length(subtitle_justification) != 1 ||
      subtitle_justification < 0 || subtitle_justification > 1) {
    stop("`subtitle_justification` must be a single numeric value between 0 and 1.")
  }
  
  if (!is.numeric(title_justification) || length(title_justification) != 1 ||
      title_justification < 0 || title_justification > 1) {
    stop("`title_justification` must be a single numeric value between 0 and 1.")
  }
  
  # Check if annotation_x is numeric if provided
  if (!is.null(annotation_x) && (!is.numeric(annotation_x) || length(annotation_x) != 1)) {
    stop("`annotation_x` must be a single numeric value or NULL.")
  }
  
  # ----------------------------#
  # 2. Handle Missing Data      #
  # ----------------------------#
  
  # Identify missing values
  missing_count <- sum(is.na(x) | is.nan(x))
  
  if (missing_count > 0) {
    warning(sprintf("Removed %d missing values (NA or NaN) from `x`.", missing_count))
    # Remove missing values
    x <- x[!is.na(x) & !is.nan(x)]
  }
  
  # Check if x has sufficient data after removal
  if (length(x) < 2) {
    stop("`x` must contain at least two non-missing numeric values.")
  }
  
  # ----------------------------#
  # 3. Compute K-S Test         #
  # ----------------------------#
  
  # Fit normal distribution parameters
  fit <- MASS::fitdistr(x, "normal")$estimate
  mean_fit <- fit["mean"]
  sd_fit <- fit["sd"]
  
  # Perform the K-S test
  ks_test <- ks.test(x, "pnorm", mean = mean_fit, sd = sd_fit, exact = FALSE)
  
  # ----------------------------#
  # 4. Prepare Data for Plotting#
  # ----------------------------#
  
  # Create data frame for empirical ECDF
  ecdf_data <- ecdf(x)
  sorted_x <- sort(x)
  ecdf_values <- ecdf_data(sorted_x)
  ecdf_df <- data.frame(x = sorted_x, y = ecdf_values)
  
  # Create data frame for theoretical normal distribution
  theoretical_df <- data.frame(
    x = sorted_x,
    y = pnorm(sorted_x, mean = mean_fit, sd = sd_fit)
  )
  
  # Compute critical value for K-S test
  approx_ksD <- function(n, alpha) {
    q_alpha <- qnorm(1 - alpha / 2)
    if (n > 80) {
      q_alpha / (sqrt(n) + 0.12 + 0.11 / sqrt(n))
    } else {
      splinefun(
        x = c(1:9, 10, 15, 20, 25, 30, 40, 50, 60, 70, 80),
        y = c(0.975, 0.84189, 0.70760, 0.62394, 0.56328,
              0.51926, 0.48342, 0.45427, 0.43001, 0.40925,
              0.33760, 0.29408, 0.24170, 0.21012, 0.18841,
              0.17231, 0.15975, 0.14960))(n) * q_alpha / 1.358
    }
  }
  
  alpha <- 1 - CI
  D_critical <- approx_ksD(length(x), alpha)
  
  # Calculate confidence intervals
  upper_ci <- pmin(ecdf_df$y + D_critical, 1)
  lower_ci <- pmax(ecdf_df$y - D_critical, 0)
  
  # Determine hypothesis result
  hypothesis_result <- ifelse(ks_test$p.value < alpha, "Rejected", "Not Rejected")
  
  # Prepare annotation text
  annotation_text <- sprintf(
    "K-S Test for Normality\nD = %.3f   p = %.3f   n = %d\nH0: %s at α = %.2f",
    ks_test$statistic,
    ks_test$p.value,
    length(x),
    hypothesis_result,
    alpha
  )
  
  # Set default y_label if not provided
  if (is.null(y_label)) {
    y_label <- sprintf("Cumulative P(%s)", x_label)
  }
  
  # Set default annotation_x if not provided
  if (is.null(annotation_x)) {
    annotation_x <- min(sorted_x)
  }
  
  # Set default subtitle_lab if not provided
  if (is.null(subtitle_lab)) {
    subtitle_lab <- sprintf("ECDF Plot with %.0f%% C.I.", CI * 100)
  }
  
  if (legend_position == "inside") {
    legend_inside = T
  } else {
    legend_inside = F
  }
  
  # ----------------------------#
  # 5. Generate the Plot        #
  # ----------------------------#
  
  plot <- ggplot() +
    # Plot fitted normal distribution
    geom_line(
      data = theoretical_df,
      aes(x = x, y = y, color = "Fitted Normal"),
      linewidth = line_width_normal
    ) +
    # Plot empirical ECDF
    stat_ecdf(
      data = ecdf_df,
      aes(x = x, y = y, color = "Empirical"),
      linewidth = line_width_ecdf
    ) +
    # Plot confidence intervals
    geom_step(
      data = ecdf_df,
      aes(x = x, y = upper_ci, color = "Confidence Interval"),
      linewidth = line_width_CI
    ) +
    geom_step(
      data = ecdf_df,
      aes(x = x, y = lower_ci, color = "Confidence Interval"),
      linewidth = line_width_CI
    ) +
    # Add annotation text
    annotate(
      "text",
      x = annotation_x,
      y = annotation_y,
      label = annotation_text,
      hjust = 0,
      vjust = 1,
      size = annotation_size,
      fontface = annotation_font_face
    ) +
    # Define labels and title
    labs(
      title = title_lab,
      subtitle = subtitle_lab,
      x = x_label,
      y = y_label,
      color = ""
    ) +
    # Customize color scale
    scale_color_manual(
      values = c(
        "Empirical" = line_color_ecdf,
        "Fitted Normal" = line_color_normal,
        "Confidence Interval" = line_color_CI
      )
    ) +
    # Apply minimal theme and customize text elements
    theme_minimal() +
    {if(legend_inside)theme(yintercept=15)}+
    theme(
      plot.title = element_text(size = title_size, face = title_face, hjust = title_justification),
      plot.subtitle = element_text(size = subtitle_size, face = subtitle_face, hjust = subtitle_justification),
      axis.title = element_text(size = 12, face = "bold"),
      legend.position = legend_position
    )+
    {if(legend_inside)theme(legend.position.inside = legend_coord)}+
    theme(
      legend.title = element_blank(),
      legend.text = element_text(size = legend_text_size, face = legend_text_face),
      legend.background = element_rect(fill = legend_background, color = ifelse(legend_box_outline, "black", NA)),
      legend.box = ifelse(legend_box, "horizontal", "none")
    )
  
  return(plot)
}
