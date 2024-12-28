#Add roxygen2-style documentation for each function to explain the input, output, and purpose




# Function to create histograms with density overlay

require(ggplot2)

create_histogram <- function(data, column, x_label, title, output_path = NULL) {
  p <- ggplot(data, aes(x = {{ column }})) +
    geom_histogram(aes(y = ..density..), fill = "gray70", color = "black", bins = 30) +
    geom_density(alpha = 0.3, fill = "blue") +
    labs(title = title, x = x_label, y = "Density") +
    theme_minimal()
  if (!is.null(output_path)) {
    ggsave(output_path, plot = p, width = 8, height = 4, dpi = 300)
  }
  return(p)
}




















