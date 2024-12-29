#' Linear Rescaling of a Numeric Vector
#'
#' This function linearly rescales a numeric input from one range to another.
#'
#' @param data A numeric vector. The values to be rescaled.
#' @param a A numeric scalar. The lower bound of the target range. Defaults to 0.
#' @param b A numeric scalar. The upper bound of the target range. Defaults to 1.
#'
#' @return A numeric vector of the same length as `t`, where each element has been rescaled 
#'   linearly to fall within the range `[a, b]` based on the input range `[c, d]`.
#'
#' @details The function applies the formula:
#' \deqn{M = \frac{(b - a) \cdot (data - c)}{d - c} + a}
#' where `data` is the input vector, and `[a, b]` and `[c, d]` define the target and source ranges, respectively.
#'
#' @examples
#' # Rescale values to [0, 1]
#' linear_rescale(data = c(0, 50, 100), a = 0, b = 1)
#'
#' # Rescale values to [0, 100]
#' linear_rescale(data = c(10, 15, 20), a = 0, b = 100)
#'
#' @export
linear_rescale <- function(data, 
                           a = 0, 
                           b = 1) {
  # determine lower bound of the source range
  c = min(data, na.rm = T)
  # determine upper bound of the source range
  d = max(data, na.rm = T)
  
  # Rescale the vector from the source range to the target range
  M <- ((b - a) * (data - c)) / (d - c) + a
  
  return(M)
  
}
