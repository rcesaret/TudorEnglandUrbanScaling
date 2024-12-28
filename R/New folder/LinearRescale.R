#' Linear Rescaling of a Numeric Vector
#'
#' This function linearly rescales a numeric input from one range to another.
#'
#' @param t A numeric vector. The values to be rescaled.
#' @param a A numeric scalar. The lower bound of the target range.
#' @param b A numeric scalar. The upper bound of the target range.
#' @param c A numeric scalar. The lower bound of the source range.
#' @param d A numeric scalar. The upper bound of the source range.
#'
#' @return A numeric vector of the same length as `t`, where each element has been rescaled 
#'   linearly to fall within the range `[a, b]` based on the input range `[c, d]`.
#'
#' @details The function applies the formula:
#' \deqn{M = \frac{(b - a) \cdot (t - c)}{d - c} + a}
#' where `t` is the input vector, and `[a, b]` and `[c, d]` define the target and source ranges, respectively.
#'
#' @examples
#' # Rescale values from the range [0, 100] to [0, 1]
#' LinearRescale(t = c(0, 50, 100), a = 0, b = 1, c = 0, d = 100)
#'
#' # Rescale values from [10, 20] to [0, 100]
#' LinearRescale(t = c(10, 15, 20), a = 0, b = 100, c = 10, d = 20)
#'
#' @export
LinearRescale <- function(t, a, b, c, d) {
  M <- ((b - a) * (t - c)) / (d - c) + a
  return(M)
}
