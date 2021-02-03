#' Random Variable Generation Function
#'
#' This function calculates a set of n random variables using the inverse method.
#' @param f The PDF used to generate n variables.
#' @param n The number of the variables to be generated.
#' @param x_min,x_max The PDF's domain range.
#' @return An array with n random generated values.
#' @keywords random generation variable pdf
#' @examples
#' f <- function(x) (1/2 * sin (x)) * (0 < x & x <= pi)
#' randomVariableGenerator (f, 100, 0, pi)
#' @export
randomVariableGenerator <- function(f, n, x_min, x_max) {
  u <- runif(n, min = 0, max = 1)
  r <- c()
  for (v in u) {
    r <- append (r, inverse(function (x) (integratePDF(f, x)), v, x_min, x_max)[1]$root)
  }
  r
}
