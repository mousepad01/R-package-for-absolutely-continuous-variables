#' Variance
#'
#' This function calculates the variance of a continuous random variable.
#' @param f(x) A probability density function
#' @keywords var variance
#' @export
#' @examples
#' var(function (x) {
#'      fun <- 3 * x ^ 2
#'      fun[x < 0] = 0
#'      fun[x > 1] = 0
#'      return ( fun )
#' })

var <- function (f) {
  return ( centralMoment(f, 2) )
}
