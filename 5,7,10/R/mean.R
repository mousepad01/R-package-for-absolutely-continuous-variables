#' Mean
#'
#' This function calculates the mean of a continuous random variable
#' @param f(x) A probability density function
#' @keywords mean
#' @export
#' @examples
#' mean(function (x) {
#'      fun <- 3 * x ^ 2
#'      fun[x < 0] = 0
#'      fun[x > 1] = 0
#'      return ( fun )
#' })

mean <- function (f) {
  xF <- function (x) { return ( x * f(x) )}
  return ( integrate(xF, -Inf, +Inf) $ value )
}
