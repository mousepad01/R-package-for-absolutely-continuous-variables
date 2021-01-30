#' Central Moment of order R
#'
#' This function calculates the central moment of order R of a continuous random variable.
#' @param f(x) A probability density function
#' @param r An integer, representing the order
#' @keywords central moment
#' @export
#' @examples
#' centralMoment(function (x) {
#'      fun <- 3 * x ^ 2
#'      fun[x < 0] = 0
#'      fun[x > 1] = 0
#'      return ( fun )
#' }, 2)

centralMoment <- function (f, r) {
  meanValue <- mean(f)

  xMinusMeanR <- function (x) { return ( (x - meanValue) ^ r ) }

  ansF <- function(x) { return ( xMinusMeanR(x) * f(x) ) }

  tryCatch (
    expr = {
      (integrate(ansF, -Inf, +Inf)) $ value
    },

    error = function (e) {
      print(e)
    }
  )
}
