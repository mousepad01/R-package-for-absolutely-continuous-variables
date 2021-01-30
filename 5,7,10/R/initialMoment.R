#' Initial Moment of order R
#'
#' This function calculates the initial moment of order R of a continuous random variable.
#' @param f(x) A probability density function
#' @param r An integer, representing the order
#' @keywords initial moment
#' @export
#' @examples
#' initialMoment(function (x) {
#'      fun <- 3 * x ^ 2
#'      fun[x < 0] = 0
#'      fun[x > 1] = 0
#'      return ( fun )
#' }, 2)

initialMoment <- function (f, r) {

  ansF <- function (x) { return ( x^r * f(x) ) }

  tryCatch (
    expr = {
      (integrate(ansF, -Inf, +Inf)) $ value
    },

    error = function (e) {
      print(e)
    }
  )
}
