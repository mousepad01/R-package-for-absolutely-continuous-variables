#' Random Variable Generation Function
#'
#' This function calculates a set of n random variables using the inverse method.
#' @param n The number of the variables to be generated.
#' @param x_min The lower part of the domain of the PDF of the variable.
#' @param lower The upper part of the domain of the PDF of the variable.
#' @return An array with n random generated values.
#' @keywords random generation variable pdf
#' @export
randomVariableGenerator <- function(n, x_min, x_max) {
  u <- runif(n, min = 0, max = 1)
  r <- c()
  for (v in u) {
    r <- append (r, inverse(integratePDF, v, x_min, x_max)[1]$root)
  }
  r
}
