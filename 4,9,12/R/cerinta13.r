# Cerin??a 13

#' Sum of two random discrete variables using convolution formula
#'
#' This function calculates the sum of two random discrete variables X and Y, returning the PDF of Z = X + Y.
#' @param f PDF of variable X.
#' @param g PDF of variable Y.
#' @return PDF of the variable Z = X + Y.
#' @keywords random variable discrete sum convolution
#' @export
convolutionSum <- function(f, g) {
  function(z) (integrate (function(x) (f(x) * g(z - x)), -Inf, +Inf)$value)
}

#' Difference of two random discrete variables using convolution formula
#'
#' This function calculates the difference of two random discrete variables X and Y, returning the PDF of Z = X - Y.
#' @param f PDF of variable X.
#' @param g PDF of variable Y.
#' @return PDF of the variable Z = X - Y.
#' @keywords random variable discrete difference convolution
#' @export
convolutionDif <- function(f, g) {
  function(z) (integrate (function(x) (f(x) * g(x - z)), -Inf, +Inf)$value)
}