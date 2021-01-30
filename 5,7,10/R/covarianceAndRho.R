#' Covariance and Correlation Coefficient
#'
#' This function calculates the covariance and the correlation coefficient of a continuous random variable.
#' @param f(x) A probability density function
#' @param suppY A vector representing the support for y
#' @param suppX A vector representing the support for x
#' @keywords cov covariance rho correlation coefficient
#' @export
#' @examples
#' f <- function (x, y) {
#'      return (3/2 * (x^2+y^2))
#' }
#'
#' covarianceAndRho (f, c(0,1), c(0, 1))

covarianceAndRho <- function (f, suppY, suppX) {
  meanX <- mean2(f, suppY, suppX, "x")
  meanY <- mean2(f, suppY, suppX, "y")

  newF <- function(x, y) { return ((x - meanX) * (y - meanY) * f(x, y)) }

  cov <- integrate2(newF, suppY, suppX)

  varX <- var2(f, suppY, suppX, "x", meanX)
  varY <- var2(f, suppY, suppX, "y", meanY)

  rho <- cov / (varX * varY)

  print(noquote(c("Covariance: ", cov)))
  print(noquote(c("Correlation coefficient: ", rho)))
}
