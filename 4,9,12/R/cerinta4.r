#' Inverse
#'
#' This function calculates the inverse of a function in a certain point.
#' @param f(x) A function.
#' @param x The point in which the inverse function will be calculated (will raise error if the inverse cannot be calculated in that point).
#' @param lower,upper The codomain's range.
#' @return Dataframe containing the root of f(x) - u.
#' @keywords inverse inv function
#' @export
inverse <- function (f, u, lower = -100, upper = 100) {
  tryCatch (
    expr = {
      uniroot((function (x) f(x) - u), lower = lower, upper = upper, extendInt = 'yes')
    },

    error = function (e) {
      print(e)
    }
  )
}

#' Plot PDF
#'
#' This functions plots the PDF for a continuous random variable.
#' @param f The PDF for a continuous random variable.
#' @param x_min,x_max The range over which the PDF will be plotted.
#' @keywords plot pdf plotpdf probability density function
#' @examples
#' f <- function(x) (1/2 * sin (x)) * (0 < x & x <= pi)
#' plotPDF(f, 0, pi)
#' @export
plotPDF <- function(f, x_min, x_max) {
  t <- seq(x_min, x_max, 0.0075)
  plot (t, f (t), col = 'red', main = 'Probability Density Function', xlab = 'x', ylab = 'y')
}

#' Integrate PDF
#'
#' This functions integrates the PDF of a certain continuous random variable. It can be used to calculate the CDF for that variable in a certain point.
#' @param X The point in which the integral will be calculated.
#' @param f The PDF for a continuous random variable.
#' @return The value of the integral calculated.
#' @keywords integrate pdf probability density function cumulative distribution function
#' @export
integratePDF <- function (f, X) {
  tryCatch (
    expr = {
      integrate (f, 0, X)$value
    },

    error = function (e) {
      print(e)
    }
  )
}

#' Plot CDF
#'
#' This functions plots the CDF for a random discrete variable.
#' @param x_min,x_max The range over which the CDF will be plotted.
#' @param f The PDF for a continuous random variable.
#' @keywords plot cdf plotcdf cumulative distribution function
#' @examples
#' f <- function(x) (1/2 * sin (x)) * (0 < x & x <= pi)
#' plotCDF(f, 0, pi)
#' @export
plotCDF <- function(f, x_min, x_max) {
  t <- seq(x_min, x_max, 0.0075)
  s <- c()
  for (v in t) {
    s = append (s, integratePDF (f, v))
  }
  plot (t, s, col = 'blue', main = 'Cumulative Distribution Function', xlab = 'x', ylab = 'y')
}
