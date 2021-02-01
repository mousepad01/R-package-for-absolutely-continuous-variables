#' Inverse
#'
#' This function calculates the inverse of a function in a certain point.
#' @param f(x) A function.
#' @param x The point in which the inverse function will be calculated (will raise error if the inverse cannot be calculated in that point).
#' @param lower The lower part of the codomain.
#' @param upper The upper part of the codomain.
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
#' This functions plots the PDF for a random discrete variable.
#' @param x_min The lower part of the PDF's domain.
#' @param x_max The upper part of the PDF's domain.
#' @keywords plot pdf plotpdf
#' @export
plotPDF <- function(x_min, x_max) {
  t <- seq(x_min, x_max, 0.0075)
  plot (t, f (t), col = 'red', main = 'Densitatea de probabilitate', xlab = 'x', ylab = 'y')
}

#' Integrate PDF
#'
#' This functions integrates the PDF. It can be used to get the CDF.
#' @param X The point in which the integral will be calculated.
#' @return The value of the integral calculated.
#' @keywords plot cdf plotcdf
#' @export
integratePDF <- function (X) {
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
#' @param x_min The minimum point of the range over which the function will be plotted.
#' @param x_max The maximum point of the range over which the function will be plotted.
#' @keywords plot cdf plotcdf
#' @export
plotCDF <- function(x_min, x_max) {
  t <- seq(x_min, x_max, 0.0075)
  s <- c()
  for (v in t) {
    s = append (s, integratePDF (v))
  }
  plot (t, s, col = 'blue', main = 'Func??ia de reparti??ie', xlab = 'x', ylab = 'y')
}
