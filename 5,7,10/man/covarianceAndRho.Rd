% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/covarianceAndRho.R
\name{covarianceAndRho}
\alias{covarianceAndRho}
\title{Covariance and Correlation Coefficient}
\usage{
covarianceAndRho(f, suppY, suppX)
}
\arguments{
\item{suppY}{A vector representing the support for y}

\item{suppX}{A vector representing the support for x}

\item{f(x)}{A probability density function}
}
\description{
This function calculates the covariance and the correlation coefficient of a continuous random variable.
}
\examples{
f <- function (x, y) {
     return (3/2 * (x^2+y^2))
}

covarianceAndRho (f, c(0,1), c(0, 1))
}
\keyword{coefficient}
\keyword{correlation}
\keyword{cov}
\keyword{covariance}
\keyword{rho}
