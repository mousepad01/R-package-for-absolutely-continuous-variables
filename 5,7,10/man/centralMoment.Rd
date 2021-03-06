% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/centralMoment.R
\name{centralMoment}
\alias{centralMoment}
\title{Central Moment of order R}
\usage{
centralMoment(f, r)
}
\arguments{
\item{r}{An integer, representing the order}

\item{f(x)}{A probability density function}
}
\description{
This function calculates the central moment of order R of a continuous random variable.
}
\examples{
centralMoment(function (x) {
     fun <- 3 * x ^ 2
     fun[x < 0] = 0
     fun[x > 1] = 0
     return ( fun )
}, 2)
}
\keyword{central}
\keyword{moment}
