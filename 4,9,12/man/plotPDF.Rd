% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cerinta4.r
\name{plotPDF}
\alias{plotPDF}
\title{Plot PDF}
\usage{
plotPDF(f, x_min, x_max)
}
\arguments{
\item{f}{The PDF for a continuous random variable.}

\item{x_min, x_max}{The range over which the PDF will be plotted.}
}
\description{
This functions plots the PDF for a continuous random variable.
}
\examples{
f <- function(x) (1/2 * sin (x)) * (0 < x & x <= pi)
plotPDF(f, 0, pi)
}
\keyword{density}
\keyword{function}
\keyword{pdf}
\keyword{plot}
\keyword{plotpdf}
\keyword{probability}
