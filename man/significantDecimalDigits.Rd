% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utility_functions.R
\name{significantDecimalDigits}
\alias{significantDecimalDigits}
\title{Returns the number of significant decimal digits}
\usage{
significantDecimalDigits(x)
}
\arguments{
\item{x}{the decimal number inputtes}
}
\value{
The number of significant digits
}
\description{
Returns the number of significant decimal digits
}
\examples{
table(sapply(c(0.2,0.33,0.1234453), significantDecimalDigits))

}
