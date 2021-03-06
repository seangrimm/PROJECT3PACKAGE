% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/my_t.test.R
\name{my_t.test}
\alias{my_t.test}
\title{T-test function}
\usage{
my_t.test(x, alternative, mu)
}
\arguments{
\item{x}{Numeric vector of data to be used in the t-test.}

\item{alternative}{Character string indicating type of alternative
hypothesis. Only accepts "\code{two.sided}", "\code{less}" or
"\code{greater}".}

\item{mu}{Numeric input indicating the value of the mean for the null
hypothesis.}
}
\value{
Returns a list containing a numeric representing the t-test test
statistic, an integer representing the degrees of freedom used, a string
representation of the given \code{alternative}, and a numeric representing
the resulting p-value.
}
\description{
This function performs a t-test.
}
\examples{
my_t.test(c(1, 2, 3, 4, 5), "two.sided", 1)
my_t.test(c(1, 2, 3, 4, 5), "less", 1)

}
\keyword{inference}
