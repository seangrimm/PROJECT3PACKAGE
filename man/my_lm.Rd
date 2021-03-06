% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/my_lm.R
\name{my_lm}
\alias{my_lm}
\title{Linear model function}
\usage{
my_lm(formula, data)
}
\arguments{
\item{formula}{A formula class object based on variables from \code{data}.}

\item{data}{A data frame object that will be used to fit the model.}
}
\value{
A table of numeric values representing estimated values, standard
errors, t-test statistics, and two-sided p-values for all coefficients
given in \code{formula}.
}
\description{
This function fits the given data to the given linear model formula.
}
\examples{
my_lm(lifeExp ~ gdpPercap + continent, my_gapminder)

}
\keyword{inference}
