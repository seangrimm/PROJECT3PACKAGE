#' Linear model function
#'
#' This function fits the given data to the given linear model formula.
#'
#' @param formula A formula class object based on variables from \code{data}.
#' @param data A data frame object that will be used to fit the model.
#'
#' @keywords inference
#'
#' @return A table containing estimated values, standard errors, t-test
#'   statistics, and two-sided p-values for all coefficients specified in
#'   \code{formula}.
#'
#' @examples
#' my_lm(lifeExp ~ gdpPercap + continent, my_gapminder)
#'
#' @export
my_lm <- function(formula, data) {
  x <- model.matrix(formula, data)
  y <- model.response(model.frame(formula, data))

  betaHat <- solve(t(x) %*% x) %*% t(x) %*% y
  df <- nrow(data) - length(betaHat)

  # calculate sigma^2
  xbeta <- x %*% betaHat
  sigma <- 0
  for (i in 1:length(y)) {
    sigma <- sigma + (((y[i] - xbeta[i]) ** 2) / df)
  }

  # calculate standard error
  seMatrix <- (sigma * solve(t(x) %*% x)) ** (1/2)
  seVect <- diag(seMatrix)

  # calculate t-stats
  tstats = betaHat / seVect

  # perform t-tests
  tests <- rep(0, length(betaHat))
  for (i in 1:length(betaHat)) {
    tests[i] <- 2 * pt(abs(tstats[i]), df = df, lower.tail = FALSE)
  }

  # compile and return
  resultMatrix <- matrix(c(betaHat, seVect, tstats, tests), ncol = 4)
  colnames(resultMatrix) <- c("Estimate", "Std. Error", "t value", "Pr(>|t|)")
  rownames(resultMatrix) <- rownames(betaHat)
  return(as.table(resultMatrix))
}
