#' T-test function
#'
#' This function performs a t-test.
#'
#' @param x Numeric vector of data to be used in the t-test.
#' @param alternative Character string indicating type of alternative
#'   hypothesis. Only accepts "\code{two.sided}", "\code{less}" or
#'   "\code{greater}".
#' @param mu Numeric input indicating the value of the mean for the null
#'   hypothesis.
#'
#' @return Returns a list containing the t-test test statistic, the degrees of
#'   freedom used, the given \code{alternative}, and the resulting p-value.
#'
#' @examples
#' my_t.test(c(1, 2, 3, 4, 5), "two.sided", 1)
#' my_t.test(c(1, 2, 3, 4, 5), "less", 1)
#'
#' @export
my_t.test <- function(x, alternative, mu) {
  # catch invalid parameters
  if (alternative != "two.sided" & alternative != "less" &
      alternative != "greater") {
    stop("Parameter 'alternative' must be 'two.sided', 'less', or 'greater'")
  }

  # t-statistic
  sampleMean <- mean(x)
  sampleSE <- sd(x) / sqrt(length(x))
  test_stat = (sampleMean - mu) / sampleSE

  # area
  if (alternative == "two.sided") {
    area = 2 * pt(abs(test_stat), df=length(x) - 1, lower.tail=FALSE)
  } else if (alternative == "less") {
    area = pt(test_stat, df=length(x) - 1, lower.tail=TRUE)
  } else {
    area = pt(test_stat, df=length(x) - 1, lower.tail=FALSE)
  }

  # compile and return
  result <- list("test_stat" = test_stat, "df" = length(x) - 1,
                 "alternative" = alternative, "p_val" = area)
  return(result)
}
