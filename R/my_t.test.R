#' T-test function
#'
#' This function performs a t-test
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
