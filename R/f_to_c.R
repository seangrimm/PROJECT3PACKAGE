#' Farenheit converter
#'
#' This function converts temperature from Farenheit to Celsius.
#'
#' @param temp_F Numeric input indicating temperature in Farenheit.
#'
#' @keywords temperature
#'
#' @return Numeric indicating temperature \code{temp_F} converted to Celsius.
#'
#' @examples
#' f_to_c(32)
#' f_to_c(212)
#'
#' @export
f_to_c <- function(temp_F) {
  temp_C <- (temp_F - 32) * 5 / 9
  return(temp_C)
}
