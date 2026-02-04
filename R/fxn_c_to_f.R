#' `fxn_c_to_f.R`: convert degrees Celsius to degrees Fahrenheit
#' @param: `valueIn` - variable with units of Celsius
#' @return: `valueOut` - variable with units of Fahrenheit


fxn_c_to_f <- function(valueIn) {
  valueOut <- round((valueIn * (9/5)) + 32, digits = 1)
  return(valueOut)
}
