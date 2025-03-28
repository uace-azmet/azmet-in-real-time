#' `fxn_mps_to_mph.R`: convert meters/second to miles/hour
#' @param: `valueIn` - variable with units of meters/second
#' @return: `valueOut` - variable with units of miles/hour


fxn_mps_to_mph <- function(valueIn) {
  valueOut <- round(valueIn * 2.237, digits = 1)
  return(valueOut)
}
