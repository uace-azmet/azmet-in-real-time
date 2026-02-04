#' `fxn_slsLatestDataUpdate.R` - Build text for latest update information
#' 
#' @param azmetStation - AZMet station selection by user
#' @param inData - Most recent AZMet 15-minute data from `fxn_nwsData.R`
#' @return `slsLatestDataUpdate` - Text for latest update information


fxn_slsLatestDataUpdate <- function(azmetStation, inData) {
  slsLatestDataUpdate <- 
    htmltools::p(
      htmltools::HTML(
        paste0(
          "<strong>Latest Update</strong><br>",
          dplyr::filter(inData, meta_station_name == azmetStation)$datetime
        )
      ),
      
      class = "sls-latest-data-update"
    )
  
  return(slsLatestDataUpdate)
}
