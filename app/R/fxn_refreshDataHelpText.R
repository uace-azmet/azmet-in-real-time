#' `fxn_refreshDataHelpText.R` - Build help text for button to refresh data
#' 
#' @param activeTab - currently active tab in user session
#' @return `refreshDataHelpText` - Help text for button to refresh data


fxn_refreshDataHelpText <- function(activeTab) {
  if (activeTab == "network-wide-summary") {
    refreshDataHelpText <- 
      "Click or tap to refresh the above table with the latest 15-minute data."
  } else if (activeTab == "station-level-summaries") {
    refreshDataHelpText <- 
      "Click or tap to refresh the above graphs with the latest 15-minute data."
  }
  
  return(refreshDataHelpText)
}
