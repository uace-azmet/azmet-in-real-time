#' `fxn_refreshDataHelpText.R` - Build help text for button to refresh data
#' 
#' @param activeTab - currently active tab in user session
#' @return `refreshDataHelpText` - Help text for button to refresh data


fxn_refreshDataHelpText <- function(activeTab) {
  if (activeTab == "network-wide-summary") {
    refreshDataHelpText <- 
      htmltools::p(
        htmltools::HTML(
          "Click or tap the button below to refresh the above table with the latest 15-minute data."
        ), 
        
        class = "refresh-data-help-text-nws"
      )
  } else if (activeTab == "station-level-summaries") {
    refreshDataHelpText <- 
      htmltools::p(
        htmltools::HTML(
          "Click or tap the button below to refresh the above graph with the latest 15-minute data."
        ), 
        
        class = "refresh-data-help-text-sls"
      )
  }
  
  return(refreshDataHelpText)
}
