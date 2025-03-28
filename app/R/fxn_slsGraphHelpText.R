#' `fxn_slsGraphHelpText.R` - Build help text for station-level summaries graph
#' 
#' @return `slsGraphHelpText` - Help text for station-level summaries graph


fxn_slsGraphHelpText <- function() {
  slsGraphHelpText <- 
    htmltools::p(
      htmltools::HTML(
        "Hover over data for variable values and click or tap on legend items to toggle data visibility. Select from the icons to the right of the graph for additional functionality."
      ), 
      
      class = "sls-graph-help-text"
    )
  
  return(slsGraphHelpText)
}
