#' `fxn_slsGraphHelpText.R` - Build help text for station-level summaries graph
#' 
#' @return `slsGraphHelpText` - Help text for station-level summaries graph


fxn_slsGraphHelpText <- function() {
  slsGraphHelpText <- 
    htmltools::p(
      htmltools::HTML(
        "Hover over data for variable values. Hover over an individual graph and click or tap on the 'Expand' button in the lower right to enlarge the graph."
      ), 
      
      class = "sls-graph-help-text"
    )
  
  return(slsGraphHelpText)
}
