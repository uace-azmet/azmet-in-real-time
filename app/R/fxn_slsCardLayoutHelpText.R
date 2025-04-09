#' `fxn_slsCardLayoutHelpText.R` - Build help text for card layout on station-level summaries tab
#' 
#' @return `slsCardLayoutHelpText` - Help text for card layout on station-level summaries tab


fxn_slsCardLayoutHelpText <- function() {
  slsCardLayoutHelpText <- 
    htmltools::p(
      htmltools::HTML(
        "Hover over data for variable values."
      ), 
      
      class = "sls-card-layout-help-text"
    )
  
  return(slsCardLayoutHelpText)
}
