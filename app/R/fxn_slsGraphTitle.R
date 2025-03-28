#' `fxn_slsGraphTitle.R` - Build title for station-level summaries graph
#' 
#' @return `slsGraphTitle` - Title for station-level summaries graph


fxn_slsGraphTitle <- function() {
  slsGraphTitle <- 
    htmltools::p(
      htmltools::HTML(
        paste0(
          bsicons::bs_icon("graph-up"), 
          htmltools::HTML("&nbsp;"),
          htmltools::HTML("&nbsp;"),
          toupper("15-minute data over the past 24 hours from across the network")
        ),
      ),
      
      class = "sls-graph-title"
    )
  
  return(slsGraphTitle)
}
