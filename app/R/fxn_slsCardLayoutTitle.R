#' `fxn_slsCardLayoutTitle.R` - Build title for time-series-graph cards on station-level summaries tab
#' 
#' @param azmetStation - AZMet station selection by user
#' @return `slsCardLayoutTitle` - Title for time-series-graph cards on station-level summaries tab


fxn_slsCardLayoutTitle <- function(azmetStation) {
  slsCardLayoutTitle <- 
    htmltools::p(
      htmltools::HTML(
        paste0(
          bsicons::bs_icon("graph-up"), 
          htmltools::HTML("&nbsp;"),
          htmltools::HTML("&nbsp;"),
          toupper(
            paste0(
              "15-minute data over the past 24 hours at the AZMet ", 
              azmetStation, 
              " Station"
            )
          ),
          htmltools::HTML("&nbsp;&nbsp;&nbsp;&nbsp;"),
          bslib::tooltip(
            bsicons::bs_icon("info-circle"),
            "Hover over data for variable values.",
            id = "infoCardLayoutTitle",
            placement = "right"
          )
        )
      ),
      
      class = "sls-card-layout-title"
    )
  
  return(slsCardLayoutTitle)
}
