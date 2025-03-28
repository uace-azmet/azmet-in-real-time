#' `fxn_slsLatestDataTitle.R` - Build title for station-level summaries graph
#' 
#' @param azmetStation - AZMet station selection by user
#' @return `slsLatestDataTitle` - Title for station-level summaries graph


fxn_slsLatestDataTitle <- function(azmetStation) {
  slsLatestDataTitle <- 
    htmltools::p(
      htmltools::HTML(
        paste0(
          bsicons::bs_icon("table"), 
          htmltools::HTML("&nbsp;"),
          htmltools::HTML("&nbsp;"),
          toupper(
            paste0(
              "The latest 15-minute data at the AZMet ", 
              azmetStation, 
              " Station"
            )
          )
        ),
      ),
      
      class = "sls-latest-data-title"
    )
  
  return(slsLatestDataTitle)
}
