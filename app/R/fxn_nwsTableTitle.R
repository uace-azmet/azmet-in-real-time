#' `fxn_nwsTableTitle.R` - Build title for network-wide summary table
#' 
#' @return `nwsTableTitle` - Title for network-wide summary table


fxn_nwsTableTitle <- function() {
  nwsTableTitle <- 
    htmltools::p(
      htmltools::HTML(
        paste0(
          bsicons::bs_icon("table"), 
          htmltools::HTML("&nbsp;"),
          htmltools::HTML("&nbsp;"),
          toupper("The latest 15-minute data from across the network")
        ),
      ),
      
      class = "nws-table-title"
    )
  
  return(nwsTableTitle)
}
