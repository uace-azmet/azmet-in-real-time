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
          toupper("The latest 15-minute data from across the network"),
          htmltools::HTML("&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;"),
          bslib::tooltip(
            bsicons::bs_icon("info-circle"),
            "Scroll or swipe over the table to view additional rows and columns.",
            id = "infoTableTitle",
            placement = "right"
          )
        )
      ),
      
      class = "nws-table-title"
    )
  
  return(nwsTableTitle)
}
