#' `fxn_nwsTableHelpText.R` - Build help text for network-wide summary table
#' 
#' @return `nwsTableHelpText` - Help text for network-wide summary table


fxn_nwsTableHelpText <- function() {
  nwsTableHelpText <- 
    htmltools::p(
      htmltools::HTML(
        "Scroll or swipe over the table to view additional rows and columns."
      ), 
      
      class = "nws-table-help-text"
    )
  
  return(nwsTableHelpText)
}
