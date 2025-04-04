#' `fxn_slsCardLayout.R` - Generate list of cards with variable time series for station-level summaries tab based on user input
#' 
#' @param azmetStation - user-specified AZMet station
#' @param inDataLatest - Most recent AZMet 15-minute data from `fxn_nwsData.R`
#' @param slsCardGraphs - `list` of plotly graphs from `fxn_slsCardGraphs.R`
#' @return `slsCardLayout` - `list` of cards with variable time series for station-level summaries tab based on user input


fxn_slsCardLayout <- function(azmetStation, inDataLatest, slsCardGraphs) {
  
  
  # Variables ----------
  
  inDataLatest <- inDataLatest %>% 
    dplyr::filter(meta_station_name == azmetStation) %>% 
    dplyr::mutate(datetime = lubridate::ymd_hms(datetime))
  
  cardHeight = "150px"
  classHeader <- "d-flex justify-content-between p-1 sls-card-header"
  styleHeaderHelpText <- "color: #989898; font-family: monospace; font-weight: normal; font-size: 0.8rem;"
  styleHeaderSup <- "font-family: monospace; font-weight: normal;"
  styleHeaderValue <- "font-family: monospace; font-weight: normal; font-size: 0.8rem;"
  styleHeaderVariable <- "font-family: monospace; font-weight: bold; font-size: 0.9rem;"
  
  
  # Cards ----------
  
  card_P1 <- bslib::card( # `precip_total_in` -----
    bslib::card_header(
      htmltools::div(
        htmltools::HTML(
          paste0(
            tags$span(style = styleHeaderVariable, "P"), 
            "<sup>", tags$span(style = styleHeaderSup, "1"), "</sup>"
          )
        )
      ),
      htmltools::div(
        htmltools::HTML(
          paste0(
            tags$span(style = styleHeaderHelpText, "Latest Update: "),
            tags$span(style = styleHeaderValue, paste0(inDataLatest$precip_total_in," in"))
          )
        )
      ),
      
      class = classHeader
    ),
    
    bslib::card_body(slsCardGraphs[[1]], class = "p-0"),
    
    class = "sls-card",
    fill = TRUE,
    full_screen = FALSE,
    height = cardHeight,
    id = NULL,
    max_height = cardHeight,
    min_height = cardHeight
  )
  
  card_RH <- bslib::card( # `relative_humidity` -----
    bslib::card_header(
      htmltools::div(
        tags$span(style = styleHeaderVariable, "RH")
      ),
      htmltools::div(
        htmltools::HTML(
          paste0(
            tags$span(style = styleHeaderHelpText, "Latest Update: "),
            tags$span(style = styleHeaderValue, paste0(inDataLatest$relative_humidity," %")
            )
          )
        )
      ),
      
      class = classHeader
    ),
    
    bslib::card_body(slsCardGraphs[[2]], class = "p-0"),
    
    class = "sls-card",
    fill = TRUE,
    full_screen = FALSE,
    height = cardHeight,
    id = NULL,
    max_height = cardHeight,
    min_height = cardHeight
  )
  
  card_SR <- bslib::card( # `sol_rad_Wm2` -----
    bslib::card_header(
      htmltools::div(
        tags$span(style = styleHeaderVariable, "SR")
      ),
      htmltools::div(
        htmltools::HTML(
          paste0(
            tags$span(style = styleHeaderHelpText, "Latest Update: "),
            tags$span(style = styleHeaderValue, htmltools::HTML(paste0(inDataLatest$sol_rad_Wm2, " W/m<sup>2</sup>")))
          )
        )
      ),
      
      class = classHeader
    ),
    
    bslib::card_body(slsCardGraphs[[3]], class = "p-0"),
    
    class = "sls-card",
    fill = TRUE,
    full_screen = FALSE,
    height = cardHeight,
    id = NULL,
    max_height = cardHeight,
    min_height = cardHeight
  )
  
  slsCardLayout <- list(
    card_P1, card_RH, card_SR, card_P1, card_RH, card_SR, card_P1, card_RH, card_SR,
    card_P1, card_RH, card_SR, card_P1, card_RH, card_SR, card_P1, card_RH, card_SR
  )
  
  return(slsCardLayout)
}
