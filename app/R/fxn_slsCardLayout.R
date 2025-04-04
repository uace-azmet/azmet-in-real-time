#' `fxn_slsCardLayout.R` - Generate list of cards with variable time series for station-level summaries
#' 
#' @param azmetStation - user-specified AZMet station
#' @param inDataLatest - Most recent AZMet 15-minute data from `fxn_nwsData.R`
#' @param slsCardGraphs - `list` of plotly graphs from `fxn_slsCardGraphs.R`
#' @return `slsCardLayout` - `list` of cards with variable time series for station-level summaries


fxn_slsCardLayout <- function(azmetStation, inDataLatest, slsCardGraphs) {
  inDataLatest <- inDataLatest %>% 
    dplyr::filter(meta_station_name == azmetStation) %>% 
    dplyr::mutate(datetime = lubridate::ymd_hms(datetime))
  
  classHeader <- "d-flex justify-content-between p-1 sls-card-header"
  styleHeaderHelpText <- "color: #989898; font-family: monospace; font-weight: normal; font-size: 0.8rem;"
  styleHeaderSup <- "font-family: monospace; font-weight: normal;"
  styleHeaderValue <- "font-family: monospace; font-weight: normal; font-size: 0.8rem;"
  styleHeaderVariable <- "font-family: monospace; font-weight: bold; font-size: 0.9rem;"
  
  card_P <- bslib::card(
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
            tags$span(
              style = styleHeaderValue, 
              paste(inDataLatest$precip_total_in," in")
            )
          )
        )
      ),
      
      class = classHeader
    ),
    
    bslib::card_body(
      slsCardGraphs[[1]],
      
      class = "p-0",
      #min_height = 150,
      #id = "slsCardBody",
      #full_screen = TRUE,
      #height = "200px"
    ),
    
    class = "sls-card",
    fill = TRUE,
    full_screen = FALSE,
    height = "120px",
    id = NULL,
    max_height = NULL,
    min_height = "150px"#,
    #wrapper = card_body
  )
  
  card_RH <- bslib::card(
    bslib::card_header(
      htmltools::div(
        tags$span(style = styleHeaderVariable, "RH")
      ),
      htmltools::div(
        htmltools::HTML(
          paste0(
            tags$span(style = styleHeaderHelpText, "Latest Update: "),
            tags$span(
              style = styleHeaderValue, 
              paste(inDataLatest$relative_humidity," %")
            )
          )
        )
      ),
      
      class = classHeader
    ),
    
    bslib::card_body(
      slsCardGraphs[[2]],
      
      class = "p-0",
      #min_height = 300,
      #id = "slsCardBody",
      #full_screen = TRUE,
      #height = "500px"
    ),
    
    class = "sls-card",
    fill = TRUE,
    full_screen = FALSE,
    height = "120px",
    id = NULL,
    max_height = NULL,
    min_height = "120px"#,
    #wrapper = card_body
  )
  
  slsCardLayout <- list(
    card_P, card_RH, card_P, card_RH, card_P, card_RH, card_P, card_RH, card_P, card_RH,
    card_P, card_RH, card_P, card_RH, card_P, card_RH, card_P, card_RH
  )
  
  return(slsCardLayout)
}
