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
  styleHeaderSub <- "font-family: monospace; font-weight: bold;"
  styleHeaderSup <- "font-family: monospace; font-weight: normal;"
  styleHeaderValue <- "font-family: monospace; font-weight: normal; font-size: 0.8rem;"
  styleHeaderVariable <- "font-family: monospace; font-weight: bold; font-size: 0.9rem;"
  
  
  # Cards ----------
  
  
  # `precip_total_in` -----
  
  card_P1 <- bslib::card(
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
              paste0(format(inDataLatest$precip_total_in, nsmall = 2)," in") 
            )
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
  
  
  # `relative_humidity` -----
  
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
              paste0(format(inDataLatest$relative_humidity, nsmall = 0)," %")
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
  
  
  # `sol_rad_Wm2` -----
  
  card_SR <- bslib::card(
    bslib::card_header(
      htmltools::div(
        tags$span(style = styleHeaderVariable, "SR")
      ),
      htmltools::div(
        htmltools::HTML(
          paste0(
            tags$span(style = styleHeaderHelpText, "Latest Update: "),
            tags$span(
              style = styleHeaderValue, 
              htmltools::HTML(paste0(format(inDataLatest$sol_rad_Wm2, nsmall = 2), " W/m<sup>2</sup>"))
            )
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
  
  
  # `temp_airF` -----
  
  card_T <- bslib::card(
    bslib::card_header(
      htmltools::div(
        tags$span(style = styleHeaderVariable, "T")
      ),
      htmltools::div(
        htmltools::HTML(
          paste0(
            tags$span(style = styleHeaderHelpText, "Latest Update: "),
            tags$span(
              style = styleHeaderValue, 
              htmltools::HTML(paste0(format(inDataLatest$temp_airF, nsmall = 1), " °F"))
            )
          )
        )
      ),
      
      class = classHeader
    ),
    
    bslib::card_body(slsCardGraphs[[4]], class = "p-0"),
    
    class = "sls-card",
    fill = TRUE,
    full_screen = FALSE,
    height = cardHeight,
    id = NULL,
    max_height = cardHeight,
    min_height = cardHeight
  )
  
  
  # `temp_air_maxF` -----
  
  card_Tmax <- bslib::card(
    bslib::card_header(
      htmltools::div(
        htmltools::HTML(
          paste0(
            tags$span(style = styleHeaderVariable, "T"),
            "<sub>", tags$span(style = styleHeaderSub, "max"), "</sub>", 
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
              paste0(format(inDataLatest$temp_air_maxF, nsmall = 1)," °F") 
            )
          )
        )
      ),
      
      class = classHeader
    ),
    
    bslib::card_body(slsCardGraphs[[5]], class = "p-0"),
    
    class = "sls-card",
    fill = TRUE,
    full_screen = FALSE,
    height = cardHeight,
    id = NULL,
    max_height = cardHeight,
    min_height = cardHeight
  )
  
  
  # `temp_air_minF` -----
  
  card_Tmin <- bslib::card(
    bslib::card_header(
      htmltools::div(
        htmltools::HTML(
          paste0(
            tags$span(style = styleHeaderVariable, "T"),
            "<sub>", tags$span(style = styleHeaderSub, "min"), "</sub>", 
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
              paste0(format(inDataLatest$temp_air_minF, nsmall = 1)," °F") 
            )
          )
        )
      ),
      
      class = classHeader
    ),
    
    bslib::card_body(slsCardGraphs[[6]], class = "p-0"),
    
    class = "sls-card",
    fill = TRUE,
    full_screen = FALSE,
    height = cardHeight,
    id = NULL,
    max_height = cardHeight,
    min_height = cardHeight
  )
  
  
  # `dwptF` -----
  
  card_Tdewpoint <- bslib::card(
    bslib::card_header(
      htmltools::div(
        htmltools::HTML(
          paste0(
            tags$span(style = styleHeaderVariable, "T"),
            "<sub>", tags$span(style = styleHeaderSub, "dew point"), "</sub>"
          )
        )
      ),
      htmltools::div(
        htmltools::HTML(
          paste0(
            tags$span(style = styleHeaderHelpText, "Latest Update: "),
            tags$span(
              style = styleHeaderValue, 
              paste0(format(inDataLatest$dwptF, nsmall = 1)," °F") 
            )
          )
        )
      ),
      
      class = classHeader
    ),
    
    bslib::card_body(slsCardGraphs[[7]], class = "p-0"),
    
    class = "sls-card",
    fill = TRUE,
    full_screen = FALSE,
    height = cardHeight,
    id = NULL,
    max_height = cardHeight,
    min_height = cardHeight
  )
  
  
  # `temp_soil_10cmF` -----
  
  card_Tsoil10cm <- bslib::card(
    bslib::card_header(
      htmltools::div(
        htmltools::HTML(
          paste0(
            tags$span(style = styleHeaderVariable, "T"),
            "<sub>", tags$span(style = styleHeaderSub, "soil 4-inch"), "</sub>"
          )
        )
      ),
      htmltools::div(
        htmltools::HTML(
          paste0(
            tags$span(style = styleHeaderHelpText, "Latest Update: "),
            tags$span(
              style = styleHeaderValue, 
              paste0(format(inDataLatest$temp_soil_10cmF, nsmall = 1)," °F") 
            )
          )
        )
      ),
      
      class = classHeader
    ),
    
    bslib::card_body(slsCardGraphs[[8]], class = "p-0"),
    
    class = "sls-card",
    fill = TRUE,
    full_screen = FALSE,
    height = cardHeight,
    id = NULL,
    max_height = cardHeight,
    min_height = cardHeight
  )
  
  
  # `temp_soil_50cmF` -----
  
  card_Tsoil50cm <- bslib::card(
    bslib::card_header(
      htmltools::div(
        htmltools::HTML(
          paste0(
            tags$span(style = styleHeaderVariable, "T"),
            "<sub>", tags$span(style = styleHeaderSub, "soil 20-inch"), "</sub>"
          )
        )
      ),
      htmltools::div(
        htmltools::HTML(
          paste0(
            tags$span(style = styleHeaderHelpText, "Latest Update: "),
            tags$span(
              style = styleHeaderValue, 
              paste0(format(inDataLatest$temp_soil_50cmF, nsmall = 1)," °F") 
            )
          )
        )
      ),
      
      class = classHeader
    ),
    
    bslib::card_body(slsCardGraphs[[9]], class = "p-0"),
    
    class = "sls-card",
    fill = TRUE,
    full_screen = FALSE,
    height = cardHeight,
    id = NULL,
    max_height = cardHeight,
    min_height = cardHeight
  )
  
  
  # Card layout list ----------

  slsCardLayout <- list(
    card_P1, 
    card_RH, 
    card_SR, 
    card_T, 
    card_Tmax, 
    card_Tmin,
    card_Tdewpoint,
    card_Tsoil10cm,
    card_Tsoil50cm
  )
  
  return(slsCardLayout)
}
