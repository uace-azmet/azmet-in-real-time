#' `fxn_slsCardLayout.R` - Generate list of cards with variable time series for station-level summaries
#' 
#' @param azmetStation - user-specified AZMet station
#' @param inDataFull - AZMet 15-minute data from `fxn_dataETL.R`
#' @param inDataLatest - Most recent AZMet 15-minute data from `fxn_nwsData.R`
#' @return `slsCardLayout` - List of cards with variable time series for station-level summaries


fxn_slsCardLayout <- function(azmetStation, inDataFull, inDataLatest) {
  #x <- c(1:100)
  #random_y <- rnorm(100, mean = 0)
  #data <- data.frame(x, random_y)
  
  inDataFull <- inDataFull %>% 
    dplyr::filter(meta_station_name == azmetStation) %>% 
    dplyr::mutate(datetime = lubridate::ymd_hms(datetime))
  
  inDataLatest <- inDataLatest %>% 
    dplyr::filter(meta_station_name == azmetStation) %>% 
    dplyr::mutate(datetime = lubridate::ymd_hms(datetime))
  
  
  fig <- 
    plotly::plot_ly(
      data = inDataFull, 
      x = ~datetime, 
      y = ~relative_humidity, 
      type = "scatter", 
      mode = "lines+markers",
      line = list(color = "#606060", width = 1),
      marker = list(color = "#606060", size = 3),
      hoverinfo = "text",
      text = ~paste0(
        "<br><b>Date:</b> ", gsub(" 0", " ", format(datetime, "%b %d, %Y")),
        "<br><b>Time:</b> ", format(datetime, "%H:%M:%S"),
        "<br><b>", "RH", ":</b> ", relative_humidity, " %"
      )
    ) %>%
    plotly::config(
      displaylogo = FALSE,
      displayModeBar = FALSE
    ) %>% 
    plotly::layout(
      font = list(
        color = "#191919",
        family = "proxima-nova, calibri, -apple-system, BlinkMacSystemFont, \"Segoe UI\", Roboto, \"Helvetica Neue\", Arial, \"Noto Sans\", sans-serif, \"Apple Color Emoji\", \"Segoe UI Emoji\", \"Segoe UI Symbol\", \"Noto Color Emoji\"",
        size = 13
      ),
      hoverlabel = list(
        font = list(
          family = "proxima-nova, calibri, -apple-system, BlinkMacSystemFont, \"Segoe UI\", Roboto, \"Helvetica Neue\", Arial, \"Noto Sans\", sans-serif, \"Apple Color Emoji\", \"Segoe UI Emoji\", \"Segoe UI Symbol\", \"Noto Color Emoji\"",
          size = 14
        )
      ),
      margin = list(
        l = 0,
        r = 0, # For space between plot and modebar
        b = 0, # For space between x-axis title and caption or figure help text
        t = 0,
        pad = 0
      ),
      xaxis = list(
        showgrid = FALSE,
        showticklabels = FALSE,
        #standoff = -20,
        #ticklen = 0,
        #ticktext = list(~min(datetime), ~max(datetime)),
        #tickvals = list(~min(datetime), ~max(datetime)),
        title = "Date and Time",
        zeroline = FALSE
      ),
      yaxis = list(
        #standoff = -20,
        #ticklen = 0,
        title = "%",
        zeroline = FALSE
      )
    )
  
  
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
      fig,
      
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
    min_height = "120px"#,
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
      fig,
      
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
