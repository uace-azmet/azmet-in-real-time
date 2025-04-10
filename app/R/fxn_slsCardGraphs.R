#' `fxn_slsCardGraphs.R` Generate time series graphs for cards on station-level summaries tab based on user input
#' 
#' @param azmetStation - user-specified AZMet station
#' @param inDataFull - AZMet 15-minute data from `fxn_dataETL.R`
#' @return `slsCardGraphs` - `list` of time series graphs for cards on station-level summaries tab based on user input

# https://plotly-r.com/ 
# https://plotly.com/r/reference/ 
# https://plotly.github.io/schema-viewer/
# https://github.com/plotly/plotly.js/blob/c1ef6911da054f3b16a7abe8fb2d56019988ba14/src/components/fx/hover.js#L1596
# https://www.color-hex.com/color-palette/1041718


fxn_slsCardGraphs <- function(azmetStation, inDataFull) {
  
  
  # Variables ----------
  
  inDataFull <- inDataFull %>% 
    dplyr::filter(meta_station_name == azmetStation) %>% 
    dplyr::mutate(datetime = lubridate::ymd_hms(datetime))
  
  hoverlabelFontColor = "#FFFFFF"
  hoverlabelFontSize = 14
  layoutFontColor = "#707070"
  layoutFontFamily = "proxima-nova, calibri, -apple-system, BlinkMacSystemFont, \"Segoe UI\", Roboto, \"Helvetica Neue\", Arial, \"Noto Sans\", sans-serif, \"Apple Color Emoji\", \"Segoe UI Emoji\", \"Segoe UI Symbol\", \"Noto Color Emoji\""
  layoutFontSize = 11
  layoutMargin = 6
  layoutPadding = 0
  traceLineColor = "#808080"
  traceLineWidth = 1
  traceMarkerColor = "#808080"
  traceMarkerSize = 3
  
  
  # Graphs ----------
  
  slsCardGraphs <- list(
    
    # `precip_total_in` -----
    
    plotly::plot_ly(
      data = inDataFull, 
      x = ~datetime, 
      y = ~precip_total_in, 
      type = "scatter", 
      mode = "lines+markers",
      line = list(color = traceLineColor, width = traceLineWidth),
      marker = list(color = traceMarkerColor, size = traceMarkerSize),
      hoverinfo = "text",
      text = ~paste0(
        "<br><b>Date:</b> ", gsub(" 0", " ", format(datetime, "%b %d, %Y")),
        "<br><b>Time:</b> ", format(datetime, "%H:%M:%S"),
        "<br><b>P</b><sup>1</sup><b>:</b> ", format(precip_total_in, nsmall = 2), " in"
      )
    ) %>%
      plotly::config(
        displaylogo = FALSE,
        displayModeBar = FALSE
      ) %>% 
      plotly::layout(
        font = list(
          color = layoutFontColor,
          family = layoutFontFamily,
          size = layoutFontSize
        ),
        hoverlabel = list(
          bordercolor = "rgba(0, 0, 0, 0)",
          font = list(
            color = hoverlabelFontColor,
            family = layoutFontFamily,
            size = hoverlabelFontSize
          )
        ),
        margin = list(
          l = layoutMargin,
          r = layoutMargin,
          b = layoutMargin,
          t = layoutMargin,
          pad = layoutPadding
        ),
        xaxis = list(
          fixedrange = TRUE,
          range = list(
            ~(min(datetime) - 3000), # unix time
            ~(max(datetime) + 3000)
          ),
          ticktext = list(
            ~(gsub(" 0", " ", format(as.Date(max(datetime)), "%b %d")))
          ),
          tickvals = list(
            ~(lubridate::ymd_hms(
              paste0(as.Date(max(datetime)), " 00:00:00"), 
              tz = "America/Phoenix"
            ))
          ),
          showgrid = TRUE,
          showticklabels = TRUE,
          title = FALSE,
          zeroline = FALSE
        ),
        yaxis = list(
          fixedrange = TRUE,
          rangemode = "nonnegative", # one of ("normal" | "tozero" | "nonnegative")
          tickformat = ".2f",
          title = "in", 
          zeroline = FALSE
        )
      ),
    
    
    # `relative_humidity` -----
    
    plotly::plot_ly(
      data = inDataFull, 
      x = ~datetime, 
      y = ~relative_humidity, 
      type = "scatter", 
      mode = "lines+markers",
      line = list(color = traceLineColor, width = traceLineWidth),
      marker = list(color = traceMarkerColor, size = traceMarkerSize),
      hoverinfo = "text",
      text = ~paste0(
        "<br><b>Date:</b> ", gsub(" 0", " ", format(datetime, "%b %d, %Y")),
        "<br><b>Time:</b> ", format(datetime, "%H:%M:%S"),
        "<br><b>RH:</b> ", format(relative_humidity, nsmall = 0), " %"
      )
    ) %>%
      plotly::config(
        displaylogo = FALSE,
        displayModeBar = FALSE
      ) %>% 
      plotly::layout(
        font = list(
          color = layoutFontColor,
          family = layoutFontFamily,
          size = layoutFontSize
        ),
        hoverlabel = list(
          bordercolor = "rgba(0, 0, 0, 0)",
          font = list(
            color = hoverlabelFontColor,
            family = layoutFontFamily,
            size = hoverlabelFontSize
          )
        ),
        margin = list(
          l = layoutMargin,
          r = layoutMargin,
          b = layoutMargin,
          t = layoutMargin,
          pad = layoutPadding
        ),
        xaxis = list(
          fixedrange = TRUE,
          range = list(
            ~(min(datetime) - 3000), # unix time
            ~(max(datetime) + 3000)
          ),
          ticktext = list(
            ~(gsub(" 0", " ", format(as.Date(max(datetime)), "%b %d")))
          ),
          tickvals = list(
            ~(lubridate::ymd_hms(
              paste0(as.Date(max(datetime)), " 00:00:00"), 
              tz = "America/Phoenix"
            ))
          ),
          showgrid = TRUE,
          showticklabels = TRUE,
          title = FALSE,
          zeroline = FALSE
        ),
        yaxis = list(
          fixedrange = TRUE,
          rangemode = "nonnegative", # one of ("normal" | "tozero" | "nonnegative")
          #tickformat = ".0f",
          title = "%", 
          zeroline = FALSE
        )
      ),
    
    
    # `sol_rad_Wm2` -----
    
    plotly::plot_ly(
      data = inDataFull, 
      x = ~datetime, 
      y = ~sol_rad_Wm2, 
      type = "scatter", 
      mode = "lines+markers",
      line = list(color = traceLineColor, width = traceLineWidth),
      marker = list(color = traceMarkerColor, size = traceMarkerSize),
      hoverinfo = "text",
      text = ~paste0(
        "<br><b>Date:</b> ", gsub(" 0", " ", format(datetime, "%b %d, %Y")),
        "<br><b>Time:</b> ", format(datetime, "%H:%M:%S"),
        "<br><b>SR:</b> ", format(sol_rad_Wm2, nsmall = 2), " W/m<sup>2</sup>"
      )
    ) %>%
      plotly::config(
        displaylogo = FALSE,
        displayModeBar = FALSE
      ) %>% 
      plotly::layout(
        font = list(
          color = layoutFontColor,
          family = layoutFontFamily,
          size = layoutFontSize
        ),
        hoverlabel = list(
          bordercolor = "rgba(0, 0, 0, 0)",
          font = list(
            color = hoverlabelFontColor,
            family = layoutFontFamily,
            size = hoverlabelFontSize
          )
        ),
        margin = list(
          l = layoutMargin,
          r = layoutMargin,
          b = layoutMargin,
          t = layoutMargin,
          pad = layoutPadding
        ),
        xaxis = list(
          fixedrange = TRUE,
          range = list(
            ~(min(datetime) - 3000), # unix time
            ~(max(datetime) + 3000)
          ),
          ticktext = list(
            ~(gsub(" 0", " ", format(as.Date(max(datetime)), "%b %d")))
          ),
          tickvals = list(
            ~(lubridate::ymd_hms(
              paste0(as.Date(max(datetime)), " 00:00:00"), 
              tz = "America/Phoenix"
            ))
          ),
          showgrid = TRUE,
          showticklabels = TRUE,
          title = FALSE,
          zeroline = FALSE
        ),
        yaxis = list(
          fixedrange = TRUE,
          rangemode = "nonnegative", # one of ("normal" | "tozero" | "nonnegative")
          tickformat = ".0f",
          title = "W/m<sup>2</sup>", 
          zeroline = FALSE
        )
      ),
    
    
    # `temp_airF` -----
    
    plotly::plot_ly(
      data = inDataFull, 
      x = ~datetime, 
      y = ~temp_airF, 
      type = "scatter", 
      mode = "lines+markers",
      line = list(color = traceLineColor, width = traceLineWidth),
      marker = list(color = traceMarkerColor, size = traceMarkerSize),
      hoverinfo = "text",
      text = ~paste0(
        "<br><b>Date:</b> ", gsub(" 0", " ", format(datetime, "%b %d, %Y")),
        "<br><b>Time:</b> ", format(datetime, "%H:%M:%S"),
        "<br><b>T:</b> ", format(temp_airF, nsmall = 1), " °F"
      )
    ) %>%
      plotly::config(
        displaylogo = FALSE,
        displayModeBar = FALSE
      ) %>% 
      plotly::layout(
        font = list(
          color = layoutFontColor,
          family = layoutFontFamily,
          size = layoutFontSize
        ),
        hoverlabel = list(
          bordercolor = "rgba(0, 0, 0, 0)",
          font = list(
            color = hoverlabelFontColor,
            family = layoutFontFamily,
            size = hoverlabelFontSize
          )
        ),
        margin = list(
          l = layoutMargin,
          r = layoutMargin,
          b = layoutMargin,
          t = layoutMargin,
          pad = layoutPadding
        ),
        xaxis = list(
          fixedrange = TRUE,
          range = list(
            ~(min(datetime) - 3000), # unix time
            ~(max(datetime) + 3000)
          ),
          ticktext = list(
            ~(gsub(" 0", " ", format(as.Date(max(datetime)), "%b %d")))
          ),
          tickvals = list(
            ~(lubridate::ymd_hms(
              paste0(as.Date(max(datetime)), " 00:00:00"), 
              tz = "America/Phoenix"
            ))
          ),
          showgrid = TRUE,
          showticklabels = TRUE,
          title = FALSE,
          zeroline = FALSE
        ),
        yaxis = list(
          fixedrange = TRUE,
          rangemode = "normal", # one of ("normal" | "tozero" | "nonnegative")
          #tickformat = ".1f",
          title = "°F", 
          zeroline = FALSE
        )
      ),
    
    
    # `temp_air_maxF` -----
    
    plotly::plot_ly(
      data = inDataFull, 
      x = ~datetime, 
      y = ~temp_air_maxF, 
      type = "scatter", 
      mode = "lines+markers",
      line = list(color = traceLineColor, width = traceLineWidth),
      marker = list(color = traceMarkerColor, size = traceMarkerSize),
      hoverinfo = "text",
      text = ~paste0(
        "<br><b>Date:</b> ", gsub(" 0", " ", format(datetime, "%b %d, %Y")),
        "<br><b>Time:</b> ", format(datetime, "%H:%M:%S"),
        "<br><b>T<sub>max</sub></b><sup>1</sup><b>:</b> ", format(temp_air_maxF, nsmall = 1), " °F"
      )
    ) %>%
      plotly::config(
        displaylogo = FALSE,
        displayModeBar = FALSE
      ) %>% 
      plotly::layout(
        font = list(
          color = layoutFontColor,
          family = layoutFontFamily,
          size = layoutFontSize
        ),
        hoverlabel = list(
          bordercolor = "rgba(0, 0, 0, 0)",
          font = list(
            color = hoverlabelFontColor,
            family = layoutFontFamily,
            size = hoverlabelFontSize
          )
        ),
        margin = list(
          l = layoutMargin,
          r = layoutMargin,
          b = layoutMargin,
          t = layoutMargin,
          pad = layoutPadding
        ),
        xaxis = list(
          fixedrange = TRUE,
          range = list(
            ~(min(datetime) - 3000), # unix time
            ~(max(datetime) + 3000)
          ),
          ticktext = list(
            ~(gsub(" 0", " ", format(as.Date(max(datetime)), "%b %d")))
          ),
          tickvals = list(
            ~(lubridate::ymd_hms(
              paste0(as.Date(max(datetime)), " 00:00:00"), 
              tz = "America/Phoenix"
            ))
          ),
          showgrid = TRUE,
          showticklabels = TRUE,
          #tickformat = ".1f",
          title = FALSE,
          zeroline = FALSE
        ),
        yaxis = list(
          fixedrange = TRUE,
          rangemode = "normal", # one of ("normal" | "tozero" | "nonnegative")
          title = "°F", 
          zeroline = FALSE
        )
      ),
    
    
    # `temp_air_minF` -----
    
    plotly::plot_ly(
      data = inDataFull, 
      x = ~datetime, 
      y = ~temp_air_minF, 
      type = "scatter", 
      mode = "lines+markers",
      line = list(color = traceLineColor, width = traceLineWidth),
      marker = list(color = traceMarkerColor, size = traceMarkerSize),
      hoverinfo = "text",
      text = ~paste0(
        "<br><b>Date:</b> ", gsub(" 0", " ", format(datetime, "%b %d, %Y")),
        "<br><b>Time:</b> ", format(datetime, "%H:%M:%S"),
        "<br><b>T<sub>min</sub></b><sup>1</sup><b>:</b> ", format(temp_air_minF, nsmall = 1), " °F"
      )
    ) %>%
      plotly::config(
        displaylogo = FALSE,
        displayModeBar = FALSE
      ) %>% 
      plotly::layout(
        font = list(
          color = layoutFontColor,
          family = layoutFontFamily,
          size = layoutFontSize
        ),
        hoverlabel = list(
          bordercolor = "rgba(0, 0, 0, 0)",
          font = list(
            color = hoverlabelFontColor,
            family = layoutFontFamily,
            size = hoverlabelFontSize
          )
        ),
        margin = list(
          l = layoutMargin,
          r = layoutMargin,
          b = layoutMargin,
          t = layoutMargin,
          pad = layoutPadding
        ),
        xaxis = list(
          fixedrange = TRUE,
          range = list(
            ~(min(datetime) - 3000), # unix time
            ~(max(datetime) + 3000)
          ),
          ticktext = list(
            ~(gsub(" 0", " ", format(as.Date(max(datetime)), "%b %d")))
          ),
          tickvals = list(
            ~(lubridate::ymd_hms(
              paste0(as.Date(max(datetime)), " 00:00:00"), 
              tz = "America/Phoenix"
            ))
          ),
          showgrid = TRUE,
          showticklabels = TRUE,
          title = FALSE,
          zeroline = FALSE
        ),
        yaxis = list(
          fixedrange = TRUE,
          rangemode = "normal", # one of ("normal" | "tozero" | "nonnegative")
          #tickformat = ".1f",
          title = "°F", 
          zeroline = FALSE
        )
      ),
    
    
    # `dwptF` -----
    
    plotly::plot_ly(
      data = inDataFull, 
      x = ~datetime, 
      y = ~dwptF, 
      type = "scatter", 
      mode = "lines+markers",
      line = list(color = traceLineColor, width = traceLineWidth),
      marker = list(color = traceMarkerColor, size = traceMarkerSize),
      hoverinfo = "text",
      text = ~paste0(
        "<br><b>Date:</b> ", gsub(" 0", " ", format(datetime, "%b %d, %Y")),
        "<br><b>Time:</b> ", format(datetime, "%H:%M:%S"),
        "<br><b>T<sub>dew point</sub>:</b> ", format(dwptF, nsmall = 1), " °F"
      )
    ) %>%
      plotly::config(
        displaylogo = FALSE,
        displayModeBar = FALSE
      ) %>% 
      plotly::layout(
        font = list(
          color = layoutFontColor,
          family = layoutFontFamily,
          size = layoutFontSize
        ),
        hoverlabel = list(
          bordercolor = "rgba(0, 0, 0, 0)",
          font = list(
            color = hoverlabelFontColor,
            family = layoutFontFamily,
            size = hoverlabelFontSize
          )
        ),
        margin = list(
          l = layoutMargin,
          r = layoutMargin,
          b = layoutMargin,
          t = layoutMargin,
          pad = layoutPadding
        ),
        xaxis = list(
          fixedrange = TRUE,
          range = list(
            ~(min(datetime) - 3000), # unix time
            ~(max(datetime) + 3000)
          ),
          ticktext = list(
            ~(gsub(" 0", " ", format(as.Date(max(datetime)), "%b %d")))
          ),
          tickvals = list(
            ~(lubridate::ymd_hms(
              paste0(as.Date(max(datetime)), " 00:00:00"), 
              tz = "America/Phoenix"
            ))
          ),
          showgrid = TRUE,
          showticklabels = TRUE,
          title = FALSE,
          zeroline = FALSE
        ),
        yaxis = list(
          fixedrange = TRUE,
          rangemode = "normal", # one of ("normal" | "tozero" | "nonnegative")
          #tickformat = ".1f",
          title = "°F", 
          zeroline = FALSE
        )
      ),
    
    
    # `temp_soil_10cmF` -----
    
    plotly::plot_ly(
      data = inDataFull, 
      x = ~datetime, 
      y = ~temp_soil_10cmF, 
      type = "scatter", 
      mode = "lines+markers",
      line = list(color = traceLineColor, width = traceLineWidth),
      marker = list(color = traceMarkerColor, size = traceMarkerSize),
      hoverinfo = "text",
      text = ~paste0(
        "<br><b>Date:</b> ", gsub(" 0", " ", format(datetime, "%b %d, %Y")),
        "<br><b>Time:</b> ", format(datetime, "%H:%M:%S"),
        "<br><b>T<sub>soil 4-inch</sub>:</b> ", format(temp_soil_10cmF, nsmall = 1), " °F"
      )
    ) %>%
      plotly::config(
        displaylogo = FALSE,
        displayModeBar = FALSE
      ) %>% 
      plotly::layout(
        font = list(
          color = layoutFontColor,
          family = layoutFontFamily,
          size = layoutFontSize
        ),
        hoverlabel = list(
          bordercolor = "rgba(0, 0, 0, 0)",
          font = list(
            color = hoverlabelFontColor,
            family = layoutFontFamily,
            size = hoverlabelFontSize
          )
        ),
        margin = list(
          l = layoutMargin,
          r = layoutMargin,
          b = layoutMargin,
          t = layoutMargin,
          pad = layoutPadding
        ),
        xaxis = list(
          fixedrange = TRUE,
          range = list(
            ~(min(datetime) - 3000), # unix time
            ~(max(datetime) + 3000)
          ),
          ticktext = list(
            ~(gsub(" 0", " ", format(as.Date(max(datetime)), "%b %d")))
          ),
          tickvals = list(
            ~(lubridate::ymd_hms(
              paste0(as.Date(max(datetime)), " 00:00:00"), 
              tz = "America/Phoenix"
            ))
          ),
          showgrid = TRUE,
          showticklabels = TRUE,
          title = FALSE,
          zeroline = FALSE
        ),
        yaxis = list(
          fixedrange = TRUE,
          rangemode = "normal", # one of ("normal" | "tozero" | "nonnegative")
          #tickformat = ".1f",
          title = "°F", 
          zeroline = FALSE
        )
      ),
    
    
    # `temp_soil_50cmF` -----
    
    plotly::plot_ly(
      data = inDataFull, 
      x = ~datetime, 
      y = ~temp_soil_50cmF, 
      type = "scatter", 
      mode = "lines+markers",
      line = list(color = traceLineColor, width = traceLineWidth),
      marker = list(color = traceMarkerColor, size = traceMarkerSize),
      hoverinfo = "text",
      text = ~paste0(
        "<br><b>Date:</b> ", gsub(" 0", " ", format(datetime, "%b %d, %Y")),
        "<br><b>Time:</b> ", format(datetime, "%H:%M:%S"),
        "<br><b>T<sub>soil 20-inch</sub>:</b> ", format(temp_soil_50cmF, nsmall = 1), " °F"
      )
    ) %>%
      plotly::config(
        displaylogo = FALSE,
        displayModeBar = FALSE
      ) %>% 
      plotly::layout(
        font = list(
          color = layoutFontColor,
          family = layoutFontFamily,
          size = layoutFontSize
        ),
        hoverlabel = list(
          bordercolor = "rgba(0, 0, 0, 0)",
          font = list(
            color = hoverlabelFontColor,
            family = layoutFontFamily,
            size = hoverlabelFontSize
          )
        ),
        margin = list(
          l = layoutMargin,
          r = layoutMargin,
          b = layoutMargin,
          t = layoutMargin,
          pad = layoutPadding
        ),
        xaxis = list(
          fixedrange = TRUE,
          range = list(
            ~(min(datetime) - 3000), # unix time
            ~(max(datetime) + 3000)
          ),
          ticktext = list(
            ~(gsub(" 0", " ", format(as.Date(max(datetime)), "%b %d")))
          ),
          tickvals = list(
            ~(lubridate::ymd_hms(
              paste0(as.Date(max(datetime)), " 00:00:00"), 
              tz = "America/Phoenix"
            ))
          ),
          showgrid = TRUE,
          showticklabels = TRUE,
          title = FALSE,
          zeroline = FALSE
        ),
        yaxis = list(
          fixedrange = TRUE,
          rangemode = "normal", # one of ("normal" | "tozero" | "nonnegative")
          tickformat = ".1f",
          title = "°F", 
          zeroline = FALSE
        )
      ),
    
    
    # `wind_vector_dir` -----
    
    plotly::plot_ly(
      data = inDataFull, 
      x = ~datetime, 
      y = ~wind_vector_dir, 
      type = "scatter", 
      mode = "lines+markers",
      line = list(color = traceLineColor, width = traceLineWidth),
      marker = list(color = traceMarkerColor, size = traceMarkerSize),
      hoverinfo = "text",
      text = ~paste0(
        "<br><b>Date:</b> ", gsub(" 0", " ", format(datetime, "%b %d, %Y")),
        "<br><b>Time:</b> ", format(datetime, "%H:%M:%S"),
        "<br><b>WD:</b> ", format(wind_vector_dir, nsmall = 0), " deg"
      )
    ) %>%
      plotly::config(
        displaylogo = FALSE,
        displayModeBar = FALSE
      ) %>% 
      plotly::layout(
        font = list(
          color = layoutFontColor,
          family = layoutFontFamily,
          size = layoutFontSize
        ),
        hoverlabel = list(
          bordercolor = "rgba(0, 0, 0, 0)",
          font = list(
            color = hoverlabelFontColor,
            family = layoutFontFamily,
            size = hoverlabelFontSize
          )
        ),
        margin = list(
          l = layoutMargin,
          r = layoutMargin,
          b = layoutMargin,
          t = layoutMargin,
          pad = layoutPadding
        ),
        xaxis = list(
          fixedrange = TRUE,
          range = list(
            ~(min(datetime) - 3000), # unix time
            ~(max(datetime) + 3000)
          ),
          ticktext = list(
            ~(gsub(" 0", " ", format(as.Date(max(datetime)), "%b %d")))
          ),
          tickvals = list(
            ~(lubridate::ymd_hms(
              paste0(as.Date(max(datetime)), " 00:00:00"), 
              tz = "America/Phoenix"
            ))
          ),
          showgrid = TRUE,
          showticklabels = TRUE,
          title = FALSE,
          zeroline = FALSE
        ),
        yaxis = list(
          fixedrange = TRUE,
          rangemode = "nonnegative", # one of ("normal" | "tozero" | "nonnegative")
          #tickformat = ".0f",
          title = "deg", 
          zeroline = FALSE
        )
      ),
    
    
    # `wind_2min_vector_dir` -----
    
    plotly::plot_ly(
      data = inDataFull, 
      x = ~datetime, 
      y = ~wind_2min_vector_dir, 
      type = "scatter", 
      mode = "lines+markers",
      line = list(color = traceLineColor, width = traceLineWidth),
      marker = list(color = traceMarkerColor, size = traceMarkerSize),
      hoverinfo = "text",
      text = ~paste0(
        "<br><b>Date:</b> ", gsub(" 0", " ", format(datetime, "%b %d, %Y")),
        "<br><b>Time:</b> ", format(datetime, "%H:%M:%S"),
        "<br><b>WD<sub>2-min</sub>:</b> ", format(wind_2min_vector_dir, nsmall = 0), " deg"
      )
    ) %>%
      plotly::config(
        displaylogo = FALSE,
        displayModeBar = FALSE
      ) %>% 
      plotly::layout(
        font = list(
          color = layoutFontColor,
          family = layoutFontFamily,
          size = layoutFontSize
        ),
        hoverlabel = list(
          bordercolor = "rgba(0, 0, 0, 0)",
          font = list(
            color = hoverlabelFontColor,
            family = layoutFontFamily,
            size = hoverlabelFontSize
          )
        ),
        margin = list(
          l = layoutMargin,
          r = layoutMargin,
          b = layoutMargin,
          t = layoutMargin,
          pad = layoutPadding
        ),
        xaxis = list(
          fixedrange = TRUE,
          range = list(
            ~(min(datetime) - 3000), # unix time
            ~(max(datetime) + 3000)
          ),
          ticktext = list(
            ~(gsub(" 0", " ", format(as.Date(max(datetime)), "%b %d")))
          ),
          tickvals = list(
            ~(lubridate::ymd_hms(
              paste0(as.Date(max(datetime)), " 00:00:00"), 
              tz = "America/Phoenix"
            ))
          ),
          showgrid = TRUE,
          showticklabels = TRUE,
          title = FALSE,
          zeroline = FALSE
        ),
        yaxis = list(
          fixedrange = TRUE,
          rangemode = "nonnegative", # one of ("normal" | "tozero" | "nonnegative")
          #tickformat = ".0f",
          title = "deg", 
          zeroline = FALSE
        )
      ),
    
    
    # `wind_2min_vector_dir_max_daily` -----
    
    plotly::plot_ly(
      data = inDataFull, 
      x = ~datetime, 
      y = ~wind_2min_vector_dir_max_daily, 
      type = "scatter", 
      mode = "lines+markers",
      line = list(color = traceLineColor, width = traceLineWidth),
      marker = list(color = traceMarkerColor, size = traceMarkerSize),
      hoverinfo = "text",
      text = ~paste0(
        "<br><b>Date:</b> ", gsub(" 0", " ", format(datetime, "%b %d, %Y")),
        "<br><b>Time:</b> ", format(datetime, "%H:%M:%S"),
        "<br><b>WD<sub>2-min max</sub></b><sup>1</sup><b>:</b> ", format(wind_2min_vector_dir_max_daily, nsmall = 0), " deg"
      )
    ) %>%
      plotly::config(
        displaylogo = FALSE,
        displayModeBar = FALSE
      ) %>% 
      plotly::layout(
        font = list(
          color = layoutFontColor,
          family = layoutFontFamily,
          size = layoutFontSize
        ),
        hoverlabel = list(
          bordercolor = "rgba(0, 0, 0, 0)",
          font = list(
            color = hoverlabelFontColor,
            family = layoutFontFamily,
            size = hoverlabelFontSize
          )
        ),
        margin = list(
          l = layoutMargin,
          r = layoutMargin,
          b = layoutMargin,
          t = layoutMargin,
          pad = layoutPadding
        ),
        xaxis = list(
          fixedrange = TRUE,
          range = list(
            ~(min(datetime) - 3000), # unix time
            ~(max(datetime) + 3000)
          ),
          ticktext = list(
            ~(gsub(" 0", " ", format(as.Date(max(datetime)), "%b %d")))
          ),
          tickvals = list(
            ~(lubridate::ymd_hms(
              paste0(as.Date(max(datetime)), " 00:00:00"), 
              tz = "America/Phoenix"
            ))
          ),
          showgrid = TRUE,
          showticklabels = TRUE,
          title = FALSE,
          zeroline = FALSE
        ),
        yaxis = list(
          fixedrange = TRUE,
          rangemode = "nonnegative", # one of ("normal" | "tozero" | "nonnegative")
          #tickformat = ".0f",
          title = "deg", 
          zeroline = FALSE
        )
      ),
    
    
    # `wind_2min_vector_dir_max_hourly` -----
    
    plotly::plot_ly(
      data = inDataFull, 
      x = ~datetime, 
      y = ~wind_2min_vector_dir_max_hourly, 
      type = "scatter", 
      mode = "lines+markers",
      line = list(color = traceLineColor, width = traceLineWidth),
      marker = list(color = traceMarkerColor, size = traceMarkerSize),
      hoverinfo = "text",
      text = ~paste0(
        "<br><b>Date:</b> ", gsub(" 0", " ", format(datetime, "%b %d, %Y")),
        "<br><b>Time:</b> ", format(datetime, "%H:%M:%S"),
        "<br><b>WD<sub>2-min max</sub></b><sup>2</sup><b>:</b> ", format(wind_2min_vector_dir_max_hourly, nsmall = 0), " deg"
      )
    ) %>%
      plotly::config(
        displaylogo = FALSE,
        displayModeBar = FALSE
      ) %>% 
      plotly::layout(
        font = list(
          color = layoutFontColor,
          family = layoutFontFamily,
          size = layoutFontSize
        ),
        hoverlabel = list(
          bordercolor = "rgba(0, 0, 0, 0)",
          font = list(
            color = hoverlabelFontColor,
            family = layoutFontFamily,
            size = hoverlabelFontSize
          )
        ),
        margin = list(
          l = layoutMargin,
          r = layoutMargin,
          b = layoutMargin,
          t = layoutMargin,
          pad = layoutPadding
        ),
        xaxis = list(
          fixedrange = TRUE,
          range = list(
            ~(min(datetime) - 3000), # unix time
            ~(max(datetime) + 3000)
          ),
          ticktext = list(
            ~(gsub(" 0", " ", format(as.Date(max(datetime)), "%b %d")))
          ),
          tickvals = list(
            ~(lubridate::ymd_hms(
              paste0(as.Date(max(datetime)), " 00:00:00"), 
              tz = "America/Phoenix"
            ))
          ),
          showgrid = TRUE,
          showticklabels = TRUE,
          title = FALSE,
          zeroline = FALSE
        ),
        yaxis = list(
          fixedrange = TRUE,
          rangemode = "nonnegative", # one of ("normal" | "tozero" | "nonnegative")
          #tickformat = ".0f",
          title = "deg", 
          zeroline = FALSE
        )
      ),
    
    
    # `wind_spd_mph` -----
    
    plotly::plot_ly(
      data = inDataFull, 
      x = ~datetime, 
      y = ~wind_spd_mph, 
      type = "scatter", 
      mode = "lines+markers",
      line = list(color = traceLineColor, width = traceLineWidth),
      marker = list(color = traceMarkerColor, size = traceMarkerSize),
      hoverinfo = "text",
      text = ~paste0(
        "<br><b>Date:</b> ", gsub(" 0", " ", format(datetime, "%b %d, %Y")),
        "<br><b>Time:</b> ", format(datetime, "%H:%M:%S"),
        "<br><b>WS:</b> ", format(wind_spd_mph, nsmall = 1), " mph"
      )
    ) %>%
      plotly::config(
        displaylogo = FALSE,
        displayModeBar = FALSE
      ) %>% 
      plotly::layout(
        font = list(
          color = layoutFontColor,
          family = layoutFontFamily,
          size = layoutFontSize
        ),
        hoverlabel = list(
          bordercolor = "rgba(0, 0, 0, 0)",
          font = list(
            color = hoverlabelFontColor,
            family = layoutFontFamily,
            size = hoverlabelFontSize
          )
        ),
        margin = list(
          l = layoutMargin,
          r = layoutMargin,
          b = layoutMargin,
          t = layoutMargin,
          pad = layoutPadding
        ),
        xaxis = list(
          fixedrange = TRUE,
          range = list(
            ~(min(datetime) - 3000), # unix time
            ~(max(datetime) + 3000)
          ),
          ticktext = list(
            ~(gsub(" 0", " ", format(as.Date(max(datetime)), "%b %d")))
          ),
          tickvals = list(
            ~(lubridate::ymd_hms(
              paste0(as.Date(max(datetime)), " 00:00:00"), 
              tz = "America/Phoenix"
            ))
          ),
          showgrid = TRUE,
          showticklabels = TRUE,
          title = FALSE,
          zeroline = FALSE
        ),
        yaxis = list(
          fixedrange = TRUE,
          rangemode = "nonnegative", # one of ("normal" | "tozero" | "nonnegative")
          #tickformat = ".0f",
          title = "mph", 
          zeroline = FALSE
        )
      ),
    
    
    # `wind_spd_max_mph` -----
    
    plotly::plot_ly(
      data = inDataFull, 
      x = ~datetime, 
      y = ~wind_spd_max_mph, 
      type = "scatter", 
      mode = "lines+markers",
      line = list(color = traceLineColor, width = traceLineWidth),
      marker = list(color = traceMarkerColor, size = traceMarkerSize),
      hoverinfo = "text",
      text = ~paste0(
        "<br><b>Date:</b> ", gsub(" 0", " ", format(datetime, "%b %d, %Y")),
        "<br><b>Time:</b> ", format(datetime, "%H:%M:%S"),
        "<br><b>WS<sub>max</sub></b><sup>1</sup><b>:</b> ", format(wind_spd_max_mph, nsmall = 1), " mph"
      )
    ) %>%
      plotly::config(
        displaylogo = FALSE,
        displayModeBar = FALSE
      ) %>% 
      plotly::layout(
        font = list(
          color = layoutFontColor,
          family = layoutFontFamily,
          size = layoutFontSize
        ),
        hoverlabel = list(
          bordercolor = "rgba(0, 0, 0, 0)",
          font = list(
            color = hoverlabelFontColor,
            family = layoutFontFamily,
            size = hoverlabelFontSize
          )
        ),
        margin = list(
          l = layoutMargin,
          r = layoutMargin,
          b = layoutMargin,
          t = layoutMargin,
          pad = layoutPadding
        ),
        xaxis = list(
          fixedrange = TRUE,
          range = list(
            ~(min(datetime) - 3000), # unix time
            ~(max(datetime) + 3000)
          ),
          ticktext = list(
            ~(gsub(" 0", " ", format(as.Date(max(datetime)), "%b %d")))
          ),
          tickvals = list(
            ~(lubridate::ymd_hms(
              paste0(as.Date(max(datetime)), " 00:00:00"), 
              tz = "America/Phoenix"
            ))
          ),
          showgrid = TRUE,
          showticklabels = TRUE,
          title = FALSE,
          zeroline = FALSE
        ),
        yaxis = list(
          fixedrange = TRUE,
          rangemode = "nonnegative", # one of ("normal" | "tozero" | "nonnegative")
          #tickformat = ".0f",
          title = "mph", 
          zeroline = FALSE
        )
      ),
    
    
    # `wind_2min_spd_mean_mph` -----
    
    plotly::plot_ly(
      data = inDataFull, 
      x = ~datetime, 
      y = ~wind_2min_spd_mean_mph, 
      type = "scatter", 
      mode = "lines+markers",
      line = list(color = traceLineColor, width = traceLineWidth),
      marker = list(color = traceMarkerColor, size = traceMarkerSize),
      hoverinfo = "text",
      text = ~paste0(
        "<br><b>Date:</b> ", gsub(" 0", " ", format(datetime, "%b %d, %Y")),
        "<br><b>Time:</b> ", format(datetime, "%H:%M:%S"),
        "<br><b>WS<sub>2-min</sub>:</b> ", format(wind_2min_spd_mean_mph, nsmall = 1), " mph"
      )
    ) %>%
      plotly::config(
        displaylogo = FALSE,
        displayModeBar = FALSE
      ) %>% 
      plotly::layout(
        font = list(
          color = layoutFontColor,
          family = layoutFontFamily,
          size = layoutFontSize
        ),
        hoverlabel = list(
          bordercolor = "rgba(0, 0, 0, 0)",
          font = list(
            color = hoverlabelFontColor,
            family = layoutFontFamily,
            size = hoverlabelFontSize
          )
        ),
        margin = list(
          l = layoutMargin,
          r = layoutMargin,
          b = layoutMargin,
          t = layoutMargin,
          pad = layoutPadding
        ),
        xaxis = list(
          fixedrange = TRUE,
          range = list(
            ~(min(datetime) - 3000), # unix time
            ~(max(datetime) + 3000)
          ),
          ticktext = list(
            ~(gsub(" 0", " ", format(as.Date(max(datetime)), "%b %d")))
          ),
          tickvals = list(
            ~(lubridate::ymd_hms(
              paste0(as.Date(max(datetime)), " 00:00:00"), 
              tz = "America/Phoenix"
            ))
          ),
          showgrid = TRUE,
          showticklabels = TRUE,
          title = FALSE,
          zeroline = FALSE
        ),
        yaxis = list(
          fixedrange = TRUE,
          rangemode = "nonnegative", # one of ("normal" | "tozero" | "nonnegative")
          #tickformat = ".0f",
          title = "mph", 
          zeroline = FALSE
        )
      ),
    
    
    # `wind_2min_spd_max_mph_daily` -----
    
    plotly::plot_ly(
      data = inDataFull, 
      x = ~datetime, 
      y = ~wind_2min_spd_max_mph_daily, 
      type = "scatter", 
      mode = "lines+markers",
      line = list(color = traceLineColor, width = traceLineWidth),
      marker = list(color = traceMarkerColor, size = traceMarkerSize),
      hoverinfo = "text",
      text = ~paste0(
        "<br><b>Date:</b> ", gsub(" 0", " ", format(datetime, "%b %d, %Y")),
        "<br><b>Time:</b> ", format(datetime, "%H:%M:%S"),
        "<br><b>WS<sub>2-min max</sub></b><sup>1</sup><b>:</b> ", format(wind_2min_spd_max_mph_daily, nsmall = 1), " mph"
      )
    ) %>%
      plotly::config(
        displaylogo = FALSE,
        displayModeBar = FALSE
      ) %>% 
      plotly::layout(
        font = list(
          color = layoutFontColor,
          family = layoutFontFamily,
          size = layoutFontSize
        ),
        hoverlabel = list(
          bordercolor = "rgba(0, 0, 0, 0)",
          font = list(
            color = hoverlabelFontColor,
            family = layoutFontFamily,
            size = hoverlabelFontSize
          )
        ),
        margin = list(
          l = layoutMargin,
          r = layoutMargin,
          b = layoutMargin,
          t = layoutMargin,
          pad = layoutPadding
        ),
        xaxis = list(
          fixedrange = TRUE,
          range = list(
            ~(min(datetime) - 3000), # unix time
            ~(max(datetime) + 3000)
          ),
          ticktext = list(
            ~(gsub(" 0", " ", format(as.Date(max(datetime)), "%b %d")))
          ),
          tickvals = list(
            ~(lubridate::ymd_hms(
              paste0(as.Date(max(datetime)), " 00:00:00"), 
              tz = "America/Phoenix"
            ))
          ),
          showgrid = TRUE,
          showticklabels = TRUE,
          title = FALSE,
          zeroline = FALSE
        ),
        yaxis = list(
          fixedrange = TRUE,
          rangemode = "nonnegative", # one of ("normal" | "tozero" | "nonnegative")
          #tickformat = ".0f",
          title = "mph", 
          zeroline = FALSE
        )
      ),
    
    
    # `wind_2min_spd_max_mph_hourly` -----
    
    plotly::plot_ly(
      data = inDataFull, 
      x = ~datetime, 
      y = ~wind_2min_spd_max_mph_hourly, 
      type = "scatter", 
      mode = "lines+markers",
      line = list(color = traceLineColor, width = traceLineWidth),
      marker = list(color = traceMarkerColor, size = traceMarkerSize),
      hoverinfo = "text",
      text = ~paste0(
        "<br><b>Date:</b> ", gsub(" 0", " ", format(datetime, "%b %d, %Y")),
        "<br><b>Time:</b> ", format(datetime, "%H:%M:%S"),
        "<br><b>WS<sub>2-min max</sub></b><sup>2</sup><b>:</b> ", format(wind_2min_spd_max_mph_hourly, nsmall = 1), " mph"
      )
    ) %>%
      plotly::config(
        displaylogo = FALSE,
        displayModeBar = FALSE
      ) %>% 
      plotly::layout(
        font = list(
          color = layoutFontColor,
          family = layoutFontFamily,
          size = layoutFontSize
        ),
        hoverlabel = list(
          bordercolor = "rgba(0, 0, 0, 0)",
          font = list(
            color = hoverlabelFontColor,
            family = layoutFontFamily,
            size = hoverlabelFontSize
          )
        ),
        margin = list(
          l = layoutMargin,
          r = layoutMargin,
          b = layoutMargin,
          t = layoutMargin,
          pad = layoutPadding
        ),
        xaxis = list(
          fixedrange = TRUE,
          range = list(
            ~(min(datetime) - 3000), # unix time
            ~(max(datetime) + 3000)
          ),
          ticktext = list(
            ~(gsub(" 0", " ", format(as.Date(max(datetime)), "%b %d")))
          ),
          tickvals = list(
            ~(lubridate::ymd_hms(
              paste0(as.Date(max(datetime)), " 00:00:00"), 
              tz = "America/Phoenix"
            ))
          ),
          showgrid = TRUE,
          showticklabels = TRUE,
          title = FALSE,
          zeroline = FALSE
        ),
        yaxis = list(
          fixedrange = TRUE,
          rangemode = "nonnegative", # one of ("normal" | "tozero" | "nonnegative")
          #tickformat = ".0f",
          title = "mph", 
          zeroline = FALSE
        )
      )
  ) # `slsCardGraphs`
  
  return(slsCardGraphs)
}
