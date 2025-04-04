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
    plotly::plot_ly( # `precip_total_in` -----
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
        "<br><b>P</b><sup>1</sup><b>:</b> ", precip_total_in, " in"
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
          title = "in", 
          zeroline = FALSE
        )
      ),
    
    plotly::plot_ly( # `relative_humidity` -----
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
        "<br><b>", "RH", ":</b> ", relative_humidity, " %"
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
          title = "in", 
          zeroline = FALSE
        )
      ),
     
    plotly::plot_ly( # `sol_rad_Wm2` -----
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
        "<br><b>", "SR", ":</b> ", sol_rad_Wm2, " W/m<sup>2</sup>"
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
          title = "W/m<sup>2</sup>", 
          zeroline = FALSE
        )
      )
  ) # `slsCardGraphs`
  
  return(slsCardGraphs)
}
