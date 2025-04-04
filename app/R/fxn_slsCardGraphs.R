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
  inDataFull <- inDataFull %>% 
    dplyr::filter(meta_station_name == azmetStation) %>% 
    dplyr::mutate(datetime = lubridate::ymd_hms(datetime))
  
  slsCardGraphs <- list(
    plotly::plot_ly(
      data = inDataFull, 
      x = ~datetime, 
      y = ~precip_total_in, 
      type = "scatter", 
      mode = "lines+markers",
      line = list(color = "#808080", width = 1),
      marker = list(color = "#808080", size = 3),
      hoverinfo = "text",
      text = ~paste0(
        "<br><b>Date:</b> ", gsub(" 0", " ", format(datetime, "%b %d, %Y")),
        "<br><b>Time:</b> ", format(datetime, "%H:%M:%S"),
        "<br><b>", "RH", ":</b> ", precip_total_in, " in"
      )
    ) %>%
      plotly::config(
        displaylogo = FALSE,
        displayModeBar = FALSE
      ) %>% 
      plotly::layout(
        font = list(
          color = "#707070",
          family = "proxima-nova, calibri, -apple-system, BlinkMacSystemFont, \"Segoe UI\", Roboto, \"Helvetica Neue\", Arial, \"Noto Sans\", sans-serif, \"Apple Color Emoji\", \"Segoe UI Emoji\", \"Segoe UI Symbol\", \"Noto Color Emoji\"",
          size = 11
        ),
        hoverlabel = list(
          bordercolor = "rgba(0, 0, 0, 0)",
          font = list(
            color = "#FFFFFF",
            family = "proxima-nova, calibri, -apple-system, BlinkMacSystemFont, \"Segoe UI\", Roboto, \"Helvetica Neue\", Arial, \"Noto Sans\", sans-serif, \"Apple Color Emoji\", \"Segoe UI Emoji\", \"Segoe UI Symbol\", \"Noto Color Emoji\"",
            size = 14
          )
        ),
        margin = list(
          l = 6,
          r = 6,
          b = 6,
          t = 6,
          pad = 0
        ),
        xaxis = list(
          range = list(~(min(datetime) - 3000), ~(max(datetime) + 3000)), # unix timestamp valueshowgrid = FALSE,
          tickfont = list(
            size = 11 
          ),
          ticktext = list(
            ~(gsub(" 0", " ", format(as.Date(max(datetime)), "%b %d")))
          ),
          tickvals = list(
            ~(lubridate::ymd_hms(
              paste0(as.Date(min(datetime)), " 23:59:59"), 
              tz = "America/Phoenix"
            )
            )
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
    
    plotly::plot_ly(
      data = inDataFull, 
      x = ~datetime, 
      y = ~relative_humidity, 
      type = "scatter", 
      mode = "lines+markers",
      line = list(color = "#808080", width = 1),
      marker = list(color = "#808080", size = 3),
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
          color = "#707070",
          family = "proxima-nova, calibri, -apple-system, BlinkMacSystemFont, \"Segoe UI\", Roboto, \"Helvetica Neue\", Arial, \"Noto Sans\", sans-serif, \"Apple Color Emoji\", \"Segoe UI Emoji\", \"Segoe UI Symbol\", \"Noto Color Emoji\"",
          size = 11
        ),
        hoverlabel = list(
          bordercolor = "rgba(0, 0, 0, 0)",
          font = list(
            color = "#FFFFFF",
            family = "proxima-nova, calibri, -apple-system, BlinkMacSystemFont, \"Segoe UI\", Roboto, \"Helvetica Neue\", Arial, \"Noto Sans\", sans-serif, \"Apple Color Emoji\", \"Segoe UI Emoji\", \"Segoe UI Symbol\", \"Noto Color Emoji\"",
            size = 14
          )
        ),
        margin = list(
          l = 6,
          r = 6,
          b = 6,
          t = 6,
          pad = 0
        ),
        xaxis = list(
          range = list(~(min(datetime) - 3000), ~(max(datetime) + 3000)), # unix timestamp valueshowgrid = FALSE,
          tickfont = list(
            size = 11 
          ),
          ticktext = list(
            ~(gsub(" 0", " ", format(as.Date(max(datetime)), "%b %d")))
          ),
          tickvals = list(
            ~(lubridate::ymd_hms(
              paste0(as.Date(min(datetime)), " 23:59:59"), 
              tz = "America/Phoenix"
             )
            )
          ),
          showgrid = TRUE,
          showticklabels = TRUE,
          title = FALSE,
          zeroline = FALSE
        ),
        yaxis = list(
          title = "%",
          zeroline = FALSE
        )
      )
  )
  
  
  return(slsCardGraphs)
}
