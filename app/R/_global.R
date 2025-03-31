# Libraries
library(azmetr)
library(bsicons)
library(bslib)
library(dplyr)
library(ggplot2)
library(htmltools)
library(plotly)
library(RColorBrewer)
library(reactable)
library(shiny)
library(shinyjs)

# Functions. Loaded automatically at app start if in `R` folder
#source("./R/fxn_functionName.R", local = TRUE)

# Scripts. Loaded automatically at app start if in `R` folder
#source("./R/scr_scriptName.R", local = TRUE)

shiny::addResourcePath("shinyjs", system.file("srcjs", package = "shinyjs"))


# Files --------------------

azmetStations <- 
  vroom::vroom(
    file = "aux-files/azmet-stations-api-db.csv", 
    delim = ",", 
    col_names = TRUE, 
    show_col_types = FALSE
  )


# Variables --------------------


x <- c(1:100)
random_y <- rnorm(100, mean = 0)
data <- data.frame(x, random_y)
dataLast <- tail(data, n = 1)

fig <- 
  plotly::plot_ly(
    data = data, 
    x = ~x, 
    y = ~random_y, 
    type = 'scatter', 
    mode = 'lines', 
    line = list(color = "#606060", width = 1),
    showlegend = FALSE
  ) %>%
  plotly::add_trace(
    data = dataLast, 
    x = ~x, 
    y = ~random_y, 
    type = 'scatter', 
    mode = 'lines+markers', 
    marker = list(color = "#606060"),
    showlegend = FALSE
  ) %>%
  plotly::config(
    displaylogo = FALSE,
    displayModeBar = FALSE
  ) %>% 
  plotly::layout(
    yaxis = list(
      zeroline = FALSE
    )
  )


card1 <- bslib::card(
  bslib::card_header(
    class = "d-flex justify-content-between p-1 sls-card-header",
    htmltools::div(
      htmltools::HTML(paste0(
        tags$span(style = "font-family: monospace; font-weight: bold; font-size: 0.9rem;", "P"),
        "<sup>", 
        tags$span(style = "font-family: monospace; font-weight: normal; ", "1"), 
        "</sup>"
      ))
    ),
    htmltools::div(
      tags$span(style = "font-family: monospace; font-weight: normal; font-size: 0.9rem;", " 0.25 in")
    )
  ),
  #height = 300,
  full_screen = TRUE,
  bslib::card_body(
    class = "p-0",
    #min_height = 300,
    #id = "slsCardBody",
    #full_screen = TRUE,
    #height = "500px",
    fig
  ),
  class = "sls-card"
  #height = "500px"
)
card2 <- bslib::card(
  bslib::card_header("Nothing much here"),
  full_screen = TRUE,
  #height = "300px",
  "This is it."
)
card3 <- bslib::card(
  bslib::card_header("Nothing much here"),
  full_screen = TRUE,
  #height = "300px",
  "This is it."
)
card4 <- bslib::card(
  #height = "300px",
  "This is it."
)

stationGroups <- 
  tibble::tibble(
    group1 = c("Ft Mohave CA", "Mohave", "Mohave ETo", "Mohave-2", "Parker", "Parker-2"),
    group2 = c("Roll", "Wellton ETo", "Yuma N.Gila", "Yuma South", "Yuma Valley", "Yuma Valley ETo"),
    group3 = c("Aguila", "Buckeye", "Harquahala", "Paloma", "Salome", NA),
    group4 = c("Desert Ridge", "Payson", "Phoenix Encanto", "Phoenix Greenway", NA, NA),
    group5 = c("Coolidge", "Maricopa", "Queen Creek", "Sahuarita", "Test", "Tucson"),
    group6 = c("Bonita", "Bowie", "Safford", "San Simon", "Willcox Bench", NA)
  )
