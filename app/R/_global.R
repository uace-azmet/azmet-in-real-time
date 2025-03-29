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

fig <- plot_ly(data, x = ~x, y = ~random_y, type = 'scatter', mode = 'lines') %>% 
  plotly::config(
    displaylogo = FALSE,
    displayModeBar = FALSE)


card1 <- bslib::card(
  id = "cardGraph",
  bslib::card_header(
    htmltools::HTML(
      paste0(
        "P<sup>", 
        tags$span(style = "font-weight: normal", "1"), 
        "</sup>",
        htmltools::HTML("&nbsp;"),
        htmltools::HTML("&nbsp;"),
        tags$span(style = "font-weight: normal; font-size: normal", " 0.25 in")
      )
    )
  ),
  full_screen = TRUE,
  height = 300,
  fig
)
card2 <- bslib::card(
  bslib::card_header("Nothing much here"),
  full_screen = TRUE,
  height = 300,
  "This is it."
)
card3 <- bslib::card(
  bslib::card_header("Nothing much here"),
  full_screen = TRUE,
  height = 300,
  "This is it."
)
card4 <- bslib::card(
  bslib::card_header("Nothing much here"),
  height = 300,
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
