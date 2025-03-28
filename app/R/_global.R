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

card1 <- bslib::card(
  bslib::card_header("Nothing much here"),
  full_screen = TRUE,
  height = 300,
  "This is it."
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
