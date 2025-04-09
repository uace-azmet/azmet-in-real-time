# Libraries --------------------

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


# Files --------------------

# Functions. Loaded automatically at app start if in `R` folder
#source("./R/fxn_functionName.R", local = TRUE)

# Scripts. Loaded automatically at app start if in `R` folder
#source("./R/scr_scriptName.R", local = TRUE)


# Variables --------------------

# Initialize, part of keeping `input$azmetStation` selection when refreshing data
azmetStation <- shiny::reactiveVal(value = "Aguila")


# Other --------------------

shiny::addResourcePath("shinyjs", system.file("srcjs", package = "shinyjs"))
