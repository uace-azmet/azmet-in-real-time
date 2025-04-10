slsSidebar <- 
  bslib::sidebar(
    width = 300,
    position = "left",
    open = list(desktop = "open", mobile = "always-above"),
    id = "sidebar",
    title = NULL,
    bg = "#FFFFFF",
    fg = "#191919", # https://www.color-hex.com/color-palette/1041718
    class = NULL,
    max_height_mobile = NULL,
    gap = NULL,
    padding = NULL,
    
    htmltools::p(
      bsicons::bs_icon("sliders"), 
      htmltools::HTML("&nbsp;"), 
      "DATA OPTIONS"
    ),
    
    shiny::helpText(
      shiny::em(
        "Select an AZMet station to display its latest 15-minute data."
      )
    ),
    
    shiny::selectInput(
      inputId = "azmetStation", 
      label = "AZMet Station",
      choices = NULL, # see `app.R`, shiny::updateSelectInput(inputId = "azmetStation")
      selected = NULL # see `app.R`, shiny::updateSelectInput(inputId = "azmetStation")
    )
  ) # bslib::sidebar()
