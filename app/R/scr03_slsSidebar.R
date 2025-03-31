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
    )#,
    
    #shiny::selectInput(
    #  inputId = "stationVariable", 
    #  label = "Station Variable",
    #  choices = NULL, # see `app.R`, shiny::updateSelectInput(inputId = "stationVariable")
    #  selected = NULL # see `app.R`, shiny::updateSelectInput(inputId = "stationVariable")
    #),
    
    #htmltools::br(),
    
    #shiny::htmlOutput(outputId = "slsLatestDataTitle"),
    #shiny::htmlOutput(outputId = "slsLatestDataUpdate")#,
    #reactable::reactableOutput(outputId = "slsLatestDataTable"),
    #htmltools::p(
    #  htmltools::HTML(
    #    paste0(
    #      bsicons::bs_icon("table"), 
    #      htmltools::HTML("&nbsp;"),
    #      htmltools::HTML("&nbsp;"),
    #      toupper("The latest 15-minute data from across the network")
    #    ),
    #  ),
    #  
    #  class = "nws-table-title"
    #),
    
    #card4
    #reactable::reactableOutput(outputId = "stationGroupsTable")
  ) # bslib::sidebar()
