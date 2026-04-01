# Tabular and graphical summaries of the most recent 15-minute data from stations across the network


# PROCESS FOR PWA -----

# dir.create("app/www/pwa")
# Copy `azmet-pwa-icon-192x192.png` to `app/www/pwa/`
# Copy `azmet-pwa-icon-512x512.png` to `app/www/pwa/`
# Copy `azmet-pwa-icon.svg` to `app/www/pwa/`
# Copy `pwa-service-worker.js` to `app/www/pwa/`
# Copy `pwa.html` to `app/www/pwa/`
# Copy `manifest.webmanifest` to `app/www/`
# Add `tags$head(includeHTML("www/pwa/pwa.html"))` to `app.R`


# UI --------------------

ui <-
  htmltools::htmlTemplate(
    filename = "azmet-shiny-template.html",

    # Apparent bug in `bslib`, see: https://github.com/rstudio/bslib/issues/834
    #pageNavbar = bslib::page_navbar(
    #navsetTab = bslib::navset_tab(
    #navsetCardTab = bslib::navset_card_tab(

    # Work-around by placing the navset in `bslib::page()`, which correctly renders tabs on webpage
    navsetCardTab = bslib::page(
      title = NULL,
      theme = theme, # `scr##_theme.R`
      htmltools::tags$head(htmltools::includeHTML("www/pwa/pwa.html")),
      bslib::navset_card_tab(
        id = "navsetCardTab",
        selected = "network-wide-summary",
        title = NULL,
        sidebar = NULL,
        header = NULL,
        footer = NULL,
        height = 780,
        full_screen = TRUE,
        #wrapper = card_body,

        # Network-wide Summary (nws) -----

        bslib::nav_panel(
          # https://getbootstrap.com/docs/5.0/utilities/display/#hiding-elements
          title = htmltools::div(
            htmltools::span(
              "Network-wide Summary",
              class = "d-none d-md-block"
            ), # on devices "medium" (md) or larger
            htmltools::span("Network-wide...", class = "d-block d-md-none") # on smaller devices
          ),

          shiny::htmlOutput(outputId = "nwsTableTitle"),
          reactable::reactableOutput(outputId = "nwsTable", height = "100%"),
          shiny::htmlOutput(outputId = "nwsTableFooter"),

          value = "network-wide-summary"
        ),

        # Station-level summaries (sls) -----

        bslib::nav_panel(
          title = htmltools::div(
            htmltools::span(
              "Station-level Summaries",
              class = "d-none d-md-block"
            ),
            htmltools::span("Station-level...", class = "d-block d-md-none")
          ),

          bslib::layout_sidebar(
            sidebar = slsSidebar, # `scr##_slsSidebar.R`

            shiny::htmlOutput(outputId = "slsCardLayoutTitle"),
            shiny::htmlOutput(outputId = "slsLatestDataUpdate"),
            shiny::htmlOutput(outputId = "slsCardLayout"),
            shiny::htmlOutput(outputId = "slsCardLayoutFooter")

            #fillable = TRUE,
            #fill = TRUE,
            #bg = NULL,
            #fg = NULL,
            #border = NULL,
            #border_radius = NULL,
            #border_color = NULL,
            #padding = NULL,
            #gap = NULL,
            #height = 2000
          ),

          value = "station-level-summaries"
        )
      ) |>
        htmltools::tagAppendAttributes(
          #https://getbootstrap.com/docs/5.0/utilities/api/
          class = "border-0 rounded-0 shadow-none"
        ),

      htmltools::div(
        shiny::uiOutput(outputId = "refreshDataButton"),
        shiny::uiOutput(outputId = "refreshDataInfo"),

        style = "display: flex; align-items: top; gap: 0px;", # Flexbox styling
      ),

      shiny::htmlOutput(outputId = "pageBottomText") # Common, regardless of card tab
    )
  )


# Server --------------------

server <- function(input, output, session) {
  shinyjs::useShinyjs(html = TRUE)
  shinyjs::hideElement(id = "pageBottomText")
  shinyjs::hideElement(id = "refreshDataButton") # Needs to be 'present' on page for `dataETL <- shiny::reactive({})` statement to work on initial page load
  shinyjs::hideElement(id = "refreshDataInfo")
  
  
  # Observables -----
  
  shiny::observeEvent(dataETL(), {
    shinyjs::showElement(id = "pageBottomText")
    shinyjs::showElement(id = "refreshDataButton")
    shinyjs::showElement(id = "refreshDataInfo")
    
    shiny::updateSelectInput(
      inputId = "azmetStation",
      label = "AZMet Station",
      choices = sort(unique(dataETL()$meta_station_name)),
      selected = azmetStation() # Reactive value initialized in `_global.R`
    )
  })
  
  shiny::observeEvent(input$azmetStation, {
    azmetStation(input$azmetStation)
  }, 
    ignoreInit = TRUE
  )

  
  # Reactives -----
  
  dataETL <- shiny::reactive({
    fxn_dataETL()
  }) %>% 
    shiny::bindEvent(
      input$refreshDataButton,
      ignoreNULL = FALSE,
      ignoreInit = TRUE
    )
  
  nwsData <- shiny::reactive({
    fxn_nwsData(inData = dataETL())
  })
  
  slsCardGraphs <- shiny::eventReactive(
    c(input$azmetStation, dataETL(), nwsData()), {
      fxn_slsCardGraphs(
        azmetStation = input$azmetStation,
        inDataFull = dataETL()
      )
    }
  )
  
  slsCardLayout <- shiny::eventReactive(
    c(input$azmetStation, nwsData(), slsCardGraphs()), {
      fxn_slsCardLayout(
        azmetStation = input$azmetStation,
        inDataLatest = nwsData(),
        slsCardGraphs = slsCardGraphs()
      )
    }
  )
  
  
  # Outputs -----
  
  output$nwsTable <- reactable::renderReactable({
    fxn_nwsTable(inData = nwsData())
  })
  
  output$nwsTableFooter <- shiny::renderUI({
    shiny::req(dataETL())
    fxn_nwsTableFooter()
  })
  
  output$nwsTableTitle <- shiny::renderUI({
    shiny::req(dataETL())
    fxn_nwsTableTitle()
  })
  
  output$pageBottomText <- shiny::renderUI({
    #shiny::req(dataETL())
    fxn_pageBottomText()
  })
  
  output$refreshDataButton <- shiny::renderUI({
    # shiny::req(dataETL())
    shiny::actionButton(
      inputId = "refreshDataButton",
      label = "REFRESH DATA",
      icon = shiny::icon(name = "rotate-right", lib = "font-awesome"),
      class = "btn btn-block btn-blue"
    )
  })
  
  output$refreshDataHelpText <- shiny::renderUI({
    #shiny::req(dataETL())
    fxn_refreshDataHelpText(activeTab = input$navsetCardTab)
  })
  
  output$refreshDataInfo <- shiny::renderUI({
    bslib::tooltip(
      bsicons::bs_icon("info-circle"),
      shiny::htmlOutput(outputId = "refreshDataHelpText"),
      id = "refreshDataInfo",
      placement = "right"
    )
  })
  
  output$slsCardLayout <- shiny::renderUI({
    layout_column_wrap(
      !!!slsCardLayout(),
      #class = ,
      fill = TRUE,
      fillable = TRUE,
      fixed_width = FALSE,
      #gap = "200px",
      #height = "200px",
      heights_equal = c("all", "row"),
      height_mobile = NULL,
      max_height = NULL,
      min_height = NULL,
      width = "300px"
    )
  })
  
  output$slsCardLayoutFooter <- shiny::renderUI({
    shiny::req(dataETL())
    fxn_slsCardLayoutFooter()
  })
  
  output$slsCardLayoutTitle <- shiny::renderUI({
    shiny::req(dataETL())
    fxn_slsCardLayoutTitle(azmetStation = input$azmetStation)
  })
  
  output$slsLatestDataUpdate <- shiny::renderUI({
    shiny::req(dataETL())
    fxn_slsLatestDataUpdate(
      azmetStation = input$azmetStation,
      inData = nwsData()
    )
  })
}


# Run --------------------

shinyApp(ui, server)
