#' GADM_input UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_GADM_input_ui <- function(id){
  ns <- NS(id)

  tagList(
    tags$head(
      ### load widgets
      shinyFeedback::useShinyFeedback(),
      shinyjs::useShinyjs()
    ),

    tabPanel("GADM select",
             h4("Spatial Boundary Input"),
             ### select admin level
             selectInput(ns("AdminLevel"), with_red_star("Select Admin Level "),
                         choices=character(0)),
             textOutput(ns("selected_country"))

    )

  )
}

#' GADM_input Server Functions
#'
#' @noRd
mod_GADM_input_server <- function(id,CountryInfo){
  moduleServer( id, function(input, output, session){

    ns <- session$ns

    observe({
      req(CountryInfo$GADM_list())
      updateSelectInput(session, "AdminLevel", choices = names(CountryInfo$GADM_list()))
    })



    observeEvent(input$AdminLevel, {

      if(is.null(input$AdminLevel) || input$AdminLevel == ""){return()
        }else{

      CountryInfo$GADM_selected(CountryInfo$GADM_list()[[input$AdminLevel]])
      CountryInfo$GADM_selected_level(input$AdminLevel)
}
    })

    output$selected_country <- renderText({
      paste("Selected Country:", CountryInfo$country())
    })


  })
}

## To be copied in the UI
# mod_GADM_input_ui("GADM_input_1")

## To be copied in the server
# mod_GADM_input_server("GADM_input_1")
