#' data_input UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_data_input_ui <- function(id){
  ns <- NS(id)
  tagList(
    tabPanel("Raw Data",
             h4("Data Input"),
             #div(style = "margin-top: 10px;",
             selectInput(ns("country"), "Choose a country", rdhs::dhs_countries()[['CountryName']]),
             selectInput(ns("Svy_year"), "Choose survey year", choices = c("2010","2011")),
             fileInput(ns("dataFile"), "Upload your dataset (CSV)"),
             actionButton(ns("uploadData"), "Submit Data")

    )
  )
}

#' data_input Server Functions
#'
#' @noRd
mod_data_input_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    svy_years_avail <- reactive({
      if(is.null(input$country) || input$country == ""){
        return(character(0)) # Return an empty character vector if no country is selected
      }
      get_survey_year(input$country)
    })

    # Correctly observe the change in country selection
    observeEvent(input$country, {
      freezeReactiveValue(input, "Svy_year")
      updateSelectInput(inputId = "Svy_year", choices = svy_years_avail())
    })




  })
}

## To be copied in the UI
# mod_data_input_ui("data_input_1")

## To be copied in the server
# mod_data_input_server("data_input_1")
