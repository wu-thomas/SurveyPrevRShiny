#' data_display UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_data_display_ui <- function(id){
  ns <- NS(id)
  tabPanel("Raw Data",
           # UI elements specific to Raw Data Examination
           h3("Raw Data Examination"),
           # Add other UI components here
  )
}

#' data_display Server Functions
#'
#' @noRd
mod_data_display_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_data_display_ui("data_display_1")

## To be copied in the server
# mod_data_display_server("data_display_1")
