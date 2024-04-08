#' result_visual UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_result_visual_ui <- function(id) {
  ns <- NS(id)

  fluidPage(

  )

}


#' result_visual Server Functions
#'
#' @noRd
mod_result_visual_server <- function(id,CountryInfo,AnalysisInfo){
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

  })
}


## To be copied in the UI
# mod_result_visual_ui("result_visual_1")

## To be copied in the server
# mod_result_visual_server("result_visual_1")
