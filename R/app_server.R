#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd


options(shiny.maxRequestSize=150*1024^2) ## make the maximum size 150Mb for data input

app_server <- function(input, output, session) {

  shinyjs::useShinyjs()

  CountryInfo <- CountryInfo$new()


  mod_country_specify_server("country_specify_1", CountryInfo = CountryInfo)
  mod_survey_dat_input_server("survey_dat_input",CountryInfo = CountryInfo)
  mod_GADM_input_server("GADM_input", CountryInfo = CountryInfo)

  mod_data_display_server("Dat_Display_1",CountryInfo = CountryInfo)
  mod_indicator_display_server("indicator_display_1",CountryInfo = CountryInfo)
}
