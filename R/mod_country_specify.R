#' country_specify UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#'

DHS_country_list <- rdhs::dhs_countries()

mod_country_specify_ui <- function(id){
  ns <- NS(id)


  tagList(
    tabPanel("Country meta",
             h4("Data Input"),              #div(style = "margin-top: 10px;",

             ### country name
             selectInput(ns("country"), with_red_star("Choose a country "),
                         c('',sort(DHS_country_list[['CountryName']]))),
             ### survey year
             selectInput(ns("Svy_year"),  with_red_star("Choose survey year "), choices = character(0)),

             tags$hr(style="border-top-color: #E0E0E0;") # (style="border-top: 2px solid #707070;")
             )


  )
}

#' country_specify Server Functions
#'
#' @noRd
mod_country_specify_server <- function(id,CountryInfo){
  moduleServer( id, function(input, output, session){
    shinyjs::useShinyjs()

    ns <- session$ns

    observeEvent(input$country, {

      if(is.null(input$country) || input$country == ""){return()}


      ### show a spinner for waiting
      session$sendCustomMessage(type = "controlSpinner", message = list(action = "show"))

      ### Update country info
      CountryInfo$country(input$country)
      CountryInfo$svyYear_list(get_survey_year(input$country))
      country_GADM <- get_country_GADM(input$country)
      CountryInfo$GADM_list(country_GADM)
      CountryInfo$GADM_selected(country_GADM[['National']])

      freezeReactiveValue(input, "Svy_year")
      updateSelectInput(inputId = "Svy_year", choices = sort(CountryInfo$svyYear_list()))


      Sys.sleep(1)
      session$sendCustomMessage(type = "controlSpinner", message = list(action = "hide"))


    })

    observeEvent(input$Svy_year, {

      if(is.null(input$Svy_year) || input$Svy_year == ""){return()}

      CountryInfo$svyYear_selected(input$Svy_year)

    })


  })
}

## To be copied in the UI
# mod_country_specify_ui("country_specify_1")

## To be copied in the server
# mod_country_specify_server("country_specify_1")
