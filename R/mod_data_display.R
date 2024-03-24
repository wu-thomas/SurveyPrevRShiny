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
  tabPanel("Meta Data",
           # UI elements specific to Raw Data Examination
           div(style = "margin-top: -20px;",
           h3("Country Meta Data Examination")),
           #textOutput(ns("selected_country")),

           div(style = "margin-top: 25px;",
               uiOutput(ns("selected_country"))
               ),

           hr(style="border-top-color: #E0E0E0;"), # (style="border-top: 2px solid #707070;"),

           tableOutput(ns("gadmTable")),
           hr(style="border-top-color: #E0E0E0;"), # (style="border-top: 2px solid #707070;"),

           div(
             id = "map-container",
             style = "width: max(50%, 600px); margin: auto;float: left;",
             leaflet::leafletOutput(ns("country_map"))
           )

           #leaflet::leafletOutput(ns("country_map"),width = "auto", height = "auto")
           # Add other UI components here
  )

}

#' data_display Server Functions
#'
#' @noRd
mod_data_display_server <- function(id,CountryInfo){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    #output$selected_country <- renderText({
    # # paste("Selected Country:", CountryInfo$country())
    #})

    output$selected_country <- renderUI({
      req(CountryInfo$country())
      country <- CountryInfo$country()
      svy_year <- CountryInfo$svyYear_selected()
      admin_level <- CountryInfo$GADM_selected_level()

      HTML(paste0("<p style='font-size: large;'><span style='background-color: lightblue;'>",
                  "You've selected <strong>", country, "</strong> with survey in <strong>", svy_year, "</strong>, ",
                  "focusing on the <strong>", admin_level, "</strong> administrative level.</span><br>",
                  "<span>Please review the table and map below for details on your selection for administrative level.",
                  " Choose a different level if necessary.</span></p>"))
    })


    output$gadmTable <- renderTable({

      gadm_list <- CountryInfo$GADM_list()
      if (is.null(gadm_list) || length(gadm_list) == 0) {

        GADM_num_df <-check_gadm_levels(NULL)
        return(GADM_num_df) # Do not render the table if no data is supplied
      }

      GADM_num_df <- check_gadm_levels(gadm_list)
    }, align = "l",rownames = TRUE)


    output$country_map <- leaflet::renderLeaflet({
      gadmData <- CountryInfo$GADM_selected()

      if (is.null(gadmData)) {

        # If no country is selected, return an empty Leaflet map
        leaflet::leaflet() %>%
          leaflet::addTiles()
      } else {

      leaflet::leaflet(gadmData) %>%
        leaflet::addTiles() %>%
        leaflet::addPolygons(weight = 1)
      }

    })


  })
}

## To be copied in the UI
# mod_data_display_ui("data_display_1")

## To be copied in the server
# mod_data_display_server("data_display_1")
