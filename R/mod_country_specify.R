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
surveyPrev_ind_list <- surveyPrev::surveyPrevIndicators

indicator_choices_vector <- setNames(surveyPrev_ind_list$ID, surveyPrev_ind_list$Description)




mod_country_specify_ui <- function(id){
  ns <- NS(id)


  tagList(
    h3("Country Meta Data Input"),

    fluidRow(

      column(4,
             #h4("Data Input"),              #div(style = "margin-top: 10px;",
             tags$hr(style="border-top-color: #E0E0E0;"), # (style="border-top: 2px solid #707070;")

             ### country name
             selectInput(ns("country"), with_red_star("Choose a country "),
                         c('',sort(DHS_country_list[['CountryName']]))),
             ### survey year
             selectInput(ns("Svy_year"),  with_red_star("Choose survey year "), choices = character(0)),

             tags$hr(style="border-top-color: #E0E0E0;"), # (style="border-top: 2px solid #707070;")


             ### survey indicators
             shinyWidgets::pickerInput(ns("Svy_indicator"),  with_red_star("Choose an indicator "),
                                       choices =indicator_choices_vector,
                                       multiple = F,
                                       selected = NULL),

             tags$hr(style="border-top-color: #E0E0E0;"), # (style="border-top: 2px solid #707070;")


             ### admin region selection

             checkboxGroupInput(
               ns("admin_levels_analysis"),
               with_red_star("Choose Admin levels for analysis "),
               #choices = c("Admin-0" = "National", "Admin-1" = "First Level", "Admin-2" = "Second Level"),
               choices = c('National','Admin-1','Admin-2'),
               inline = F
             ),
             textOutput(ns("selected_admin_levels")),

             selectInput(ns("AdminLevel"), "Check out maps for Admin levels",
                         choices=character(0)),

             tags$hr(style="border-top-color: #E0E0E0;") # (style="border-top: 2px solid #707070;")
             ),
      column(8,
             div(style = "width: max(50%, 600px); margin: auto;float: left;",
                 uiOutput(ns("country_meta_display"))
             ),

             div(style = "width: max(50%, 600px); margin-top: -15px;margin-bottom: -30px; float: left; font-size: 1.625rem;",
             tableOutput(ns("gadmTable"))
             ),

             div(style = "width: max(50%, 600px); margin: auto;float: left;",
                 uiOutput(ns("text_admin_display"))
             ),
             #hr(style="border-top-color: #E0E0E0;"), # (style="border-top: 2px solid #707070;"),
             div(
               id = "map-container",
               style = "width: max(50%, 600px); margin: auto; margin-top: 20px; float: left;",
               leaflet::leafletOutput(ns("country_map"))
             ))
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

    observeEvent(CountryInfo$use_preloaded_Zambia(),{

      if(CountryInfo$use_preloaded_Zambia()){
      freezeReactiveValue(input, "country")
      updateSelectInput(inputId = "country", choices = c('Zambia'))

      CountryInfo$country('Zambia')
      CountryInfo$svyYear_list('2018')
      country_GADM <- zmb.ex.GADM.list
      CountryInfo$GADM_list(country_GADM)
      CountryInfo$GADM_display_selected(country_GADM[['National']])
      freezeReactiveValue(input, "Svy_year")
      updateSelectInput(inputId = "Svy_year", choices = sort(CountryInfo$svyYear_list(),decreasing = T))
      }else{return()}

    })


    observeEvent(CountryInfo$use_preloaded_Madagascar(),{

      if(CountryInfo$use_preloaded_Madagascar()){
        freezeReactiveValue(input, "country")
        updateSelectInput(inputId = "country", choices = c('Madagascar'))

        CountryInfo$country('Madagascar')
        CountryInfo$svyYear_list('2021')

        country_GADM <- mdg.ex.GADM.list
        CountryInfo$GADM_list(country_GADM)
        CountryInfo$GADM_display_selected(country_GADM[['National']])
        CountryInfo$GADM_strata_level(2)


        freezeReactiveValue(input, "Svy_year")
        updateSelectInput(inputId = "Svy_year", choices = sort(CountryInfo$svyYear_list(),decreasing = T))
      }else{return()}

    })


    ### update country specific information once a country has been selected
    observeEvent(input$country, {

      if(is.null(input$country) || input$country == ""){return()}
      if(CountryInfo$use_preloaded_Zambia()){return()}
      if(CountryInfo$use_preloaded_Madagascar()){return()}

      ### show a spinner for waiting
      session$sendCustomMessage('controlSpinner', list(action = "show", message = "Loading country meta data, please wait..."))

      ### Update country info
      CountryInfo$country(input$country)
      CountryInfo$svyYear_list(get_survey_year(input$country))


      country_GADM <- get_country_GADM(input$country)

      #mdg.ex.GADM.list <-country_GADM
      #save(mdg.ex.GADM.list, file='E:/Dropbox/YunhanJon/SurveyPrevRShiny/data/mdg_example_GADM.rda')

      CountryInfo$GADM_list(country_GADM)
      CountryInfo$GADM_display_selected(country_GADM[['National']])

      freezeReactiveValue(input, "Svy_year")
      updateSelectInput(inputId = "Svy_year", choices = sort(CountryInfo$svyYear_list(),decreasing = T))


      Sys.sleep(1)

      session$sendCustomMessage('controlSpinner', list(action = "hide"))


    })



    ### update survey year selection
    observeEvent(input$Svy_year, {

      if(is.null(input$Svy_year) || input$Svy_year == ""){return()}

      CountryInfo$svyYear_selected(input$Svy_year)

    })

    ### update survey indicator selection
    observeEvent(input$Svy_indicator, {

      if(is.null(input$Svy_indicator) || input$Svy_indicator == ""){return()}

      CountryInfo$svy_indicator_var(input$Svy_indicator)
      CountryInfo$svy_indicator_des(surveyPrev_ind_list[surveyPrev_ind_list$ID==input$Svy_indicator,]$Description)

    })

    ### update admin level visualization

    observe({
      req(CountryInfo$GADM_list())
      updateSelectInput(session, "AdminLevel", choices = names(CountryInfo$GADM_list()))
      updateCheckboxGroupInput(session, "admin_levels_analysis", choices = names(CountryInfo$GADM_list()),
                        selected = 'National')
      #updateSelectInput(session, "admin_levels_analysis", selected = 'National')

    })

    ### make sure national is always selected
    observe({
      selected <- input$admin_levels_analysis
      if (is.null(selected) || !("National" %in% selected)) {
        selected <- c("National", selected)
        updateCheckboxGroupInput(session, "admin_levels_analysis", selected = selected)
      }
    })



    ### display: update GADM files based on selection of admin level

    observeEvent(input$AdminLevel, {

      if(is.null(input$AdminLevel) || input$AdminLevel == ""){return()
      }else{

        CountryInfo$GADM_display_selected(CountryInfo$GADM_list()[[input$AdminLevel]])
        CountryInfo$GADM_display_selected_level(input$AdminLevel)
      }
    })

    ### analysis: update GADM files based on selection of admin level

    observeEvent(input$admin_levels_analysis, {

      if(is.null(input$admin_levels_analysis) || length(input$admin_levels_analysis) == 0){return()
      }else{

        CountryInfo$GADM_analysis_levels(input$admin_levels_analysis)
      }
    })


    ### text regarding meta information on country, survey etc.

    output$country_meta_display <- renderUI({
      req(CountryInfo$country())
      country <- CountryInfo$country()
      svy_year <- CountryInfo$svyYear_selected()
      admin_level <- CountryInfo$GADM_display_selected_level()
      #indicator_description <- surveyPrev_ind_list[surveyPrev_ind_list$ID==input$Svy_indicator,]$Description


      HTML(paste0(
        "<p style='font-size: large;'>",
        "<span style='background-color: lightblue;'>",
        "You've selected <strong>", country, "</strong> with survey in <strong>", svy_year, "</strong>, to estimate ",
        "<br> <strong>",CountryInfo$svy_indicator_des(), "</strong>.",
        "<br> You intend to conduct analysis at <strong>",concatenate_vector_with_and(CountryInfo$GADM_analysis_levels()), "</strong>  level(s).",
        "</span><br>",
        "<span> Please review the table and map below for your Admin level selections. ",
        "Choose a different level to display if necessary.",
        "</span>",
        "</p>",
        #"<br>",
        "<hr style='border-top-color: #E0E0E0;'>"
      ))

    })

    ### present number of regions at each admin level

    output$gadmTable <- renderTable({

      req(CountryInfo$country())
      gadm_list <- CountryInfo$GADM_list()
      if (is.null(gadm_list) || length(gadm_list) == 0) {

        GADM_num_df <-check_gadm_levels(NULL)
        return(GADM_num_df) # Do not render the table if no data is supplied
      }

      GADM_num_df <- check_gadm_levels(gadm_list)
    }, align = "l",rownames = TRUE)


    ### text display above the map

    output$text_admin_display <- renderUI({
      req(CountryInfo$country())
      req(CountryInfo$GADM_display_selected_level())

      country <- CountryInfo$country()
      admin_level <- CountryInfo$GADM_display_selected_level()

      HTML(paste0(
        "<hr style='border-top-color: #E0E0E0;'>",
        "<p style='font-size: large;'>",
        "<span style='background-color: lightblue;'>",
        "The map below displays <strong>", admin_level, "</strong> boundaries in <strong>", country, "</strong>. ",
        "</span>",
        "</p>"
      ))

    })


    ### display map for selected country

    output$country_map <- leaflet::renderLeaflet({
      gadmData <- CountryInfo$GADM_display_selected()

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
# mod_country_specify_ui("country_specify_1")

## To be copied in the server
# mod_country_specify_server("country_specify_1")
