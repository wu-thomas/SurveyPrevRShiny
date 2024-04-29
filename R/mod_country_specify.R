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


#surveyPrev_ind_list <-  full_ind_des # surveyPrev::surveyPrevIndicators
#surveyPrev_ind_list <- surveyPrev::surveyPrevIndicators

#indicator_choices_vector <- setNames(surveyPrev_ind_list$ID, surveyPrev_ind_list$Description)
#load(file='data/DHS_meta_preload_04172024.rda')
#DHS.country.meta <- DHS.country.meta.preload
#DHS.survey.meta <- DHS.survey.meta.preload
#DHS.dataset.meta <- DHS.dataset.meta.preload


mod_country_specify_ui <- function(id){
  ns <- NS(id)



  fluidPage(
    div(class = "module-title",
    h4("Country Meta Data Input")),

    fluidRow(

      column(4,
             #h4("Data Input"),              #div(style = "margin-top: 10px;",
             #tags$hr(style="border-top-color: #E0E0E0;"), # (style="border-top: 2px solid #707070;")

             ### country name
             selectInput(ns("country"), with_red_star("Choose a country "),
                         character(0)),
             ### survey year
             selectInput(ns("Svy_year"),  with_red_star("Choose survey year "), choices = character(0)),

             tags$hr(style="border-top-color: #E0E0E0;"), # (style="border-top: 2px solid #707070;")


             ### survey indicators
             selectInput(ns("Svy_ind_group"),  with_red_star("Choose an indicator group "), choices = character(0)),

             shinyWidgets::pickerInput(ns("Svy_indicator"),  with_red_star("Choose an indicator "),
                                       choices = character(0),
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

             tags$hr(style="border-top-color: #E0E0E0;"), # (style="border-top: 2px solid #707070;")

             ### toggle input for interactive map
             shinyWidgets::materialSwitch(inputId = ns("mapType"), label = HTML("<strong>Interactive Map Enabled</strong>"),
                                          status = "success",value =T)

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
             #div(
            #   id = "map-container",
              # style = "width: max(50%, 600px); margin: auto; margin-top: 20px; float: left;",
              # leaflet::leafletOutput(ns("country_map"))
             #))
            div(
              id = "map-container",
              style = "width: max(50%, 600px); margin: auto; margin-top: 20px; float: left;",
              uiOutput(ns("mapUI"))
            ))
    )
  )
}

#' country_specify Server Functions
#'
#' @noRd
mod_country_specify_server <- function(id,CountryInfo){
  moduleServer( id, function(input, output, session){


    ns <- session$ns


    ### initialize selection for country, indicator group and indicators
    # update country
    country_name_list <- sort(DHS.country.meta[['CountryName']])
    country_name_list <- country_name_list[country_name_list!='Nigeria (Ondo State)']
    updateSelectInput(inputId = "country", choices = c('',country_name_list))
    surveyPrev_ind_list <- full_ind_des

    # update indicator group

    updateSelectInput(inputId = "Svy_ind_group", choices = sort(unique(full_ind_des$Topic),decreasing = F))


    ### preload Zambia
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

      #message('loading MDG')
      if(CountryInfo$use_preloaded_Madagascar()){
        freezeReactiveValue(input, "country")
        updateSelectInput(inputId = "country", choices = c('Madagascar'))

        CountryInfo$GADM_strata_level(2)
        CountryInfo$country('Madagascar')
        CountryInfo$svyYear_list('2021')

        country_iso3 <- DHS.country.meta[DHS.country.meta$CountryName== CountryInfo$country(),'ISO3_CountryCode']
        country_GADM <- readRDS(file=paste0('data/GADM_shp/',country_iso3,'/',country_iso3,'_GADM_analysis.rds'))
        country_GADM_smoothed <- readRDS(file=paste0('data/GADM_shp/',country_iso3,'/',country_iso3,'_GADM_display.rds'))

        CountryInfo$GADM_list(country_GADM)
        CountryInfo$GADM_list_smoothed(country_GADM_smoothed)

        CountryInfo$GADM_display_selected(country_GADM[['National']])
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
      session$sendCustomMessage('controlSpinner', list(action = "show", message = "Loading country GADM, please wait..."))

      ### Update country info
      CountryInfo$country(input$country)
      CountryInfo$svyYear_list(get_survey_year(input$country))

      country_iso3 <- DHS.country.meta[DHS.country.meta$CountryName==input$country,'ISO3_CountryCode']
      # country_GADM <- readRDS(file=paste0('data/GADM_shp/',country_iso3,'/',country_iso3,'_GADM_analysis.rds'))
      # country_GADM_smoothed <- readRDS(file=paste0('data/GADM_shp/',country_iso3,'/',country_iso3,'_GADM_display.rds'))


      ### load prepared or download GADM shapefile

      if(file.exists(paste0('data/GADM_shp/',country_iso3,'/',country_iso3,'_GADM_analysis.rds'))){
        message('loading prepared GADM shapefile for analysis.')
        country_GADM <- readRDS(file=paste0('data/GADM_shp/',country_iso3,'/',country_iso3,'_GADM_analysis.rds'))
        country_GADM <- lapply(country_GADM, function(x) {
          sf::st_set_crs(x, 4326)
        })

      } else{
        message('downloading GADM shapefile for analysis.')
        country_GADM <- get_country_GADM(input$country)
        }

      if(file.exists(paste0('data/GADM_shp/',country_iso3,'/',country_iso3,'_GADM_display.rds'))){
        message('loading prepared GADM shapefile for display.')
        country_GADM_smoothed <- readRDS(file=paste0('data/GADM_shp/',country_iso3,'/',country_iso3,'_GADM_display.rds'))
        country_GADM_smoothed <- lapply(country_GADM_smoothed, function(x) {
          sf::st_set_crs(x, 4326)
        })

      } else{
        message('downloading GADM shapefile for display.')
        country_GADM_smoothed <- get_country_GADM(input$country,resolution=2)
      }

      #country_GADM <- get_country_GADM(input$country)
      #country_GADM_smoothed <- get_country_GADM(input$country,resolution=2)

      #mdg.ex.GADM.list <-country_GADM
      #save(mdg.ex.GADM.list, file='E:/Dropbox/YunhanJon/SurveyPrevRShiny/data/mdg_example_GADM.rda')

      CountryInfo$GADM_list(country_GADM)
      CountryInfo$GADM_list_smoothed(country_GADM_smoothed)

      CountryInfo$GADM_display_selected(country_GADM_smoothed[['National']])

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


    ### update survey indicator selection based on group

    observeEvent(input$Svy_ind_group, {

      group_ind_list <- full_ind_des %>%
        subset( Topic==input$Svy_ind_group)

      indicator_choices_vector <- setNames(group_ind_list$ID, group_ind_list$Description)

      shinyWidgets::updatePickerInput(inputId = "Svy_indicator", choices = sort(indicator_choices_vector,decreasing = F))

    })

    ### update survey indicator selection
    observeEvent(input$Svy_indicator, {

      if(is.null(input$Svy_indicator) || input$Svy_indicator == ""){return()}

      CountryInfo$svy_indicator_var(input$Svy_indicator)
      CountryInfo$svy_indicator_des(surveyPrev_ind_list[surveyPrev_ind_list$ID==input$Svy_indicator,]$Description)

    })


    ### prompt when recode are not all available for the selected combination
    observe({
      req(input$country)
      req(input$Svy_year)
      req(input$Svy_indicator)


      recode.avail <- check_dat_avail(country = input$country , svy_year = input$Svy_year , indicator =input$Svy_indicator)

      if(length(recode.avail$missing_recode)>0){
        showNoRecodeModal(recode=recode.avail$missing_recode,
                          Svy_indicator=CountryInfo$svy_indicator_des())
      }


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

        CountryInfo$GADM_display_selected(CountryInfo$GADM_list_smoothed()[[input$AdminLevel]])
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
        "You've selected ",
        "<strong style='background-color: #D0E4F7;'>", country, "</strong>",
        " with survey in ",
        "<strong style='background-color: #D0E4F7;'>", svy_year, "</strong>",
        ", to estimate ",
        "<br> <strong style='background-color: #D0E4F7;'>", CountryInfo$svy_indicator_des(), "</strong>.",
        "<br> You intend to conduct analysis at ",
        "<strong style='background-color: #D0E4F7;'>", concatenate_vector_with_and(CountryInfo$GADM_analysis_levels()), "</strong>",
        " level(s).",
        "<br><br>",
        "Please review the table and map below for your Admin level selections. ",
        "Choose a different level to display if necessary.",
        "</p>",
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
        "The map below displays ",
        "<span style='background-color: #D0E4F7;'><strong>", admin_level,
        "</strong></span> boundaries of ",
        "<span style='background-color: #D0E4F7;'><strong>", country,
        "</strong></span>. ",
        "</p>"
      ))

    })


    ###############################################################
    ### country boundaries map
    ###############################################################

    observeEvent(input$mapType,{


       CountryInfo$display_interactive(input$mapType)

    })

    observeEvent(CountryInfo$display_interactive(),{

      interactive_map <- CountryInfo$display_interactive()
      shinyWidgets::updateMaterialSwitch(session=session, inputId="mapType", value = interactive_map)

    })



    ### determine interactive/static map for selected country
    output$mapUI <- renderUI({
      if (input$mapType) {  # if TRUE, show interactive map
        leaflet::leafletOutput(ns("interactive_country_map"))
      } else {  # if FALSE, show static map
        plotOutput(ns("static_country_map"))
      }
    })

    ### interactive map
    output$interactive_country_map <- leaflet::renderLeaflet({

      req(CountryInfo$GADM_display_selected_level())
      req(CountryInfo$GADM_list())


      #gadm_list <- CountryInfo$GADM_list()

      selected_level <- CountryInfo$GADM_display_selected_level()
      gadmData <-  CountryInfo$GADM_list()[[selected_level]]

      #if(is.null(selected_level)){selected_level='National'}

      #message('map is rendering')
      if (is.null(gadmData)) {

        # If no country is selected, return an empty Leaflet map
        leaf_plot <- leaflet::leaflet() %>%
          leaflet::addTiles()

      } else {

        leaf_plot<- country.boundary.leaflet(gadm.level= selected_level,
                                              gadmData=gadmData)
        #leaf_plot <- leaflet::leaflet(gadmData) %>%
          #leaflet::addTiles() %>%
          #leaflet::addPolygons(weight = 1)
      }

      return(leaf_plot)

    })


    ### static map

    output$static_country_map <- renderPlot({

      req(CountryInfo$GADM_display_selected_level())
      req(CountryInfo$GADM_list())


      #gadm_list <- CountryInfo$GADM_list()

      selected_level <- CountryInfo$GADM_display_selected_level()
      gadmData <-  CountryInfo$GADM_list()[[selected_level]]

      if (is.null(gadmData)) {

        # If no country is selected, return nothing

        return(NULL)

      } else {

        map_plot <- ggplot2::ggplot() +
          #ggspatial::annotation_map_tile(type = "osm") +
          ggplot2::geom_sf(data = gadmData, color = "#00008B", size = 2) +
          ggplot2::theme_bw()
      }

      return(map_plot)
    })

  })
}

## To be copied in the UI
# mod_country_specify_ui("country_specify_1")

## To be copied in the server
# mod_country_specify_server("country_specify_1")
