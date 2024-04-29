#' result_visual UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_res_visual_prev_map_ui <- function(id) {
  ns <- NS(id)

  fluidPage(
    tags$head(
      # Custom CSS for styling
      tags$style(HTML("
      .button-container {
        display: flex;           /* Use flexbox to center the button */
        justify-content: center; /* Center button horizontally */
        width: max(50%, 600px);  /* Max width same as map */
        margin: 20px auto;       /* Centering the container itself horizontally */
      }
    "))
    ),

    div(class = "module-title",
        h4("Subnational Results Mapping")
    ),

    fluidRow(
      column(4,
             selectInput(ns("selected_method"), "Select Method",
                         choices = c("Direct Estimates"="Direct",
                                     "Area-level Model"= "FH", "Unit-level Model"="Unit"))
      ),
      column(4,
             selectInput(ns("selected_adm"), "Select Admin Level", choices = character(0))
      ),
      column(4,
             selectInput(ns("selected_measure"), "Select Statistics",
                         choices = c("Mean"="mean",
                                     "Coefficient of Variation"= "cv",
                                     "Width of 95% Credible Interval"="CI.width"))
      )
    ),

    fluidRow(
      column(12,
             div(style = " margin: auto;float: left;",
                 uiOutput(ns("text_display"))
             )
      )
    ),
    fluidRow(
      column(12,
             #tags$h4("Map for estimates from selected model"),
             hr(style="border-top-color: #E0E0E0;"), # More subtle horizontal line
             shinyWidgets::materialSwitch(inputId = ns("PrevmapType"), label = "Interactive Map Enabled",
                                          status = "success",value =T),

             div(
               id = "map-container",
               style = "width: max(50%, 600px); margin-top: 20px;",
               uiOutput(ns("prev_map"))
               #leaflet::leafletOutput(ns("prev_map"))
             ),
             div( style = "width: max(50%, 600px); margin-top: 20px; display: flex; justify-content: center;",
                  uiOutput(ns("download_button_ui"))

                  #downloadButton(ns("dl"), "Download as HTML", icon = icon("download"),
                  #            class = "btn-primary")
             )
      )
    ),


  )

}


#' result_visual Server Functions
#'
#' @noRd
mod_res_visual_prev_map_server <- function(id,CountryInfo,AnalysisInfo){
  moduleServer(id, function(input, output, session) {
    ns <- session$ns


    row_names <- c("Direct", "FH", "Unit")
    nrows <- length(row_names)

    col_names <- reactive({ CountryInfo$GADM_analysis_levels()    })
    ncols <- reactive({ length(col_names()) })

    observeEvent(col_names(), {
      adm.choice <- col_names()
      adm.choice <- adm.choice[adm.choice!='National']
      updateSelectInput(inputId = "selected_adm",
                        choices = adm.choice)
    })



    ###############################################################
    ### determine interactive vs static map based on user selection
    ###############################################################

    observeEvent(input$PrevmapType,{

      CountryInfo$display_interactive(input$PrevmapType)

    })

    observeEvent(CountryInfo$display_interactive(),{

      interactive_map <- CountryInfo$display_interactive()
      shinyWidgets::updateMaterialSwitch(session=session, inputId="PrevmapType", value = interactive_map)

    })

    ### determine which UI to present plot

    output$prev_map <- renderUI({
      if (input$PrevmapType) {  # if TRUE, show interactive map
        leaflet::leafletOutput(ns("prev_map_interactive"))
      } else {  # if FALSE, show static map
        plotOutput(ns("prev_map_static"))
      }
    })

    output$download_button_ui <- renderUI({
      if (input$PrevmapType) {  # HTML download
        downloadButton(ns("download_interactive"), "Download as HTML", icon = icon("download"),
                                  class = "btn-primary")
        } else {
          downloadButton(ns("download_static"), "Download as PDF", icon = icon("download"),
                         class = "btn-primary")
        }
    })

    ###############################################################
    ### prepare maps
    ###############################################################

    output$text_display <- renderUI({

      ### return empty map if no subnational level selected
      if (length(input$selected_adm) == 0 || input$selected_adm == "") {
        return(NULL)
      }

      ### extract selections
      selected_adm <- input$selected_adm
      selected_method <- input$selected_method
      selected_measure <- input$selected_measure

      ### initialize parameters
      model_res_all <- AnalysisInfo$model_res_list()
      model_res_selected <- model_res_all[[selected_method]][[selected_adm]]

      method_match <- c(
        "Direct" = "Direct estimates",
        "Unit" = "Unit-level",
        "FH" = "Area-level"
      )

      method_des <- method_match[selected_method]

      if(is.null(model_res_selected)){

        HTML(paste0(
          "<p style='font-size: large;'>",
          "Results for ",
          "<span style='background-color: #D0E4F7;'><b>", method_des, "</b></span> ",
          "model at ",
          "<span style='background-color: #D0E4F7;'><b>", selected_adm, "</b></span>",
          " level are not available. Please make the model has been successfully fitted.",
          "</p>"
        ))

      }else{

        HTML(paste0(
          "<p style='font-size: large;'>",
          "Presenting map for ",
          "<span style='background-color: #D0E4F7;'><b>", method_des, "</b></span> ",
          "model at ",
          "<span style='background-color: #D0E4F7;'><b>", selected_adm, "</b></span> level.",
          "</p>"
        ))

      }


    })

    ### interactive map

    prev.map.interactive.output <- reactiveVal(NULL)

    output$prev_map_interactive <- leaflet::renderLeaflet({

      ### initialize base map
      prev.interactive.plot <- leaflet::leaflet() %>%
        leaflet::addTiles()

      ### return empty map if no subnational level selected
      if (length(input$selected_adm) == 0 || input$selected_adm == "") {
        return(prev.interactive.plot)
      }

      ### extract selections
      selected_adm <- input$selected_adm
      selected_method <- input$selected_method
      selected_measure <- input$selected_measure


      ### load Madagascar example
      if(CountryInfo$use_preloaded_Madagascar()){
        AnalysisInfo$model_res_list(mdg.ex.model.res)}


      ### initialize parameters
      model_res_all <- AnalysisInfo$model_res_list()
      strat.gadm.level <- CountryInfo$GADM_strata_level()
      model_res_selected <- model_res_all[[selected_method]][[selected_adm]]


      # determine hatching density by country size
      tryCatch(
        {
          country.area <- as.numeric(sf::st_area(CountryInfo$GADM_list_smoothed()[["National"]])/1e6)
          hatching.density.country <- round(sqrt(9e07/country.area))

        },error = function(e) {

          hatching.density.country <- 12

        })

      if(is.null(model_res_selected)|selected_adm=='National'){

        return(prev.interactive.plot)

      }else{

        prev.interactive.plot <- suppressWarnings(prevMap.leaflet(res.obj = model_res_selected,
                                    gadm.shp = CountryInfo$GADM_list_smoothed()[[selected_adm]],
                                    model.gadm.level = admin_to_num(selected_adm),
                                    strata.gadm.level = CountryInfo$GADM_strata_level(),
                                    value.to.plot =selected_measure,
                                    legend.label = 'Estimates',
                                    hatching.density = hatching.density.country,
                                    map.title=NULL))

      }
      prev.map.interactive.output(prev.interactive.plot)
      #message(paste0(input$prev_map$lng,'_',input$map_center$lat,'_', input$map_zoom))
      return(prev.interactive.plot)

    })

    #output$check_ID <-renderPrint({reactiveValuesToList(input)})

    #output$prev_map_static <- ()



    ### static map

    prev.map.static.output <- reactiveVal(NULL)

    output$prev_map_static <- renderPlot({

      if (length(input$selected_adm) == 0 || input$selected_adm == "") {
        return(NULL)
      }

      ### initialize parameters
      selected_adm <- input$selected_adm
      selected_method <- input$selected_method
      selected_measure <- input$selected_measure


      ### load Madagascar example
      if(CountryInfo$use_preloaded_Madagascar()){
        AnalysisInfo$model_res_list(mdg.ex.model.res)}


      ### load results
      model_res_all <- AnalysisInfo$model_res_list()

      strat.gadm.level <- CountryInfo$GADM_strata_level()

      model_res_selected <- model_res_all[[selected_method]][[selected_adm]]

      ### plot
      if(is.null(model_res_selected)|selected_adm=='National'){

        return(NULL)

      }else{

        prev.static.plot <- suppressWarnings(prevMap.static(res.obj = model_res_selected,
                                                      gadm.shp = CountryInfo$GADM_list_smoothed()[[selected_adm]],
                                                      model.gadm.level = admin_to_num(selected_adm),
                                                      strata.gadm.level = CountryInfo$GADM_strata_level(),
                                                      value.to.plot =selected_measure,
                                                      legend.label = 'Estimates',
                                                      direction=-1,
                                                      map.title=NULL))

      }
      prev.map.static.output(prev.static.plot)
      #message(paste0(input$prev_map$lng,'_',input$map_center$lat,'_', input$map_zoom))
      return(prev.static.plot)

    })



    ###############################################################
    ### download maps
    ###############################################################

    output$download_interactive <- downloadHandler(

      filename = function() {

        ### informative file name
        file.prefix <- paste0(CountryInfo$country(),'_',
                              input$selected_adm,'_',
                              input$selected_method,'_',
                              input$selected_measure)
        file.prefix <- gsub("[-.]", "_", file.prefix)
        return(paste0(file.prefix,'_prevMap.html'))

      },
      content = function(file) {
        # Save the Leaflet widget as an HTML file directly
        # htmlwidgets::saveWidget(prev_map_output(), file, selfcontained = TRUE)
        htmlwidgets::saveWidget(prev.map.interactive.output(),file, selfcontained = TRUE)

        #htmlwidgets::saveWidget(tmp.plot, map_html_temp, selfcontained = TRUE)
        #webshot2::webshot(url='mymap.html', file = file)
      }
    )

    output$download_static <- downloadHandler(
      filename = function() {

        ### informative file name
        file.prefix <- paste0(CountryInfo$country(),'_',
                              input$selected_adm,'_',
                              input$selected_method,'_',
                              input$selected_measure)
        file.prefix <- gsub("[-.]", "_", file.prefix)

        return(paste0(file.prefix,'_prevMap.pdf'))

        },

      content = function(file) {
        # Create the PDF
        pdf(file, width = 10, height = 10)  # Set width and height of the PDF
        print(prev.map.static.output())  # Print the plot to the PDF
        dev.off()  # Close the PDF
      }
    )




  })
}


## To be copied in the UI
# mod_result_visual_ui("result_visual_1")

## To be copied in the server
# mod_result_visual_server("result_visual_1")
