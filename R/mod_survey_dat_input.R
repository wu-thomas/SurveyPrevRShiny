#' survey_dat_input UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_survey_dat_input_ui <- function(id){
  ns <- NS(id)

  tabPanel("DHS input",
           h4("Survey Data Input"),

           ### upload survey data
           fileInput(ns("Svy_dataFile"),
                     with_red_star("Upload DHS survey data (Stata format, .zip or .DTA) ")),

           div(style = "margin-top: -20px"),
           actionButton(ns("upload_Svy_Data"), "Submit Survey Data"),

           uiOutput(ns("Svy_Data_alert")),


           ### upload survey GPS
           div(style = "margin-top: 15px"),
           fileInput(ns("Svy_GPSFile"), with_red_star("Upload DHS GPS data (shapefile, .zip or .shp) ")),

           div(style = "margin-top: -20px"),
           actionButton(ns("upload_Svy_GPS"), "Submit GPS Data"),

           uiOutput(ns("Svy_GPS_alert")),
           tags$hr(style="border-top-color: #E0E0E0;") # (style="border-top: 2px solid #707070;")


  )
}

#' survey_dat_input Server Functions
#'
#' @noRd
mod_survey_dat_input_server <- function(id,CountryInfo){
  moduleServer( id, function(input, output, session){
    ns <- session$ns



    ###############################################################
    ### load DHS data into the system, accept .zip or .DTA
    ###############################################################

    ### Check if the file has the right extension

    observeEvent(input$Svy_dataFile, {
      req(input$Svy_dataFile)

      ext <- tools::file_ext(input$Svy_dataFile$name)
      svy_dat_correct_ext <- ext %in% c('zip', 'dta', 'DTA')

      shinyFeedback::feedbackWarning("Svy_dataFile", !svy_dat_correct_ext, "Supported file types: .zip, .DTA, .dta")
    })

    ### read in stata file

    svy_raw_data <- eventReactive(input$upload_Svy_Data, {

      # Check if a file has been uploaded
      if (is.null(input$Svy_dataFile)) {
        showNoFileSelectedModal()
        return()
      }

      req(input$Svy_dataFile)

      # double check file extension
      ext <- tools::file_ext(input$Svy_dataFile$name)
      svy_dat_correct_ext <- ext %in% c('zip', 'dta', 'DTA')

      # give warning if not
      if(!svy_dat_correct_ext){
        shinyFeedback::feedbackWarning("Svy_dataFile", !svy_dat_correct_ext, "Supported file types: .zip, .DTA, .dta")
      }

      # load stata file with progress bar
      withProgress(message = "Processing raw survey data... This might take a moment... \n",
                   detail = "Unzipping file...", value = 0, {

                     Sys.sleep(0.5)
                     incProgress(1/3)
                     path_found <- find_svy_dat_path(uploaded_file=input$Svy_dataFile)

                     if(is.null(path_found)){
                       shinyFeedback::feedbackWarning("Svy_dataFile", is.null(path_found),
                                                      "Wrong .zip file, not containing Stata format data")
                       return()
                     }


                     Sys.sleep(1) # Simulate delay
                     incProgress(1/3,detail = "Reading Stata file...")
                     #data <- suppressWarnings(haven::read_dta(path_found))
                     #data <- as.data.frame(data)
                     data <- zmb.ex.dat
                     CountryInfo$svy_dat(data)

                     Sys.sleep(1) # Simulate delay
                     incProgress(1/3, detail = "Finalizing...")
                     return(data)

                   })
    })


    ### indicate success
    output$Svy_Data_alert <- renderUI({

      req(svy_raw_data())

      if(dim(svy_raw_data())[1]>0) {
        success_wall(successMessage="Survey raw data upload successful")
      }else{error_wall()}

    })



    ### Check if the file has the right extension

    observeEvent(input$Svy_GPSFile, {
      req(input$Svy_GPSFile)

      ext <- tools::file_ext(input$Svy_GPSFile$name)
      svy_GPS_correct_ext <- ext %in% c('zip', 'shp')

      shinyFeedback::feedbackWarning("Svy_GPSFile", !svy_GPS_correct_ext, "Supported file types: .zip, .shp")
    })


    ### read in shape file

    svy_GPS_data <- eventReactive(input$upload_Svy_GPS, {

      # Check if a file has been uploaded
      if (is.null(input$Svy_GPSFile)) {
        showNoFileSelectedModal()
        return()
      }

      req(input$Svy_GPSFile)

      # double check file extension
      ext <- tools::file_ext(input$Svy_GPSFile$name)
      svy_GPS_correct_ext <- ext %in% c('zip', 'shp')

      # give warning if not
      if(!svy_GPS_correct_ext){
        shinyFeedback::feedbackWarning("Svy_GPSFile", !svy_GPS_correct_ext, "Supported file types: .zip, .shp")
      }

      # load stata file with progress bar
      withProgress(message = "Processing survey GPS data... This might take a moment... \n",
                   detail = "Unzipping file...", value = 0, {

                     Sys.sleep(2)
                     incProgress(1/3)
                     path_found <- find_svy_GPS_path(uploaded_file=input$Svy_GPSFile)

                     if(is.null(path_found)){
                       shinyFeedback::feedbackWarning("Svy_GPSFile", is.null(path_found),
                                                      "Wrong .zip file, not containing shp data")
                       return()
                     }


                     Sys.sleep(2) # Simulate delay
                     incProgress(1/3,detail = "Reading shapefile...")

                     data <- suppressWarnings(rgdal::readOGR(path_found, verbose = FALSE))

                     #data <- zmb.ex.dat
                     CountryInfo$svy_GPS_dat(data)

                     Sys.sleep(2) # Simulate delay
                     incProgress(1/3, detail = "Finalizing...")
                     return(data)

                   })
    })

    ### indicate success
    output$Svy_GPS_alert <- renderUI({

      req(svy_GPS_data())

      if(dim(svy_GPS_data())[1]>0) {
        success_wall(successMessage="Survey GPS data upload successful")
      }else{error_wall()}

    })


  })
}

## To be copied in the UI
# mod_survey_dat_input_ui("survey_dat_input_1")

## To be copied in the server
# mod_survey_dat_input_server("survey_dat_input_1")
