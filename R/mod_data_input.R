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
    tags$head(
      tags$style(HTML("
       /* Increase font size of shinyFeedback messages */
       .shiny-feedback .feedback {
        font-size: 16px !important; /* Increase the text size */
      }
        /* Reduce space below all input fields and action buttons */
        .shiny-input-container, .shiny-action-button {
          margin-bottom: 5px !important; /* Tighten spacing */
        }

        /* Adjust top margin for action buttons if needed */
        .shiny-action-button {
          margin-top: 0px !important;
        }

      "))
    ),
    shinyFeedback::useShinyFeedback(),
    tabPanel("Raw Data",
             h4("Data Input"),              #div(style = "margin-top: 10px;",
             ### country name
             selectInput(ns("country"), with_red_star("Choose a country "),
                         c('',sort(rdhs::dhs_countries()[['CountryName']]))),


             ### survey year
             selectInput(ns("Svy_year"),  with_red_star("Choose survey year "), choices = character(0)),


             ### upload survey data
             fileInput(ns("Svy_dataFile"),
                           with_red_star("Upload DHS survey data (Stata format, .zip or .DTA)")),

             actionButton(ns("upload_Svy_Data"), "Submit Survey Data"),
             uiOutput(ns("Svy_Data_alert")),



             ### upload survey GPS
             fileInput(ns("Svy_GPSFile"), with_red_star("Upload DHS GPS data (Stata format, .zip or .DTA)")),
             actionButton(ns("upload_Svy_GPS"), "Submit GPS Data"),
             tableOutput(ns("Svy_GPS_alert"))
    )
  )
}

#' data_input Server Functions
#'
#' @noRd
mod_data_input_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    ###############################################################
    ### load survey and year
    ###############################################################

    ### Update selections for survey years once a country has been selected
    svy_years_avail <- reactive({
      if(is.null(input$country) || input$country == ""){
        return(character(0)) # Return an empty character vector if no country is selected
      }
      get_survey_year(input$country)
    })

    # Correctly observe the change in country selection
    observeEvent(input$country, {
      freezeReactiveValue(input, "Svy_year")
      updateSelectInput(inputId = "Svy_year", choices = sort(svy_years_avail()))
    })

    ###############################################################
    ### load DHS data into the system, accept .zip or .dta
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

                     Sys.sleep(2)
                     incProgress(1/3)
                     path_found <- find_svy_dat_path(uploaded_file=input$Svy_dataFile)

                     if(is.null(path_found)){
                       shinyFeedback::feedbackWarning("Svy_dataFile", is.null(path_found),
                                                      "Wrong .zip file, not containing Stata format data")
                       return()
                     }


                     Sys.sleep(2) # Simulate delay
                     incProgress(1/3,detail = "Reading Stata file...")
                     data <- suppressWarnings(readstata13::read.dta13(path_found))
                     #data <- zmb.ex.dat

                     Sys.sleep(2) # Simulate delay
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


  })
}

## To be copied in the UI
# mod_data_input_ui("data_input_1")

## To be copied in the server
# mod_data_input_server("data_input_1")
