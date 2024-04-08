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

  fluidPage(
    tags$head(
      tags$style(HTML("
      .checklist-item {
        padding: 2px;
        border-radius: 2px;
        transition: background-color 0.3s ease;
      }
      .checklist-item:hover {
        background-color: #f0f0f0;
      }
    "))
    ),

    h3("Data Upload Checklist"),
    fluidRow(
      column(8,
             div(style = " margin: auto;float: left;",
                 uiOutput(ns("text_display"))
             )
      ),
      column(4,
             div(style = "display: flex; flex-wrap: wrap;",
                 uiOutput(ns("checklist"))
             )
      )
    ),
    tags$hr(style="border-top-color: #E0E0E0;"), # (style="border-top: 2px solid #707070;")
    fluidRow(
      # Main panel on the left
      column(6,
             tabsetPanel(id = ns("data_provision_method"),
                         tabPanel("Manual Upload",
                                  div(style = "margin: auto;float: left;",
                                      uiOutput(ns("manual_upload_text"))
                                  ),
                                  ### upload survey data
                                  fileInput(ns("Svy_dataFile"),
                                            with_red_star("Upload DHS survey data (.zip)")),

                                  actionButton(ns("upload_Svy_Data"), "Submit Survey Data"),
                                  uiOutput(ns("Svy_Data_alert")),
                         ),
                         tabPanel("Provide Credentials",
                                  textInput(ns("username"), "Username:"),
                                  passwordInput(ns("password"), "Password:"),
                                  actionButton(ns("login_button"), "Login")
                         )
      )
      ),

      column(6,
             tags$h4("Data Preview"),
             hr(style="border-top-color: #E0E0E0;"), # (style="border-top: 2px solid #707070;"),
             div(style = " margin: auto;float: left;width:100%",
                 DT::dataTableOutput(ns("Dat_Preview"))
             )
      )
    )
  )
}

#' survey_dat_input Server Functions
#'
#' @noRd
mod_survey_dat_input_server <- function(id,CountryInfo){
  moduleServer( id, function(input, output, session){


    ### initialize variables for recode
    recode_for_ind_abbrev <- reactiveVal(NULL)
    recode_for_ind_names <- reactiveVal(NULL)

    recode_list_abbrev <- c('IR','PR','KR','BR','HR','MR','AR','CR')
    recode_list_names <- c("Individual Recode","Household Member Recode","Children's Recode",
                           "Births Recode","Household Recode","Men's Recode",
                           "HIV Test Results Recode","Couples' Recode")

    ###############################################################
    ### text instructions on recode
    ###############################################################

    output$text_display <- renderUI({
      req(CountryInfo$country())
      req(CountryInfo$svy_indicator_var())

      ### country information
      country <- CountryInfo$country()
      svy_year <- CountryInfo$svyYear_selected()
      admin_level <- CountryInfo$GADM_display_selected_level()
      #indicator_description <- surveyPrev_ind_list[surveyPrev_ind_list$ID==input$Svy_indicator,]$Description

      ### which recode (abbreviation) are needed for this indicator
      recode_for_ind_abbrev(recode_list_abbrev[which(full_ind_des[full_ind_des$ID==CountryInfo$svy_indicator_var(),
                                                                    recode_list_abbrev]==T)])

      ### which recode (full names) are needed for this indicator
      recode_for_ind_names(recode_list_names[which(full_ind_des[full_ind_des$ID==CountryInfo$svy_indicator_var(),
                                                               recode_list_abbrev]==T)])

      ### display the recode as, full names (abbreviation), format
      recode_for_display <- paste(recode_for_ind_abbrev(), " (", recode_for_ind_names(), ")", sep="")

      HTML(paste0(
        "<p style='font-size: large;'>",
        "Based on your goal of estimating ",
        "<span style='background-color: lightblue;'>",
        "<br> <strong>",CountryInfo$svy_indicator_des(), "</strong>, ",
        "</span> <br> in <strong>", country,
        "</strong> with <strong> DHS ", svy_year, "</strong> survey ",
        "<br> Please upload your data in ",
        "<span style='background-color: lightblue;'>",
        "<strong>",concatenate_vector_with_and(recode_for_display), "</strong> </span>.",
        "</p>"
        #"<br>",
        #"<hr style='border-top-color: #E0E0E0;'>"
      ))

    })

    ###############################################################
    ### text instructions on manually upload data
    ###############################################################

    output$manual_upload_text <- renderUI({

      req(CountryInfo$country())
      req(CountryInfo$svy_indicator_var())


      ### which file(s) listed on the DHS website to download
      dhs_dat_names <- unlist(lapply(recode_for_ind_names(), function(recode) {
        find_DHS_dat_name(country= CountryInfo$country(),
                          svy_year = CountryInfo$svyYear_selected(),
                          recode=recode)
      }))

      ### which GPS file to download
      dhs_GPS_names <- find_DHS_dat_name(country= CountryInfo$country(),
                          svy_year = CountryInfo$svyYear_selected(),
                          recode='Geographic Data')


      HTML(paste0(
        "<p style='font-size: large; margin-bottom: 20px;'>",
        "Please follow the steps below to select and download data from the DHS website:",
        "</p>",
        "<ol style='font-size: large; margin-top: 0; margin-bottom: 20px;'>",
        "<li>",
        "Navigate to the <strong>DHS website</strong> and locate the download section.",
        "</li>",
        "<li>",
        "Select data with the following file names: <br>",
        "<ul>",
        "<li>Survey Datasets: ",
        "<span style='background-color: lightblue; padding: 2px;'>",
        "<strong>", concatenate_vector_with_and(toupper(dhs_dat_names)), "</strong>",
        "</span>",
        "</li>",
        "<li>Geographic Datasets: ",
        "<span style='background-color: lightblue; padding: 2px;'>",
        "<strong>", concatenate_vector_with_and(toupper(dhs_GPS_names)), "</strong>",
        "</span>",
        "</li>",
        "</ul>",
        "</li>",
        "<li>",
        "Include all needed files in a single download request.",
        "</li>",
        "<li>",
        "Upload the downloaded <strong>.zip file</strong> using the upload bar provided below.",
        "</li>",
        "</ol>",
        "<hr style='border-top-color: #E0E0E0; margin-top: 20px;'>"
      ))

    })


    ###############################################################
    ### manually upload data
    ###############################################################

    observeEvent(CountryInfo$use_preloaded_Zambia(), {

      if(CountryInfo$use_preloaded_Zambia()){

        recode.data <- as.data.frame(zmb.ex.IR.dat)
        CountryInfo$update_svy_dat(recode_abbrev='IR', new_dat=recode.data)

        GPS.dat <- zmb.ex.GPS
        CountryInfo$svy_GPS_dat(GPS.dat)

      }else{return()}


    })

    observeEvent(CountryInfo$use_preloaded_Madagascar(), {

      if(CountryInfo$use_preloaded_Madagascar()){

        recode.data <- as.data.frame(mdg.ex.KR.dat)
        CountryInfo$update_svy_dat(recode_abbrev='KR', new_dat=recode.data)

        GPS.dat <- mdg.ex.GPS
        CountryInfo$svy_GPS_dat(GPS.dat)

      }else{return()}


    })


    observeEvent(input$upload_Svy_Data, {

      # Check if a file has been uploaded
      if (is.null(input$Svy_dataFile)) {
        showNoFileSelectedModal()
        return()
      }

      if(CountryInfo$use_preloaded_Zambia()){
        return()
      }

      req(input$Svy_dataFile)

      # set parameters
      country= CountryInfo$country()
      svy_year = CountryInfo$svyYear_selected()
      recode_names_list=recode_for_ind_names()
      file_path <- input$Svy_dataFile$datapath

      ## set survey recode data

      for (i in 1:length(recode_names_list)){

        file_prefix <- find_DHS_dat_name(country,svy_year,recode =recode_names_list[i] )

        recode_path_found <- find_recode_path(file_path = file_path,
                                       recode_file =file_prefix,
                                       extensions = 'DTA')

        if(!is.null(recode_path_found)){
          #message(paste0(recode_names_list[i] ,' recode found at: ',(recode_path_found)))

          session$sendCustomMessage('controlSpinner', list(action = "show",
                                                           message = paste0(recode_names_list[i],
                                                                            " found, loading...")))


          # F= use reploaded Zambia data
          ### whether to use new data
          recode.data <- suppressWarnings(haven::read_dta(recode_path_found))

          recode.data <- as.data.frame(recode.data)

          #mdg.ex.KR.dat <- recode.data
          #save(mdg.ex.KR.dat, file='E:/Dropbox/YunhanJon/SurveyPrevRShiny/data/mdg_example_KR_dat.rda')


          #recode.data <- as.data.frame(zmb.ex.IR.dat)

          #Sys.sleep(1)

          CountryInfo$update_svy_dat(recode_abbrev=recode_for_ind_abbrev()[i], new_dat=recode.data)

          #message(paste0(recode_for_ind_abbrev()[i]))
          session$sendCustomMessage('controlSpinner', list(action = "hide"))




        }
      }

      ## set survey GPS data

      GPS_prefix <- find_DHS_dat_name(country,svy_year,recode = 'Geographic Data' )

      GPS_path_found <- find_recode_path(file_path = file_path,
                                     recode_file = GPS_prefix,
                                     extensions = 'shp')

      #message(GPS_path_found)

      if(!is.null(GPS_path_found)){
        #message(paste0(GPS_prefix ,' recode found at: ',(GPS_path_found)))

        session$sendCustomMessage('controlSpinner', list(action = "show",
                                                         message = paste0( 'Geographic Data',
                                                                          " found, loading...")))
        GPS.dat <- suppressWarnings(sf::st_read(GPS_path_found))

        #GPS.dat <- zmb.ex.GPS
        #mdg.ex.GPS <- GPS.dat
        #save(mdg.ex.GPS, file='E:/Dropbox/YunhanJon/SurveyPrevRShiny/data/mdg_example_GPS.rda')


        CountryInfo$svy_GPS_dat(GPS.dat)
        session$sendCustomMessage('controlSpinner', list(action = "hide"))

      }


      #message(tmp.list)
      #message(CountryInfo$svy_GPS_dat())


    })



    ###############################################################
    ### Data preview
    ###############################################################

    output$Dat_Preview <- DT::renderDataTable({


      req(recode_for_ind_abbrev())
      req(CountryInfo$svy_dat_list())
      req(CountryInfo$svy_indicator_var())

      analysis_dat <- NULL

      tryCatch({
      svy_dat_list <- CountryInfo$svy_dat_list()
      svy_dat_recode <- svy_dat_list[[recode_for_ind_abbrev()[1]]]

      analysis_dat <- surveyPrev::getDHSindicator(Rdata=svy_dat_recode,
                                                  indicator = CountryInfo$svy_indicator_var())

      CountryInfo$svy_analysis_dat(analysis_dat)

      }, error = function(e) {
        message(e$message)
      })

      if(is.null(analysis_dat)){return()
      }else{

        analysis_dat <- haven::as_factor(analysis_dat)

        dt <- DT::datatable(analysis_dat,
                            options = list(pageLength = 5,scrollX = TRUE,
                                           scroller = TRUE),
                            filter = 'top', rownames = FALSE)
        # Apply formatting styles
        dt <- DT::formatStyle(dt,
                              columns = names(analysis_dat),
                              backgroundColor = 'rgba(255, 255, 255, 0.8)',
                              border = '1px solid #ddd',
                              fontSize = '14px',
                              fontWeight = 'normal',
                              lineHeight = '1.42857143')
        dt

        #DT::datatable(analysis_dat)



      }

    })



    ###############################################################
    ### Generate checklist UI
    ###############################################################

    output$checklist <- renderUI({

      req(recode_for_ind_abbrev())

      #message(recode_for_ind_abbrev())
      recode_status_check <- CountryInfo$check_svy_dat_upload(recode_for_ind_abbrev(),CountryInfo$svy_dat_list())

      #message(recode_status_check)
      GPS_status_check <- is.null(CountryInfo$svy_GPS_dat())

      tagList(
        div(style = "margin-top: -5px;margin-bottom: -10px",
        tags$h4("Upload Checklist:")
        ),
        ### display progress once indicator selected

        lapply(1:length(recode_for_ind_abbrev()), function(i) {
          checklistItem(paste(recode_for_ind_abbrev()[i],' Recode'), !recode_status_check[i])
        }),
        checklistItem(paste("GPS data"), !GPS_status_check)


      )
    })



  })

}

checklistItem <- function(name, completed = FALSE) {
  icon <- if(completed) "fas fa-check-circle" else "fas fa-times-circle"
  color <- if(completed) "green" else "orange"
  statusText <- if(completed) "Completed" else "Pending"

  tags$div(class = "checklist-item",
           tags$i(class = icon, style = sprintf("color: %s; margin-right: 5px;", color)),
           tags$span(style = sprintf("color: %s;", color), sprintf("%s: %s", name, statusText)),
           style = "margin-bottom: -2px; cursor: pointer;font-size: 1.2em;"
           #onclick = sprintf("this.style.opacity = '%s'", if(completed) "0.5" : "1")
  )
}

## To be copied in the UI
# mod_survey_dat_input_ui("survey_dat_input_1")

## To be copied in the server
# mod_survey_dat_input_server("survey_dat_input_1")

