#' model_selection UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_model_selection_ui <- function(id){
  ns <- NS(id)

  fluidPage(
    tags$head(
      tags$script('Shiny.addCustomMessageHandler("unbinding_table_elements", function(x) {
                        Shiny.unbindAll($(document.getElementById(x)).find(".dataTable"));
                        });'
      ),
      tags$style(HTML("
        /* General Styles for the checkboxTable within the model-checkbox-table class */
        .shiny-input-container:not(.shiny-input-container-inline) {
         width: 700px;
         max-width: 100%;
        }

        .model-checkbox-table {
          width: 100%; /* Full width to contain the DataTable */
          max-width: 800px;
          margin: 0 auto; /* Center the table horizontally */
          float: left;
        }

        .model-checkbox-table .dataTable {
          font-size: 16px; /* Larger text for readability */
          width: 100% !important; /* Force the table to expand to the container width */
          table-layout: fixed; /* Equal column widths */
          border-collapse: collapse; /* For border styling */
        }

        /* Header and cells styling */
        .model-checkbox-table .dataTable th,
        .model-checkbox-table .dataTable td {
          border: 1px solid #ddd; /* Light grey border */
          text-align: center; /* Center alignment for text */
  max-width: 300px !important; /* Ensure cells are less than 300px in width */

        }

        /* Zebra striping for rows */
        .model-checkbox-table .dataTable tr:nth-child(even){background-color: #f2f2f2;}

        /* Column and row headers styling */
        .model-checkbox-table .dataTable thead th {
          background-color: #ADD8E6; /* Green background for column headers */
          color: white; /* White text for contrast */
        }

         .model-checkbox-table .dataTable tbody tr td:first-child,
        .model-checkbox-table .dataTable thead th:first-child {
          width: 20%; /* Increase the width of the row names */
        }

        .model-checkbox-table .dataTable td input[type='checkbox'],
        .model-checkbox-table .dataTable td input[type='radio'] {
          display: block;
          margin-top: 10px;
          padding-left:3px;
          display: flex !important; justify-content: center !important; align-items: center !important;
          /* Additional custom styles for checkboxes and radio buttons can go here */
        }
        .navbar { background-color: #ADD8E6; }
      .navbar .navbar-nav .nav-item .nav-link { color: #FFFFFF; }
        .pretty-button {
      background-color: #ADD8E6; /* Green */
      border: none;
      color: white;
      padding: 15px 32px;
      text-align: center;
      text-decoration: none;
      display: block;
      font-size: 20px;
      margin: 4px 2px;
      cursor: pointer;
      border-radius: 12px; /* Rounded corners */
    }


      "))
    ),

    h3("Model Fitting"),
    fluidRow(
      column(8,
             div(style = " margin: auto;float: left;",
                 uiOutput(ns("model_text_display"))
             )
      ),
      column(4,
             div(style = "display: flex; flex-wrap: wrap;",
                 uiOutput(ns("checklist"))
             )
      )
    ),
    navbarPage(title = "",
               tabPanel("Model Choices",
                        fluidRow(
                          column(12,
                                 div(DT::DTOutput(ns('checkboxTable')), class = "model-checkbox-table")
                                 ),
                          column(12,
                                 div(style = "display: flex; justify-content: center; padding: 20px 0;width: 100%; max-width: 800px;",
                                     actionButton(ns("run_analysis"), "Run all selected models", class = "pretty-button"))
                                 ),
                                 #DT::DTOutput(ns('valuesTable')),
                                 #DT::DTOutput(ns('Res_Tracker_Table')),
                                 #div(DT::DTOutput(ns('Selected_Res_Tracker_Table')),class = "model-checkbox-table"),
                          column(12,
                          div(DT::DTOutput(ns('Res_Status')),class = "model-checkbox-table")
                          )

                          )

                        ),
               tabPanel("Model Details"
                        )
               )


    #div(DT::DTOutput(ns('checkboxTable')), class = "model-checkbox-table"),
    #DT::DTOutput(ns('valuesTable'))

    #tags$hr(style="border-top-color: #E0E0E0;"), # (style="border-top: 2px solid #707070;")
  )

}

#' model_selection Server Functions
#'
#' @noRd
mod_model_selection_server <- function(id,CountryInfo,AnalysisInfo){
  moduleServer( id, function(input, output, session){
    ns <- session$ns



    method_names <- c('Direct Estimates','Area-level Model','Unit-level Model')
    ###############################################################
    ### text instructions on model selection
    ###############################################################

    output$model_text_display <- renderUI({

      req(CountryInfo$country())
      req(CountryInfo$svy_indicator_var())
      req(CountryInfo$svy_analysis_dat())

      country <- CountryInfo$country()
      svy_year <- CountryInfo$svyYear_selected()
      admin_level <- CountryInfo$GADM_display_selected_level()
      #indicator_description <- surveyPrev_ind_list[surveyPrev_ind_list$ID==input$Svy_indicator,]$Description

      if(FALSE){
        # report odds ratio later when incorporating stratified model
      OR_vec <- get_natl_UR_OR(CountryInfo$svy_analysis_dat())

      hi_or_lo <- 'higher'
      if(OR_vec[1]<1){hi_or_lo ='lower'}

      #'The odds ratio for urban vs rural is ',
      #sprintf("%.2f(%.2f, %.2f)", OR_vec[1], OR_vec[2], OR_vec[3]),
      #', with urban having a ',hi_or_lo,' odds.',
      }


      HTML(paste0(
        "<p style='font-size: large;'>",
        "Your selection: <span style='font-weight:bold;'>", country, "</span>, survey in <span style='font-weight:bold;'>", svy_year, "</span>. ",
        "<br> You aim to estimate <span style='font-weight:bold;'>", CountryInfo$svy_indicator_des(), "</span> ",
        "across <span style='font-weight:bold;'>", concatenate_vector_with_and(CountryInfo$GADM_analysis_levels()), "</span> level(s).",
        "</p>",
        "<p style='background-color: lightblue; padding: 10px;font-size: large;'>",
        "We suggest the following analytical approaches for each level:",
        "<ul style='font-size: large;'>",
        "<li><strong>National level:</strong> Use <span style='font-weight:bold; background-color: lightblue;'>survey weighted direct estimates</span>.</li>",
        "<li><strong>Admin-1 level:</strong> Use <span style='font-weight:bold; background-color: lightblue;'>smoothed direct estimates</span>.</li>",
        "<li><strong>Finer levels:</strong> Implement <span style='font-weight:bold; background-color: lightblue;'>unit-level models</span>.</li>",
        "</ul>",
        "</p>",
        "<hr style='border-top-color: #E0E0E0;'>"
      ))



    })

    ###############################################################
    ### indicator for reset analysis
    ###############################################################

    ### When changes in the following variables are detected,
    ### reset all analysis parameters
    ### including model selection, fitted models and results tracker

    ## setup indicator for changes
    meta_snapshot <- reactive({
      list(
        country_selected = CountryInfo$country(),
        year_selected = CountryInfo$svyYear_selected(),
        indicator_selected = CountryInfo$svy_indicator_var()
      )
    })



    ###############################################################
    ### model selection checkbox table
    ###############################################################

    row_names <- c("Direct", "FH", "Unit")
    nrows <- length(row_names)

    col_names <- reactive({ CountryInfo$GADM_analysis_levels()    })
    ncols <- reactive({ length(col_names()) })



    ### detach checkboxes to table if the table is modified
    observeEvent(CountryInfo$GADM_analysis_levels(),{
      session$sendCustomMessage('unbinding_table_elements', ns('checkboxTable'))
    })

    observeEvent(meta_snapshot(),{
      session$sendCustomMessage('unbinding_table_elements', ns('checkboxTable'))
    })


    # Render the DataTable
    output$checkboxTable <- DT::renderDataTable({
      tmp.meta <- meta_snapshot()
      # Convert the reactive matrix to a regular matrix to create the dataframe
      df <- as.data.frame(matrix(vector('list', nrows * ncols()), nrow = nrows, dimnames = list(row_names, col_names())))

      # Populate the dataframe with checkbox inputs
      for (i in seq_len(nrows)) {
        for (j in seq_len(ncols())) {


          df[i, j] <- as.character(shiny::checkboxInput(inputId = ns(paste0("cb_", i, "_", j)),
                                                        label = NULL))

        }
      }


      if( 'National' %in%  col_names()){
        df[2, which( col_names()=='National')] <- as.character(HTML('<div style="display: flex; justify-content: center; align-items: center; height: 100%;"><input type="checkbox" disabled="disabled" style="margin-top: 10px;margin-bottom:10px;margin-left: -7px"></div>'))
        df[3, which( col_names()=='National')] <- as.character(HTML('<div style="display: flex; justify-content: center; align-items: center; height: 100%;"><input type="checkbox" disabled="disabled" style="margin-top: 10px;margin-bottom:10px;margin-left: -7px"></div>'))
      }


      rownames(df) <- method_names
      # Return the DataTable
      DT::datatable(df, escape = FALSE, selection = 'none',
                    options = list(dom = 't', paging = FALSE, ordering = FALSE,
                                   #autoWidth = TRUE,
                                   #columnDefs = list(list(width = '150px', targets = "_all")),
                                   preDrawCallback = DT::JS('function() { Shiny.unbindAll(this.api().table().node()); }'),
                                   drawCallback = DT::JS('function() { Shiny.bindAll(this.api().table().node()); }')))
    }, server = FALSE)


    ### track user's selection on models

    observe({

      matrix_status <- matrix(FALSE, nrow = nrows, ncol = ncols(), dimnames = list(row_names, col_names()))
      for (i in seq_len(nrows)) {
        for (j in seq_len(ncols())) {
          inputId <- paste0("cb_", i, "_", j)
          matrix_status[i, j] <- input[[inputId]] %||% FALSE
        }
      }
      AnalysisInfo$model_selection_mat(matrix_status)

    })


    ###############################################################
    ### run analysis based on model selection
    ###############################################################

    ### Only when the run_anlaysis button is hit, models will be fitted and results will be tracked.

    observeEvent(input$run_analysis, {


      ### pop-up window if no model is selected
      selected_matrix <- AnalysisInfo$model_selection_mat()

      if(sum(selected_matrix == T, na.rm = TRUE)==0){
        showNoModelModal()
        return()
      }

      ### pop-up window if data upload is incomplete
      if(is.null(CountryInfo$svy_analysis_dat())){
        showNoDataModal()
        return()
      }

      req(CountryInfo$svy_analysis_dat())



      col_names_tmp <- col_names()
      res_tracker_list <- AnalysisInfo$model_res_tracker_list()
      strat.gadm.level <- CountryInfo$GADM_strata_level()


      for (i in seq_len(nrows)) {
        for (j in seq_len(ncols())) {

          if(selected_matrix[i,j]==T){
            #message(paste0(i),':',paste0(j))

            tmp.method <- row_names[i]
            tmp.method.display <- method_names[i]
            tmp.adm <- col_names_tmp[j]
            tmp.adm.num <- admin_to_num(tmp.adm)



            # message('Modelling at ',tmp.adm,' using ',tmp.method,' model.')
            session$sendCustomMessage('controlSpinner', list(action = "show",
                                                             message = paste0('Modelling at ',tmp.adm,' using ',tmp.method.display,' approach. This might take a few minutes. Please wait...')))


            tmp.tracker.list <- res_tracker_list[[tmp.method]][[tmp.adm]]
            if(!is.null(tmp.tracker.list$status)){

              message('Skip. Already tried modelling at ',tmp.adm,' using ',tmp.method.display,' approach.')
              session$sendCustomMessage('controlSpinner', list(action = "show",
                message = paste0('Skip. Already tried modelling at ',tmp.adm,' using ',tmp.method.display,' approach.')))
              Sys.sleep(1)
              session$sendCustomMessage('controlSpinner', list(action = "hide"))

              next
            }

            ### set model fitting status to Successful, assuming no error occurs
            tmp.tracker.list$status <- 'Successful'
            tmp.tracker.list$message <- 'Successful'

            ### Run model
            tmp.res <- tryCatch(
              {
                #R.utils::withTimeout({
                tmp.res <- suppressWarnings(fit_svy_model(cluster.geo= CountryInfo$svy_GPS_dat(),  #mdg.ex.GPS
                                         gadm.list = CountryInfo$GADM_list(),  #mdg.ex.GADM.list
                                         analysis.dat =   CountryInfo$svy_analysis_dat(),
                                         model.gadm.level = tmp.adm.num,
                                         strat.gadm.level = strat.gadm.level,
                                         method = tmp.method,
                                         aggregation =T

                ))
                #}, timeout = 300) ### 5 minutes for timeout
              },error = function(e) {
                tmp.tracker.list$status <<- 'Unsuccessful'

                if(inherits(e, "TimeoutException")) {
                  message("The operation timed out!")
                  tmp.tracker.list$message <<- 'Timed out. Took too long to fit the model.'

                } else {
                  tmp.tracker.list$message <<- e$message
                  message(e$message)
                }
                return(NULL)
              }
            )



            ### store model results
            AnalysisInfo$set_track_res(tmp.method,tmp.adm,tmp.tracker.list)

            AnalysisInfo$set_fitted_res(tmp.method,tmp.adm,tmp.res)

            #AnalysisInfo$set_res_tracker(tmp.method,tmp.adm,T)


            session$sendCustomMessage('controlSpinner', list(action = "hide"))

          }

        }
      }


    })



    # reset tracker matrix on all models when new country/indicator/survey is selected
    observeEvent(meta_snapshot(),{

      AnalysisInfo$model_res_list(NULL)
      AnalysisInfo$model_res_tracker_list(NULL)


    })


    if(FALSE){

    # reset tracker matrix on all models when new country/indicator/survey is selected
    observe({

      tmp.meta <- meta_snapshot()

      res_tracker_mat <- matrix(NA, nrow = nrows,
                                ncol = length(CountryInfo$GADM_list()),
                                dimnames = list(row_names, names(CountryInfo$GADM_list())))

      AnalysisInfo$model_res_tracker_mat_old(res_tracker_mat)


    })
    }




    ###############################################################
    ### reactive tables internally checking models
    ###############################################################

    if(FALSE){
    # Render a reactive table showing the current status of checkboxes
    output$valuesTable <- DT::renderDT({

      df <- as.data.frame(AnalysisInfo$model_selection_mat())

      DT::datatable(df, options = list(dom = 't', paging = FALSE, ordering = FALSE))
    })

    # Render a reactive table showing the status of fitted models
    output$Res_Tracker_Table <- DT::renderDT({

      df <- as.data.frame(AnalysisInfo$model_res_tracker_mat_old())
      rownames(df) <- method_names

      DT::datatable(df, options = list(dom = 't', paging = FALSE, ordering = FALSE))
    })

    }

    ###############################################################
    ### Render a reactive table showing the status of selected models
    ###############################################################

    if(FALSE){
    output$Selected_Res_Tracker_Table <- DT::renderDT({


      all_res_tracker <- AnalysisInfo$model_res_tracker_mat_old()
      model_selection_tracker <- AnalysisInfo$model_selection_mat()

      selected_res_tracker <- model_selection_tracker
      selected_res_tracker[,] <- NA  # Assign NA to all entries

      all_res_subset <- all_res_tracker[, colnames(model_selection_tracker), drop = FALSE]


      selected_res_tracker[all_res_subset == TRUE & model_selection_tracker == TRUE] <- TRUE
      selected_res_tracker[all_res_subset == FALSE & model_selection_tracker == TRUE] <- FALSE

      #message(selected_res_tracker)
      rownames(selected_res_tracker) <- method_names

      DT::datatable(selected_res_tracker, options = list(dom = 't', paging = FALSE, ordering = FALSE))
    })
    }


    ###############################################################
    ### Render a reactive table showing the status of selected models
    ###############################################################

    output$Res_Status <- DT::renderDT({


      #res_status_list <- mdg.ex.res.tracker
      res_status_list <- AnalysisInfo$model_res_tracker_list()

      model_selection_tracker <- AnalysisInfo$model_selection_mat()

      selected_res_tracker <- model_selection_tracker
      rownames(selected_res_tracker) <- method_names

      selected_res_tracker[,] <- NA

      for (i in seq_len(dim(model_selection_tracker)[1])) {
        for (j in seq_len(dim(model_selection_tracker)[2])) {


          # do not display anything if not selected
          if(model_selection_tracker[i,j]==F){


            next

          }else{
            tmp.method <- rownames(model_selection_tracker)[i]
            tmp.adm <- colnames(model_selection_tracker)[j]

            tmp.status <- res_status_list[[tmp.method]][[tmp.adm]]$status
            tmp.message <- res_status_list[[tmp.method]][[tmp.adm]]$message

            #if(tmp.adm=='National'){tmp.status <- NULL}
            #message('Now at row ',tmp.method,' and column ',tmp.adm,' with model fitted',tmp.status, ' and message ',tmp.message)

            if(is.null(tmp.status)){
              selected_res_tracker[i, j] <- as.character(htmltools::HTML('<span style="color:orange;">&#9888; Model has not been implemented. Please click run models. </span>'))
              next
            }

            if(tmp.status=='Successful'){
              selected_res_tracker[i,j] <- as.character(htmltools::HTML('<span style="color:green;">&#10004; Successful</span>'))
              next
            }

            if(tmp.status=='Unsuccessful'){
              selected_res_tracker[i, j] <- as.character(htmltools::HTML(paste('<span style="color:red;">&#10008;', 'Unsuccessful: ',tmp.message, '</span>')))
              next
            }


          }


        }
      }

      df <- DT::datatable(selected_res_tracker,
                          escape = FALSE, options = list(dom = 't',paging = FALSE, ordering = FALSE))
      return(df)

    })




  })
}

## To be copied in the UI
# mod_model_selection_ui("model_selection_1")

## To be copied in the server
# mod_model_selection_server("model_selection_1")

