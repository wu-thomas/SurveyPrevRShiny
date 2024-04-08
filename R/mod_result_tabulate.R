#' result_tabulate UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_result_tabulate_ui <- function(id){
  ns <- NS(id)
  fluidPage(
    titlePanel("Results Tabulation"), # Add a title
    fluidRow(
      column(4,
             selectInput(ns("selected_method"), "Select Method",
                         choices = c("Direct Estimates"="Direct" ,
                                     "Area-level Model"= "FH" , "Unit-level Model"="Unit" ))
      ),
      column(4, # Another half-width column for the second selection bar
             selectInput(ns("selected_adm"), "Select Admin Level", choices = character(0))
      )
    ),
    fluidRow(
      column(12,
             tags$h4("Estimates from models"),
             hr(style="border-top-color: #E0E0E0;"), # (style="border-top: 2px solid #707070;"),
             div(style = " margin: auto;float: left;width:100%;max-width:800px",
                 DT::dataTableOutput(ns("Res_tab"))
             )
      )
    )
    # Place additional UI elements below
  )
}

#' result_tabulate Server Functions
#'
#' @noRd
mod_result_tabulate_server <- function(id,CountryInfo,AnalysisInfo){
  moduleServer( id, function(input, output, session){
    ns <- session$ns


    row_names <- c("Direct", "FH", "Unit")
    nrows <- length(row_names)

    col_names <- reactive({ CountryInfo$GADM_analysis_levels()    })
    ncols <- reactive({ length(col_names()) })

    observeEvent(col_names(), {
      updateSelectInput(inputId = "selected_adm",
                        choices = col_names())
    })


    output$Res_tab <- DT::renderDataTable({

      req(input$selected_adm)
      req(input$selected_method)

      selected_adm <- input$selected_adm
      selected_method <- input$selected_method


      ### load Madagascar example
      if(CountryInfo$use_preloaded_Madagascar()){
        AnalysisInfo$model_res_list(mdg.ex.model.res)}

      model_res_all <- AnalysisInfo$model_res_list()

      #model_res_all <- mdg.ex.model.res

      strat.gadm.level <- CountryInfo$GADM_strata_level()

      if(admin_to_num(selected_adm) > strat.gadm.level){pseudo_level=2}else{
        if(admin_to_num(selected_adm)==0){pseudo_level=0}else{pseudo_level=1}}


      #message(pseudo_level)
      model_res_selected <- model_res_all[[selected_method]][[selected_adm]][[paste0('res.admin',pseudo_level)]]

      if(is.null(model_res_selected)){return()
      }else{

        dt <- DT::datatable(model_res_selected,
                            options = list(pageLength = 5,scrollX = TRUE,
                                           scroller = TRUE),
                            filter = 'top', rownames = FALSE)

        numeric_columns <- sapply(model_res_selected, is.numeric)

        dt <- DT::formatRound(dt, columns = numeric_columns, digits = 3)

        # Apply formatting styles
        dt <- DT::formatStyle(dt,
                              columns = names(model_res_selected),
                              backgroundColor = 'rgba(255, 255, 255, 0.8)',
                              border = '1px solid #ddd',
                              fontSize = '14px',
                              fontWeight = 'normal',
                              lineHeight = '1.42857143')
        dt




      }


    })

  })
}

## To be copied in the UI
# mod_result_tabulate_ui("result_tabulate_1")

## To be copied in the server
# mod_result_tabulate_server("result_tabulate_1")
