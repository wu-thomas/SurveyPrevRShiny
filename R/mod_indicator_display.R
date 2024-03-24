#' indicator_display UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_indicator_display_ui <- function(id){
  ns <- NS(id)

    tabPanel("Survey Indicator",
             div(style = "margin-top: -20px;",
                 h3("Choose Indicator for Analysis")),

             div(style = "margin-top: 25px;",
             selectInput(ns("ChooseInd"), with_red_star("Select an indicator for analysis "),
                         choices=character(0))
             ),
             hr(style="border-top-color: #E0E0E0;"), # (style="border-top: 2px solid #707070;"),

             div(style = "width: max(70%, 600px); margin: auto;float: left;",
             DT::dataTableOutput(ns("Dat_Preview"))
             ),


  )
}

#' indicator_display Server Functions
#'
#' @noRd
mod_indicator_display_server <- function(id,CountryInfo){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    observe({
      req(CountryInfo$svy_dat())
      updateSelectInput(session, "ChooseInd", choices = c("ancvisit4+"))
    })

    observeEvent(input$ChooseInd, {

      if(is.null(input$ChooseInd) || input$ChooseInd == ""){return()
      }else{

        CountryInfo$svy_indicator(input$ChooseInd)
        CountryInfo$svy_analysis_dat(surveyPrev::getDHSindicator(Rdata=CountryInfo$svy_dat(),
                                                                  indicator = input$ChooseInd))
      }
    })

    output$Dat_Preview <- DT::renderDataTable({


      analysis_dat <- CountryInfo$svy_analysis_dat()

      if(is.null(analysis_dat)){return()
      }else{

      analysis_dat <- haven::as_factor(analysis_dat)

      dt <- DT::datatable(analysis_dat,
                      options = list(pageLength = 5, autoWidth = TRUE),
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
    # "ancvisit4+"
    #  tmp.label <- rdhs::get_variable_labels(zmb.ex.dat)


  })
}

## To be copied in the UI
# mod_indicator_display_ui("indicator_display_1")

## To be copied in the server
# mod_indicator_display_server("indicator_display_1")
