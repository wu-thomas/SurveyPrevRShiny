#' data_input UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#'
mod_data_input_ui <- function(id){
  ns <- NS(id)
  tagList(
    tabPanel("Raw Data",
             h4("Data Input"),
             tabsetPanel(type = "tabs",
                         tabPanel("Upload Data",
                                  div(style = "margin-top: 10px;",
                                  fileInput(ns("dataFile"), "Upload your dataset (CSV)"),
                                  actionButton(ns("uploadData"), "Submit Data")
                                  )
                         ),
                         tabPanel("Enter Credentials",
                                  div(style = "margin-top: 10px;",
                                  textInput(ns("username"), "Username"),
                                  textInput(ns("ProjectTitle"), "ProjectTitle"),
                                  passwordInput(ns("password"), "Password"),
                                  actionButton(ns("downloadData"), "Download From DHS website")
                                  )
                         )
             )
    )
  )
}

#' data_input Server Functions
#'
#' @noRd
mod_data_input_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_data_input_ui("data_input_1")

## To be copied in the server
# mod_data_input_server("data_input_1")
