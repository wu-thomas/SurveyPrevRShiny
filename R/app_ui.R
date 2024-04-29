#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
#'
#'
#'
#library(shiny.semantic)
library(shinydashboard)
library(sf)
library(magrittr)
library(INLA)
library(sn)
#library(prettymapr)
#library(mapview)
#library(bookdown)
#library(markdown)

#library(semantic.dashboard)

app_ui <- function(request) {
  tagList(
    tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css?family=Roboto:400,700&display=swap"),
    tags$style(HTML("
          /* Add space between icons and text in sidebar menu items */
          .fas.fa-globe {
            margin-right: 6px;
          }
          .fas.fa-database {
            margin-right: 7px;
          }
          .fas.fa-sliders {
            margin-right: 6px;
          }
          .fas.fa-chart-line {
            margin-right: 6px;
          }
          .fas.fa-earth-americas {
            margin-right: 6px;
          }
          h1, h2, h3, h4, h5, h6 {
        font-family: 'Roboto', sans-serif;
          }
          .module-title {
          background-color: #f7f7f7;
          border-bottom: 1px solid #e1e1e1;
          padding: 10px;
          text-align: left;
          margin-bottom: 20px;
          }

        ")),

    # Global loading screen, defined outside of any module
    # Define a global spinner with custom text

    # Leave this function for adding external resources
    golem_add_external_resources(),

    custom_spinner("loadingSpinnerCountry", "Loading country meta data, please wait..."),

    # Your application UI logic
    dashboardPage(skin = "black",
     dashboardHeader(title = "Small Area Estimation for Improving Maternal Health in the region of Africa",titleWidth='750px'),
      dashboardSidebar(
       sidebarMenu(
          menuItem("Country Specification", tabName = "country_spec", icon = icon("globe")),
          menuItem("Data Upload", tabName = "data_upload", icon = icon("database")),
          menuItem("Model Fitting", tabName = "model_fit", icon = icon("sliders-h")),
          menuItem("Result Tabulation", tabName = "res_tab", icon = icon("line-chart")),
          menuItem("Result Visualization", tabName = "res_visual", icon = icon("earth"),
                   menuSubItem(HTML("&nbsp &nbsp &nbsp &nbsp Prevalence Map"), tabName = "res_prev_map",icon = NULL),
                   menuSubItem(HTML("&nbsp &nbsp &nbsp &nbsp Scatter Plot"), tabName = "res_scatter",icon = NULL))
          )
      ),
      dashboardBody(
        tabItems(
          tabItem(tabName = "country_spec",
                  mod_country_specify_ui("country_specify_1")), # Use the module UI here
          tabItem(tabName = "data_upload",
                  mod_survey_dat_input_ui("survey_dat_input_1")),
          tabItem(tabName = "model_fit",
                  mod_model_selection_ui("model_selection_1")),
          tabItem(tabName = "res_tab",
                  mod_result_tabulate_ui("result_tabulate_1")),
          # Adding individual content for each subtab
          tabItem(tabName = "res_prev_map",
                  mod_res_visual_prev_map_ui("res_visual_prev_map_1")), # Content for Map subtab
          tabItem(tabName = "res_scatter",
                  mod_res_visual_scatter_ui("res_visual_scatter_1")) # Content for Comparison Scatter subtab
        )
      ),
     tags$head(tags$style(HTML("
     @import url('https://fonts.googleapis.com/css?family=Lato:400,700&display=swap');
      * {
        font-family: 'Lato', sans-serif;
      }")))

    )
    # Your application UI logic

  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("/app/www")
  )

  tags$head(
    favicon(ext = 'png'),
    bundle_resources(
      path = app_sys("/app/www"),
      app_title = "SurveyPrevRshiny"
    ),

    ### add message handler
    #tags$script(src = "handlers.js"),

    ### add style sheets for html objects
    #tags$link(href = "div_style.css")

    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
    shinyFeedback::useShinyFeedback(),
    shinyjs::useShinyjs(),
  )

}


