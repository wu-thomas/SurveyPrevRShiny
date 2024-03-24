#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    tags$head(
      tags$style(HTML("
      .navbar .container-fluid {padding-left: 0; padding-right: 0;}
      .navbar-header {margin-left: 0; margin-right: 0;}
      .nav {margin: 0;}
    "))
    ),
    shinyFeedback::useShinyFeedback(),
    shinyjs::useShinyjs(),
    # Global loading screen, defined outside of any module
    # Define a global spinner with custom text

    # Leave this function for adding external resources
    golem_add_external_resources(),

    # Your application UI logic
    fluidPage(

      custom_spinner("loadingSpinnerCountry", "Loading country meta data, please wait..."),

      titlePanel("Small Area Estimation in LMIC"),
      sidebarLayout(
        sidebarPanel(
          mod_country_specify_ui("country_specify_1"),
          mod_survey_dat_input_ui("survey_dat_input"),
          mod_GADM_input_ui("GADM_input")
        ),
        mainPanel(
          navbarPage(title = "",
                     # This is a single tab in the navbar
                     mod_data_display_ui("Dat_Display_1"),
                     mod_indicator_display_ui("indicator_display_1"),

                     # Example of a dropdown menu in the navbar
                     navbarMenu("More Options",
                                tabPanel("Option 1", "Content for Option 1"),
                                tabPanel("Option 2", "Content for Option 2")
                                # You can add more options here
                     )
          )
        )
      )
    )
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
    app_sys("app/www")
  )

  tags$head(
    favicon(ext = 'png'),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "SurveyPrevRshiny"
    ),

    ### add message handler
    tags$script(src = "handler.js"),

    ### add style sheets for html objects
    tags$link(href = "div_style.css")

    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )

}
