#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
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
          mod_data_input_ui("Dat_Input")
        ),
        mainPanel(
          tabsetPanel(type = "tabs",
                      mod_data_display_ui("Dat_Display"),
                      mod_data_display_ui("Dat_Display2")
                      # Invoke other UI modules here
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
    tags$script(src = "handler.js"),
    tags$link(href = "div_style.css")
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )

}
