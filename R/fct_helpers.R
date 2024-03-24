#' helpers
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
CountryInfo <- R6::R6Class(
  "CountryInfo",
  public = list(

    country = NULL,
    svyYear_list = NULL,
    svyYear_selected = NULL,
    GADM_list = NULL,
    GADM_selected = NULL,
    GADM_selected_level=NULL,
    svy_dat = NULL,
    svy_indicator = NULL,
    svy_analysis_dat = NULL,
    svy_GPS_dat = NULL,

    initialize = function() {

      self$country <- reactiveVal(NULL)

      self$svyYear_list <- reactiveVal(NULL)
      self$svyYear_selected <- reactiveVal(NULL)

      self$GADM_list <- reactiveVal(NULL)
      self$GADM_selected <- reactiveVal(NULL)
      self$GADM_selected_level <- reactiveVal(NULL)

      self$svy_dat <- reactiveVal(NULL)
      self$svy_indicator <- reactiveVal(NULL)
      self$svy_analysis_dat <- reactiveVal(NULL)
      self$svy_GPS_dat <- reactiveVal(NULL)

    }
  )
)
