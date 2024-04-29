#' helpers
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
#'
###############################################################
###  country meta info and data
###############################################################

CountryInfo <- R6::R6Class(
  "CountryInfo",
  public = list(

    use_preloaded_Zambia = NULL,
    use_preloaded_Madagascar = NULL,

    country = NULL,
    svyYear_list = NULL,
    svyYear_selected = NULL,


    display_interactive = NULL,

    GADM_list = NULL,
    GADM_list_smoothed = NULL,

    GADM_display_selected = NULL,
    GADM_display_selected_level=NULL,
    GADM_analysis_levels=NULL,
    GADM_strata_level=NULL,

    svy_indicator_var = NULL,
    svy_indicator_des = NULL,

    svy_dat_list = NULL,
    svy_GPS_dat = NULL,

    svy_analysis_dat = NULL,

    initialize = function() {

      self$use_preloaded_Zambia <- reactiveVal(NULL)
      self$use_preloaded_Madagascar <- reactiveVal(NULL)

      self$country <- reactiveVal(NULL)

      self$svyYear_list <- reactiveVal(NULL)
      self$svyYear_selected <- reactiveVal(NULL)

      self$display_interactive <- reactiveVal(NULL)


      self$GADM_list <- reactiveVal(NULL)
      self$GADM_list_smoothed <- reactiveVal(NULL)

      self$GADM_display_selected <- reactiveVal(NULL)
      self$GADM_display_selected_level <- reactiveVal(NULL)
      self$GADM_analysis_levels <- reactiveVal(NULL)

      self$GADM_strata_level <- reactiveVal(1)

      self$svy_indicator_var <- reactiveVal(NULL)
      self$svy_indicator_des <- reactiveVal(NULL)

      self$svy_dat_list <-  reactiveVal(list(IR = NULL, PR = NULL, KR = NULL, BR = NULL, HR = NULL, MR = NULL, AR = NULL, CR = NULL))
      self$svy_GPS_dat <- reactiveVal(NULL)

      self$svy_analysis_dat <- reactiveVal(NULL)

    },


    ### update survey data
    update_svy_dat = function(recode_abbrev, new_dat) {
      # Ensure modifications to svy_dat_list are reactive
      current_list <- self$svy_dat_list()  # Access current value
      if (recode_abbrev %in% names(current_list)) {
        current_list[[recode_abbrev]] <- new_dat  # Update the list
        self$svy_dat_list(current_list)  # Set the modified list
      }
    },


    ### check wheter the data for recodes exist
    check_svy_dat_upload = function(requiredRecodes,svy_dat) {
      sapply(requiredRecodes, function(recode) {
        is.null(svy_dat[[recode]])
      })
    }





  )
)








###############################################################
###  analysis object
###############################################################

AnalysisInfo <- R6::R6Class(
  "AnalysisInfo",

  public = list(

    country = NULL,
    GADM_list = NULL,

    model_selection_mat = NULL,
    model_res_list = NULL, ### fitted objects and results
    model_res_tracker_list = NULL, ### error message and success indicator
    model_res_tracker_mat_old = NULL, ### error message and success indicator

    model_selected_res_tracker_mat=NULL,

    initialize = function() {

      self$country <- reactiveVal(NULL)
      self$GADM_list <- reactiveVal(NULL)

      self$model_selection_mat <- reactiveVal(NULL)


      self$model_res_list <- reactiveVal(NULL)
      self$model_res_tracker_list <- reactiveVal(NULL)


      self$model_res_tracker_mat_old <- reactiveVal(NULL)

      self$model_selected_res_tracker_mat <- reactiveVal(NULL)

    },

    set_res_tracker = function(method,adm.level,success) {
      #message(dim(self$model_res_tracker_mat))

        tmp.matrix <- self$model_res_tracker_mat_old()
        #message(paste0('trying at ',method,':',adm.level))

        tryCatch({
          tmp.matrix[method,adm.level] <-success
        },error= function(e) {
          message(paste0('not succesful at ',method,':',adm.level))
          return()})
        self$model_res_tracker_mat_old(tmp.matrix)


    },

    set_fitted_res = function(method,adm.level,model_res) {
      #message(dim(self$model_res_tracker_mat))

      tmp.list <- self$model_res_list()
      #message(paste0('trying at ',method,':',adm.level))

      tryCatch({
        tmp.list[[method]][[adm.level]] <- model_res
      },error= function(e) {
        message(paste0('not succesful at ',method,':',adm.level))
        return()})
      self$model_res_list(tmp.list)


    },

    set_track_res = function(method,adm.level,model_res) {
      #message(dim(self$model_res_tracker_mat))

      tmp.list <- self$model_res_tracker_list()
      #message(paste0('trying at ',method,':',adm.level))

      tryCatch({
        tmp.list[[method]][[adm.level]] <- model_res
      },error= function(e) {
        message(paste0('not succesful at ',method,':',adm.level))
        return()})
      self$model_res_tracker_list(tmp.list)


    }

  )

)

