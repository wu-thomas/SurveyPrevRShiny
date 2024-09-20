
###############################################################
### test Rwanda contraceptive
###############################################################

if(FALSE){
  reactiveConsole(TRUE)

  CountryInfo <- CountryInfo$new()
  AnalysisInfo <- AnalysisInfo$new()

  ### initialize settings
  CountryInfo$WHO_version(F)
  CountryInfo$use_basemap('OSM')

  ### country meta
  ex.country <- 'Rwanda'
  ex.svy.year <- '2019'
  strat.gadm.level <- 1

  ### indicator
  ex.indicator.abbrev <-'FP_CUSA_W_IUD'
  #file_path <-'C:/Users/wu-th/Downloads/KE_2022_DHS_04132024_852_143411.zip'
  #file_path <-'C:/Users/wu-th/Downloads/RW_2019-20_DHS_04082024_724_143411.zip'
  #file_path <-'C:/Users/wu-th/Downloads/RW_2019-20_dat.zip'
  file_path <- 'E:/Downloads/RW_2019-20_DHS_05302024_1759_143411.zip'
  ###############################################################
  ### store country meta in R6
  ###############################################################

  ### country and svy year info
  CountryInfo$country(ex.country)
  CountryInfo$svyYear_selected(ex.svy.year) #CountryInfo$svyYear_list(ex.svy.year)

  country_iso3 <- DHS.country.meta[DHS.country.meta$CountryName==CountryInfo$country(),'ISO3_CountryCode']


  ### get shapefiles

  if(!CountryInfo$WHO_version()){
    country_shapefile <- get_country_shapefile(country=ex.country,source='GADM-preload')
  }else{
    country_shapefile <- get_country_shapefile(country=ex.country,source='WHO')
  }

  CountryInfo$GADM_list(country_shapefile$country_shp_analysis)
  CountryInfo$GADM_list_smoothed(country_shapefile$country_shp_smoothed)

  CountryInfo$GADM_display_selected(country_shapefile$country_shp_smoothed[['National']])


  ### indicator info and stratification level
  CountryInfo$svy_indicator_var(ex.indicator.abbrev)
  CountryInfo$GADM_strata_level(strat.gadm.level)


  ###############################################################
  ### load data
  ###############################################################

  ### get recode and filenames for this variable

  recode_for_ind_abbrev <- reactiveVal(NULL)
  recode_for_ind_names <- reactiveVal(NULL)

  recode_list_abbrev <- c('IR','PR','KR','BR','HR','MR','AR','CR')
  recode_list_names <- c("Individual Recode","Household Member Recode","Children's Recode",
                         "Births Recode","Household Recode","Men's Recode",
                         "HIV Test Results Recode","Couples' Recode")

  recode_for_ind_abbrev(recode_list_abbrev[which(ref_tab_all[ref_tab_all$ID==CountryInfo$svy_indicator_var(),
                                                              recode_list_abbrev]==T)])

  ### which recode (full names) are needed for this indicator
  recode_for_ind_names(recode_list_names[which(ref_tab_all[ref_tab_all$ID==CountryInfo$svy_indicator_var(),
                                                            recode_list_abbrev]==T)])

  ### load survey data
  country= CountryInfo$country()
  svy_year = CountryInfo$svyYear_selected()
  recode_names_list=recode_for_ind_names()

  for (i in 1:length(recode_names_list)){
    file_prefix <- find_DHS_dat_name(ex.country,ex.svy.year,recode =recode_names_list[i])

    recode_path_found <- find_recode_path(file_path = file_path,
                                          recode_file =file_prefix,
                                          extensions = 'DTA')

    recode.data <- suppressWarnings(haven::read_dta(recode_path_found))

    recode.data <- as.data.frame(recode.data)

    CountryInfo$update_svy_dat(recode_abbrev=recode_for_ind_abbrev()[i], new_dat=recode.data)


  }


  ### load GPS data
  ## set survey GPS data

  GPS_prefix <- find_DHS_dat_name(country,svy_year,recode = 'Geographic Data' )

  GPS_path_found <- find_recode_path(file_path = file_path,
                                     recode_file = GPS_prefix,
                                     extensions = 'shp')

  GPS.dat <- suppressWarnings(sf::st_read(GPS_path_found))

  CountryInfo$svy_GPS_dat(GPS.dat)



  ### get analysis data set

  svy_dat_list <- CountryInfo$svy_dat_list()


  if(length(recode_for_ind_abbrev())>1){

    svy_dat_recode <- svy_dat_list[recode_for_ind_abbrev()]
    names(svy_dat_recode) <- as.character(get_recode_names(recode_for_ind_abbrev()))
  }else{

    svy_dat_recode <- svy_dat_list[[recode_for_ind_abbrev()]]

  }

  analysis_dat_fun =  getFromNamespace(CountryInfo$svy_indicator_var(), "surveyPrevGithub")



  analysis_dat <- surveyPrev::getDHSindicator(Rdata=svy_dat_recode,
                                              indicator = CountryInfo$svy_indicator_var())
  CountryInfo$svy_analysis_dat(analysis_dat)

  ###############################################################
  ### data sparsity check
  ###############################################################

  ### initialize parameters
  col_names_tmp <- names(CountryInfo$GADM_list())
  n_adm_level <- length(col_names_tmp)
  row_names <- c("Direct", "FH", "Unit")
  nrows <- length(row_names)

  options(survey.adjust.domain.lonely=TRUE)
  options(survey.lonely.psu="adjust")


  strat.gadm.level <- CountryInfo$GADM_strata_level()

  ### initialize storage
  AnalysisInfo$model_screen_list(NULL)
  screen_check_list <- AnalysisInfo$model_screen_list()
  AnalysisInfo$cluster_admin_info_list(NULL)

  for (j in seq_len(n_adm_level)){

    tmp.adm <- col_names_tmp[j]
    tmp.adm.num <- admin_to_num(tmp.adm)

    for (i in seq_len(nrows)) {

      #message(paste0(i),':',paste0(j))

      tmp.method <- row_names[i]

      message('Checking at ',tmp.adm,' using ',tmp.method,' model.')

      tmp.check.model <- screen_check_list[[tmp.method]][[tmp.adm]]


      ### skip model if already tried
      if(!is.null(tmp.check.model$screen.flag)){
        next
      }


      ### prepare admin level GPS info if not stored
      geo_info_list <- AnalysisInfo$cluster_admin_info_list()
      tmp.geo.info <- geo_info_list[[tmp.adm]]

      if(is.null(tmp.geo.info)){

        tryCatch({

          message(tmp.adm)

          tmp.cluster.adm.info <- cluster_admin_info(cluster.geo= CountryInfo$svy_GPS_dat(),  #mdg.ex.GPS
                                                     gadm.list = CountryInfo$GADM_list(),  #mdg.ex.GADM.list
                                                     model.gadm.level = admin_to_num(tmp.adm),
                                                     strat.gadm.level = CountryInfo$GADM_strata_level())


          AnalysisInfo$set_info_list(tmp.adm,tmp.cluster.adm.info)

          geo_info_list <- AnalysisInfo$cluster_admin_info_list()
          tmp.geo.info <- geo_info_list[[tmp.adm]]

        },error = function(e) {
          message(e$message)
        })
      }

      ### set model fitting status to Successful, assuming no error occurs
      tmp.check.model$screen.flag <- 'Error'
      tmp.check.model$screen.message <- 'Unable to process cluster and admin information.'

      ### process check results

      tryCatch(
        {
          #R.utils::withTimeout({
          tmp.check.model <- suppressWarnings(
            screen_svy_model(cluster.admin.info=tmp.geo.info,
                             analysis.dat= CountryInfo$svy_analysis_dat(),
                             model.gadm.level= tmp.adm.num,
                             strat.gadm.level = strat.gadm.level,
                             method=tmp.method)
          )
          #}, timeout = 300) ### 5 minutes for timeout
        },error = function(e) {
          tmp.check.model$screen.flag  <<- 'Error'
          tmp.check.model$screen.message <<- e$message
          message(e$message)

        }
      )


      if(tmp.check.model$screen.flag == 'Warning' & tmp.method=='FH'){
        tmp.check.model$screen.flag  <- 'Error'
      }


      message(tmp.check.model$screen.flag)

      ### store model results
      AnalysisInfo$set_screen_Check(tmp.method,tmp.adm,tmp.check.model)




    }

  }

  ###############################################################
  ### analysis
  ###############################################################


  res_list <- list()
  res_tracker_list <- list()

  AnalysisInfo$model_res_list(res_list)
  AnalysisInfo$model_res_tracker_list(res_tracker_list)

  ### tryout model

  col_names <- names(CountryInfo$GADM_list())

  res_tracker_list <- AnalysisInfo$model_res_tracker_list()

  for (tmp.adm in col_names){

    tmp.adm.num <- admin_to_num(tmp.adm)

    for(tmp.method in c('Direct','FH','Unit')){

      message('Modelling at ',tmp.adm,' using ',tmp.method,' model.')


      tmp.tracker.list <- res_tracker_list[[tmp.method]][[tmp.adm]]
      if(!is.null(tmp.tracker.list$status)){

        message('Skip. Already tried modelling at Admin-',tmp.adm,' using ',tmp.method,' model.')

        next
      }


      ### set model fitting status to Successful, assuming no error occurs
      tmp.tracker.list$status <- 'Successful'
      tmp.tracker.list$message <- 'Successful'

      ### Run model
      tmp.res <- tryCatch(
        {
          #R.utils::withTimeout({
          tmp.res <- fit_svy_model(cluster.geo= CountryInfo$svy_GPS_dat(),  #mdg.ex.GPS
                                   gadm.list = CountryInfo$GADM_list(),  #mdg.ex.GADM.list
                                   analysis.dat =   CountryInfo$svy_analysis_dat(),
                                   model.gadm.level = tmp.adm.num,
                                   strat.gadm.level = CountryInfo$GADM_strata_level(),
                                   method = tmp.method,
                                   aggregation =T

          )
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



    }

  }


  examine.tracker <- AnalysisInfo$model_res_tracker_list()
  examine.res <- AnalysisInfo$model_res_list()

  #tmp.unit <- examine.res$Unit$`Admin-2`
  #tmp.FH <- examine.res$FH$`Admin-2`
  gadm.list.tmp <-  CountryInfo$GADM_list_smoothed()

  ###############################################################
  ### visualization
  ###############################################################

  ## unit-level admin-3 html
  tmp.plot.html <- prevMap.leaflet(res.obj =  examine.res$Unit$`Admin-3`,
                  gadm.shp = gadm.list.tmp[["Admin-3"]],
                  model.gadm.level = 3,
                  strata.gadm.level = 1,
                  value.to.plot = 'mean',
                  legend.label = 'Estimates',
                  hatching.density = 15,
                  map.title=NULL,
                  threshold.p = 0,
                  use.basemap = 'OSM',
                  legend.color.reverse=F)




  tmp.area <- as.numeric(sf::st_area(gadm.list.tmp[["National"]])/1e6)

  hatching.density.country <- round(sqrt(9e07/tmp.area))

  tmp.plot <- prevMap.leaflet(res.obj = examine.res$Direct$`Admin-3`,
                              gadm.shp = gadm.list.tmp[["Admin-3"]],
                              model.gadm.level = 3,
                              strata.gadm.level = 1,
                              value.to.plot ='mean', #mean $CI.width
                              legend.label = 'Estimates',
                              map.title='this is a long longlong title',
                              hatching.density=60)


  sqrt(tmp.area/6)

  tmp.area <- as.numeric(sf::st_area(gadm.list.tmp[["National"]])/1e6)
  ### debug

  ### scatter
  tmp.res <- mdg.ex.model.res$FH$`Admin-2`$res.admin1

  tmp.res <- examine.res$FH$`Admin-2`$res.admin2

  tmp.res2 <- examine.res$Direct$`Admin-2`$res.admin2

  tmp.scatter <- scatter.plot( res.obj.x = examine.res$FH$`Admin-2`,
                               res.obj.y = examine.res$Direct$`Admin-2`,
                               value.to.plot = 'mean',
                               model.gadm.level = 2,
                               strata.gadm.level = 1,
                               label.x = 'M1',
                               label.y = 'M2',
                               plot.title=NULL,
                               interactive=T)

  ### ridge plot

  tmp.res <- examine.res$Unit$'Admin-2'
  test.post <- posterior_ridge_plot(res.obj =  tmp.res,
                                    plot.extreme.num=354, #plot.extreme.num=10
                                    model.gadm.level = admin_to_num('Admin-2'),
                                    strata.gadm.level = 1,
                                    legend.label = 'Value',
                                    color.reverse= T,
                                    plot.format = c('Long','Wide')[1], # for extreme regions, side-by-side or long plot
                                    top.bottom.label=c('Top','Bottom') # how to name the extremes, top 10 bottom 10? need to change when close to 0 is bad for the indicator
  )
  tmp.res11 <- examine.res$Direct$'Admin-2'$res.admin2


}





###############################################################
### test Rwanda Github function, IR recode
###############################################################

if(FALSE){
  reactiveConsole(TRUE)

  CountryInfo <- CountryInfo$new()
  AnalysisInfo <- AnalysisInfo$new()

  ### initialize settings
  CountryInfo$WHO_version(F)
  CountryInfo$use_basemap('OSM')

  ### country meta
  ex.country <- 'Rwanda'
  ex.svy.year <- '2019'
  strat.gadm.level <- 1

  ### indicator
  ex.indicator.abbrev <-'FP_CUSA_W_FST'
  #file_path <-'C:/Users/wu-th/Downloads/KE_2022_DHS_04132024_852_143411.zip'
  #file_path <-'C:/Users/wu-th/Downloads/RW_2019-20_DHS_04082024_724_143411.zip'
  #file_path <-'C:/Users/wu-th/Downloads/RW_2019-20_dat.zip'
  file_path <- 'E:/Downloads/RW_2019-20_DHS_05302024_1759_143411.zip'
  ###############################################################
  ### store country meta in R6
  ###############################################################

  ### country and svy year info
  CountryInfo$country(ex.country)
  CountryInfo$svyYear_selected(ex.svy.year) #CountryInfo$svyYear_list(ex.svy.year)

  country_iso3 <- DHS.country.meta[DHS.country.meta$CountryName==CountryInfo$country(),'ISO3_CountryCode']


  ### get shapefiles

  if(!CountryInfo$WHO_version()){
    country_shapefile <- get_country_shapefile(country=ex.country,source='GADM-preload')
  }else{
    country_shapefile <- get_country_shapefile(country=ex.country,source='WHO')
  }

  CountryInfo$GADM_list(country_shapefile$country_shp_analysis)
  CountryInfo$GADM_list_smoothed(country_shapefile$country_shp_smoothed)

  CountryInfo$GADM_display_selected(country_shapefile$country_shp_smoothed[['National']])


  ### indicator info and stratification level
  CountryInfo$svy_indicator_var(ex.indicator.abbrev)
  CountryInfo$GADM_strata_level(strat.gadm.level)


  ###############################################################
  ### load data
  ###############################################################

  ### get recode and filenames for this variable

  recode_for_ind_abbrev <- reactiveVal(NULL)
  recode_for_ind_names <- reactiveVal(NULL)

  recode_list_abbrev <- c('IR','PR','KR','BR','HR','MR','AR','CR')
  recode_list_names <- c("Individual Recode","Household Member Recode","Children's Recode",
                         "Births Recode","Household Recode","Men's Recode",
                         "HIV Test Results Recode","Couples' Recode")

  recode_for_ind_abbrev(recode_list_abbrev[which(ref_tab_new[ref_tab_new$ID==CountryInfo$svy_indicator_var(),
                                                              recode_list_abbrev]==T)])

  ### which recode (full names) are needed for this indicator
  recode_for_ind_names(recode_list_names[which(ref_tab_new[ref_tab_new$ID==CountryInfo$svy_indicator_var(),
                                                            recode_list_abbrev]==T)])

  ### load survey data
  country= CountryInfo$country()
  svy_year = CountryInfo$svyYear_selected()
  recode_names_list=recode_for_ind_names()

  for (i in 1:length(recode_names_list)){
    file_prefix <- find_DHS_dat_name(ex.country,ex.svy.year,recode =recode_names_list[i])

    recode_path_found <- find_recode_path(file_path = file_path,
                                          recode_file =file_prefix,
                                          extensions = 'DTA')

    recode.data <- suppressWarnings(haven::read_dta(recode_path_found))

    recode.data <- as.data.frame(recode.data)

    CountryInfo$update_svy_dat(recode_abbrev=recode_for_ind_abbrev()[i], new_dat=recode.data)


  }


  ### load GPS data
  ## set survey GPS data

  GPS_prefix <- find_DHS_dat_name(country,svy_year,recode = 'Geographic Data' )

  GPS_path_found <- find_recode_path(file_path = file_path,
                                     recode_file = GPS_prefix,
                                     extensions = 'shp')

  GPS.dat <- suppressWarnings(sf::st_read(GPS_path_found))

  CountryInfo$svy_GPS_dat(GPS.dat)



  ### get analysis data set

  svy_dat_list <- CountryInfo$svy_dat_list()


  if(length(recode_for_ind_abbrev())>1){

    svy_dat_recode <- svy_dat_list[recode_for_ind_abbrev()]
    names(svy_dat_recode) <- as.character(get_recode_names(recode_for_ind_abbrev()))
  }else{

    svy_dat_recode <- svy_dat_list[[recode_for_ind_abbrev()]]

  }


  if(CountryInfo$svy_indicator_var() %in% ref_tab_new$ID){
     analysis_dat_fun =  getFromNamespace(CountryInfo$svy_indicator_var(), "surveyPrevGithub")
     library(labelled)
     library(naniar)
     library(sjlabelled)
     library(dplyr)
     analysis_dat = surveyPrev::getDHSindicator(Rdata=svy_dat_recode, indicator = NULL, FUN =analysis_dat_fun)

  }else{

  analysis_dat <- surveyPrev::getDHSindicator(Rdata=svy_dat_recode,
                                              indicator = CountryInfo$svy_indicator_var())
  }


  CountryInfo$svy_analysis_dat(analysis_dat)

  ###############################################################
  ### data sparsity check
  ###############################################################

  ### initialize parameters
  col_names_tmp <- names(CountryInfo$GADM_list())
  n_adm_level <- length(col_names_tmp)
  row_names <- c("Direct", "FH", "Unit")
  nrows <- length(row_names)

  options(survey.adjust.domain.lonely=TRUE)
  options(survey.lonely.psu="adjust")


  strat.gadm.level <- CountryInfo$GADM_strata_level()

  ### initialize storage
  AnalysisInfo$model_screen_list(NULL)
  screen_check_list <- AnalysisInfo$model_screen_list()
  AnalysisInfo$cluster_admin_info_list(NULL)

  for (j in seq_len(n_adm_level)){

    tmp.adm <- col_names_tmp[j]
    tmp.adm.num <- admin_to_num(tmp.adm)

    for (i in seq_len(nrows)) {

      #message(paste0(i),':',paste0(j))

      tmp.method <- row_names[i]

      message('Checking at ',tmp.adm,' using ',tmp.method,' model.')

      tmp.check.model <- screen_check_list[[tmp.method]][[tmp.adm]]


      ### skip model if already tried
      if(!is.null(tmp.check.model$screen.flag)){
        next
      }


      ### prepare admin level GPS info if not stored
      geo_info_list <- AnalysisInfo$cluster_admin_info_list()
      tmp.geo.info <- geo_info_list[[tmp.adm]]

      if(is.null(tmp.geo.info)){

        tryCatch({

          message(tmp.adm)

          tmp.cluster.adm.info <- cluster_admin_info(cluster.geo= CountryInfo$svy_GPS_dat(),  #mdg.ex.GPS
                                                     gadm.list = CountryInfo$GADM_list(),  #mdg.ex.GADM.list
                                                     model.gadm.level = admin_to_num(tmp.adm),
                                                     strat.gadm.level = CountryInfo$GADM_strata_level())


          AnalysisInfo$set_info_list(tmp.adm,tmp.cluster.adm.info)

          geo_info_list <- AnalysisInfo$cluster_admin_info_list()
          tmp.geo.info <- geo_info_list[[tmp.adm]]

        },error = function(e) {
          message(e$message)
        })
      }

      ### set model fitting status to Successful, assuming no error occurs
      tmp.check.model$screen.flag <- 'Error'
      tmp.check.model$screen.message <- 'Unable to process cluster and admin information.'

      ### process check results

      tryCatch(
        {
          #R.utils::withTimeout({
          tmp.check.model <- suppressWarnings(
            screen_svy_model(cluster.admin.info=tmp.geo.info,
                             analysis.dat= CountryInfo$svy_analysis_dat(),
                             model.gadm.level= tmp.adm.num,
                             strat.gadm.level = strat.gadm.level,
                             method=tmp.method)
          )
          #}, timeout = 300) ### 5 minutes for timeout
        },error = function(e) {
          tmp.check.model$screen.flag  <<- 'Error'
          tmp.check.model$screen.message <<- e$message
          message(e$message)

        }
      )


      if(tmp.check.model$screen.flag == 'Warning' & tmp.method=='FH'){
        tmp.check.model$screen.flag  <- 'Error'
      }


      message(tmp.check.model$screen.flag)

      ### store model results
      AnalysisInfo$set_screen_Check(tmp.method,tmp.adm,tmp.check.model)




    }

  }

  ###############################################################
  ### analysis
  ###############################################################


  res_list <- list()
  res_tracker_list <- list()

  AnalysisInfo$model_res_list(res_list)
  AnalysisInfo$model_res_tracker_list(res_tracker_list)

  ### tryout model

  col_names <- names(CountryInfo$GADM_list())

  res_tracker_list <- AnalysisInfo$model_res_tracker_list()

  for (tmp.adm in col_names){

    tmp.adm.num <- admin_to_num(tmp.adm)

    for(tmp.method in c('Direct','FH','Unit')){

      message('Modelling at ',tmp.adm,' using ',tmp.method,' model.')


      tmp.tracker.list <- res_tracker_list[[tmp.method]][[tmp.adm]]
      if(!is.null(tmp.tracker.list$status)){

        message('Skip. Already tried modelling at Admin-',tmp.adm,' using ',tmp.method,' model.')

        next
      }


      ### set model fitting status to Successful, assuming no error occurs
      tmp.tracker.list$status <- 'Successful'
      tmp.tracker.list$message <- 'Successful'

      ### Run model
      tmp.res <- tryCatch(
        {
          #R.utils::withTimeout({
          tmp.res <- fit_svy_model(cluster.geo= CountryInfo$svy_GPS_dat(),  #mdg.ex.GPS
                                   gadm.list = CountryInfo$GADM_list(),  #mdg.ex.GADM.list
                                   analysis.dat =   CountryInfo$svy_analysis_dat(),
                                   model.gadm.level = tmp.adm.num,
                                   strat.gadm.level = CountryInfo$GADM_strata_level(),
                                   method = tmp.method,
                                   aggregation =T

          )
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



    }

  }


  examine.tracker <- AnalysisInfo$model_res_tracker_list()
  examine.res <- AnalysisInfo$model_res_list()

  #tmp.unit <- examine.res$Unit$`Admin-2`
  #tmp.FH <- examine.res$FH$`Admin-2`
  gadm.list.tmp <-  CountryInfo$GADM_list_smoothed()

  ###############################################################
  ### visualization
  ###############################################################

  ## unit-level admin-3 html
  tmp.plot.html <- prevMap.leaflet(res.obj =  examine.res$Unit$`Admin-3`,
                                   gadm.shp = gadm.list.tmp[["Admin-3"]],
                                   model.gadm.level = 3,
                                   strata.gadm.level = 1,
                                   value.to.plot = 'mean',
                                   legend.label = 'Estimates',
                                   hatching.density = 15,
                                   map.title=NULL,
                                   threshold.p = 0,
                                   use.basemap = 'OSM',
                                   legend.color.reverse=F)




  tmp.area <- as.numeric(sf::st_area(gadm.list.tmp[["National"]])/1e6)

  hatching.density.country <- round(sqrt(9e07/tmp.area))

  tmp.plot <- prevMap.leaflet(res.obj = examine.res$Direct$`Admin-3`,
                              gadm.shp = gadm.list.tmp[["Admin-3"]],
                              model.gadm.level = 3,
                              strata.gadm.level = 1,
                              value.to.plot ='mean', #mean $CI.width
                              legend.label = 'Estimates',
                              map.title='this is a long longlong title',
                              hatching.density=60)


  sqrt(tmp.area/6)

  tmp.area <- as.numeric(sf::st_area(gadm.list.tmp[["National"]])/1e6)
  ### debug

  ### scatter
  tmp.res <- mdg.ex.model.res$FH$`Admin-2`$res.admin1

  tmp.res <- examine.res$FH$`Admin-2`$res.admin2

  tmp.res2 <- examine.res$Direct$`Admin-2`$res.admin2

  tmp.scatter <- scatter.plot( res.obj.x = examine.res$FH$`Admin-2`,
                               res.obj.y = examine.res$Direct$`Admin-2`,
                               value.to.plot = 'mean',
                               model.gadm.level = 2,
                               strata.gadm.level = 1,
                               label.x = 'M1',
                               label.y = 'M2',
                               plot.title=NULL,
                               interactive=T)

  ### ridge plot

  tmp.res <- examine.res$Unit$'Admin-2'
  test.post <- posterior_ridge_plot(res.obj =  tmp.res,
                                    plot.extreme.num=354, #plot.extreme.num=10
                                    model.gadm.level = admin_to_num('Admin-2'),
                                    strata.gadm.level = 1,
                                    legend.label = 'Value',
                                    color.reverse= T,
                                    plot.format = c('Long','Wide')[1], # for extreme regions, side-by-side or long plot
                                    top.bottom.label=c('Top','Bottom') # how to name the extremes, top 10 bottom 10? need to change when close to 0 is bad for the indicator
  )
  tmp.res11 <- examine.res$Direct$'Admin-2'$res.admin2


}





###############################################################
### test Rwanda birth certificate
###############################################################

if(FALSE){
  reactiveConsole(TRUE)

  CountryInfo <- CountryInfo$new()
  AnalysisInfo <- AnalysisInfo$new()

  ### initialize settings
  CountryInfo$WHO_version(F)
  CountryInfo$use_basemap('OSM')

  ### country meta
  ex.country <- 'Rwanda'
  ex.svy.year <- '2019'
  strat.gadm.level <- 1

  ### indicator
  ex.indicator.abbrev <-'CP_BREG_C_CRT'
  #file_path <-'C:/Users/wu-th/Downloads/KE_2022_DHS_04132024_852_143411.zip'
  #file_path <-'C:/Users/wu-th/Downloads/RW_2019-20_DHS_04082024_724_143411.zip'
  #file_path <-'C:/Users/wu-th/Downloads/RW_2019-20_dat.zip'
  file_path <- 'E:/Downloads/RW_2019-20_DHS_09182024_618_143411.zip'
  ###############################################################
  ### store country meta in R6
  ###############################################################

  ### country and svy year info
  CountryInfo$country(ex.country)
  CountryInfo$svyYear_selected(ex.svy.year) #CountryInfo$svyYear_list(ex.svy.year)

  country_iso3 <- DHS.country.meta[DHS.country.meta$CountryName==CountryInfo$country(),'ISO3_CountryCode']


  ### get shapefiles

  if(!CountryInfo$WHO_version()){
    country_shapefile <- get_country_shapefile(country=ex.country,source='GADM-preload')
  }else{
    country_shapefile <- get_country_shapefile(country=ex.country,source='WHO')
  }

  CountryInfo$GADM_list(country_shapefile$country_shp_analysis)
  CountryInfo$GADM_list_smoothed(country_shapefile$country_shp_smoothed)

  CountryInfo$GADM_display_selected(country_shapefile$country_shp_smoothed[['National']])


  ### indicator info and stratification level
  CountryInfo$svy_indicator_var(ex.indicator.abbrev)
  CountryInfo$GADM_strata_level(strat.gadm.level)


  ###############################################################
  ### load data
  ###############################################################

  ### get recode and filenames for this variable

  recode_for_ind_abbrev <- reactiveVal(NULL)
  recode_for_ind_names <- reactiveVal(NULL)

  recode_list_abbrev <- c('IR','PR','KR','BR','HR','MR','AR','CR')
  recode_list_names <- c("Individual Recode","Household Member Recode","Children's Recode",
                         "Births Recode","Household Recode","Men's Recode",
                         "HIV Test Results Recode","Couples' Recode")

  recode_for_ind_abbrev(recode_list_abbrev[which(ref_tab_all[ref_tab_all$ID==CountryInfo$svy_indicator_var(),
                                                             recode_list_abbrev]==T)])

  ### which recode (full names) are needed for this indicator
  recode_for_ind_names(recode_list_names[which(ref_tab_all[ref_tab_all$ID==CountryInfo$svy_indicator_var(),
                                                           recode_list_abbrev]==T)])

  ### load survey data
  country= CountryInfo$country()
  svy_year = CountryInfo$svyYear_selected()
  recode_names_list=recode_for_ind_names()

  for (i in 1:length(recode_names_list)){
    file_prefix <- find_DHS_dat_name(ex.country,ex.svy.year,recode =recode_names_list[i])

    recode_path_found <- find_recode_path(file_path = file_path,
                                          recode_file =file_prefix,
                                          extensions = 'DTA')

    recode.data <- suppressWarnings(haven::read_dta(recode_path_found))

    recode.data <- as.data.frame(recode.data)

    CountryInfo$update_svy_dat(recode_abbrev=recode_for_ind_abbrev()[i], new_dat=recode.data)


  }


  ### load GPS data
  ## set survey GPS data

  GPS_prefix <- find_DHS_dat_name(country,svy_year,recode = 'Geographic Data' )

  GPS_path_found <- find_recode_path(file_path = file_path,
                                     recode_file = GPS_prefix,
                                     extensions = 'shp')

  GPS.dat <- suppressWarnings(sf::st_read(GPS_path_found))

  CountryInfo$svy_GPS_dat(GPS.dat)



  ### get analysis data set

  svy_dat_list <- CountryInfo$svy_dat_list()


  if(length(recode_for_ind_abbrev())>1){

    svy_dat_recode <- svy_dat_list[recode_for_ind_abbrev()]
    names(svy_dat_recode) <- as.character(get_recode_names(recode_for_ind_abbrev()))
  }else{

    svy_dat_recode <- svy_dat_list[[recode_for_ind_abbrev()]]

  }

  analysis_dat_fun =  getFromNamespace(CountryInfo$svy_indicator_var(), "surveyPrevGithub")



  if(CountryInfo$svy_indicator_var() %in% ref_tab_new$ID){
    analysis_dat_fun =  getFromNamespace(CountryInfo$svy_indicator_var(), "surveyPrevGithub")
    library(labelled)
    library(naniar)
    library(sjlabelled)
    library(dplyr)
    library(data.table)
    analysis_dat = surveyPrev::getDHSindicator(Rdata=svy_dat_recode, indicator = NULL, FUN =analysis_dat_fun)
    detach("package:data.table", unload=TRUE)
  }else{

    analysis_dat <- surveyPrev::getDHSindicator(Rdata=svy_dat_recode,
                                                indicator = CountryInfo$svy_indicator_var())
  }


  CountryInfo$svy_analysis_dat(analysis_dat)

  ###############################################################
  ### data sparsity check
  ###############################################################

  ### initialize parameters
  col_names_tmp <- names(CountryInfo$GADM_list())
  n_adm_level <- length(col_names_tmp)
  row_names <- c("Direct", "FH", "Unit")
  nrows <- length(row_names)

  options(survey.adjust.domain.lonely=TRUE)
  options(survey.lonely.psu="adjust")


  strat.gadm.level <- CountryInfo$GADM_strata_level()

  ### initialize storage
  AnalysisInfo$model_screen_list(NULL)
  screen_check_list <- AnalysisInfo$model_screen_list()
  AnalysisInfo$cluster_admin_info_list(NULL)

  for (j in seq_len(n_adm_level)){

    tmp.adm <- col_names_tmp[j]
    tmp.adm.num <- admin_to_num(tmp.adm)

    for (i in seq_len(nrows)) {

      #message(paste0(i),':',paste0(j))

      tmp.method <- row_names[i]

      message('Checking at ',tmp.adm,' using ',tmp.method,' model.')

      tmp.check.model <- screen_check_list[[tmp.method]][[tmp.adm]]


      ### skip model if already tried
      if(!is.null(tmp.check.model$screen.flag)){
        next
      }


      ### prepare admin level GPS info if not stored
      geo_info_list <- AnalysisInfo$cluster_admin_info_list()
      tmp.geo.info <- geo_info_list[[tmp.adm]]

      if(is.null(tmp.geo.info)){

        tryCatch({

          message(tmp.adm)

          tmp.cluster.adm.info <- cluster_admin_info(cluster.geo= CountryInfo$svy_GPS_dat(),  #mdg.ex.GPS
                                                     gadm.list = CountryInfo$GADM_list(),  #mdg.ex.GADM.list
                                                     model.gadm.level = admin_to_num(tmp.adm),
                                                     strat.gadm.level = CountryInfo$GADM_strata_level())


          AnalysisInfo$set_info_list(tmp.adm,tmp.cluster.adm.info)

          geo_info_list <- AnalysisInfo$cluster_admin_info_list()
          tmp.geo.info <- geo_info_list[[tmp.adm]]

        },error = function(e) {
          message(e$message)
        })
      }

      ### set model fitting status to Successful, assuming no error occurs
      tmp.check.model$screen.flag <- 'Error'
      tmp.check.model$screen.message <- 'Unable to process cluster and admin information.'

      ### process check results

      tryCatch(
        {
          #R.utils::withTimeout({
          tmp.check.model <- suppressWarnings(
            screen_svy_model(cluster.admin.info=tmp.geo.info,
                             analysis.dat= CountryInfo$svy_analysis_dat(),
                             model.gadm.level= tmp.adm.num,
                             strat.gadm.level = strat.gadm.level,
                             method=tmp.method)
          )
          #}, timeout = 300) ### 5 minutes for timeout
        },error = function(e) {
          tmp.check.model$screen.flag  <<- 'Error'
          tmp.check.model$screen.message <<- e$message
          message(e$message)

        }
      )


      if(tmp.check.model$screen.flag == 'Warning' & tmp.method=='FH'){
        tmp.check.model$screen.flag  <- 'Error'
      }


      message(tmp.check.model$screen.flag)

      ### store model results
      AnalysisInfo$set_screen_Check(tmp.method,tmp.adm,tmp.check.model)




    }

  }

  ###############################################################
  ### analysis
  ###############################################################


  res_list <- list()
  res_tracker_list <- list()

  AnalysisInfo$model_res_list(res_list)
  AnalysisInfo$model_res_tracker_list(res_tracker_list)

  ### tryout model

  col_names <- names(CountryInfo$GADM_list())

  res_tracker_list <- AnalysisInfo$model_res_tracker_list()

  for (tmp.adm in col_names){

    tmp.adm.num <- admin_to_num(tmp.adm)

    for(tmp.method in c('Direct','FH','Unit')){

      message('Modelling at ',tmp.adm,' using ',tmp.method,' model.')


      tmp.tracker.list <- res_tracker_list[[tmp.method]][[tmp.adm]]
      if(!is.null(tmp.tracker.list$status)){

        message('Skip. Already tried modelling at Admin-',tmp.adm,' using ',tmp.method,' model.')

        next
      }


      ### set model fitting status to Successful, assuming no error occurs
      tmp.tracker.list$status <- 'Successful'
      tmp.tracker.list$message <- 'Successful'

      ### Run model
      tmp.res <- tryCatch(
        {
          #R.utils::withTimeout({
          tmp.res <- fit_svy_model(cluster.geo= CountryInfo$svy_GPS_dat(),  #mdg.ex.GPS
                                   gadm.list = CountryInfo$GADM_list(),  #mdg.ex.GADM.list
                                   analysis.dat =   CountryInfo$svy_analysis_dat(),
                                   model.gadm.level = tmp.adm.num,
                                   strat.gadm.level = CountryInfo$GADM_strata_level(),
                                   method = tmp.method,
                                   aggregation =T

          )
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



    }

  }


  examine.tracker <- AnalysisInfo$model_res_tracker_list()
  examine.res <- AnalysisInfo$model_res_list()

  #tmp.unit <- examine.res$Unit$`Admin-2`
  #tmp.FH <- examine.res$FH$`Admin-2`
  gadm.list.tmp <-  CountryInfo$GADM_list_smoothed()

  ###############################################################
  ### visualization
  ###############################################################

  ## unit-level admin-3 html
  tmp.plot.html <- prevMap.leaflet(res.obj =  examine.res$Unit$`Admin-3`,
                                   gadm.shp = gadm.list.tmp[["Admin-3"]],
                                   model.gadm.level = 3,
                                   strata.gadm.level = 1,
                                   value.to.plot = 'mean',
                                   legend.label = 'Estimates',
                                   hatching.density = 15,
                                   map.title=NULL,
                                   threshold.p = 0,
                                   use.basemap = 'OSM',
                                   legend.color.reverse=F)




  tmp.area <- as.numeric(sf::st_area(gadm.list.tmp[["National"]])/1e6)

  hatching.density.country <- round(sqrt(9e07/tmp.area))

  tmp.plot <- prevMap.leaflet(res.obj = examine.res$Direct$`Admin-3`,
                              gadm.shp = gadm.list.tmp[["Admin-3"]],
                              model.gadm.level = 3,
                              strata.gadm.level = 1,
                              value.to.plot ='mean', #mean $CI.width
                              legend.label = 'Estimates',
                              map.title='this is a long longlong title',
                              hatching.density=60)


  sqrt(tmp.area/6)

  tmp.area <- as.numeric(sf::st_area(gadm.list.tmp[["National"]])/1e6)
  ### debug

  ### scatter
  tmp.res <- mdg.ex.model.res$FH$`Admin-2`$res.admin1

  tmp.res <- examine.res$FH$`Admin-2`$res.admin2

  tmp.res2 <- examine.res$Direct$`Admin-2`$res.admin2

  tmp.scatter <- scatter.plot( res.obj.x = examine.res$FH$`Admin-2`,
                               res.obj.y = examine.res$Direct$`Admin-2`,
                               value.to.plot = 'mean',
                               model.gadm.level = 2,
                               strata.gadm.level = 1,
                               label.x = 'M1',
                               label.y = 'M2',
                               plot.title=NULL,
                               interactive=T)

  ### ridge plot

  tmp.res <- examine.res$Unit$'Admin-2'
  test.post <- posterior_ridge_plot(res.obj =  tmp.res,
                                    plot.extreme.num=354, #plot.extreme.num=10
                                    model.gadm.level = admin_to_num('Admin-2'),
                                    strata.gadm.level = 1,
                                    legend.label = 'Value',
                                    color.reverse= T,
                                    plot.format = c('Long','Wide')[1], # for extreme regions, side-by-side or long plot
                                    top.bottom.label=c('Top','Bottom') # how to name the extremes, top 10 bottom 10? need to change when close to 0 is bad for the indicator
  )
  tmp.res11 <- examine.res$Direct$'Admin-2'$res.admin2


}




