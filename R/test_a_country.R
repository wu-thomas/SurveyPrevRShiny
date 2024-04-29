###############################################################
### test Kenya contraceptive
###############################################################

if(FALSE){
  reactiveConsole(TRUE)

  CountryInfo <- CountryInfo$new()
  AnalysisInfo <- AnalysisInfo$new()

  ### country meta
  ex.country <- 'Kenya'
  ex.svy.year <- '2022'
  strat.gadm.level <- 1

  ### indicator
  ex.indicator.abbrev <-'FP_CUSA_W_MOD'
  #file_path <-'C:/Users/wu-th/Downloads/KE_2022_DHS_04132024_852_143411.zip'

  ###############################################################
  ### store country meta in R6
  ###############################################################

  CountryInfo$country(ex.country)
  CountryInfo$svyYear_selected(ex.svy.year) #CountryInfo$svyYear_list(ex.svy.year)

  country_GADM <- get_country_GADM(ex.country)
  CountryInfo$GADM_list(country_GADM)
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

  recode_for_ind_abbrev(recode_list_abbrev[which(full_ind_des[full_ind_des$ID==CountryInfo$svy_indicator_var(),
                                                              recode_list_abbrev]==T)])

  ### which recode (full names) are needed for this indicator
  recode_for_ind_names(recode_list_names[which(full_ind_des[full_ind_des$ID==CountryInfo$svy_indicator_var(),
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
  svy_dat_recode <- svy_dat_list[[recode_for_ind_abbrev()]]

  analysis_dat <- surveyPrev::getDHSindicator(Rdata=svy_dat_recode,
                                              indicator = CountryInfo$svy_indicator_var())
  CountryInfo$svy_analysis_dat(analysis_dat)

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

  tmp.unit <- examine.res$Unit$`Admin-2`
  tmp.FH <- examine.res$FH$`Admin-2`

  gadm.list.tmp <-  CountryInfo$GADM_list()

  ###############################################################
  ### visualization
  ###############################################################

  tmp.plot <- prevMap.leaflet(res.obj = examine.res$FH$`Admin-2`,
                              gadm.shp = gadm.list.tmp[["Admin-2"]],
                              model.gadm.level = 2,
                              strata.gadm.level = 1,
                              value.to.plot ='CI.width', #mean $CI.width
                              legend.label = 'Estimates',
                              map.title='this is a long longlong title')





}



###############################################################
### test Zambia HIV
###############################################################

if(FALSE){
  reactiveConsole(TRUE)

  CountryInfo <- CountryInfo$new()
  AnalysisInfo <- AnalysisInfo$new()

  ### country meta
  ex.country <- 'Zambia'
  ex.svy.year <- '2018'
  strat.gadm.level <- 1

  ### indicator
  ex.indicator.abbrev <-'HA_HIVP_B_HIV'
  #file_path <-'C:/Users/wu-th/Downloads/ZM_2018_DHS_04152024_654_143411.zip'

  ###############################################################
  ### store country meta in R6
  ###############################################################

  CountryInfo$country(ex.country)
  CountryInfo$svyYear_selected(ex.svy.year) #CountryInfo$svyYear_list(ex.svy.year)

  country_GADM <- get_country_GADM(ex.country)
  CountryInfo$GADM_list(country_GADM)
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

  recode_for_ind_abbrev(recode_list_abbrev[which(full_ind_des[full_ind_des$ID==CountryInfo$svy_indicator_var(),
                                                              recode_list_abbrev]==T)])

  ### which recode (full names) are needed for this indicator
  recode_for_ind_names(recode_list_names[which(full_ind_des[full_ind_des$ID==CountryInfo$svy_indicator_var(),
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

  analysis_dat <- surveyPrev::getDHSindicator(Rdata=svy_dat_recode,
                                              indicator = CountryInfo$svy_indicator_var())
  CountryInfo$svy_analysis_dat(analysis_dat)

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

  tmp.unit <- examine.res$Unit$`Admin-2`
  tmp.FH <- examine.res$FH$`Admin-2`

  gadm.list.tmp <-  CountryInfo$GADM_list()

  ###############################################################
  ### visualization
  ###############################################################

  tmp.plot <- prevMap.leaflet(res.obj = examine.res$FH$`Admin-2`,
                              gadm.shp = gadm.list.tmp[["Admin-2"]],
                              model.gadm.level = 2,
                              strata.gadm.level = 1,
                              value.to.plot ='CI.width', #mean $CI.width
                              legend.label = 'Estimates',
                              map.title='this is a long longlong title')





}




###############################################################
### test Rwanda contraceptive
###############################################################

if(FALSE){
  reactiveConsole(TRUE)

  CountryInfo <- CountryInfo$new()
  AnalysisInfo <- AnalysisInfo$new()

  ### country meta
  ex.country <- 'Rwanda'
  ex.svy.year <- '2019'
  strat.gadm.level <- 1

  ### indicator
  ex.indicator.abbrev <-'FP_CUSA_W_MOD'
  #file_path <-'C:/Users/wu-th/Downloads/KE_2022_DHS_04132024_852_143411.zip'
  #file_path <-'C:/Users/wu-th/Downloads/RW_2019-20_DHS_04082024_724_143411.zip'
  #file_path <-'C:/Users/wu-th/Downloads/RW_2019-20_dat.zip'

  ###############################################################
  ### store country meta in R6
  ###############################################################

  CountryInfo$country(ex.country)
  CountryInfo$svyYear_selected(ex.svy.year) #CountryInfo$svyYear_list(ex.svy.year)

  country_iso3 <- DHS.country.meta[DHS.country.meta$CountryName==CountryInfo$country(),'ISO3_CountryCode']

  if(file.exists(paste0('data/GADM_shp/',country_iso3,'/',country_iso3,'_GADM_analysis.rds'))){
    message('loading prepared GADM shapefile for analysis.')
    country_GADM <- readRDS(file=paste0('data/GADM_shp/',country_iso3,'/',country_iso3,'_GADM_analysis.rds'))
  } else{
    message('downloading GADM shapefile for analysis.')
    country_GADM <- get_country_GADM(input$country)
  }

  if(file.exists(paste0('data/GADM_shp/',country_iso3,'/',country_iso3,'_GADM_display.rds'))){
    message('loading prepared GADM shapefile for display.')
    country_GADM_smoothed <- readRDS(file=paste0('data/GADM_shp/',country_iso3,'/',country_iso3,'_GADM_display.rds'))
  } else{
    message('downloading GADM shapefile for display.')
    country_GADM_smoothed <- get_country_GADM(input$country,resolution=2)
  }

  CountryInfo$GADM_list(country_GADM)
  CountryInfo$GADM_list_smoothed(country_GADM_smoothed)
  #country_GADM <- get_country_GADM(ex.country)
  #CountryInfo$GADM_list(country_GADM)
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

  recode_for_ind_abbrev(recode_list_abbrev[which(full_ind_des[full_ind_des$ID==CountryInfo$svy_indicator_var(),
                                                              recode_list_abbrev]==T)])

  ### which recode (full names) are needed for this indicator
  recode_for_ind_names(recode_list_names[which(full_ind_des[full_ind_des$ID==CountryInfo$svy_indicator_var(),
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

  analysis_dat <- surveyPrev::getDHSindicator(Rdata=svy_dat_recode,
                                              indicator = CountryInfo$svy_indicator_var())
  CountryInfo$svy_analysis_dat(analysis_dat)


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


}
