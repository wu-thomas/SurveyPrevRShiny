###############################################################
### function to fit a model with any method
###############################################################

#' fit model using functions in surveyPrev
#'
#' @description A fct function
#'
#' @param cluster.geo cluster GPS from DHS
#'
#' @param gadm.list list of GADM shapefiles, with names ('National','Admin-1',...)
#'
#' @param analysis.dat analysis dataset for the indicator
#'
#' @param strata.gadm.level which GADM admin level is used as the stratification in DHS
#'
#' @param model.gadm.level which GADM admin level to produce results for
#'
#' @param aggregation whether to conduct aggregation to upper national levels
#'
#' @param method %in% c('Direct','FH','Unit')
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd


#cluster.geo <- mdg.ex.GPS
#gadm.list <- mdg.ex.GADM.list
#analysis.dat <- surveyPrev::getDHSindicator(Rdata=mdg.ex.KR.dat,indicator = 'CH_VACC_C_NON')

#model.gadm.level <- 4
#strat.gadm.level <- 2

fit_svy_model <- function(cluster.geo,
                          gadm.list,
                          analysis.dat,
                          model.gadm.level,
                          strat.gadm.level=1,
                          aggregation=F,
                          method=c('Direct','FH','Unit')[1]){


  if(model.gadm.level==0){


    if(method=='Direct'){
    cluster.info <- surveyPrev::clusterInfo(geo=cluster.geo,
                                            poly.adm1=gadm.list[[paste0('Admin-',1)]],
                                            poly.adm2=gadm.list[[paste0('Admin-',1)]],
                                            by.adm1 = paste0("NAME_",1),
                                            by.adm2 = paste0("NAME_",1))

    res_adm <- surveyPrev::directEST(data=analysis.dat,
                         cluster.info= cluster.info,
                         admin=0,
                         strata="all")

    return(res_adm)
    }else{stop("Only direct estimates can be computed at National level.")}

  }


  ### determine whether the gadm level is finer than stratification level
  if(model.gadm.level > strat.gadm.level){psedo_level=2}else{psedo_level=1}


  if(psedo_level==2 && aggregation==T){
    message('Currectly not supporting aggregation for fine spatial resolution.')
    aggregation = F
  }

  ### for levels not finer than stratification level

  if(psedo_level==1){

    ### define cluster level
    cluster.info <- surveyPrev::clusterInfo(geo=cluster.geo,
                                            poly.adm1=gadm.list[[paste0('Admin-',model.gadm.level)]],
                                            poly.adm2=gadm.list[[paste0('Admin-',model.gadm.level)]],
                                            by.adm1 = paste0("NAME_",model.gadm.level),
                                            by.adm2 = paste0("NAME_",model.gadm.level)
    )


    if(aggregation){
      ### find aggregation weights using survey
      agg.survey <- surveyPrev::aggSurveyWeight(data = analysis.dat,
                                                cluster.info = cluster.info,
                                                admin = psedo_level)

      ### define admin information
      admin.info <- surveyPrev::adminInfo(poly.adm = gadm.list[[paste0('Admin-',model.gadm.level)]],
                                          admin = psedo_level,
                                          by.adm= paste0("NAME_",model.gadm.level),
                                          agg.pop =agg.survey)

    }else{  admin.info <- surveyPrev::adminInfo(poly.adm = gadm.list[[paste0('Admin-',model.gadm.level)]],
                                                admin = psedo_level,
                                                by.adm= paste0("NAME_",model.gadm.level))}


  }

  ### for levels finer than stratification level

  if(psedo_level==2){

    aggregation = F

    cluster.info <- surveyPrev::clusterInfo(geo=cluster.geo,
                                            poly.adm1=gadm.list[[paste0('Admin-',model.gadm.level-1)]],
                                            poly.adm2=gadm.list[[paste0('Admin-',model.gadm.level)]],
                                            by.adm1 = paste0("NAME_",model.gadm.level-1),
                                            by.adm2 = paste0("NAME_",model.gadm.level)
    )

    ### when need aggregation to stratification, use the following
    #cluster.info.tmp <- surveyPrev::clusterInfo(geo=tmp.geo,
    #                                            poly.adm1=gadm.list[[paste0('Admin-',strata.level)]],
    #                                            poly.adm2=gadm.list[[paste0('Admin-',model.gadm.level)]],
    #                                            by.adm1 = paste0("NAME_",strata.level),
    #                                            by.adm2 = paste0("NAME_",model.gadm.level)
    #)

    admin.info <- surveyPrev::adminInfo(poly.adm = gadm.list[[paste0('Admin-',model.gadm.level)]],
                                        admin = psedo_level,
                                        by.adm= paste0("NAME_",model.gadm.level),
                                        by.adm.upper = paste0("NAME_",model.gadm.level-1))




  }



  ### direct estimates, possibly with aggregation to natl and strata level (D:admin-1)
  if(method=='Direct'){res_adm <- surveyPrev::directEST(data = analysis.dat,
                                                        cluster.info = cluster.info,
                                                        admin = psedo_level,
                                                        weight = "population",
                                                        admin.info = admin.info,
                                                        aggregation = aggregation)

  }


  ### F-H estimates, possibly with aggregation to natl and strata level (D:admin-1)

  if(method=='FH'){

    res_direct <- surveyPrev::directEST(data = analysis.dat,
                                        cluster.info = cluster.info,
                                        admin = psedo_level,
                                        weight = "population",
                                        admin.info = admin.info,
                                        aggregation = F)

    if(psedo_level==1){
      res_adm <- surveyPrev::fhModel(data= analysis.dat,
                                     cluster.info = cluster.info,
                                     admin.info = admin.info,
                                     admin = psedo_level,
                                     model = "bym2",
                                     aggregation =aggregation)

    }

    if(psedo_level==2){

      ### identify bad cluster
      bad_admins <- subset(res_direct$res.admin2, direct.var < 1e-30|is.na(direct.var)|direct.var==Inf)$admin2.name.full

      ### remove bad clusters
      updated.analysis.dat <- analysis.dat

      if(length(bad_admins)>0){
        if((length(bad_admins)/
           dim(res_direct$res.admin2)[1])>0.6){
          message((length(bad_admins)/dim((res_direct$res.admin2)[1])))
          stop("More than 60% of regions do not have valid direct estimates.")
        }

        bad_clusters <- subset(cluster.info$data, admin2.name.full %in% bad_admins)$cluster



        #message(bad_clusters)
        updated.analysis.dat <-subset(analysis.dat, !cluster %in% bad_clusters)

        #manually adjust only PSU, remove
        #updated.analysis.dat <- updated.analysis.dat %>%
        #  dplyr::group_by(v024) %>%
        #  dplyr::filter(dplyr::n_distinct(cluster) > 1) %>%
        #  dplyr::ungroup()

      }

      ### fit FH model
      #options(survey.lonely.psu="remove")
      #options(survey.adjust.domain.lonely=TRUE)

      res_adm <- surveyPrev::fhModel(data = updated.analysis.dat,
                                     cluster.info = cluster.info,
                                     admin.info = admin.info,
                                     admin = psedo_level,
                                     model = "bym2",
                                     aggregation =aggregation)
    }


  }

  ### unit-level model estimates, possibly with aggregation to natl

  if(method=='Unit'){
    if(dim(admin.info$mat)[1]>2000){
      stop(paste0("Too many admin regions (",dim(admin.info$mat)[1],") to yield meaningful results."))
    }

    res_adm <- surveyPrev::clusterModel(data=analysis.dat,
                                        cluster.info= cluster.info,
                                        admin.info = admin.info,
                                        model = "bym2",
                                        stratification =FALSE,
                                        admin = psedo_level,
                                        aggregation = aggregation,
                                        CI = 0.95)
  }


  ### return estimates

  return(res_adm)

}




###############################################################
### test assigning results
###############################################################

if(FALSE){
reactiveConsole(TRUE)

CountryInfo <- CountryInfo$new()
AnalysisInfo <- AnalysisInfo$new()

### initialize Zambia example

# country meta info
if(FALSE){
CountryInfo$country('Zambia')
CountryInfo$svyYear_list('2018')
CountryInfo$GADM_list(zmb.ex.GADM.list)
CountryInfo$svy_indicator_var('RH_ANCN_W_N4P')
strat.gadm.level <- 1

# analysis data info
recode.data <- as.data.frame(zmb.ex.IR.dat)
CountryInfo$update_svy_dat(recode_abbrev='IR', new_dat=recode.data)
GPS.dat <- zmb.ex.GPS
CountryInfo$svy_GPS_dat(GPS.dat)
GPS.dat <- CountryInfo$svy_GPS_dat()


svy_dat_list <- CountryInfo$svy_dat_list()
svy_dat_recode <- svy_dat_list[['IR']]
}

### initialize Madagascar example

# country meta info
if(FALSE){
  CountryInfo$country('Madagascar')
  CountryInfo$svyYear_list('2021')
  CountryInfo$GADM_list(mdg.ex.GADM.list)
  CountryInfo$svy_indicator_var('CH_VACC_C_NON')
  strat.gadm.level <- 2

  # analysis data info
  recode.data <- as.data.frame(mdg.ex.KR.dat)
  CountryInfo$update_svy_dat(recode_abbrev='KR', new_dat=recode.data)
  GPS.dat <- mdg.ex.GPS
  CountryInfo$svy_GPS_dat(GPS.dat)
  GPS.dat <- CountryInfo$svy_GPS_dat()


  svy_dat_list <- CountryInfo$svy_dat_list()
  svy_dat_recode <- svy_dat_list[['KR']]
}



analysis_dat <- surveyPrev::getDHSindicator(Rdata=svy_dat_recode,
                                            indicator = CountryInfo$svy_indicator_var())

CountryInfo$svy_analysis_dat(analysis_dat)
analysis_dat <- CountryInfo$svy_analysis_dat()


### initialize dimensions for storage of analysis results

#row_names <- c("Direct", "FH", "Unit")
#nrows <- length(row_names)

#col_names <- reactive({ CountryInfo$GADM_analysis_levels()    })
#ncols <- reactive({ length(col_names()) })




### initialize analysis related, once new country/survey/indicator is set

#tmp.meta <- meta_snapshot()

#n_admin <- length(CountryInfo$GADM_list())
#adm_level_names <- names(CountryInfo$GADM_list())

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
                                 strat.gadm.level = strat.gadm.level,
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

#### save Madagascar example
#mdg.ex.model.res <- examine.res
#mdg.ex.res.tracker <- examine.tracker

#save(mdg.ex.model.res,file='mdg_example_model_results.rda')
#save(mdg.ex.res.tracker,file='mdg_example_model_tracker.rda')

#AnalysisInfo$set_fitted_res('Direct','Admin-1',';yes')
#AnalysisInfo$model_res_list()

#examine.res[['Unit']][['Admin-3']]$res.admin2[1:10,]


}




###############################################################
### xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
###############################################################



if(FALSE){

  row_names <- c("Direct", "FH", "Unit")
  nrows <- length(row_names)

  # col_names <- names(mdg.ex.GADM.list)
  col_names <- names(zmb.ex.GADM.list)

  ncols <- length(col_names)

  adm_res_mat <- matrix(vector('list', nrows * ncols), nrow = nrows, dimnames = list(row_names, col_names))

  for (tmp.adm in col_names){

    tmp.adm.num <- admin_to_num(tmp.adm)

    for(tmp.method in c('Direct','FH','Unit')){

      message('Modelling at Admin-',tmp.adm,' using ',tmp.method,' model.')


      ### Run model
      tmp.res <- tryCatch(
        {
          tmp.res <- fit_svy_model(cluster.geo= zmb.ex.GPS,  #mdg.ex.GPS
                        gadm.list = zmb.ex.GADM.list,  #mdg.ex.GADM.list
                        analysis.dat =  surveyPrev::getDHSindicator(Rdata=zmb.ex.IR.dat,indicator = 'RH_ANCN_W_N4P'), #surveyPrev::getDHSindicator(Rdata=mdg.ex.KR.dat,indicator = 'CH_VACC_C_NON'),
                        model.gadm.level = tmp.adm.num,
                        strat.gadm.level = 1,
                        method = tmp.method,
                        aggregation =T

          )
        },error = function(e) {
          message(e$message)
          return(NULL)
        }
      )


      ### store model results
      adm_res_mat[[tmp.method, tmp.adm]] <- tmp.res



    }

  }

}
### use Madagascar to implement multiple admin
if(FALSE){
  tmp.geo <- zmb.ex.GPS
  tmp.gadm.list <- zmb.ex.GADM.list
  tmp.analysis.dat <- surveyPrev::getDHSindicator(Rdata=zmb.ex.IR.dat,indicator = 'RH_ANCN_W_N4P')

  #tmp.geo <- mdg.ex.GPS
  #tmp.gadm.list <- mdg.ex.GADM.list
  #tmp.analysis.dat <- surveyPrev::getDHSindicator(Rdata=mdg.ex.KR.dat,indicator = 'CH_VACC_C_NON')

#names(tmp.gadm.list) <- c('National','Customized-1','Admin-1','Admin-2','Customized-2')
#sf::sf_use_s2(F)
### setup admin info for admin-1 and admin-2


strata.level <- 1


### analysis at admin-2 level in GADM (admin1 in DHS)

adm.level <- 4

if( adm.level > strata.level){
cluster.info.tmp <- surveyPrev::clusterInfo(geo=tmp.geo, poly.adm1=tmp.gadm.list[[paste0('Admin-',strata.level)]],
                                             by.adm1 = paste0("NAME_",strata.level),by.adm2 = paste0("NAME_",adm.level),
                                             poly.adm2=tmp.gadm.list[[paste0('Admin-',adm.level)]])
}else{
  cluster.info.tmp <- surveyPrev::clusterInfo(geo=tmp.geo, poly.adm1=tmp.gadm.list[[paste0('Admin-',adm.level)]],
                                              by.adm1 = paste0("NAME_",adm.level),by.adm2 = paste0("NAME_",adm.level),
                                              poly.adm2=tmp.gadm.list[[paste0('Admin-',adm.level)]])
}

if(adm.level> strata.level){
admin.info.tmp <- surveyPrev::adminInfo(poly.adm = tmp.gadm.list[[paste0('Admin-',adm.level)]], admin = 2,
                                        by.adm= paste0("NAME_",adm.level),
                                        by.adm.upper = paste0("NAME_",strata.level))
}else{admin.info.tmp <- surveyPrev::adminInfo(poly.adm = tmp.gadm.list[[paste0('Admin-',adm.level)]], admin = 1,
                                              by.adm= paste0("NAME_",adm.level))}


if(strata.level< adm.level){psedo_level=2}else{psedo_level=1}

res_direct_adm <- surveyPrev::directEST(data = tmp.analysis.dat,
                     cluster.info = cluster.info.tmp,
                     admin = psedo_level)

res_fh_adm <- surveyPrev::fhModel(data=tmp.analysis.dat,
                             cluster.info = cluster.info.tmp,
                             admin.info = admin.info.tmp,
                             admin = psedo_level,
                             model = "bym2",aggregation =FALSE)

res_cl_adm <- surveyPrev::clusterModel(data=tmp.analysis.dat,
                           cluster.info= cluster.info.tmp,
                           admin.info = admin.info.tmp,
                           model = "bym2",
                           stratification =FALSE,
                           admin = psedo_level,
                           aggregation =FALSE,
                           CI = 0.95)



res_direct_adm[[paste0('res.admin1')]]
res_cl_adm$res.admin1
res_fh_adm$res.admin1


agg.survey.tmp <- surveyPrev::aggSurveyWeight(data = tmp.analysis.dat,
                                              cluster.info = cluster.info.tmp,
                                              admin = psedo_level)




if(strata.level< adm.level){
  admin.info.tmp <- surveyPrev::adminInfo(poly.adm = tmp.gadm.list[[paste0('Admin-',adm.level)]], admin = 2,
                                          by.adm= paste0("NAME_",adm.level),
                                          by.adm.upper = paste0("NAME_",strata.level),
                                          agg.survey.tmp)

}else{admin.info.tmp <- surveyPrev::adminInfo(poly.adm = tmp.gadm.list[[paste0('Admin-',adm.level)]],
                                              admin = 1,
                                              by.adm= paste0("NAME_",adm.level),
                                              agg.pop =agg.survey.tmp)}



res_aggre_adm <- surveyPrev::directEST(data = tmp.analysis.dat,
                        cluster.info = cluster.info.tmp,
                        admin = psedo_level,
                        weight = "population",
                        admin.info = admin.info.tmp,
                        aggregation = TRUE)

}


#tmp.analysis.dat <- surveyPrev::getDHSindicator(Rdata=zmb.ex.IR.dat,indicator = 'RH_ANCN_W_N4P')

#tmp.geo <- zmb.ex.GPS
#tmp.gadm.list <- zmb.ex.GADM.list
#tmp.geo <- mdg.ex.GPS
#tmp.gadm.list <- mdg.ex.GADM.list
#points_sf <- sf::st_as_sf(tmp.geo)
#sf::sf_use_s2(FALSE)
#tmp.cluster.info <- surveyPrev::clusterInfo(geo=tmp.geo, poly.adm1=tmp.gadm.list[['Admin-1']],
#                                            poly.adm2=tmp.gadm.list[['Admin-2']])

if(FALSE){

poly.adm1<- sf::st_as_sf(tmp.gadm.list[['Admin-1']])
poly.adm2<-sf::st_as_sf(tmp.gadm.list[['Admin-4']])
points_sf <- sf::st_as_sf(tmp.geo)

# Select required columns and filter out wrong points
cluster.info <- points_sf %>%
  dplyr::select(cluster = DHSCLUST, LONGNUM, LATNUM) #%>%
#filter(!(LATNUM < 0.0000001 & LONGNUM < 0.0000001))
#removing wrong.points that has weird LONGNUM LATNUM
wrong.points <- which(points_sf$LATNUM < 0.0000001 & points_sf$LONGNUM < 0.0000001)

by.adm1 ='NAME_1'
by.adm2 ='NAME_4'

admin1.sf <- sf::st_join(cluster.info, poly.adm1) %>%
  sf::st_transform(sf::st_crs(poly.adm1)) # Transform CRS if needed

cluster.info$admin1.name <- as.data.frame( admin1.sf)[,by.adm1]

# Spatial join for admin2
admin2.sf <- sf::st_join(cluster.info, poly.adm2) %>%
  sf::st_transform(sf::st_crs(poly.adm2)) # Transform CRS if needed

# Add admin2 name to cluster.info
cluster.info$admin2.name <- as.data.frame( admin2.sf)[,by.adm2]
cluster.info$admin2.name.full <- paste0(cluster.info$admin1.name,"_",cluster.info$admin2.name)




tmp.cluster.info <- surveyPrev::clusterInfo(geo=tmp.geo, poly.adm1=tmp.gadm.list[['Admin-3']],
                                            poly.adm2=tmp.gadm.list[['Admin-4']],
                                            by.adm2 = 'NAME_4',by.adm1 = 'NAME_3')
}

#invalid_geoms <- sf::st_is_valid(tmp.geo, reason = TRUE)

if(FALSE){
tmp.gadm.list <- zmb.ex.GADM.list
tmp.geo <- zmb.ex.GPS

tmp.analysis.dat <- surveyPrev::getDHSindicator(Rdata=zmb.ex.IR.dat,indicator = 'RH_ANCN_W_N4P')
colnames(tmp.analysis.dat)
tmp.cluster.info <- surveyPrev::clusterInfo(geo=tmp.geo, poly.adm1=tmp.gadm.list[['Admin-1']], poly.adm2=tmp.gadm.list[['Admin-2']])


tmp.analysis.dat <- surveyPrev::getDHSindicator(Rdata=zmb.ex.IR.dat,indicator = 'FP_NADA_W_UNT')
}


###############################################################
### national U/R ratio
###############################################################



get_natl_UR_OR <- function(tmp.analysis.dat){

  if(sum(is.na(tmp.analysis.dat$value))>0){
    tmp.analysis.dat <- tmp.analysis.dat[rowSums(is.na(tmp.analysis.dat)) == 0, ]
    #message("Removing NAs in indicator response")
  }


  tmp.analysis.dat <- dplyr::mutate(tmp.analysis.dat, strata = dplyr::case_when(
    strata == "urban" ~ 1,
    strata == "rural" ~ 0,
    TRUE ~ NA_real_  # Sets to NA for any other case
  ))

  tmp.analysis.dat$value <- as.integer(tmp.analysis.dat$value)

  tmp.dhs.design <- survey::svydesign(id = ~cluster, weights = ~weight, strata = ~v024,
                                      nest = TRUE, survey.lonely.psu = "adjust", data = tmp.analysis.dat)

  tmp.glm.model <- survey::svyglm(value ~ strata, family = quasibinomial , design = tmp.dhs.design)
  summary(tmp.glm.model)

  coef_model <- coef(tmp.glm.model)

  odds_ratio_strata <- exp(coef_model["strata"])

  # Calculate the confidence intervals for the model coefficients
  confint_model <- confint(tmp.glm.model, level = 0.95)

  # Confidence intervals for the odds ratio of 'strata'
  confint_strata <- exp(confint_model["strata", ])

  return(c(odds_ratio_strata, confint_strata[1], confint_strata[2]))
}

#get_natl_UR_OR(tmp.analysis.dat)
#formatted_output <- sprintf("%.2f(%.2f, %.2f)", odds_ratio_strata, confint_strata[1], confint_strata[2])


#res_ad0 <- surveyPrev::directEST(data=tmp.analysis.dat,
#                     cluster.info= tmp.cluster.info,
#                     admin=1,aggregation = F,
#                     strata="all")
