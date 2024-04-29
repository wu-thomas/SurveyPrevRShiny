###############################################################
### load DHS meta data
###############################################################

DHS_api_timeout = F

if(FALSE){
tryCatch({
    R.utils::withTimeout({

      DHS.country.meta <- rdhs::dhs_countries()
      DHS.survey.meta <- rdhs::dhs_surveys()
      DHS.dataset.meta <- rdhs::dhs_datasets()
      #Sys.sleep(20)  # Simulating a delay

      message('DHS API is working fine. Use most up-to-date info.')
    },
    timeout = 3)  # Timeout in seconds
    },
    TimeoutException = function(ex) {

      message("DHS API call timed out, using backup data.")
      DHS_api_timeout = T

      ### use backup
      pkg_files <- list.files('data')
      filtered_files <- grep("^DHS_meta_preload_", pkg_files, value = TRUE)# Filter files that start with 'DHS_meta_preload_'

      load(file=paste0('data/',filtered_files))


      DHS.country.meta <- DHS.country.meta.preload
      DHS.survey.meta <- DHS.survey.meta.preload
      DHS.dataset.meta <- DHS.dataset.meta.preload

    },
    error = function(e) {

      message("Error loading 'rdhs' library: ", e$message)
      DHS_api_timeout = T

      ### use backup
      pkg_files <- list.files('data')
      filtered_files <- grep("^DHS_meta_preload_", pkg_files, value = TRUE)# Filter files that start with 'DHS_meta_preload_'

      load(file=paste0('data/',filtered_files))

      DHS.country.meta <- DHS.country.meta.preload
      DHS.survey.meta <- DHS.survey.meta.preload
      DHS.dataset.meta <- DHS.dataset.meta.preload

})
}
### use backup
pkg_files <- list.files('data')
filtered_files <- grep("^DHS_meta_preload_", pkg_files, value = TRUE)# Filter files that start with 'DHS_meta_preload_'

load(file=paste0('data/',filtered_files))

DHS.country.meta <- DHS.country.meta.preload
DHS.survey.meta <- DHS.survey.meta.preload
DHS.dataset.meta <- DHS.dataset.meta.preload

#message(list.files('data'))
#message(list.files('data/GADM_shp'))
#load('data/GADM_shp/zmb_example_GADM.rda')


###############################################################
### prepare preloaded DHS meta data
###############################################################

if(FALSE){

  DHS.country.meta.preload <- rdhs::dhs_countries()
  DHS.survey.meta.preload <- rdhs::dhs_surveys()
  DHS.dataset.meta.preload <- rdhs::dhs_datasets()



  formatted_date <- format(Sys.Date(), "%m%d%Y")

  setwd('/data/')
  save(DHS.country.meta.preload,DHS.survey.meta.preload,DHS.dataset.meta.preload,file=paste0('DHS_meta_preload_',formatted_date,'.rda'))

}


###############################################################
### prepare preloaded GADM shapefile
###############################################################


if(FALSE){

  all_country_names <- sort(DHS.country.meta[['CountryName']])
  all_country_names <- all_country_names[all_country_names!='Nigeria (Ondo State)']


  for(country in all_country_names){
    #country = 'Ethiopia'
   #country='Armenia'
    message(paste0('working on ',country))

    country_iso3 <- DHS.country.meta[DHS.country.meta$CountryName==country,'ISO3_CountryCode']

    #country_iso3='ARM'
    dir.create(file.path('data/GADM_shp', country_iso3), showWarnings = FALSE)

    if(length(list.files(paste0('data/GADM_shp/', country_iso3)))==2){
      message(paste0(country,' already downloaded, skip.'))
      next}

    ### store fine level GADM for analysis (neighborhood structure etc.)
    one_country_GADM <- get_country_GADM(country,resolution=1)

    adm_region_nums <- check_gadm_levels(one_country_GADM)

    ### only keep admin levels that have less than 1000 regions
    model_levels <- names(adm_region_nums[,adm_region_nums<1000])

    country.GADM.list.fine <- one_country_GADM[model_levels]

    saveRDS(country.GADM.list.fine,file=paste0('data/GADM_shp/',country_iso3,'/',country_iso3,'_GADM_analysis.rds'))

    ### store smoothed GADM for display
    one_country_GADM <- get_country_GADM(country,resolution=2)

    adm_region_nums <- check_gadm_levels(one_country_GADM)
    model_levels <- names(adm_region_nums[,adm_region_nums<1000])

    country.GADM.list.smoothed <- one_country_GADM[model_levels]

    saveRDS(country.GADM.list.smoothed,file=paste0('data/GADM_shp/',country_iso3,'/',country_iso3,'_GADM_display.rds'))
  }

}
