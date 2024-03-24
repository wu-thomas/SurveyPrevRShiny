#' dat_input_helpers
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
###############################################################
###  Given a country, find the available surveys (years)
###############################################################

get_survey_year <- function(country=NULL){

  if(is.null(country)){return(NULL)}
  surveys <- rdhs::dhs_surveys()

  # To see the structure of the returned surveys data frame
  country_svy <- surveys[surveys$CountryName == country, ]
  country_svy_years <- unique(country_svy$SurveyYear)
  if(length(country_svy_years)==0){return(NULL)}
  return(country_svy_years)


}

###############################################################
###  Given a zip file, unzip and find the correct extension
###############################################################

find_zip_path_extension <- function(uploaded_file = NULL, extensions = c(".DTA", ".dta")) {
  if (is.null(uploaded_file)) {
    return(NULL)
  }

  file_path <- uploaded_file$datapath
  temp <- tempfile()
  unzip(file_path, exdir = temp)

  # Use lapply to iterate over extensions and find files for each extension
  files_list <- lapply(extensions, function(ext) {
    # Create regex pattern for the current extension
    pattern <- paste0(gsub("\\.", "\\\\.", ext), "$") # Escape dot for regex
    list.files(temp, recursive = TRUE, pattern = pattern, full.names = TRUE)
  })

  # Flatten the list and remove empty elements
  files <- unlist(files_list)
  files <- files[files != ""]

  if (length(files) < 1) {
    return(NULL)
  }

  return(files[1])
}



###############################################################
###  determine the file path for loading DHS survey data
###############################################################

find_svy_dat_path <- function(uploaded_file=NULL){

  ext <- tools::file_ext(uploaded_file$name)
  path_found <- switch(ext,
                       zip = find_zip_path_extension(uploaded_file=uploaded_file),
                       dta = uploaded_file$datapath,
                       DTA = uploaded_file$datapath)
  #validate("Invalid file; Please upload a .zip or .dta file")
  #)
  # "Wrong .zip file, not containing Stata format data"
  #return(suppressWarnings(haven::read_dta(path_found)))

  return(path_found)

}

#### Check the data can be loaded with labels
# file_path <- paste0('E:/Dropbox/YunhanJon/Fertility-Analysis/Scripts/Madagascar/contraceptive_usage/','ZM_2018_DHS_03132024_2146_143411.zip')
# extensions <- '.DTA'
# file_to_download <- files[1]
# tmp.zmb.dat <- haven::read_dta(file_to_download)
# tmp.zmb.dat <- foreign::read.dta(file_to_download)

###############################################################
###  determine the file path for loading DHS GPS data
###############################################################

find_svy_GPS_path <- function(uploaded_file=NULL){

  ext <- tools::file_ext(uploaded_file$name)
  path_found <- switch(ext,
                       zip = find_zip_path_extension(uploaded_file=uploaded_file,
                                                     extensions = c(".shp")),
                       shp = uploaded_file$datapath)

  return(path_found)

}











###############################################################
###  load GADM files
###############################################################


get_country_GADM <- function(country) {

  country_iso3 <- DHS_country_list[DHS_country_list$CountryName==country,'ISO3_CountryCode']

  gadm_list <- list()
  levels <- 0
  repeat {
    tmp.gadm <- geodata::gadm(country = country_iso3, resolution=2,
                              level = levels,
                              path = tempdir())
    if (is.null(tmp.gadm)) {
      break
    } else {
      tmp.gadm <- sf::st_as_sf(tmp.gadm)
      tmp.gadm <- sf::st_set_crs(tmp.gadm, 4326)

      if(levels==0){      gadm_list[['National']]  <- tmp.gadm
}else{
      gadm_list[[paste0('Admin-',levels)]]  <- tmp.gadm}
      levels <- levels + 1
    }
  }


  return(gadm_list)
}


###############################################################
###  check number of regions at each admin level
###############################################################

check_gadm_levels <- function(gadm_list) {
  if(is.null(gadm_list)){
    df <- data.frame(Admin_Level=c('National','Admin-1','Admin-2'),
                     Num_Regions=c(1,NA,NA))

    row.names(df) <- NULL
    colnames(df) <- c("Admin Level", "Number of Regions")

    transposed_df <- as.data.frame(t(df))

    colnames(transposed_df) <- transposed_df[1, ]
    transposed_df <- transposed_df[-1, ]

    return(transposed_df)
  }


  df <- data.frame(
    Admin_Level = names(gadm_list),
    Num_Regions = sapply(gadm_list, function(x) nrow(x))
  )

  df$Admin_Level <- ifelse(df$Admin_Level == "Admin-0", "National", df$Admin_Level)
  row.names(df) <- NULL
  colnames(df) <- c("Admin Level", "Number of Regions")

  transposed_df <- as.data.frame(t(df))

  colnames(transposed_df) <- transposed_df[1, ]
  transposed_df <- transposed_df[-1, ]

  return(transposed_df)
}





###############################################################
###  load indicator data
###############################################################

if(FALSE){
tmp.dhs <-surveyPrev::getDHSindicator(Rdata =mdg.dat, indicator = "ancvisit4+")

zmb.ex.dat$v025[1:10]
mdg.dat$v025[1:10]

dhs_data_labeled <- haven::as_factor(zmb.ex.dat)
dhs_data_labeled$v025[1:10]

library(labelled)
attr(zmb.ex.dat[, paste0( "v025")], which='labels')
attr(mdg.dat[, paste0( "v025")], which='labels')
attr(tmp.zmb.dat[, paste0( "v025")], which='labels')

zmb.ex.dat <- as.data.frame(tmp.zmb.dat)

}
