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
###  Given a country, year and recode, find survey ID
###############################################################


#tmp.list.file <- find_DHS_dat_name(country= 'Malawi',
#                                    svy_year = 2015,
#                                    recode='Individual Recode')



find_DHS_dat_name <- function(country,svy_year,
                              recode){

  tryCatch({
    DHS_country_code <- DHS_country_list[DHS_country_list$CountryName == country,]$DHS_CountryCode

    if(!recode== 'Geographic Data'){
    survey_list <- rdhs::dhs_datasets(countryIds =DHS_country_code, surveyYear = svy_year)%>%
      dplyr::filter( FileFormat=='Stata dataset (.dta)')%>%
      dplyr::filter(FileType == recode)}else{

      survey_list <- rdhs::dhs_datasets(countryIds =DHS_country_code, surveyYear = svy_year)%>%
      dplyr::filter( FileType== recode)

      }
    return(survey_list$FileName)
  }, error = function(e) {
    # What to do in case of error
    return(NULL)
  })
}




######################################################################
###  Process downloaded DHS .zip (in a single file), for each recode
######################################################################


find_recode_path <- function(recode_file=NULL,
                             file_path=NULL,
                             extensions='DTA'){
  tryCatch({
    ### unzip
    temp <- tempfile()
    unzip(file_path, exdir = temp)

    ### specify the pattern according to DHS data naming system
    recode_file_prefix <- unlist(strsplit(recode_file,  '[.]'))[1]


    ### find the subfolder has the DHS data set name
    allPaths <- list.files(temp, pattern =  recode_file_prefix, full.names = TRUE, recursive = T, include.dirs = TRUE,
                           ignore.case = TRUE)
    dirPaths <- allPaths[sapply(allPaths, function(x) file.info(x)$isdir)][1]


    files_list <- lapply(extensions, function(ext) {
      # Create regex pattern for the current extension
      pattern <- paste0(gsub("\\.", "\\\\.", ext), "$") # Escape dot for regex
      list.files(dirPaths, recursive = TRUE, pattern = pattern, full.names = TRUE,ignore.case = TRUE)
    })

    # Flatten the list and remove empty elements
    files <- unlist(files_list)
    files <- files[files != ""]

    if (length(files) < 1) {
      return(NULL)
    }

    return(files[1])

  }, error = function(e) {
    # What to do in case of error
    return(NULL)
  })

}

#country= CountryInfo$country()
#svy_year = CountryInfo$svyYear_selected()
#recode_dat_list=recode_for_ind_names()

#find_DHS_dat_name('Zambia',2018,recode='Geographic Data')
#find_DHS_dat_name('Zambia',2018,recode='Individual Recode')


###############################################################
###  Given a zip file, unzip and find the correct extension
###############################################################

# retired
if(FALSE){

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


}


###############################################################
###  determine the file path for loading DHS survey data
###############################################################

# retired
if(FALSE){
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

# retired
if(FALSE){
find_svy_GPS_path <- function(uploaded_file=NULL){

  ext <- tools::file_ext(uploaded_file$name)
  path_found <- switch(ext,
                       zip = find_zip_path_extension(uploaded_file=uploaded_file,
                                                     extensions = c(".shp")),
                       shp = uploaded_file$datapath)

  return(path_found)

}

}









###############################################################
###  load GADM files
###############################################################


get_country_GADM <- function(country) {

  country_iso3 <- DHS_country_list[DHS_country_list$CountryName==country,'ISO3_CountryCode']

  gadm_list <- list()
  levels <- 0
  repeat {
    tmp.gadm <- geodata::gadm(country = country_iso3, resolution=1,
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



###############################################################
###  create example data frame for recode
###############################################################

if(FALSE){

IR_Individual <- c( "RH_ANCN_W_N4P",  "AN_ANEM_W_ANY",
                    "FP_NADA_W_UNT", "FP_CUSA_W_MOD", "AN_NUTS_W_THN","HA_HIVP_B_HIV")
PR_Household_Member <- c("CN_ANMC_C_ANY", "CN_NUTS_C_WH2", "CN_NUTS_C_HA2",
                         "WS_TLET_H_IMP", "WS_TLET_P_BAS",
                         "WS_SRCE_P_BAS")
KR_Children <- c("CH_DIAT_C_ORT", "CH_VACC_C_DP3", "CH_VACC_C_DP1",
                 "CH_VACC_C_BAS", "CH_VACC_C_NON", "CN_BRFS_C_EXB", "CH_VACC_C_MSL"
                 )
BRdata_Birth <- c("RH_DELA_C_SKP", "CM_ECMR_C_NNR")
HRdata_Household <- c("ML_NETP_H_IT2")

MR_men <- c("HA_HIVP_B_HIV")
AR_HIV<- c("HA_HIVP_B_HIV")
CR_couple<- NA


# Combine all indicators into a single vector and create a data frame
all_indicators <- unique(c(IR_Individual, PR_Household_Member, KR_Children, BRdata_Birth, HRdata_Household,MR_men))
wide_format <- data.frame(ID = all_indicators, stringsAsFactors = FALSE)

# Create columns for each data type and check if the indicator belongs to that type
wide_format$IR <- wide_format$ID %in% IR_Individual
wide_format$PR <- wide_format$ID %in% PR_Household_Member
wide_format$KR <- wide_format$ID %in% KR_Children
wide_format$BR <- wide_format$ID %in% BRdata_Birth
wide_format$HR <- wide_format$ID %in% HRdata_Household
wide_format$MR <- wide_format$ID %in% MR_men
wide_format$AR <- wide_format$ID %in% AR_HIV
wide_format$CR <- wide_format$ID %in% CR_couple

# merge back with the information data frame
full_ind_des <- merge(surveyPrev_ind_list,wide_format,by='ID',all.x=T)

save(full_ind_des,file='indicator_list.rda')

#recode_list <- c('IR','PR','KR','BR','HR','MR','AR','CR')
#recode_list[which(wide_format[7,recode_list]==T)]
#recode_list[which(full_ind_des[full_ind_des$ID=='HA_HIVP_B_HIV',recode_list]==T)]
}

