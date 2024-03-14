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
###  Given a zip file, unzip and find the dta to load
###########################################################l####

find_zip_path <- function(uploaded_file=NULL){

  file.path <- uploaded_file$datapath

  temp <- tempfile()

  unzip(file.path, exdir = temp)

  # Assuming the first .DAT file in the zip is one we want
  files <- c(list.files(temp,recursive = TRUE, "\\.DTA$",full.names = TRUE),
             list.files(temp,recursive = TRUE, "\\.dta$",full.names = TRUE)
  )

  if(length(files)<1){
    return(validate("Wrong .zip file, not containing Stata Format"))
  }

  return(files[1])

}

###############################################################
###  determine the file path for loading
###############################################################

find_svy_dat_path <- function(uploaded_file=NULL){

  ext <- tools::file_ext(uploaded_file$name)
  path_found <- switch(ext,
                       zip = find_zip_path(uploaded_file=uploaded_file),
                       dta = uploaded_file$datapath,
                       DTA = uploaded_file$datapath,
                       validate("Invalid file; Please upload a .zip or .dta file")
  )

  #return(suppressWarnings(readstata13::read.dta13(path_found)))

  return(path_found)

}



###############################################################
###  prompt when no data provided
###############################################################

showNoFileSelectedModal <- function() {
  showModal(modalDialog(
    title = "No File Selected",
    "Please upload a file before submitting.",
    easyClose = TRUE,
    footer = modalButton("OK")
  ))
}
