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
    return(NULL)
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
                       DTA = uploaded_file$datapath)
                       #validate("Invalid file; Please upload a .zip or .dta file")
  #)
  # "Wrong .zip file, not containing Stata format data"
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





###############################################################
###  panels shows messages
###############################################################

success_wall <- function(successMessage="Survey raw data upload successful") {
  wellPanel(
    style = "margin-top: 5px;margin-bottom: 5px; padding: 1px; max-width: 400px; margin-left: auto; margin-right: auto;",
    tags$h5(successMessage, style = "color: green; text-align: center;")
  )
}


error_wall <- function(errorMessage="Wrong") {
  wellPanel(
    style = "margin-top: 20px; background-color: #f7f7f7; padding: 10px; max-width: 400px; margin-left: auto; margin-right: auto;",
    tags$h5(errorMessage, style = "color: #333; text-align: center;")
  )
}


