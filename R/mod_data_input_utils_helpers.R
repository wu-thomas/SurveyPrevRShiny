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
    pattern <- gsub("\\.", "\\\\.", ext) # Escape dot for regex
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
  #return(suppressWarnings(readstata13::read.dta13(path_found)))

  return(path_found)

}


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
###  load DHS GPS data
###############################################################

if(FALSE){
file.path <- 'E:/Dropbox/YunhanJon/Fertility-Analysis/Scripts/Madagascar/contraceptive_usage/ZM_2018_DHS_03142024_31_143411.zip'
file.path <- 'E:/Dropbox/YunhanJon/Fertility-Analysis/Scripts/Madagascar/contraceptive_usage/ZMGE71FL/ZMGE71FL.shp'
file.path <- 'E:/Dropbox/YunhanJon/Fertility-Analysis/Scripts/Madagascar/contraceptive_usage/ZM_2018_DHS_03132024_2146_143411.zip'

temp <- tempfile()

unzip(file.path, exdir = temp)

files <- list.files(temp,recursive = TRUE, "\\.shp$",full.names = TRUE)
shp <- rgdal::readOGR(files, verbose = FALSE)


shp <- sf::st_as_sf(shp)

unzip(input$upload$datapath)
shp <- rgdal::readOGR(paste(getwd(), list.files(pattern = "*.shp$"), sep="/"), verbose = FALSE)
shp <- sf::st_as_sf(shp)
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


