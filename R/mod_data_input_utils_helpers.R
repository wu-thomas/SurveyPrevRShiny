
get_survey_year <- function(country=NULL){

  if(is.null(country)){return(NULL)}
  surveys <- rdhs::dhs_surveys()

  # To see the structure of the returned surveys data frame
  country_svy <- surveys[surveys$CountryName == country, ]
  country_svy_years <- unique(country_svy$SurveyYear)
  if(length(country_svy_years)==0){return(NULL)}
  return(country_svy_years)


}
