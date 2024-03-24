###############################################################
###  Spinner with customized message
###############################################################
#'
#' @description Show a spinner in the middle of the screen with
#' customized message
#'
#' @param message message to display on top of spinner
#'
#' @return NA
#'
#' @noRd

custom_spinner <- function(id, message = "Loading data, please Wait...") {
  div(
    id = id,
    style = "display: none; position: fixed;
    top: 50%; left: 50%; transform: translate(-50%, -50%);
    background-color: rgba(68, 85, 90, 0.2);
    padding: 20px; border-radius: 10px; text-align: center;
    z-index: 9999",
    tags$h3(message),
    shiny::icon("spinner", class = "fa-spin fa-3x", style = "color: #007bff;")  # Adjust color as needed
  )
}

#rgba(255, 255, 255, 0.2)
###############################################################
###  Panels shows whether data step is success
###############################################################
#'
#' @description a success/error panel
#'
#' @param message message to display on the panel
#'
#' @return NA
#'
#' @noRd

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


###############################################################
###  prompt when no data provided
###############################################################
#'
#' @description a pop up window with information
#'
#' @param message message to display on the panel
#'
#' @return NA
#'
#' @noRd

showNoFileSelectedModal <- function() {
  showModal(modalDialog(
    title = "No File Selected",
    "Please upload a file before submitting.",
    easyClose = TRUE,
    footer = modalButton("OK")
  ))
}


###############################################################
###  prompt to whether overwrite existing data
###############################################################

overwrite_svy_dat_confirm <- modalDialog(
  "Overwrite file already oploaded?",
  title = "Overwriting files",
  footer = tagList(
    actionButton("cancel", "Cancel"),
    actionButton("ok", "Delete", class = "btn btn-danger")
  )
)

