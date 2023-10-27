#' chargement UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @import shinyFiles
#' @import datamods
mod_chargement2_ui <- function(id) {
  ns <- NS(id)
  import_ui(ns("test"))
}

#' chargement Server Functions
#'
#' @noRd
mod_chargement2_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    imported <- import_server("test", return_class = "tbl_df")
    
  })
}

## To be copied in the UI
# mod_chargement_ui("chargement_1")

## To be copied in the server
# mod_chargement_server("chargement_1")
