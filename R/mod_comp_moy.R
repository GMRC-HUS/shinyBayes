#' comp_moy UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_comp_moy_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' comp_moy Server Functions
#'
#' @noRd 
mod_comp_moy_server <- function(id,r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_comp_moy_ui("comp_moy_1")
    
## To be copied in the server
# mod_comp_moy_server("comp_moy_1")
