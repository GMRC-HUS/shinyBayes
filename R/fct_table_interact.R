#' table_interact 
#'
#' @description A fct function
#' @import shiny
#' @import DT
#' @return The return value, if any, from executing the function.
#'
#' @noRd
#' 


library(shiny)
library(DT)
table_interact_UI <- function(id){
  ns <- NS(id)
  tagList(
    
    DT::dataTableOutput(ns("mod_table")),
    
    
    
    
    
  )
}


table_interact_server <- function(id, v, seuil=T) {
  moduleServer(
    id,
    
function(input, output, session) {

  ns <- session$ns
  proxy = dataTableProxy("mod_table")
  
  observeEvent(input$mod_table_cell_edit, {
 
    info = input$mod_table_cell_edit
  
    i = info$row
    j = info$col
    k = info$value

    isolate(
    
    if(j >0 & (j<i+1|!seuil)){
        v$data[i, j] <<- DT::coerceValue(k, v$data[i, j])
      
    }else{
      
    
      }
  
    )
    replaceData(proxy, v$data, resetPaging = F)  # replaces data displayed by the updated table
  })
  
  

  output$mod_table <- DT::renderDataTable({
    DT::datatable(v$data, editable = TRUE,options = list(dom="t",ordering=F,columnDefs = list(list(className = 'dt-center', targets = "_all")))
                  
                  )
    
  })
}
)
}

