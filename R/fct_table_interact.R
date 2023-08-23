#' table_interact 
#'
#' @description A fct function
#' @import shiiny
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
    
    DTOutput(ns("mod_table")),
    
    
    
    
    
  )
}


table_interact_server <- function(id, v) {
  moduleServer(
    id,
    
function(input, output, session) {

  ns <- session$ns
  proxy = dataTableProxy(ns("mod_table"))
  
  observeEvent(input$mod_table_cell_edit, {
    print(names(v$data))
    info = input$mod_table_cell_edit
    str(info)
    i = info$row
    j = info$col
    k = info$value
    str(info)
    
    isolate(
    
      {
        v$data[i, j] <<- DT::coerceValue(k, v$data[i, j])
        print(v$data)
}
  
    )
    replaceData(proxy, v$data, resetPaging = FALSE)  # replaces data displayed by the updated table
  })
  
  
  print(isolate(colnames(v$data)))
  output$mod_table <- DT::renderDataTable({
    DT::datatable(v$data, editable = TRUE,options = list(dom="t",ordering=F))
    
  })
}
)
}

input_data <- data.frame(Brand = c("Brand1", "Brand2","Brand3"),
                         ratio = c (.5, .5, .5),
                         cost = c(2000, 3000, 4000),
                         stringsAsFactors = FALSE) %>%
  mutate(updated_price = cost * ratio)

shinyApp(
  ui = basicPage(
    mainPanel(
      
      actionButton("reset", "Reset"),
      tags$hr(),
      modFunctionUI("editable")
    )
  ),
  server = function(input, output) {
    demodata<-input_data
    callModule(modFunction,"editable", demodata,
               reset = reactive(input$reset))
    
  }
)