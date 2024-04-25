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
#' 
#' 




mod_chargement_ui <- function(id) {
  ns <- NS(id)
  set_i18n("fr")
  tagList(
    h1("Hello chargement !")
  )
  fluidPage(
    # Try with different Bootstrap version
    # theme = bslib::bs_theme(version = 4),
  
    import_ui2(ns("import"),id,from =c( "file", "copypaste", "googlesheets", "url"))
    
    
  )
}

#' chargement Server Functions
#'
#' @noRd
mod_chargement_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

observe({print(input$label_nas)})
    imported <- import_server2("import",
                              return_class = "data.frame"
                              )
    
    output$name <- renderPrint({
      req(imported$name())
      imported$name()
    })
    
    output$data <- renderPrint({
      req(imported$data())
      imported$data()
    })


    observeEvent(imported$data(), {
  
      DD <- imported$data()
      lignesVides <- apply(DD, 1, function(x) {
        sum(is.na(x))
      }) == dim(DD)[2]
      DD <- DD[!lignesVides, ]
      r$BDD <- DD%>%as.data.frame()
      print(str(DD))
      r$contentInput <- DD
    })


    observe({
      # r$BASEchargee<-tryCatch(dim(r$BDD)[1]>0,warning= function(e) F,error= function(e) F)
      r$BASEchargee <- !is.null( r$BDD )
    })
    

   
    # })

    observeEvent(r$BDD, ignoreInit = T, {
      r$noms <- colnames(r$BDD)
      r$nbSujet <- dim(r$BDD)[1]
      D <- r$BDD
      Y <- as.data.frame(lapply(D, factor))
      z <- as.numeric(lapply(Y, nlevels))
      r$nbModeVariable <- z
    })

    observeEvent(r$BDD, ignoreInit = T, {
      D <- r$BDD
      r$variableNum <- as.logical(lapply(D, is.numeric))
    })
    

    observeEvent(r$BDD, ignoreInit = T, {
      D <- r$BDD
      ret <- rep(NA, ncol(D))
      ret[which(r$variableNum)] <- as.logical(lapply(as.data.frame(D[, which(r$variableNum)]), desctable::is.normal))
      r$variableNormale <- ret
    })

  })
  
  
  observeEvent(r$BDD,{
    showNotification(HTML("<font color='grey' > <center> Données importées avec succès </center> </font>"))})
}

## To be copied in the UI
# mod_chargement_ui("chargement_1")

## To be copied in the server
# mod_chargement_server("chargement_1")
