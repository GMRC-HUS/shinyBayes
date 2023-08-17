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
mod_chargement_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h1("Hello chargement !")
  )
  fluidPage(
    sidebarLayout(
      sidebarPanel(
        fileInput(ns("file1"), "Choose CSV File",
          accept = c(
            "text/csv",
            "text/comma-separated-values,text/plain",
            ".csv"
          )
        ),
        tags$hr(),
        checkboxInput(ns("header"), "Titre       (Votre fichier contient-il des titres de colonnes ?)", TRUE),
        radioButtons(
          ns("sep"), "Séparateur",
          c(
            Virgule = ",",
            `Point Virgule` = ";",
            Tabulation = "\t"
          ),
          ";"
        ),
        radioButtons(
          ns("manquants"), "Manquants (Quel symbole est utilisé pour les données manquantes ?)",
          c(
            Slash = "/",
            Etoile = "*",
            `Lettres NA` = "NA"
          ),
          "*"
        ),
        radioButtons(
          ns("decimale"), "Symbole de décimale",
          c(
            Virgule = ",",
            Point = "."
          ),
          ","
        ),
        tags$br(),
        # actionButton(ns("upload"), "Charger/actualiser la base",class = "btn-success") ,
        radioButtons(
          ns("encodage"), "Si vous avez des problèmes d'import de la base de données, potentiellement liés à l'encodage",
          c(
            `Windows/Excel` = "windows-1252",
            `Linux/LibreOffice` = "utf-8"
          ),
          "windows-1252"
        ),
        tags$br(), tags$br(), tags$br(),
        "Les lignes entièrement vides seront retirées pour la suite des analyses."
      ),
      mainPanel(
        tableOutput(ns("contents"))
      )
    )
  )
}

#' chargement Server Functions
#'
#' @noRd
mod_chargement_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns


    output$contents <- renderTable({

     

      if (is.null( r$BDD)) {
        return(NULL)
      }

      DD <-  r$BDD 
        lignesVides <- apply(DD, 1, function(x) {
        sum(is.na(x))
      }) == dim(DD)[2]
      DD <- DD[!lignesVides, ]
      DD
    })

 


    observeEvent(input$file1, ignoreInit = T, {
      inFile <- input$file1
      DD <- read.csv(inFile$datapath, header = input$header, sep = input$sep, na.string = c("", input$manquants), dec = input$decimale)
      lignesVides <- apply(DD, 1, function(x) {
        sum(is.na(x))
      }) == dim(DD)[2]
      DD <- DD[!lignesVides, ]
      r$BDD <- DD
      r$contentInput <- DD
    })


    observe({
      # r$BASEchargee<-tryCatch(dim(r$BDD)[1]>0,warning= function(e) F,error= function(e) F)
      r$BASEchargee <- !is.null( r$BDD )
    })

   
    # })

    observeEvent(r$BDD, ignoreInit = T, {
      r$noms <- colnames(r$BDD)
      r$nbSujet <- dim(r$contentInput)[1]
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
}

## To be copied in the UI
# mod_chargement_ui("chargement_1")

## To be copied in the server
# mod_chargement_server("chargement_1")
