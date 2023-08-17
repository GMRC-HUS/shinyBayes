#' Info_base UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_Info_base_ui <- function(id){
  ns <- NS(id)
  tagList(
    
    uiOutput(ns('info_base'))
  )
}
    
#' Info_base Server Functions
#'
#' @noRd 
mod_Info_base_server <- function(id,r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    
    info_base <-      fluidPage(
      titlePanel("Informations sur la base de données"),
      navlistPanel(
        "Menu",
        tabPanel(
          "Informations brutes",
          fluidRow(
            splitLayout(
              cellWidths = c("30%", "70%"),
              downloadButton(ns("PDFdescriptif1o1"), label = "AIDE et Détails", class = "butt")
            )
          ), # finFluidRow
          tags$head(tags$style(".butt{background-color:#E9967A;} .butt{color: black;}")),
          verbatimTextOutput(ns("tableauBASE")),
          plotOutput(ns("plotNAbase1"))
        ),
        tabPanel(
          "Données manquantes cumulées par variable",
          fluidRow(
            splitLayout(
              cellWidths = c("30%", "70%"),
              downloadButton(ns("PDFdescriptif1o2"), label = "AIDE et Détails", class = "butt")
            )
          ), # finFluidRow
          
          tags$head(tags$style(".butt{background-color:#E9967A;} .butt{color: black;}")),
          h4("Descriptif cumulé des données manquantes par variable", align = "center"),
          p("On représente ci-dessous les données manquantes en proportions par variable étudiée."),
          plotOutput(ns("plotNAbase2")),
          tableOutput(ns("tableNAbase2"))
        ),
        tabPanel(
          "Données manquantes cumulées par sujet",
          fluidRow(
            splitLayout(
              cellWidths = c("30%", "70%"),
              downloadButton(ns("PDFdescriptif1o3"), label = "AIDE et Détails", class = "butt")
            )
          ), # finFluidRow
          
          tags$head(tags$style(".butt{background-color:#E9967A;} .butt{color: black;}")),
          h4("Descriptif cumulé des données manquantes par sujet", align = "center"),
          p("On représente ci-dessous les données manquantes en proportions par sujet d'étude."),
          plotOutput(ns("plotNAbase3")),
          tableOutput(ns("tableNAbase3"))
        )
      ) # fin navlistpanel
    )
    
    pasDeBase <- pasDeBase_ui()
    
    
    
    
    observe({
      output$info_base <- renderUI({
        if (!r$BASEchargee) {
          do.call(tabPanel, pasDeBase)
        } else {
          do.call(tabPanel, info_base)
        }
      })
    })
    
    
    output$plotNAbase1 <- renderPlot({
      plot.na(r$BDD)
      # DataExplorer::plot_missing(r$BDD)+theme_clean()
    })
    
    
    
    
    
    output$plotNAbase2 <- renderPlot({
      barplot(apply(is.na(r$BDD), 2, sum), xlab = "", col = "palegreen3")
    })
    
    output$plotNAbase3 <- renderPlot({
      barplot(apply(is.na(r$BDD), 1, sum), xlab = "", col = "lightblue1")
    })
    
    output$tableauBASE <- renderPrint({
      descd(r$BDD)
    })
    
    output$tableNAbase2 <- renderTable(
      {
        D <- r$BDD
        NbVariables <- dim(D)[2]
        matriceNA <- matrix(NA, nrow = NbVariables, ncol = 3)
        for (i in 1:NbVariables) {
          matriceNA[i, 1] <- round(sum(is.na(D[, i])))
          matriceNA[i, 2] <- round(length(is.na(D[, i])))
          matriceNA[i, 3] <- round(sum(100 * is.na(D[, i])) / length(is.na(D[, i])), 2)
        }
        colnames(matriceNA) <- c("Nb.manquants", "Nb.données", "%")
        rownames(matriceNA) <- colnames(D)
        matriceNA
      },
      rownames = TRUE
    )
    
    output$tableNAbase3 <- renderTable(
      {
        D <- t(r$BDD)
        NbVariables <- dim(D)[2]
        matriceNA <- matrix(NA, nrow = NbVariables, ncol = 3)
        for (i in 1:NbVariables) {
          matriceNA[i, 1] <- round(sum(is.na(D[, i])))
          matriceNA[i, 2] <- round(length(is.na(D[, i])))
          matriceNA[i, 3] <- round(sum(100 * is.na(D[, i])) / length(is.na(D[, i])), 2)
        }
        colnames(matriceNA) <- c("Nb.manquants", "Nb.données", "%")
        rownames(matriceNA) <- colnames(D)
        matriceNA
      },
      rownames = TRUE
    )
    
 
  })
}
    
## To be copied in the UI
# mod_Info_base_ui("Info_base_1")
    
## To be copied in the server
# mod_Info_base_server("Info_base_1")
