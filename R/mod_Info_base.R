#' Info_base UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @import plotly
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
          br(),
          tags$head(tags$style(".butt{background-color:#E9967A;} .butt{color: black;}")),
          uiOutput(ns("tableauBASE")),
          br(),
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
          
          tags$head(tags$style(".butt{background-color:#E9967A;} .butt{color: black;}")),br(),
          box(title="Descriptif cumulé des données manquantes par variable",width =12 ,status = "primary", solidHeader = TRUE,
       
          p("On représente ci-dessous les données manquantes en proportions par variable étudiée."),
          
          plotOutput(ns("plotNAbase2")),
          br(),
          
          tableOutput(ns("tableNAbase2")))
        ),
        tabPanel(
          "Données manquantes cumulées par Ligne",
          fluidRow(
            splitLayout(
              cellWidths = c("30%", "70%"),
              downloadButton(ns("PDFdescriptif1o3"), label = "AIDE et Détails", class = "butt")
            )
          ), # finFluidRow
          br(),
          tags$head(tags$style(".butt{background-color:#E9967A;} .butt{color: black;}")),
          h4("Descriptif cumulé des données manquantes par ligne", align = "center"),
          p("On représente ci-dessous les données manquantes en proportions par ligne."),
          plotOutput(ns("plotNAbase3")),br(),
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
      plot.na(r$BDD)+theme_ShiBA()
      
    })
    
    
    
    
    
    output$plotNAbase2 <- renderPlot({
      freq <-  apply(is.na(r$BDD), 2, sum)%>%as.data.frame()%>%tibble::rownames_to_column()
      names(freq)<- c("Var", "Nombre")
  p <-freq%>%ggplot(aes(x=Var, y=Nombre)) + geom_col()+theme_ShiBA()
  (p)  
  })
    
    output$plotNAbase3 <- renderPlot({
      freq <- apply(is.na(r$BDD), 1, sum)%>%as.data.frame()%>%tibble::rownames_to_column()
      names(freq)<- c("Sujet", "Nombre")
      
      d<- freq%>%ggplot(aes(x=Sujet, y=Nombre)) + geom_col()+theme_ShiBA()+
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
      (d)  
    })
    
    output$tableauBASE <- renderUI({
      res<- descd(r$BDD)
      
      tagList(
        box(title="Dimension de la base",status = "primary", solidHeader = TRUE,
            renderTable(res[[1]]%>%as.data.frame(),include.colnames=F, include.rownames=T)),
        box(title="Données manquantes",status = "primary", solidHeader = TRUE,
            renderTable(res[[2]]%>%as.data.frame(),include.colnames=F, include.rownames=T)),
        box(title="Types de variables",status = "primary", solidHeader = TRUE,
            renderTable(res[[3]]%>%as.data.frame(),include.colnames=F, include.rownames=T))
            
        
        
      )
      
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
