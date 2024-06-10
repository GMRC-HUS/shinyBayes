#' table1 UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_table1_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns("table1_ui"))
  )
}

#' table1 Server Functions
#'
#' @noRd
mod_table1_server <- function(id,r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    library(gtsummary)
    library(gt)
    
    table_1 <-
      
      fluidPage(
        titlePanel("Analyses descriptives croisées"),
        sidebarLayout(
          sidebarPanel(
            #
            uiOutput(ns("propositionsTableauCROISE")),
            uiOutput(ns("selectionVariablesCroisees1")),
            uiOutput(ns("selectionVariablesCroisees3")),
            uiOutput(ns("selectionVariablesCroisees2")),
            radioButtons(
              ns("tableauCroiseSimpli"), "Tableau avec abréviation :",
              c(Oui = 1, Non = 0), 0
            ),
            sliderInput(ns("nbDec"), "Nombre de décimales : ",
                        min = 0,
                        max = 5, value = 3, step = 1
            )
          ),
          mainPanel(
            
            
            # tags$head(tags$style(".butt{background-color:#E9967A;} .butt{color: black;}")),
            h3("Tableau de comparaison de population"),
            conditionalPanel(condition = "!is.null(input$VariableCroisees)", ns = ns, gt_output(ns("tableauCroisement"))),
            uiOutput(ns("legende"))
          ) # fin MainPanel
        ) # fin sidebarlayout
      )
    
    
    observe({
      output$table1_ui <- renderUI({
        if (!r$BASEchargee) {
          do.call(tabPanel, pasDeBase)
        } else {
          do.call(tabPanel, table_1)
        }
      })
      
      
      # ####################### Tableau de croisement ###########################
      #
      output$propositionsTableauCROISE <- renderUI({
        selectInput(ns("VariableCroisement"), "Variable de croisement:", choices = r$noms[r$nbModeVariable < 5])
      })
     
    })
    
    observe({
      if (sum(as.logical(r$variableNormale), na.rm = T) == 0) {
        listeVariableNormale <- ""
      } else {
        listeVariableNormale <- c(r$noms[which(r$nbModeVariable > 1 & r$variableNum & as.logical(r$variableNormale))], "")
      }
      r$listeVariableNormale <- listeVariableNormale
    })
    
    
    observe({
      if (sum(!as.logical(r$variableNormale), na.rm = T) == 0) {
        listeVariableNonNormale <- ""
      } else {
        listeVariableNonNormale <- c(r$noms[which(r$nbModeVariable > 1 & r$variableNum & !as.logical(r$variableNormale))], "")
      }
      r$listeVariableNonNormale <- listeVariableNonNormale
    })
    
    observe({
      output$selectionVariablesCroisees1 <- renderUI({
        selectInput(ns("VariableCroisees1"), "Variables Croisées Quantitatives (moyenne et écart-type)",
                    choices = list(
                      `Variables Normales` = r$listeVariableNormale,
                      `Variables non Normales` = r$listeVariableNonNormale
                    ),
                    selected = NULL,
                    multiple = TRUE
        )
      })
      
      output$selectionVariablesCroisees3 <- renderUI({
        selectInput(ns("VariableCroisees3"), "Variables Croisées Quantitatives (médiane, 1er et 3ème quartiles)",
                    choices = list(
                      `Variables non Normales` = r$listeVariableNonNormale,
                      `Variables Normales` = r$listeVariableNormale
                    ),
                    selected = NULL,
                    multiple = TRUE
        )
      })
      
      output$selectionVariablesCroisees2 <- renderUI({
        selectInput(ns("VariableCroisees2"), "Variables Croisées Qualitatives (pourcentage et effectif)",
                    choices = r$noms[r$nbModeVariable < 13 & r$nbModeVariable > 1],
                    selected = NULL,
                    multiple = TRUE
        )
      })
    })
    
    
    
    observe({
      base <- r$BDD
      if (is.null(input$VariableCroisees1) & is.null(input$VariableCroisees2) & is.null(input$VariableCroisees3)) {
      } else {
        
        
        # Création tableau de croisement des variables à distributions normales moyenne / écart-type
        if (!is.null(input$VariableCroisees1)) {
          tableauCroisement1 <- base %>%
            tbl_summary(by = input$VariableCroisement,
                        include = input$VariableCroisees1,
                        statistic = ~ "{mean} ({sd})",
                        digits = ~ input$nbDec,
                        missing_text = "Données manquantes"
            ) %>%
            bold_labels() %>%
            modify_footnote(everything() ~ NA)
          
          tableauCroisement1Sortie <- tableauCroisement1
          
        } else {
          tableauCroisement1Sortie <- NULL
        }
        
        
        # Création tableau de croisement de variables qualitatives
        if (!is.null(input$VariableCroisees2)) {
          tableauCroisement2 <- base %>%
            tbl_summary(by = input$VariableCroisement,
                        include = input$VariableCroisees2,
                        statistic = ~ "{p} ({n})",
                        digits = ~ c(input$nbDec, 0),
                        missing_text = "Données manquantes"
            ) %>%
            bold_labels() %>%
            modify_footnote(everything() ~ NA)
          tableauCroisement2Sortie <- tableauCroisement2
        } else {
          tableauCroisement2Sortie <- NULL
        }
        
        
        # Création tableau de croisement des variables à distributions normales mediane / 1er et 3eme quartiles
        if (!is.null(input$VariableCroisees3)) {
          tableauCroisement3 <- base %>%
            tbl_summary(by = input$VariableCroisement,
                        include = input$VariableCroisees3,
                        statistic = ~ "{median} ({p25} - {p75})",
                        digits = ~ input$nbDec,
                        missing_text = "Données manquantes"
            ) %>%
            bold_labels() %>%
            modify_footnote(everything() ~ NA)
          tableauCroisement3Sortie <- tableauCroisement3
        } else {
          tableauCroisement3Sortie <- NULL
        }
        
        
        tableauCroisementSortie <- list(tableauCroisement1Sortie, tableauCroisement3Sortie, tableauCroisement2Sortie) %>%
          purrr::discard(is.null) %>%
          tbl_stack() %>%
          as_gt()
        r$tableCroise <- tableauCroisementSortie
        
        
        
      }
      req(input$tableauCroiseSimpli)
      observe({
        if (input$tableauCroiseSimpli == 1) {
          theme_gtsummary_journal("jama")
        } else if (input$tableauCroiseSimpli == 0) {
          reset_gtsummary_theme()
          theme_gtsummary_language("fr")
        }
      })
      
    })
    
    observe({
      output$downloadData <- downloadHandler(
        filename = "Tableau Croisement.csv",
        content = function(file) {
          write.csv2(r$tableCroise, file)
        }
      )
     
    })
    
    output$tableauCroisement <- render_gt(
      {
        r$tableCroise
      }
    )
 
  })
}

## To be copied in the UI
# mod_table1_ui("table1_1")

## To be copied in the server
# mod_table1_server("table1_1")
