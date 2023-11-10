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
 
table_1<- 

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
                  sliderInput(ns("nbDec"), "Nombre de decimales : ",
                    min = 0,
                    max = 5, value = 3, step = 1
                  )
                      ),
                mainPanel(


                  # tags$head(tags$style(".butt{background-color:#E9967A;} .butt{color: black;}")),
                  h3("Tableau de comparaison de population"),
                  conditionalPanel(condition = "!is.null(input$VariableCroisees)", ns = ns, tableOutput(ns("tableauCroisement"))),
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


        # Création tableau de croisement des variables à distributions normales
        if (!is.null(input$VariableCroisees1)) {
          tableauCroisement1 <- cbind(base %>% select(input$VariableCroisees1))
          names(tableauCroisement1) <- paste(input$VariableCroisees1, "(moyenne (sd))")
          names(tableauCroisement1) <- paste(names(tableauCroisement1), "(Na=", apply(tableauCroisement1, 2, function(x) sum(is.na(x))), ")", sep = "")

          tableauCroisement1 <- tableauCroisement1 %>%
            group_by(as.factor(base[, input$VariableCroisement])) %>%
            desctable::desctable(stats = list(
              "mean_p" = is.factor ~ percent | mean,
              "sd" = is.factor ~ length | sd
            ), tests = tests_autoGMRC) %>%
            as.data.frame()
          colonne <- (dim(tableauCroisement1)[2] - 3) / 2
          tableauCroisement1Sortie <- tableauCroisement1[, c(1, (c(1:colonne) * 2), dim(tableauCroisement1)[2] - 1, dim(tableauCroisement1)[2])]
          for (i in 1:colonne) {
            tableauCroisement1Sortie[, (1 + i)] <- paste(round(tableauCroisement1[, (i * 2)], input$nbDec), " (", round(tableauCroisement1[, (i * 2 + 1)], input$nbDec), ")", sep = "")
          }

          names(tableauCroisement1Sortie)[1] <- "Variables"
        } else {
          tableauCroisement1Sortie <- NULL
        }



        if (!is.null(input$VariableCroisees2)) {
          tableauCroisement2 <- cbind(base %>% select(input$VariableCroisees2) %>% mutate_all(factor))
          # names(tableauCroisement2) <-paste( input$VariableCroisees2, "(pourcentage(nombre))")
          names(tableauCroisement2) <- paste(names(tableauCroisement2), "\n (Na=", apply(tableauCroisement2, 2, function(x) sum(is.na(x))), ")", sep = "")

          tableauCroisement2 <- tableauCroisement2 %>%
            group_by(as.factor(base[, input$VariableCroisement])) %>%
            desctable::desctable(stats = list(
              "mean_p" = is.factor ~ percent | mean,
              "sd" = is.factor ~ length | sd
            ), tests = tests_autoGMRC) %>%
            as.data.frame()
          colonne <- (dim(tableauCroisement2)[2] - 3) / 2
          tableauCroisement2Sortie <- tableauCroisement2[, c(1, (c(1:colonne) * 2), dim(tableauCroisement2)[2] - 1, dim(tableauCroisement2)[2])]
          for (i in 1:colonne) {
            tableauCroisement2Sortie[, (1 + i)] <- paste(round(tableauCroisement2[, (i * 2)], input$nbDec), "% (", round(tableauCroisement2[, (i * 2 + 1)], input$nbDec), ")")
          }
        
          tableauCroisement2Sortie[, 1] <- gsub(" Groupe", "", tableauCroisement2Sortie[, 1])
          tableauCroisement2Sortie[, 1] <- paste(gsub(".*:", "Groupe :", tableauCroisement2Sortie[, 1]), "(pourcentage(effectif))")
          tableauCroisement2Sortie[, 1] <- gsub(") .*", ")(effectif)", tableauCroisement2Sortie[, 1])
          for (i in 2:(dim(tableauCroisement2Sortie)[2] - 2)) {
            tableauCroisement2Sortie[, i] <- gsub("NA %", "", tableauCroisement2Sortie[, i])
          }
          names(tableauCroisement2Sortie)[1] <- "Variables"
        } else {
          tableauCroisement2Sortie <- NULL
        }


        # Création tableau de croisement des variables à distributions normales mediane / 1er et 3eme quartiles
        if (!is.null(input$VariableCroisees3)) {
          tableauCroisement3 <- cbind(base %>% select(input$VariableCroisees3))
          names(tableauCroisement3) <- paste(input$VariableCroisees3, "(mediane(1er-3ème quartiles))")
          names(tableauCroisement3) <- paste(names(tableauCroisement3), "\n (Na=", apply(tableauCroisement3, 2, function(x) sum(is.na(x))), ")", sep = "")
          tableauCroisement3 <- tableauCroisement3 %>%
            group_by(as.factor(base[, input$VariableCroisement])) %>%
            desctable::desctable(stats = list(
              "mean_p" = median,
              "Q1" = function(x) round(quantile(x, 0.25), input$nbDec), "Q3" = function(x) round(quantile(x, 0.75), input$nbDec)
            ), tests = tests_autoGMRC) %>%
            as.data.frame()
          colonne <- (dim(tableauCroisement3)[2] - 3) / 3
          tableauCroisement3Sortie <- tableauCroisement3[, c(1, (c(1:colonne) * 3 - 1), dim(tableauCroisement3)[2] - 1, dim(tableauCroisement3)[2])]
          for (i in 1:colonne) {
            tableauCroisement3Sortie[, (1 + i)] <- paste(round(tableauCroisement3[, (i * 3) - 1], input$nbDec), "
                                                  (", tableauCroisement3[, (i * 3)], "-", tableauCroisement3[, (i * 3) + 1], ")", sep = "")
          }

          names(tableauCroisement3Sortie)[1] <- "Variables"
        } else {
          tableauCroisement3Sortie <- NULL
        }


        tableauCroisementSortie <- rbind(tableauCroisement1Sortie, tableauCroisement3Sortie, tableauCroisement2Sortie)

        nomColonne <- names(tableauCroisementSortie)

        nomColonne <- gsub("tests /", "", nomColonne)
        nomColonne <- gsub("/ ", "", nomColonne)
        nomColonne <- gsub("\\_p.*", "", nomColonne)
        nomColonne <- gsub("sd.*", "", nomColonne)
        nomColonne <- gsub("mean", "", nomColonne)
        nomColonne <- gsub(".*:", paste(input$VariableCroisement, ":"), nomColonne)
        # nomColonne<-gsub("\\base","",nomColonne)
        names(tableauCroisementSortie) <- nomColonne
        

    
        colp <- dim(tableauCroisementSortie)[2] - 1
        tableauCroisementSortie[, colp] <- ifelse(tableauCroisementSortie[, colp] < 1 / 10^(input$nbDec), paste("<", as.character(1 / 10^(input$nbDec)), sep = ""), round(tableauCroisementSortie[, colp], input$nbDec))
        tableauCroisementSortie[, colp + 1] <- gsub("%>%", "", tableauCroisementSortie[, colp + 1])
        if (input$tableauCroiseSimpli == 1) {
          tableauCroisementSortie[, 1] <- gsub("\\(moyenne \\(sd\\)\\)", "*", tableauCroisementSortie[, 1])
          tableauCroisementSortie[, 1] <- gsub("\\(mediane\\(1er-3ème quartiles\\)\\)", "°", tableauCroisementSortie[, 1])
          tableauCroisementSortie[, 1] <- gsub("\\(pourcentage\\(effectif\\)\\)", "§", tableauCroisementSortie[, 1])
          tableauCroisementSortie[, 1] <- gsub("\\(effectif\\)", "§", tableauCroisementSortie[, 1])
        }
        tableauCroisementSortie<-tableauCroisementSortie[,-c(colp: dim(tableauCroisementSortie)[2])]
        r$tableCroise <- tableauCroisementSortie
      }
    })

    observe({
      output$downloadData <- downloadHandler(
        filename = "Tableau Croisement.csv",
        content = function(file) {
          write.csv2(r$tableCroise, file)
        }
      )

   
    
    })
    
    output$tableauCroisement <- renderTable(
      {
        r$tableCroise
      },
      na = "",
      digits = 3
    )
    
    output$legende <- renderUI({
      if (input$tableauCroiseSimpli == 1) {
        affichage <- HTML("* moyenne (sd)
          <br/>° mediane (1er-3ème quartile)
          <br/>§ pourcentage (effectif)")
        affichage
      }
    })
    
    
    
    
  })
}
    
## To be copied in the UI
# mod_table1_ui("table1_1")
    
## To be copied in the server
# mod_table1_server("table1_1")
