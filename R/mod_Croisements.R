#' Croisements UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @import xtable
#' @import dplyr

mod_Croisements_ui <- function(id) {
  ns <- NS(id)
  tagList()
  uiOutput(ns("CroisementsInference"))
}

#' Croisements Server Functions
#'
#' @noRd
mod_Croisements_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    pasDeBase <- fluidPage(
      h4("Aucune base n'a été chargée en mémoire, cet onglet n'est pas accessible."),
      p("Pour charger une base de données, rendez-vous sur l'onglet « Base de Données » dans la barre latérale.")
    )

    CroisementsInference <-
      fluidPage(
        navbarPage(
          id = "Panel 2.x", title = NULL,
          tabPanel(
            "Croisement 2 à 2",
            fluidPage( # includeCSS("./www/tables.css"),
              tags$head(
                tags$style(HTML("
.pure-table {
    /* Remove spacing between table cells (from Normalize.css) */
    border-collapse: collapse;
    border-spacing: 0;
    empty-cells: show;
    border: 1px solid #cbcbcb;
	text-align: center;
}

.pure-table caption {
    color: #000;
    font: italic 85%/1 arial, sans-serif;
    padding: 1em 0;
    text-align: center;
}

.pure-table td,
.pure-table th {
    border-left: 1px solid #cbcbcb;/*  inner column border */
    border-bottom: 1px solid #cbcbcb;

    font-size: inherit;
    margin: 0;
    overflow: visible; /*to make ths where the title is really long work*/
    padding: 0.5em 1em; /* cell padding */
	text-align: center;
}

.pure-table tr:hover {background-color: #f5f5f5}

/* Consider removing this next declaration block, as it causes problems when
there's a rowspan on the first cell. Case added to the tests. issue#432 */
.pure-table td:first-child,
.pure-table th:first-child {
    border-left-width: 0;
}

.pure-table thead {
    background-color: #e0e0e0;
    color: #000;
    text-align: left;
    vertical-align: bottom;
}

/*
striping:
   even - #fff (white)
   odd  - #f2f2f2 (light gray)
*/
.pure-table td {
    background-color: transparent;
}
"))
              ),
              titlePanel("Analyses descriptives croisées"),
              sidebarLayout(
                sidebarPanel(
                  uiOutput(ns("propositionsCROISE1")),
                  uiOutput(ns("ui_qualiquantiCROISE1")),
       
                  uiOutput(ns("propositionsCROISE2")),
                  uiOutput(ns("ui_qualiquantiCROISE2")),
 
                  conditionalPanel(
                    condition = "input.qualiquantiCROISE1 == 'qual' && input.qualiquantiCROISE2 == 'qual'",
                    radioButtons(
                      ns("NATableau"),
                      "Afficher les données manquante",
                      c(Non = "no", Oui = "always"),
                      "no"
                    )
                  )
                ),
                mainPanel(
                  fluidRow(
                    splitLayout(
                      cellWidths = c("30%", "70%"),
                      downloadButton(ns("PDFcroisements"), label = "AIDE et Détails", class = "butt")
                    )
                  ), # finFluidRow

                  tags$head(tags$style(".butt{background-color:#E9967A;} .butt{color: black;}")),
                  h3("Représentation graphique du lien entre les deux variables"),
                  plotOutput(ns("plotCROISE")),
                  # debut conditionnal panel QualiQuali
                  conditionalPanel(
                    condition = "input.qualiquantiCROISE1 == 'qual' && input.qualiquantiCROISE2 == 'qual'", ns = ns,
                    h3("Tableau croisé", align = "left", style = "color:#08088A"),
                    tableOutput(ns("montableauCroisAUTO")), br(),
                    tableOutput(ns("montableauCroise2AUTO")),
                    tableOutput(ns("montableauCroise3AUTO")),
     
                   
                    tableOutput(ns("oddratioAUTO"))
                  ), # fin panelQualiQuali,
                  # debut conditionnal panel QuantiQuali
                  conditionalPanel(
                    condition = "input.qualiquantiCROISE1 != input.qualiquantiCROISE2", ns = ns,
                    h3("Descriptif complet", align = "left", style = "color:#08088A"),
                    tableOutput(ns("descr3DESCRIPTIF")),
       
                 
                
                       ), # fin Panel Quali Quanti
                  # debut conditionnal panel QuantiQuanti
                  conditionalPanel(
                    condition = "input.qualiquantiCROISE1 == 'quant' && input.qualiquantiCROISE2 == 'quant'", ns = ns,
                    h3("Corrélation entre deux variables quantitatives", align = "left", style = "color:#08088A"),
                    htmlOutput(ns("CorrelationCROISE"))
                  ), # fin panelQuantiQuali,
                  plotOutput(ns("plotCROISE2"))
                ) # fin MainPanel
              ) # fin sidebarlayout
            )
          ) # fin fluidpage
          ,
          tabPanel(
            "Tableau croisement",
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
                  ),
                  downloadButton(ns("downloadData"), "Télécharger la table")
                ),
                mainPanel(
                  #   fluidRow(
                  #
                  #                 downloadButton('PDFcroisements',label="AIDE et Détails",class = "butt")
                  #
                  #
                  # ),#finFluidRow

                  # tags$head(tags$style(".butt{background-color:#E9967A;} .butt{color: black;}")),
                  h3("Tableau de comparaison de population"),
                  conditionalPanel(condition = "!is.null(input$VariableCroisees)", ns = ns, tableOutput(ns("tableauCroisement"))),
                  uiOutput(ns("legende"))
                ) # fin MainPanel
              ) # fin sidebarlayout
            ) # fin fluidpage
          ) # fin tabPanel tableau Croisement
        ) # fin tabset
      )

    # source("./CodeSansDependance.R", local = TRUE)
    # source("./fonctions.R", local = TRUE)
    # source("./miseEnForme.R", local = TRUE)
    # eval(parse("./miseEnForme.R", encoding="UTF-8"))

    output$PDFcroisements <- downloadHandler(
      filename    = "3_Croisements.pdf",
      content     = function(file) file.copy("www/3_Croisements.pdf", file, overwrite = TRUE),
      contentType = "application/pdf"
    )

    observe({
      output$CroisementsInference <- renderUI({
        if (!r$BASEchargee) {
          do.call(tabPanel, pasDeBase)
        } else {
          do.call(tabPanel, CroisementsInference)
        }
      })



      output$propositionsCROISE1 <- renderUI({
        print("##########################")
        print(r)
        print("##########################")
        selectInput(ns("variableCROISE1"), "Variable:", choices = r$noms)
      })

      output$propositionsCROISE2 <- renderUI({
        selectInput(ns("variableCROISE2"), "Variable:", choices = r$noms)
      })

      output$ui_qualiquantiCROISE1 <- renderUI({
        
        choice <- ifelse_perso(sum(is.na(as.numeric(as.character(r$BDD[, input$variableCROISE1])))) > sum(is.na(r$BDD[, input$variableCROISE])),
        "qual",
        "quant")
        if(length(unique(r$BDD[, input$variableCROISE1]))<5)   choice <-"qual"
        radioButtons(
          ns("qualiquantiCROISE1"),
          "Nature de la variable",
          c(Quantitative = "quant", Qualitative = "qual"),
          choice
        )
        
      })
      
      output$ui_qualiquantiCROISE2 <- renderUI({
        
        choice <- ifelse_perso(sum(is.na(as.numeric(as.character(r$BDD[, input$variableCROISE2])))) > sum(is.na(r$BDD[, input$variableCROISE2])),
                               "qual",
                               "quant")
        
        if(length(unique(r$BDD[, input$variableCROISE2]))<5)   choice <-"qual"
        radioButtons(
          ns("qualiquantiCROISE2"),
          "Nature de la variable",
          c(Quantitative = "quant", Qualitative = "qual"),
          choice
        )
        
        
      })


      output$plotCROISE <- renderPlot({
        base <- r$BDD
        variableCROISE1 <- base[, input$variableCROISE1]
        variableCROISE2 <- base[, input$variableCROISE2]
        if (input$qualiquantiCROISE1 == "quant" & input$qualiquantiCROISE2 == "quant") {
          print(ggpoints(variableCROISE1, variableCROISE2, nomx = input$variableCROISE1, nomy = input$variableCROISE2)+theme_ShiBA())
        }
        if (input$qualiquantiCROISE1 == "quant" & input$qualiquantiCROISE2 == "qual") {
           return(base%>%ggplot(aes(y=!!sym(input$variableCROISE1) , group =  !!sym(input$variableCROISE2)))+geom_boxplot()+theme_ShiBA())
         
        }
        if (input$qualiquantiCROISE1 == "qual" & input$qualiquantiCROISE2 == "quant") {
          return(base%>%ggplot(aes(y=!!sym(input$variableCROISE2) , group =  !!sym(input$variableCROISE1)))+geom_boxplot()+theme_ShiBA())
   
        }
        if (input$qualiquantiCROISE1 == "qual" & input$qualiquantiCROISE2 == "qual") {
          # print(ggpie(as.factor(variableCROISE1),as.factor(variableCROISE2)))
          barplotCroise <- barplot_croise(base = base, var1 = input$variableCROISE1, var2 = input$variableCROISE2)+theme_ShiBA()
          return(barplotCroise)
        }
      })
      output$plotCROISE2 <- renderPlot({
        base <- r$BDD
        variableCROISE1 <- base[, colnames(base) == input$variableCROISE1]
        variableCROISE2 <- base[, colnames(base) == input$variableCROISE2]
        if (input$qualiquantiCROISE1 == "quant" & input$qualiquantiCROISE2 == "quant") {
            }
        if (input$qualiquantiCROISE1 == "quant" & input$qualiquantiCROISE2 == "qual") {
          print(ggcompar(input$variableCROISE1, input$variableCROISE2, base)+theme_ShiBA())
        }
        if (input$qualiquantiCROISE1 == "qual" & input$qualiquantiCROISE2 == "quant") {
          print(ggcompar(input$variableCROISE2, input$variableCROISE1, base)+theme_ShiBA())
        }
      })




      output$montableauCroisAUTO <- renderText({
        base <- r$BDD
        x <- base[, input$variableCROISE1]
        y <- base[, input$variableCROISE2]
        Matrice <- addmargins(table(x, y, dnn = c(input$variableCROISE1, input$variableCROISE2), useNA = input$NATableau))
        colnames(Matrice) <- ifelse(is.na(colnames(Matrice)), "Données Manquantes", colnames(Matrice))
        rownames(Matrice) <- ifelse(is.na(rownames(Matrice)), "Données Manquantes", rownames(Matrice))
        var2 <- input$variableCROISE2
        var1 <- input$variableCROISE1
        entete <- paste("<tr><th></th><th colspan='", dim(Matrice)[2], "'>", var2, "</td></tr>
                  <tr> <th>", var1, " </th>", paste(unlist(lapply(unlist(dimnames(as.matrix(Matrice))[2]), function(x) paste("<th>", x, "</th>", sep = ""))), collapse = ""), "</tr>", sep = "")


        print.xtable(xtable(Matrice, caption = "Effectifs"),
          size = "footnotesize", # Change size; useful for bigger tables
          include.rownames = T,
          caption.placement = "top",
          hline.after = NULL,
          include.colnames = FALSE,
          add.to.row = list(
            pos = list(0),
            command = entete
          ), type = "html", html.table.attributes = 'class = "pure-table"  ',
          print.results = F
        )
      })

      output$montableauCroise2AUTO <- renderText({
        base <- r$BDD
        x <- base[, colnames(base) == input$variableCROISE1]
        y <- base[, colnames(base) == input$variableCROISE2]
        Matrice <- addmargins(100 * prop.table(addmargins(table(x, y, useNA = input$NATableau), 1), 1), 2)
        colnames(Matrice) <- ifelse(is.na(colnames(Matrice)), "Données Manquantes", colnames(Matrice))
        rownames(Matrice) <- ifelse(is.na(rownames(Matrice)), "Données Manquantes", rownames(Matrice))
        var2 <- input$variableCROISE2
        var1 <- input$variableCROISE1
        entete <- paste("<tr><th></th><th colspan='", dim(Matrice)[2], "'>", var2, "</td></tr>
                  <tr> <th>", var1, " </th>", paste(unlist(lapply(unlist(dimnames(as.matrix(Matrice))[2]), function(x) paste("<th>", x, "</th>", sep = ""))), collapse = ""), "</tr>", sep = "")


        print.xtable(xtable(Matrice, caption = "Pourcentages ligne", digits = 2),
          size = "footnotesize", # Change size; useful for bigger tables
          include.rownames = T,
          caption.placement = "top",
          hline.after = NULL,
          include.colnames = FALSE,
          add.to.row = list(
            pos = list(0),
            command = entete
          ), type = "html", html.table.attributes = 'class = "pure-table"  ',
          print.results = F
        )
      })

      output$montableauCroise3AUTO <- renderText({
        base <- r$BDD
        x <- base[, colnames(base) == input$variableCROISE1]
        y <- base[, colnames(base) == input$variableCROISE2]
        Matrice <- addmargins(100 * prop.table(addmargins(table(x, y, useNA = input$NATableau), 2), 2), 1)
        colnames(Matrice) <- ifelse(is.na(colnames(Matrice)), "Données Manquantes", colnames(Matrice))
        rownames(Matrice) <- ifelse(is.na(rownames(Matrice)), "Données Manquantes", rownames(Matrice))
        var2 <- input$variableCROISE2
        var1 <- input$variableCROISE1
        entete <- paste("<tr><th></th><th colspan='", dim(Matrice)[2], "'>", var2, "</td></tr>
                  <tr> <th>", var1, " </th>", paste(unlist(lapply(unlist(dimnames(as.matrix(Matrice))[2]), function(x) paste("<th>", x, "</th>", sep = ""))), collapse = ""), "</tr>", sep = "")


        print.xtable(xtable(Matrice, caption = "Pourcentages colonne", digits = 2),
          size = "footnotesize", # Change size; useful for bigger tables
          include.rownames = T,
          caption.placement = "top",
          hline.after = NULL,
          include.colnames = FALSE,
          add.to.row = list(
            pos = list(0),
            command = entete
          ), type = "html", html.table.attributes = 'class = "pure-table"  ',
          print.results = F
        )
      })



   




      output$oddratioAUTO <- renderTable(
        {
          base <- r$BDD
          x <- base[, colnames(base) == input$variableCROISE1]
          y <- base[, colnames(base) == input$variableCROISE2]
          Mat <- table(x, y)

          Nblignes <- dim(Mat)[1]
          Nbcolonnes <- dim(Mat)[2]
          if (Nblignes > 2 | Nbcolonnes > 2) {
            OR <- NULL
          } else {
            FI2 <- fisher.test(Mat)
            OR <- data.frame("Rapport de cotes" = FI2$estimate)
            
            rownames(OR) <- "Résultat"
          }
          OR
        },
        caption = "Rapport de cotes et IC",
        caption.placement = getOption("xtable.caption.placement", "bottom"),
        caption.width = getOption("xtable.caption.width", NULL),
        rownames = TRUE
      )


      output$descr3DESCRIPTIF <- renderTable(
        {
          base <- r$BDD
          variableCROISE1 <- base[, colnames(base) == input$variableCROISE1]
          variableCROISE2 <- base[, colnames(base) == input$variableCROISE2]
          if (input$qualiquantiCROISE1 == "quant" & input$qualiquantiCROISE2 == "qual") {
            res <- descr3(variableCROISE1, variableCROISE2, nom = input$variableCROISE2, nomY = input$variableCROISE1)
          }
          if (input$qualiquantiCROISE1 == "qual" & input$qualiquantiCROISE2 == "quant") {
            res <- descr3(variableCROISE2, variableCROISE1, nom = input$variableCROISE1, nomY = input$variableCROISE2)
          }
          res
        },
        caption = "Descriptif global et par modalité",
        caption.placement = getOption("xtable.caption.placement", "bottom"),
        caption.width = getOption("xtable.caption.width", NULL),
        rownames = TRUE
      )



      output$CorrelationCROISE <- renderUI({
        base <- r$BDD
        variableCROISE1 <- base[, colnames(base) == input$variableCROISE1]
        variableCROISE2 <- base[, colnames(base) == input$variableCROISE2]
        x <- variableCROISE1
        y <- variableCROISE2
        resultatCorrelation <-
          paste0(
            "Coefficient de corrélation de Pearson : ",
            round(cor.test(x, y, method = "pearson")$estimate, 3),".<br/>",
            
            "Coefficient de corrélation de Spearman : ",
            round(cor.test(x, y, method = "s")$estimate, 3),"."
          
                 )
        HTML(resultatCorrelation)
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
  })
}

## To be copied in the UI
# mod_Croisements_ui("Croisements_1")

## To be copied in the server
# mod_Croisements_server("Croisements_1")
