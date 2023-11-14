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
  uiOutput(ns("Croisement"))
}

#' Croisements Server Functions
#'
#' @noRd
mod_Croisements_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    pasDeBase <- pasDeBase_ui()
    
    
    Croisement <-
  
           
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


                  tags$head(tags$style(".butt{background-color:#E9967A;} .butt{color: black;}")),
               
                  box(title="Représentation graphique du lien entre les deux variables",width =6 ,status = "primary", solidHeader = TRUE,column(12,align="center", plotOutput(ns("plotCROISE")))),
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
          
    
    
    output$PDFcroisements <- downloadHandler(
      filename    = "3_Croisements.pdf",
      content     = function(file) file.copy("www/3_Croisements.pdf", file, overwrite = TRUE),
      contentType = "application/pdf"
    )

    observe({
      output$Croisement <- renderUI({
        if (!r$BASEchargee) {
          do.call(tabPanel, pasDeBase)
        } else {
          do.call(tabPanel, Croisement)
        }
      })



      output$propositionsCROISE1 <- renderUI({

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
          return(ggpoints(variableCROISE1, variableCROISE2, nomx = input$variableCROISE1, nomy = input$variableCROISE2)+theme_ShiBA())
        }
        if (input$qualiquantiCROISE1 == "quant" & input$qualiquantiCROISE2 == "qual") {
           return(base%>%ggplot(aes(y=!!sym(input$variableCROISE1) , group =  !!sym(input$variableCROISE2)))+geom_boxplot()+theme_ShiBA())
         
        }
        if (input$qualiquantiCROISE1 == "qual" & input$qualiquantiCROISE2 == "quant") {
          return(base%>%ggplot(aes(y=!!sym(input$variableCROISE2) , group =  !!sym(input$variableCROISE1)))+geom_boxplot()+theme_ShiBA())
   
        }
        if (input$qualiquantiCROISE1 == "qual" & input$qualiquantiCROISE2 == "qual") {
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
          return(ggcompar(input$variableCROISE1, input$variableCROISE2, base)+theme_ShiBA())
        }
        if (input$qualiquantiCROISE1 == "qual" & input$qualiquantiCROISE2 == "quant") {
          return(ggcompar(input$variableCROISE2, input$variableCROISE1, base)+theme_ShiBA())
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

    

    })
  })
}

## To be copied in the UI
# mod_Croisements_ui("Croisements_1")

## To be copied in the server
# mod_Croisements_server("Croisements_1")
