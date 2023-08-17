#' Descriptifs UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @import ggplot2
#' @import xtable
#' @import ggthemes


mod_Descriptifs_ui <- function(id) {
  ns <- NS(id)
  tagList(

  uiOutput(ns('univarie'))
  )
}

#' Descriptifs Server Functions
#'
#' @noRd
#'
mod_Descriptifs_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    
    univarie <- fluidPage(
      
        
        
        
        
        ###################################################
        #####   PAGE 3.2     ###############################
        ###################################################
    
   
            titlePanel("Analyses descriptives"),
            sidebarLayout(
              sidebarPanel(width = 3,
                uiOutput(ns("propositions")),
                uiOutput(ns("choix_quanti_quali_ui"))
              
              ),
        
              mainPanel(

                
                tags$head(tags$style(".butt{background-color:#E9967A;} .butt{color: black;}")),
                fluidRow(
                  column(6,align="center", uiOutput(ns("descriptifUni")), br(), tableOutput(ns("descvar"))%>% withSpinner()),
                  column(6,align="center", plotOutput(ns("plot1"))%>% withSpinner(), plotOutput(ns("plot2"))%>% withSpinner())
                ) # fin fluid row du main panel
              ) # fin MainPanel
            ) # fin sidebarlayout
          ) # fin fluidpage
       
    
   

    pasDeBase <- pasDeBase_ui()




    observe({
      output$univarie <- renderUI({
        if (!r$BASEchargee) {
          do.call(tabPanel, pasDeBase)
        } else {
          do.call(tabPanel, univarie)
        }
      })
    })


  
   

  
      ########################################################################################################################
      ####    OUTPUT page 3 : Descriptifs univaries
      ########################################################################################################################


      output$propositions <- renderUI({
        selectInput(ns("variable"), "Variable:", choices = r$noms)
      })
    
    output$choix_quanti_quali_ui<- renderUI({
      print(input$variable)
      choix_quali_quanti(ns("qualiquanti"), r$BDD[,input$variable])
    })
    
    

      output$descriptifUni <- renderUI(HTML(paste0("<h2>Descriptif de la variable ", bold(input$variable), "</h2>")))

      output$descvar <- renderTable(
        {
          base <- r$BDD
          variable <- base[, colnames(base) == input$variable]
          print(input$variable)
          if (input$qualiquanti == "quant") {
            res <- data.frame(descr1(variable)$Descriptif[1:18,])
            colnames(res) <- c("Descriptif")
          }
          if (input$qualiquanti == "qual") {
            res <- data.frame(desql(variable))
            colnames(res) <- c("Effectifs", "Proportions")
          }
          xtable(res, "essai")
        },
        hover = T,
        rownames = TRUE
      )

      output$plot1 <- renderPlot({
        base <- r$BDD
        variable <- base[, input$variable]
        if (input$qualiquanti == "quant") {
  
          return(ggplot(base, aes(x=!!sym(input$variable)))+geom_histogram(fill="#75AADB", color="white")+
            xlab(input$variable)+ylab("Effectif")+ggtitle("Histogramme")+theme_ShiBA())
          # g<-; print(g)
        }
        if (input$qualiquanti == "qual") {
          variable <- as.character(variable)
          return(diagrammeBarre(variable)+theme_ShiBA())
        }
      })

      output$plot2 <- renderPlot({
        base <- r$BDD
        variable <- base[, colnames(base) == input$variable]

        if (input$qualiquanti == "quant") {
          g <- ggplot(base, aes(y = !!sym(input$variable),x=factor(0))) +
            geom_boxplot(width = 0.2) +
            theme_ShiBA()+
            theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank())
          return(g)
        }
        # if(input$qualiquanti=="qual"){print(graphics::pie(as.vector(table(variable))))}
        if (input$qualiquanti == "qual") {

          if(length(unique(variable))>3) return()
          print(as.data.frame(table(variable)))
          g <- ggplot(as.data.frame(table(variable)), aes(x = "", y = Freq, fill = variable)) +
            geom_bar(stat = "identity", width = 1) +
            coord_polar("y", start = 0) +
            theme_void() +
            ggtitle("Diagramme circulaire") +
            geom_text(aes(label = variable),
              position = position_stack(vjust = 0.5)
            ) +
            theme(legend.position = "none")
          return(g)
        }
      })

      #
      # quali
      #
    #})
  })
}

## To be copied in the UI
# mod_Descriptifs_ui("Descriptifs_1")

## To be copied in the server
# mod_Descriptifs_server("Descriptifs_1")
