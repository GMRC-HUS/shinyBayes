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
                fluidRow( uiOutput(ns("descriptifUni")),
                          box(title="Tableau descriptif",width =6 ,status = "primary", solidHeader = TRUE,column(12,align="center", br(), tableOutput(ns("descvar"))%>% withSpinner())),
                          box(title="Graphiques",width =6 ,status = "primary", solidHeader = TRUE,column(12,align="center", plotOutput(ns("plot1"))%>% withSpinner(), plotOutput(ns("plot2"))%>% withSpinner()))
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

    debounced <- debounce(c(input$variable,input$qualiquanti), 1000)
      output$propositions <- renderUI({
        selectInput(ns("variable"), "Variable:", choices = r$noms)
      })
    
    output$choix_quanti_quali_ui<- renderUI({
      
      choix_quali_quanti(ns("qualiquanti"), r$BDD[,input$variable])
      

    
    })


      output$descriptifUni <- renderUI(HTML(paste0("<h2>Descriptif de la variable ", bold(input$variable), "</h2>")))

      output$descvar <- renderTable(
       
        { debounced()
          base <- r$BDD
          variable <- base[, colnames(base) == isolate(input$variable)]
      quali_qaunti<- isolate(input$qualiquanti)
          if (quali_qaunti == "quant") {
            res <- data.frame(descr1(variable)$Descriptif[1:18,])
            colnames(res) <- c("Descriptif")
          }
          if (quali_qaunti == "qual") {
            res <- data.frame(desql(variable))
            colnames(res) <- c("Effectifs", "Proportions")
          }
          xtable(res, "essai")
        },
        hover = T,
        rownames = TRUE
      )

      output$plot1 <- renderPlot({
        debounced()
        base <- r$BDD
        variable <- base[, isolate(input$variable)]
        name_var  = isolate(input$variable)
        quali_qaunti<- isolate(input$qualiquanti)
        
        if (quali_qaunti == "quant") {
  
          return(ggplot(base, aes(x=!!sym(name_var)))+geom_histogram(fill="#75AADB", color="white")+
            xlab(name_var)+ylab("Effectif")+ggtitle("Histogramme")+theme_ShiBA())
         
        }
        if (quali_qaunti == "qual") {
          variable <- as.character(variable)
          return(diagrammeBarre(variable)+theme_ShiBA())
        }
      })

      output$plot2 <- renderPlot({
        debounced()
        base <- r$BDD
        name_var  = isolate(input$variable)
        variable <- base[, colnames(base) == name_var]
        quali_qaunti<- isolate(input$qualiquanti)

        if (quali_qaunti == "quant") {
          g <- ggplot(base, aes(y = !!sym(name_var),x=factor(0))) +
            geom_boxplot(width = 0.2) +
            theme_ShiBA()+
            theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank())
          return(g)
        }
          if (quali_qaunti == "qual") {

          if(length(unique(variable))>3) return()
         
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
