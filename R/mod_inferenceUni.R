#' inferenceUni UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @import gmrc.bayes  

mod_inferenceUni_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidPage(    
      titlePanel("Inférence univarié"),
      sidebarLayout( 
        sidebarPanel(                              
          uiOutput(ns("propositions")),
          radioButtons(ns('qualiquanti'), "Nature de la variable",
                       c(Quantitative='quant', Qualitative='qual'),'quant'
          ),
          uiOutput(ns("apriori"))
        ),
        
        mainPanel( 
          
          tags$head(tags$style(".butt{background-color:#E9967A;} .butt{color: black;}")),
          fluidRow(
            h2(textOutput(ns("nameVariable"))),
            # column(6,
                   renderText(ns("inferenceUni")),br()#,  #tableOutput(ns("descvar"))
            #                                                            ),
            ,
            # # column(6,
                   plotOutput(ns('plotinferenceUni'))
                   # )
          )# fin fluid row du main panel 
          
        )# fin MainPanel
        
      )# fin sidebarlayout
    )# fin fluidpage
    
    
  )
}
    
#' inferenceUni Server Functions
#'
#' @noRd 
mod_inferenceUni_server <- function(id,r){
  moduleServer( id, function(input, output, session){
    
    ns <- session$ns


      
      ########################################################################################################################
      ####    OUTPUT : Inférence univarie          
      ########################################################################################################################
      
      
      output$propositions <- renderUI({
        num<-r$noms[which(r$variableNum)]
        non_num<-r$noms[which(!r$variableNum)]
        
        if(length(num)>0 & length(non_num)>0){
          liste_choix<-list(numeric =num, 
                            autre = non_num
          )
        }else if(length(num)>0 & !length(non_num)>0){
          liste_choix<-list(numeric =num
          )
        }else{
          liste_choix<-list(
                            autre = non_num
          )
          }
        
        selectInput(ns("variable"), "Variable:",   choices=
                      liste_choix)
      })
      
      output$apriori <- renderUI({
        x<- BDD[,input$variable]
        x <- x[!is.na(x)]
        min_x<- min(x)
        max_x <- max(x)
        fluidRow(
          html2("Apriori sur la moyenne : "),
          sliderInput(ns("mu0"), "μ0, moyenne à priori :",   
                      min = min_x-max_x, max = max_x*2, value = mean(x)),
          br(),
          sliderInput(ns("k0"),"κ0, taille pseudo-Echantillon",
                      min = 0, max = length(x), value =1)
        )
      })
      
      fitInference <- reactive(
        oneMeanEstim(r$BDD[,input$variable],
                     alpha = 0.15,mu_0 = input$mu0 ,kappa_0 = input$k0,
                     alpha_0 = 0.005,beta_0 = 0.01,seuil = 10,theta_P = c(-10,5),theta_A = c(5,12))
        )
      output$nameVariable <- renderText(input$variable)
      output$inferenceUni <- renderPrint(pander(fitInference()))
      output$plotinferenceUni <- renderPlot({
        plot( fitInference())
      })
      
      # output$descvar <- renderTable({
      #   base    <-r$BDD
      #   variable<-base[,colnames(base)==input$variable]
      #   print(input$variable)
      #   if(input$qualiquanti=="quant"){res<-data.frame(descr1(variable)$Descriptif)
      #   colnames(res) <- c("Descriptif")}
      #   if(input$qualiquanti=="qual") {res<-data.frame(desql(variable))
      #   colnames(res) <- c("Effectifs", "Proportions")}
      #   xtable(res, "essai")
      # },hover = T,rownames=TRUE)
      
    
      
   
      
      
      
    
  })
}
    
## To be copied in the UI
# mod_inferenceUni_ui("inferenceUni_1")
    
## To be copied in the server
# mod_inferenceUni_server("inferenceUni_1")
