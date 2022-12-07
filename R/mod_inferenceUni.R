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
        sidebarPanel(width=4,                       
          uiOutput(ns("propositions")),
          radioButtons(ns('qualiquanti'), "Nature de la variable",
                       c(Quantitative='quant', Qualitative='qual'),'quant'
          ),
          uiOutput(ns("apriori")),
          checkboxInput(ns("twit"), "Two IT ?", FALSE),
          actionButton(ns("go"),"Go :")
        ),
        
        mainPanel( 
          
          tags$head(tags$style(".butt{background-color:#E9967A;} .butt{color: black;}")),
          fluidRow(
            h2(textOutput(ns("nameVariable"))),
            # column(6,
            uiOutput(ns("inferenceUni")),br()#,  #tableOutput(ns("descvar"))
            #                                                            ),
            ,
             column(6,
                   plotOutput(ns('plotinferenceUni'))
                    )
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
        num<-r$noms[which(r$variableNum)]%>%as.list()
        non_num<-r$noms[which(!r$variableNum)]%>%as.list()
        # if(length(non_num)==1) non_num <- list(non_num)
        if(length(num)>0 & length(non_num)>0){
          liste_choix<-list("numeric" =c(num), 
                            "autre" = c(non_num)
          )
        }else if(length(num)>0 & !length(non_num)>0){
          liste_choix<-list("numeric" =c(num)
          )
        }else{
          liste_choix<-list(
                            "autre" = c(non_num)
          )
          
          }
        print(liste_choix)
          selectInput(ns("variable"), "Variable:",   choices=
                      liste_choix)
      })
      
      output$apriori <- renderUI({
        
        x<- r$BDD[,input$variable]
        x <- x[!is.na(x)]
        min_x<- min(x)
        max_x <- max(x)
        fluidRow(
          h2("Apriori sur la moyenne : "),
          sliderInput(ns("mu0"), "μ0, moyenne à priori :",   
                      min = min_x-max_x, max = max_x*2, value = mean(x),
                      width = "25%"),
          br(),
          sliderInput(ns("k0"),"κ0, taille pseudo-Echantillon",
                      min = 0, max = length(x), value =1,
                      width = "25%"),
          
          h2("Apriori sur l'écart type : "),
          splitLayout(cellWidths = c("10%","10%"),
          numericInput(ns("alpha_0"), "Alpha :",   
                      # min = min_x-max_x, max = max_x*2, 
                      value = 1,
                      min = 0, 
                      max = 100,
                      ),
      
          numericInput(ns("beta_0"),"Beta :",
                      min = 0, max = 100, value =1)
          ),
      
                 div(class = "center50",
          plotOutput(ns("priorSigma"))
          
          )
        )
      })
      
      fitInference <- reactive({
        randomVals()
        BDD<-isolate(r$BDD[,input$variable])
        oneMeanEstim(BDD,
                     alpha = 0.15,mu_0 = isolate(input$mu0) ,kappa_0 = isolate(input$k0),
                     alpha_0 = isolate(input$alpha_0),
                     beta_0 = isolate(input$beta_0),seuil = 3,
                     # theta_P = c(-10,5),theta_A = c(5,12)
                     )
        })
      
      
      output$nameVariable <- renderText({
        randomVals()

        isolate(input$variable)
        })
  randomVals<- eventReactive(input$go,{
    
    alea=runif(n = 1)
  print(alea)
  alea  
  })
      observeEvent(randomVals(),{
     
        if(tryCatch(length(fitInference())>0,error = function(cond) return(F) )){
          
  
          
          output$inferenceUni <- renderUI({
            noms<-names(fitInference())
            # tableOutput(ns(noms[1]))
            # print(noms)
             tagList(
             lapply(noms, function(x) list(h3(x),
            renderTable(as.data.frame.list(fitInference()[[x]]))
               #  print(x)
                )
                )
               )
              })
          
        }

      })
      
    
      output$plotinferenceUni <- renderPlot({
        
        plot( fitInference())
      })
      
      output$priorSigma<- renderPlot(width = 200, height = 100,{
        print(list(shape=input$alpha_0, rate=input$beta_0))
        ggplot(data=data.frame(x=c(0,1)),aes(x))+
          stat_function(fun=dgamma,n=101, args=list(shape=input$alpha_0, rate=input$beta_0))+
          theme_light()+  theme(axis.text.y=element_blank(),
                                axis.ticks.y=element_blank(),
                                axis.ticks.x=element_blank()
          )+ylab("")+xlab("")
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
