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
          
          h2("Two IT ?"),
          shinyWidgets::materialSwitch(ns("twit"),"", FALSE, status = "success",right=T),
          uiOutput(ns("twit_ui")),
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
          splitLayout(cellWidths = c("10%","10%"),
          numericInput(ns("mu0"), "μ0 : ",   
                      min = min_x-max_x, max = max_x*2, value = round(mean(x))),
          br(),
          numericInput(ns("k0"),"Pseudo-Echantillon :",
                      min = 0, max = length(x)*4, value =1)),
          
          
          plotOutput(width = 200, height = 100,ns("priorMean")),
          h2("Apriori sur l'écart type : "),
          splitLayout(cellWidths = c("10%","10%"),
          numericInput(ns("alpha_0"), "Alpha :",   
                      # min = min_x-max_x, max = max_x*2, 
                      value = 1,
                      min = 1, 
                      max = Inf,
                      ),
      
          numericInput(ns("beta_0"),"Beta :",
                      min = 1, max = Inf, value =1)
          ),
      
                 
          plotOutput(width = 200, height = 100,ns("priorSigma")),
          actionButton(ns("ellicitation"), "Aide ellicitation")
          
        )
      })
      
     
        output$twit_ui <- renderUI({
          if(input$twit){
            
          tags$div(splitLayout(cellWidths = c("50%","50%"),
            h3("Rejet :"),h3("Acceptation : ")),
            splitLayout(cellWidths = c("25%","25%","25%","25%"),
                        numericInput(ns("theta_P_min"), "Min :",   
                                     # min = min_x-max_x, max = max_x*2, 
                                     value = 0
                                     
                        ),
                        
                        numericInput(ns("theta_P_max"),"Max :",
                                     value =0)
            ,
            
           
                        numericInput(ns("theta_A_min"), "Min :",   
                                     # min = min_x-max_x, max = max_x*2, 
                                     value = 0
                                     
                        ),
                        
                        numericInput(ns("theta_A_max"),"Max :",
                                     value =0)
            ))
          
}
                    })
      
      
      fitInference <- reactive({
        randomVals()
        BDD<-isolate(r$BDD[,input$variable])
        
       isolate({ theta_P<-ifelse_perso(input$twit,c(input$theta_P_min, input$theta_P_max), NULL  )
        theta_A<-ifelse_perso(input$twit,c(input$theta_A_min, input$theta_A_max), NULL  )
       })
        oneMeanEstim(BDD,
                     alpha = 0.15,mu_0 = isolate(input$mu0) ,kappa_0 = isolate(input$k0),
                     alpha_0 = isolate(input$alpha_0),
                     beta_0 = isolate(input$beta_0),seuil = 3,
                      theta_P = theta_P,theta_A = theta_A
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
             tagList(
             lapply(noms, function(x) {
               if(is.null(fitInference()[[x]])) return()
            list(h3(x),
            renderTable(as.data.frame.list(fitInference()[[x]]))
               #  print(x)
                )
             }
                )
               )
              })
        }
      })
      
   
      
      observeEvent(input$ellicitation,ignoreInit = T, {
        mod_sd_prec_to_alph_beta_server("id",input$alpha_0,input$beta_0)
        print(input)
      })
      
      observeEvent(input$sigma_0, {
        
        updateNumericInput(session, "alpha_0", value = input$sigma_0)
      })
      output$plotinferenceUni <- renderPlot(plot( fitInference()))
      
      output$priorSigma<- renderPlot({
        print(list(shape=input$alpha_0, rate=input$beta_0))
        ggplot(data=data.frame(x=c(0,1)),aes(x))+
          stat_function(fun=dgamma,n=101, args=list(shape=input$alpha_0, rate=input$beta_0))+
          theme_light()+  theme(axis.text.y=element_blank(),
                                axis.ticks.y=element_blank(),
                                axis.ticks.x=element_blank()
          )+ylab("")+xlab("")
      })
      
      
      
      
      output$plotinferenceUni <- renderPlot(plot( fitInference()))
      
      output$priorSigma<- renderPlot({
        print(list(shape=input$alpha_0, rate=input$beta_0))
        ggplot(data=data.frame(x=c(0,input$alpha_0*4)),aes(x))+
          stat_function(fun=dgamma,n=101, args=list(shape=input$alpha_0, rate=input$beta_0))+
          theme_light()+  theme(axis.text.y=element_blank(),
                                axis.ticks.y=element_blank(),
                                axis.ticks.x=element_blank()
          )+ylab("")+xlab("")
      })
      
      output$priorMean<- renderPlot({
        x<- r$BDD[,input$variable]
        x <- x[!is.na(x)]
        min_x<- min(x)
        max_x <- max(x)
        print(list(shape=input$alpha_0, rate=input$beta_0))
        ggplot(data=data.frame(x=c(min_x-max_x,2*max_x)),aes(x))+
          stat_function(fun=dnorm,n=101, args = list(mean = input$mu0, sd = 1/input$k0))+
          theme_light()+  theme(axis.text.y=element_blank(),
                                axis.ticks.y=element_blank(),
                                axis.ticks.x=element_blank()
          )+ylab("")+xlab("")
      })
  
  })
  
  
 
  
}
    
## To be copied in the UI
# mod_inferenceUni_ui("inferenceUni_1")
    
## To be copied in the server
# mod_inferenceUni_server("inferenceUni_1")
