#' Multivarie UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @param r variable in the evironnement
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @import broom.mixed
#' @import sortable
#' @import rstanarm

mod_Multivarie_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidPage(    
      titlePanel("Multivarié"),
      sidebarLayout( 
        sidebarPanel(width=4,
                     radioButtons(ns('type_glm'), "Type de regression",
                                  c(Linéaire = "lin",
                                    Binomial ="binom",
                                    Beta = "beta",
                                    Poisson = "poiss"),'lin'
                     ),
                     uiOutput(ns("choix_y")),
                     uiOutput(ns("propositions_multi")),
                   
                     actionButton(ns("ellicitation"), "Aide ellicitation"),
                     
                     
                     
                     h2("Two IT ?"),
                     shinyWidgets::materialSwitch(ns("twit"),"", FALSE, status = "success",right=T),
                     uiOutput(ns("twit_ui")),
                     actionButton(ns("go"),"Go :")
        ),
        
        mainPanel( 
          
          tags$head(tags$style(".butt{background-color:#E9967A;} .butt{color: black;}")),
          fluidRow( textOutput(ns("model_text"))),
           
          fluidRow( tableOutput(ns("res_multi")))
            
          
        )# fin MainPanel
        
      )# fin sidebarlayout
    )# fin fluidpage
    
    
    
 
  )
}
    
#' Multivarie Server Functions
#'
#' @noRd 
mod_Multivarie_server <- function(id,r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    output$choix_y <- renderUI({
      selectInput(ns("variable"), "Variable d'interêt :",   choices=
                    r$noms)
    
    })
    
 
  
   
    
    
    
    output$propositions_multi <- renderUI({ 
     
      liste_choix<- r$noms
      liste_choix<-liste_choix[-which(liste_choix == input$variable )]
      bucket_list(
      header = "Choix des variables pour le modèle",
      group_name = "bucket_list_group",
      orientation = "vertical",
      add_rank_list(
        text = "Noms des variables",
        labels =liste_choix,
        
        input_id = ns("choix_base")
      ),
      add_rank_list(
        text = "Variables quantitatives",
        labels = NULL,
        input_id = ns("list_quanti")
      ),
      add_rank_list(
        text = "Variables qualitatives",
        labels = NULL,
        input_id = ns("list_quali")
      ))
      
      
    })
 
 
    randomVals<- eventReactive(input$go,{
      
     runif(n = 1)
     
     
    })
  output$model_text <- renderText({
    randomVals()
  paste(isolate(input$variable),"~",paste(c(isolate(input$list_quanti), isolate(input$list_quali)), collapse = " + "))
  })
    
  
  model_2<- reactiveVal(value = NULL)
  
  
  prior_lm<-reactiveValues(
    prior_intercept= NULL,
    prior_beta_scale=NULL,
    prior_beta_location = NULL
  )

  observeEvent(input$go, {
    
    formule =  paste(input$variable,"~",paste(c(input$list_quanti, input$list_quali), collapse = " + "))
    if(is.null(prior_lm$prior_intercept)){
      if(!is.null(prior_lm$prior_beta_scale)&!is.null(prior_lm$prior_beta_location)){
        
        model_2( stan_glm(formule, family = gaussian(link = "identity"),
                          data = r$BDD, refresh = 0,
                          prior = normal(prior_lm$prior_beta_scale, prior_lm$prior_beta_location))) 
                     
      }else{
        model_2( stan_glm(formule, family = gaussian(link = "identity"),
                          data = r$BDD, refresh = 0))
      }
    }else if(!is.null(prior_lm$prior_beta_scale)&!is.null(prior_lm$prior_beta_location)){
      model_2( stan_glm(formule, family = gaussian(link = "identity"),
                        data = r$BDD, refresh = 0,
                        prior_intercept = normal(prior_intercept[1],prior_intercept[2]),
                        prior = normal(prior_lm$prior_beta_scale, prior_lm$prior_beta_location))) 
    }else{
      model_2( stan_glm(formule, family = gaussian(link = "identity"),
                        data = r$BDD, refresh = 0,
                        prior_intercept = normal(prior_intercept[1],prior_intercept[2]))
                        ) 
    }
    

    

    
  })
  
  output$res_multi<- renderTable({
    if(is.null(model_2())) return()
     tidyMCMC(model_2()$stanfit, conf.int = TRUE, conf.level = 0.95, 
              robust=TRUE, rhat=TRUE, ess=TRUE)           
    
  })
  
  observeEvent(input$ellicitation,ignoreInit = T, {
    # paste(isolate(input$variable),"~",paste(c(isolate(input$list_quanti), isolate(input$list_quali)), collapse = " + "))
    # 
    prior_intercept_def = c(mean(r$BDD[,input$variable], na.rm= T), sd(r$BDD[,input$variable], na.rm= T))
    prior_beta_scale_def = 2.5
    prior_beta_location_def = 0 
    
    nom_var_quali<- lapply(input$list_quali, function(x) paste(x,levels(factor(r$BDD[,x]))))%>%unlist
    showModal(
   

      
      modalDialog(
        tagList(lapply(c("intercept",input$list_quanti,nom_var_quali), function(x){
          list( plotOutput(width = 200, height = 100,ns(paste(x,"_courbe",sep=""))),
                
                numericInput(ns(paste(x,"_mu_0",sep="")), "A priori % : ",   
                             min = -10, max = 10, value = ifelse(x=="intercept",prior_intercept_def[1] , prior_beta_location_def)),
                numericInput(ns(paste(x,"_sigma_0",sep="")), "Taille pseudo population % : ",   
                             min = 0, max = 30, value = ifelse(x=="intercept",prior_intercept_def[2] , prior_beta_scale_def)))
        }
        )
        ),
                  footer = tagList(
                    modalButton("ok"),
                    actionButton(ns("defaut"), "Défaut")
                  ))
    )
    
    lapply( c("intercept",input$list_quanti,nom_var_quali) ,function(i) {
      print(i)
      output[[paste(i,"_courbe",sep="")]] <- renderPlot({
        ggplot(data=data.frame(x=c(-20,20)),aes(x))+
          stat_function(fun=dnorm, args=list(mean=input[[paste(i,"_mu_0",sep="")]], sd=input[[paste(i,"_sigma_0",sep="")]]))+
          theme_light()+  theme(axis.text.y=element_blank(),
                                axis.ticks.y=element_blank(),
                                axis.ticks.x=element_blank()
          )+ylab("")+xlab(i)
      })
      
})
    
    observeEvent(input$defaut, {
 

      lapply(c("intercept",input$list_quanti,nom_var_quali), function(x){
        updateNumericInput(session,paste(x,"_mu_0",sep=""),value = ifelse(x=="intercept",prior_intercept_def[1] , prior_beta_location_def))
        
        updateNumericInput(session,ns(paste(x,"_sigma_0",sep="")),value = ifelse(x=="intercept",prior_intercept_def[2] , prior_beta_scale_def))
        

    })
    })
    
    # print(input)
  })
  
  
  
  })
}
    
## To be copied in the UI
# mod_Multivarie_ui("Multivarie_1")
    
## To be copied in the server
# mod_Multivarie_server("Multivarie_1")
