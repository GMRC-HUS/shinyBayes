#' sd_prec_to_alpha_beta 
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd


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

mod_sd_prec_to_alph_beta_ui <- function(id){
  ns <- NS(id)
  tagList(
    actionButton(ns("ellicitation"), "Aide ellicitation")
  )
}

#' inferenceUni Server Functions
#'
#' @noRd 
mod_sd_prec_to_alph_beta_server <- function(id,alpha, beta){
  moduleServer( id, function(input, output, session){
    # alpha=nu_0
    # beta=nu_0*sigma_02
 
    ns <- session$ns 
    
 
    
     
      showModal(modalDialog(
 
        plotOutput(width = 200, height = 100,ns("ellicitationcurve")),
        
        numericInput(ns("sigma_0"), "A priori % : ",   
                     min = 0, max = 100, value = alpha),
        numericInput(ns("n_sigma_0"), "Taille pseudo population % : ",   
                    min = 0, max = 2000, value = (1/(alpha/beta))),
        
        
        
        
      ))
    
    
    

    
    output$ellicitationcurve<- renderPlot({
 # print(input)
      alpha_0=input$sigma_0
      beta_0=alpha_0*input$n_sigma_0
      
      ggplot(data=data.frame(x=c(0,input$sigma_0*4)),aes(x))+
        stat_function(fun=dgamma, args=list(shape=alpha_0, rate=beta_0))+
        theme_light()+  theme(axis.text.y=element_blank(),
                              axis.ticks.y=element_blank(),
                              axis.ticks.x=element_blank()
        )+ylab("")+xlab("")
    })
    
    
  
    
   return(reactiveValues(
     sigma_0=input$sigma_0,
     n_sigma_0=input$n_sigma_0
     ))
    
  })
  

  
}

## To be copied in the UI
# mod_inferenceUni_ui("inferenceUni_1")

## To be copied in the server
# mod_inferenceUni_server("inferenceUni_1")
