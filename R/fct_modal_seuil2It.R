#' modal_seuil2It 
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd


twitUi <- function(id) {
  # `NS(id)` returns a namespace function, which was save as `ns` and will
  # invoke later.
  ns <- NS(id)
  showModal(
    modalDialog(title = "Définition des seuils ou Two It",
                
                # là   
                
             
 
  tags$div(
    shinyWidgets:: switchInput(
      inputId = ns("choix_seuil_2it"),
      onLabel = "Seuil",
      offLabel = "Two It",
      offStatus = "primary",
      onStatus = "primary"
    ),
    # shinyWidgets::materialSwitch(ns("twit_funct"),"Two It", FALSE, status = "success",right=T),
    uiOutput(ns("seuil_ou_two_it_ui"))
    #shinyWidgets::materialSwitch(ns("seuil"),"Seuil", FALSE, status = "success",right=T),
    
    
  ),
  
  footer = tagList(
    actionButton(ns("ok_seuil"), "OK")
    
  )
    )
  )
  
  
}

# Module server function
twitServer <- function(id,list_var) {
  moduleServer(
    id,
    
    
    ## Below is the module function
    function(input, output, session) {
      ns <- session$ns
      observeEvent(input$choix_seuil_2it,{
        output$seuil_ou_two_it_ui<- renderUI({
          if(input$choix_seuil_2it){
            print("rij")
            tagList(h2("Choix des seuils"),
                           awesomeRadio(
                             inputId = ns("plusieur_seuils"),
                             label = "Le même seuil pour l'ensemble des covariables", 
                             choices = c("Oui", "Non"),
                             selected = "Non",
                             inline = TRUE, 
                             status = "success",
                             checkbox = TRUE
                           ),
                           uiOutput(ns("choix_seuils")
            )
            )
          }else{
            
            tagList(h2("Choix des Bornes du Two It"),
                    
                    
                    pickerInput(
                      inputId = ns("var_sel"),
                      label = "Variable à monitorer", 
                      choices = list_var,
                      options = list(
                        size = 5)
                    ),
                    
                    
                    
            splitLayout(cellWidths = c("50%", "50%"),
                        h3("Rejet :"), h3("Acceptation : ")),
            splitLayout(
              cellWidths = c("25%", "25%", "25%", "25%"),
              numericInput(ns("theta_P_min"), "Min :",
                           # min = min_x-max_x, max = max_x*2,
                           value = 0),
              numericInput(ns("theta_P_max"), "Max :",
                           value = 0)
              ,
              numericInput(ns("theta_A_min"), "Min :",
                           # min = min_x-max_x, max = max_x*2,
                           value = 0),
              numericInput(ns("theta_A_max"), "Max :",
                           value = 0)
            )
            )
            
            
            
          }
          
        })})
      
      
      output$choix_seuils <- renderUI({
        if(input$plusieur_seuils=="Non"){
          
          tagList(
            lapply(1:length(list_var), function(i) {
              
              list(
                numericInput(inputId =ns(paste( "seuil_", list_var[i], sep = "")), label= list_var[i] , value = 0 )
                
              )
            }
            )
          )
          
        }else{
          
          
          numericInput(ns("valeur_seuil"), "Valeur du seuil :",
                       # min = min_x-max_x, max = max_x*2,
                       value = 0)
          
        }
        
        
      })
      
      
    
      myreturn <- reactiveValues()
      observeEvent(input$ok_seuil, {
        
       
         if(input$choix_seuil_2it){
          type = "seuil"
          if(!input$plusieur_seuils=="Non"){
            plusieur_seuils=F
            val = input$valeur_seuil
          }else{
            plusieur_seuils=T
            val = sapply(list_var,function(x) input[[paste( "seuil_", x, sep = "")]])
          }
        }else{
          type = "2It"
          plusieur_seuils= NA
          val = list(var =input$var_sel,
                     "theta_P_max"= input$theta_P_min,
                     "theta_P_max" = input$theta_P_max, 
                     "theta_A_min" = input$theta_A_min,
                     "theta_A_max" = input$theta_A_max)
        }
       
        removeModal()
        myreturn$ls = list(type=type, plusieur_seuils=plusieur_seuils, val=val
                       
        )
      })
      
      
  return(myreturn)
      
    }
  )    
}

