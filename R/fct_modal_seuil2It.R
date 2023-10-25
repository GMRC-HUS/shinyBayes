#' modal_seuil2It
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#' @import shinyWidgets
#' @noRd


twitUi <- function(id) {
  # `NS(id)` returns a namespace function, which was save as `ns` and will
  # invoke later.
  ns <- NS(id)
  showModal(
    modalDialog(size ="l",
      title = "Définition des seuils ou Two It",

      # là



      tags$div(
        shinyWidgets::switchInput(
          inputId = ns("choix_seuil_2it"),
          onLabel = "Seuil",
          offLabel = "Two It",
          offStatus = "primary",
          onStatus = "primary"
        ),
        # shinyWidgets::materialSwitch(ns("twit_funct"),"Two It", FALSE, status = "success",right=T),
        uiOutput(ns("seuil_ou_two_it_ui"))
        # shinyWidgets::materialSwitch(ns("seuil"),"Seuil", FALSE, status = "success",right=T),
      ),
      footer = tagList(
        actionButton(ns("ok_seuil"), "OK")
      )
    )
  )
}

# Module server function
twitServer <- function(id, list_var, type_glm) {
  moduleServer(
    id,


    ## Below is the module function
    function(input, output, session) {
      ns <- session$ns
      observeEvent(input$choix_seuil_2it, {
        output$seuil_ou_two_it_ui <- renderUI({
          if (length(list_var) == 0) {
            return(h2("Pas de variables dans le modèle"))
          }
          if (input$choix_seuil_2it) {
            tagList(
              h2("Choix des seuils"),
              awesomeRadio(
                inputId = ns("plusieur_seuils"),
                label = "Le même seuil pour l'ensemble des covariables",
                choices = c("Oui", "Non"),
                selected = "Non",
                inline = TRUE,
                status = "success",
                checkbox = TRUE
              ),
              uiOutput(ns("choix_seuils"))
            )
          } else {
            tagList(
              h2("Choix des Bornes du Two It"),
              pickerInput(
                inputId = ns("var_sel"),
                label = "Variable à monitorer",
                choices = list_var,
                options = list(
                  size = 5
                )
              ),
              splitLayout(
                cellWidths = c("50%", "50%"),
                h3("Présence d'effet :"), h3("Absence d'effet : ")
              ),
              splitLayout(
                cellWidths = c("25%", "25%", "25%", "25%"),
                ifelse_perso(
                  type_glm %in% c("poiss", "binom"), numericInput(ns("theta_P_min"), "Min :",
                    min = 0.0000,
                    value = 1, step = 0.1
                  ),
                  numericInput(ns("theta_P_min"), "Min :",
                    value = 0, step = 0.1
                  )
                ),
                ifelse_perso(
                  type_glm %in% c("poiss", "binom"), numericInput(ns("theta_P_max"), "Max :",
                    min = 0,
                    value = 1, step = 0.1
                  ),
                  numericInput(ns("theta_P_max"), "Max :",
                    value = 0, step = 0.1
                  )
                ),
                ifelse_perso(
                  type_glm %in% c("poiss", "binom"), numericInput(ns("theta_A_min"), "Min :",
                    min = 0,
                    value = 1, step = 0.1
                  ),
                  numericInput(ns("theta_A_min"), "Min :",
                    value = 0, step = 0.1
                  )
                ),
                ifelse_perso(
                  type_glm %in% c("poiss", "binom"), numericInput(ns("theta_A_max"), "Max :",
                    min = 0,
                    value = 1, step = 0.1
                  ),
                  numericInput(ns("theta_A_max"), "Max :",
                    value = 0, step = 0.1
                  )
                )
              )
            )
          }
        })
      })


      output$choix_seuils <- renderUI({
        if (input$plusieur_seuils == "Non") {
          tagList(
            lapply(1:length(list_var), function(i) {
              list(
                numericInput(
                  inputId = ns(paste("seuil_", list_var[i], sep = "")), label = list_var[i],
                  value = ifelse(type_glm %in% c("poiss", "binom"), 1, 0),
                  min = ifelse(type_glm %in% c("poiss", "binom"), 0.001, NA), step = 0.1
                )
              )
            })
          )
        } else {
          numericInput(ns("valeur_seuil"), "Valeur du seuil :",
            value = ifelse(type_glm %in% c("poiss", "binom"), 1, 0),
            min = ifelse(type_glm %in% c("poiss", "binom"), 0.001, NA), step = 0.1
          )
        }
      })



      myreturn <- reactiveValues()
      observeEvent(input$ok_seuil, {
        if (length(list_var) == 0) {
          removeModal()
          return(NULL)
        }
        if (input$choix_seuil_2it) {
          type <- "seuil"
          if (!input$plusieur_seuils == "Non") {
            plusieur_seuils <- F
            val <- input$valeur_seuil
          } else {
            plusieur_seuils <- T
            val <- sapply(list_var, function(x) input[[paste("seuil_", x, sep = "")]])
          }
        } else {
          type <- "2It"
          plusieur_seuils <- NA
          val <- list(
            var = isolate(input$var_sel),
            "theta_P_min" = isolate(input$theta_P_min),
            "theta_P_max" = isolate(input$theta_P_max),
            "theta_A_min" = isolate(input$theta_A_min),
            "theta_A_max" = isolate(input$theta_A_max)
          )
        }

        removeModal()
        myreturn$ls <- list(type = type, plusieur_seuils = plusieur_seuils, val = val)
      })


      return(myreturn)
    }
  )
}




twitUi_prop <- function(id) {
  # `NS(id)` returns a namespace function, which was save as `ns` and will
  # invoke later.
  ns <- NS(id)
  showModal(
    modalDialog(size ="l",
      title = "Définition des seuils ou Two It",
      
      # là
      
      
      
      tags$div(
        shinyWidgets::switchInput(
          inputId = ns("choix_seuil_2it"),
          onLabel = "Seuil",
          offLabel = "Two It",
          offStatus = "primary",
          onStatus = "primary"
        ),
        # shinyWidgets::materialSwitch(ns("twit_funct"),"Two It", FALSE, status = "success",right=T),
        uiOutput(ns("seuil_ou_two_it_ui"))
        # shinyWidgets::materialSwitch(ns("seuil"),"Seuil", FALSE, status = "success",right=T),
      ),
      footer = tagList(
        actionButton(ns("ok_seuil"), "OK")
      )
    )
  )
}

# Module server function
twitServer_prop <- function(id, twit, seuil_unique,seuil_diff, seuil_OR, seuil_RR,group,seuil_comp_retour ) {
  moduleServer(
    id,
    
    
    ## Below is the module function
    function(input, output, session) {
      ns <- session$ns
      observeEvent(input$choix_seuil_2it, {
        
        
        output$seuil_ou_two_it_ui <- renderUI({

          
          choix_var<- apply(utils::combn(group, 2),2, function(x) c(paste(x, collapse = "vs"),paste(rev(x), collapse = "vs")))%>%as.vector()
          print(input$choix_seuil_2it)
          if (input$choix_seuil_2it) {
  
            
            tagList(
              h2("Choix des seuils"),
              awesomeRadio(
                inputId = ns("plusieur_seuils"),
                label = "Le même seuil pour l'ensemble des covariables",
                choices = c("Oui", "Non"),
                selected = "Non",
                inline = TRUE,
                status = "success",
                checkbox = TRUE
              ),
              uiOutput(ns("choix_seuils"))
            )
          } else {
            tagList(
              h2("Choix des Bornes du Two It"),
              pickerInput(
                inputId = ns("var_sel"),
                label = "Variable à monitorer",
                choices =choix_var ,
                options = list(
                  size = 5
                )
              ),
             
              box("Définition du twit", table_interact_UI(ns("twit_choix")))
                
              )
            
          }
          
        })
          
          if (!input$choix_seuil_2it) {
            twit$var <- input$var_sel 
          twit$data <-data.frame(Type= c("Diff", "RR","OR"), 
                                             minHa = c(0,1,1), 
                                             maxHa = c(0,1,1), 
                                             minHr = c(0,1,1), 
                                             maxHr = c(0,1,1), row.names = "Type"
          )
          
          
         
          seuil_comp_retour$type <-"twit"
          
          }else{
          
            twit$data <- NULL
            seuil_comp_retour$type <-"seuil"
        }
    
      })
      
        
     
        
    
        observeEvent(input$plusieur_seuils, {  
        if (input$choix_seuil_2it) {
          seuil_comp_retour$data$seuil <- "seuil"
          print(input$plusieur_seuils4)
          print(names(input))
  if( input$plusieur_seuils == "Oui"){
    seuil_unique$data <-  data.frame(Type= c("Diff", "RR","OR"), seuil = c(0,1,1),row.names = "Type")
    seuil_diff$data = NULL
    seuil_OR$data= NULL
    seuil_RR$data=NULL
    seuil_comp_retour$plusieurs = F
  }else{
    seuil_comp_retour$plusieurs = T
    group<- as.character(group)
    diff_moy<- rr<- or <-data.frame(row.names= group[-1])
    diff_moy[,group[-length(group)]]<-0
    rr[,group[-length(group)]]<-1
    or[,group[-length(group)]]<-1
    for(i in 1:dim(rr)[1]){
      for(j in min(2,dim(rr)[2]) :dim(rr)[2]){
        if(j>=i+1){
        rr [i,j]<- NA
        or [i,j]<- NA
        diff_moy[i,j]<-NA
      }
      }
    }
    seuil_diff$data = diff_moy
    seuil_OR$data= or
    seuil_RR$data=rr
    seuil_unique$data <-  NULL

  }
           
        print(seuil_unique$data)
        
         twit$data<- NULL 
        }else{
         
      }

        })

      output$choix_seuils <- renderUI({
        if (input$plusieur_seuils == "Non") {
          tagList(
            box("Seuils sur la différence",table_interact_UI(ns("seuil_diff"))),
            box("Seuils sur l'odds ratio",table_interact_UI(ns("seuil_OR"))),
            box("Seuils sur le risque relatif", table_interact_UI(ns("seuil_RR")))
            
          )
        } else {
         
          print(seuil_unique$data)
          tagList(table_interact_UI(ns("test"))
          )
        }
      })
      
      table_interact_server("test",seuil_unique,F)
      
      table_interact_server("seuil_OR",seuil_OR)
      table_interact_server("seuil_diff",seuil_diff)
      table_interact_server("seuil_RR",seuil_RR)
      table_interact_server("twit_choix",twit,F)
      
 
      myreturn <- reactiveValues()
      observeEvent(input$ok_seuil, {
        if (input$choix_seuil_2it) {
          seuil_comp_retour$type <-"seuil"
          if( input$plusieur_seuils == "Oui"){
            seuil_comp_retour$plusieurs = F
          }else{
            seuil_comp_retour$plusieurs = T
          }
        }else{
          
          seuil_comp_retour$type <-"twit"
          twit$var <- input$var_sel 
          
        }
        
        removeModal()
     
      })
      
      
     }
  )
}


twitUi_prop_infe <- function(id) {
  # `NS(id)` returns a namespace function, which was save as `ns` and will
  # invoke later.
  ns <- NS(id)
  showModal(
    modalDialog(size ="l",
                title = "Définition des seuils ou Two It",
                
                # là
                
                
                
                tags$div(
                  shinyWidgets::switchInput(
                    inputId = ns("choix_seuil_2it"),
                    onLabel = "Seuil",
                    offLabel = "Two It",
                    offStatus = "primary",
                    onStatus = "primary"
                  ),
                  # shinyWidgets::materialSwitch(ns("twit_funct"),"Two It", FALSE, status = "success",right=T),
                  uiOutput(ns("seuil_ou_two_it_ui"))
                  # shinyWidgets::materialSwitch(ns("seuil"),"Seuil", FALSE, status = "success",right=T),
                ),
                footer = tagList(
                  actionButton(ns("ok_seuil"), "OK")
                )
    )
  )
}

# Module server function
twitServer_prop_infe <- function(id, twit, seuil_unique,seuil_comp_retour,prop=T ) {
  moduleServer(
    id,
    
    
    ## Below is the module function
    function(input, output, session) {
      ns <- session$ns
      observeEvent(input$choix_seuil_2it, {
        
        
        output$seuil_ou_two_it_ui <- renderUI({
          
          
        
          print(input$choix_seuil_2it)
          if (input$choix_seuil_2it) {
            
            
            tagList(
              h2("Choix du seuil"),
  
              uiOutput(ns("choix_seuils"))
            )
          } else {
            tagList(
              h2("Choix des Bornes du Two It"),
  
              box("Définition du twit", table_interact_UI(ns("twit_choix")))
              
            )
            
          }
          
        })
        
        if (!input$choix_seuil_2it) {
if(prop){
          twit$data <-data.frame(
                                 minHa = c(0), 
                                 maxHa = c(0.5), 
                                 minHr = c(0.5), 
                                 maxHr = c(1), row.names = c("Seuil")
          )
          
}else{
  twit$data <-data.frame(
    minHa = c(0), 
    maxHa = c(0), 
    minHr = c(0), 
    maxHr = c(0), row.names = c("Seuil")
  )
  
}
          
          seuil_comp_retour$type <-"twit"
          
        }else{
          
          seuil_comp_retour$data$seuil <- "seuil"
          print(input$plusieur_seuils4)
          print(names(input))
          
          seuil_unique$data <-  ifelse_perso(prop,data.frame( seuil = c(0.5),row.names = "Seuil"),
                                             data.frame( seuil = c(0),row.names = "Seuil"))
          
          seuil_comp_retour$plusieurs = F
          
          
          print(seuil_unique$data)
          
          twit$data<- NULL 
        }
        
      })
      
      
      
      

      
      output$choix_seuils <- renderUI({

          
          print(seuil_unique$data)
          tagList(table_interact_UI(ns("seuil_unique"))
          )
        
      })
      
      table_interact_server("seuil_unique",seuil_unique,F)
      


      table_interact_server("twit_choix",twit,F)
      
      
      myreturn <- reactiveValues()
      observeEvent(input$ok_seuil, {
        if (input$choix_seuil_2it) {
          seuil_comp_retour$type <-"seuil"
  
        }else{
          
          seuil_comp_retour$type <-"twit"
          twit$var <- input$var_sel 
          
        }
        
        removeModal()
        
      })
      
      
    }
  )
}


# Module server function
twitServer_moy <- function(id, twit, seuil_unique,seuil_diff,group,seuil_comp_retour ) {
  moduleServer(
    id,
    
    
    ## Below is the module function
    function(input, output, session) {
      ns <- session$ns
      observeEvent(input$choix_seuil_2it, {
        
        
        output$seuil_ou_two_it_ui <- renderUI({
          
          
          choix_var<- apply(utils::combn(group, 2),2, function(x) c(paste(x, collapse = "vs"),paste(rev(x), collapse = "vs")))%>%as.vector()
          print(input$choix_seuil_2it)
          if (input$choix_seuil_2it) {
            
            
            tagList(
              h2("Choix des seuils"),
              awesomeRadio(
                inputId = ns("plusieur_seuils"),
                label = "Le même seuil pour l'ensemble des covariables",
                choices = c("Oui", "Non"),
                selected = "Non",
                inline = TRUE,
                status = "success",
                checkbox = TRUE
              ),
              uiOutput(ns("choix_seuils"))
            )
          } else {
            tagList(
              h2("Choix des Bornes du Two It"),
              pickerInput(
                inputId = ns("var_sel"),
                label = "Variable à monitorer",
                choices =choix_var ,
                options = list(
                  size = 5
                )
              ),
              
              box("Définition du twit", table_interact_UI(ns("twit_choix")))
              
            )
            
          }
          
        })
        
        if (!input$choix_seuil_2it) {
          twit$var <- input$var_sel 
          twit$data <-data.frame(Type= c("Diff"), 
                                 minHa = c(0), 
                                 maxHa = c(0), 
                                 minHr = c(0), 
                                 maxHr = c(0), row.names = "Type"
          )
          
          
          
          seuil_comp_retour$type <-"twit"
          
        }else{
          
          twit$data <- NULL
          seuil_comp_retour$type <-"seuil"
        }
        
      })
      
      
      
      
      
      observeEvent(input$plusieur_seuils, {  
        if (input$choix_seuil_2it) {
          seuil_comp_retour$data$seuil <- "seuil"
          print(input$plusieur_seuils4)
          print(names(input))
          if( input$plusieur_seuils == "Oui"){
            seuil_unique$data <-  data.frame(Type= c("Diff"), seuil = c(0),row.names = "Type")
            seuil_diff$data = NULL
            
            seuil_comp_retour$plusieurs = F
          }else{
            seuil_comp_retour$plusieurs = T
            group<- as.character(group)
            diff_moy<-data.frame(row.names= group[-1])
            diff_moy[,group[-length(group)]]<-0
            
            for(i in 1:dim(diff_moy)[1]){
              for(j in min(2,dim(diff_moy)[2]) :dim(diff_moy)[2]){
                if(j>=i+1){
                  
                  diff_moy[i,j]<-NA
                }
              }
            }
            seuil_diff$data = diff_moy
            
            seuil_unique$data <-  NULL
            
          }
          
          print(seuil_unique$data)
          
          twit$data<- NULL 
        }else{
          
        }
        
      })
      
      output$choix_seuils <- renderUI({
        if (input$plusieur_seuils == "Non") {
          tagList(
            box("Seuils sur la différence",table_interact_UI(ns("seuil_diff"))),
  
            
          )
        } else {
          
          print(seuil_unique$data)
          tagList(table_interact_UI(ns("test"))
          )
        }
      })
      
      table_interact_server("test",seuil_unique,F)
      
      
      table_interact_server("seuil_diff",seuil_diff)
      
      table_interact_server("twit_choix",twit,F)
      
      
      myreturn <- reactiveValues()
      observeEvent(input$ok_seuil, {
        if (input$choix_seuil_2it) {
          seuil_comp_retour$type <-"seuil"
          if( input$plusieur_seuils == "Oui"){
            seuil_comp_retour$plusieurs = F
          }else{
            seuil_comp_retour$plusieurs = T
          }
        }else{
          
          seuil_comp_retour$type <-"twit"
          twit$var <- input$var_sel 
          
        }
        
        removeModal()
        
      })
      
      
    }
  )
}
