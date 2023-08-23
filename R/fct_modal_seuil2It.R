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
    modalDialog(
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
    modalDialog(
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
twitServer_prop <- function(id, twit, seuil,group) {
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
        if (input$choix_seuil_2it) {
  if( input$plusieur_seuils == "Non"){
          seuil$data <-  list(data.frame(Type= c("Diff", "RR","OR"), seuil = c(0,1,1)))
  }else{
    
    diff_moy<- rr<- or <-data.frame(Group= group) 
    diff_moy[,group]<-0
    rr[,group]<-1
    or[,group]<-1
    for(i in 2:dim(rr)[1]){
      for(j in 2:dim(rr)[2]){
        if(j>=i){
        rr [i,j]<- NA
        or [i,j]<- NA
        diff_moy[i,j]<-NA
      }
      }
    }
    seuil$data <-  list(diff_moy=diff_moy,
                        rr=rr,
                        or= or)
    
  }
           
        
        
         twit$data<- NULL 
        }else{
          twit$data <-  list(data.frame(Type= c("Diff", "RR","OR"), 
                                        minHa = c(0,1,1), 
                                          maxHa = c(0,1,1), 
                                        minHr = c(0,1,1), 
                                        maxHr = c(0,1,1)
                                        ))
          
          
          seuil$data<- NULL
          
          
        }
        
        
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
