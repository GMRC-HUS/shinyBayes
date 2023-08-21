#' comp_pour UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_comp_pour_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns('mod_comp_pour'))
  )
}
    
#' comp_pour Server Functions
#'
#' @noRd 
mod_comp_pour_server <- function(id,r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 

    mod_comp_pour <- fluidPage(
      
      
      
      
      
      ###################################################
      #####   PAGE 3.2     ###############################
      ###################################################
      
      
      titlePanel("Comparaison de proportions"),
      sidebarLayout(
        sidebarPanel(width = 3,
                     h2("Variable de groupe :"),
                     uiOutput(ns("vbl_grp")),
                     br(),
                     h2("Variable proportion :"),
                     uiOutput(ns("vbl_prop")), 
                     h3("Choix des priors"),
                     actionButton(ns("ellicitation"), "Ellicitation"),

                     
                     
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
      output$mod_comp_pour <- renderUI({
        if (!r$BASEchargee) {
          do.call(tabPanel, pasDeBase)
        } else {
          do.call(tabPanel, mod_comp_pour)
        }
      })
    })
    
    output$vbl_grp <- renderUI({
      choix_var <-r$BDD%>%apply(2, function(x) between(nlevels(as.factor(x)),2,4))%>%which()%>%names
      if(length(choix_var)==0){ return(h3("Pas de variables avec 2 à 4 groupes dans la base"))}
      return(
        pickerInput(
          inputId = ns("var_grp"),
          label = "", 
          choices = choix_var
        )
        
      )
      
      
      observeEvent(input$ellicitation, ignoreInit = T, {
        if (is.null(prior_glm$prior_intercept) | is.null(prior_glm$prior_beta_scale)) {
          prior_quali_sd <- sd_quali(input$list_quali, r$BDD)
          
          prior_quanti_sd <- sapply(input$list_quanti, function(x) sd(r$BDD[, x], na.rm = T))
          
          if (length(prior_quanti_sd) == 0) prior_quanti_sd <- NULL
          
          if (input$type_glm == "lin") {
            prior_glm$prior_intercept <- c(
              round(mean(r$BDD[, input$variable], na.rm = T), 2),
              round(2.5 * sd(r$BDD[, input$variable], na.rm = T), 2)
            )
          } else if (input$type_glm %in% c("binom", "poiss")) {
            prior_glm$prior_intercept <- c(0, 2.5)
          }
          prior_glm$prior_beta_scale <- round(2.5 / c(prior_quanti_sd, prior_quali_sd) * sd(r$BDD[, input$variable], na.rm = T), 2)
          prior_glm$prior_beta_location <- rep(0, length(c(prior_quanti_sd, prior_quali_sd)))
        }
        
        
        
        prior_beta_scale_def <- c(prior_glm$prior_intercept[2], prior_glm$prior_beta_scale) # ), default_prior_beta_scale_def, prior_glm$prior_beta_scale)
        prior_beta_location_def <- c(prior_glm$prior_intercept[1], prior_glm$prior_beta_location) # ), default_prior_beta_location_def, prior_glm$prior_beta_location)
        
        if (length(input$list_quali) > 0) {
          nom_var_quali <- lapply(input$list_quali, function(x) paste(x, levels(factor(r$BDD[, x]))[-1], sep = "_")) %>% unlist()
        } else {
          nom_var_quali <- NULL
        }
        
        
        variables <-c("intercept",input$list_quanti,nom_var_quali)
        if (input$type_glm == "lin") {
          showModal(
            modalDialog(size = "l",
                        tagList(lapply(1:length(variables), function(i) ui_choix_prior_norm(i,variables,ns,prior_beta_location_def,prior_beta_scale_def )
                        )),
                        footer = tagList(
                          actionButton(ns("ok"), "OK"),
                          actionButton(ns("defaut"), "Défaut")
                        )
            )
          )
          
          noms <-variables
          positions <- c(prior_glm$prior_intercept[1], prior_glm$prior_beta_location)
          dispersions <- c(prior_glm$prior_intercept[2], prior_glm$prior_beta_scale)
          
          lapply(1:length(noms), function(i) {
            output[[paste(noms[i], "_courbe", sep = "")]] <- ui_ggplot_prior_norm(i,input,variables)
          })
        }else if (input$type_glm %in% c("binom", "poiss")) {
          
          showModal(
            modalDialog(size = "l",
                        tagList(lapply(2:length(variables), 
                                       function(i) ui_choix_prior_exp(i,variables,ns,prior_beta_location_def,prior_beta_scale_def ))),
                        footer = tagList(
                          actionButton(ns("ok"), "OK"),
                          actionButton(ns("defaut"), "Défaut")
                        )
            )
          )
          
          noms <-variables
          positions <- c(prior_glm$prior_intercept[1], prior_glm$prior_beta_location)
          dispersions <- c(prior_glm$prior_intercept[2], prior_glm$prior_beta_scale)
          
          lapply(1:length(noms), function(i) {
            output[[paste(noms[i], "_courbe", sep = "")]] <- ui_ggplot_prior_exp(i,input,variables)
          })  
        }
        
        
      })
      
      
    })
    
    
    
    output$vbl_prop <- renderUI({
      choix_var <-r$BDD%>%apply(2, function(x) nlevels(as.factor(x))==2)%>%which()%>%names
      if(length(choix_var)==0){ return(h3("Pas de variables avec 2 à 4 groupes dans la base"))}
      return(
        pickerInput(
          inputId = ns("var_prop"),
          label = "", 
          choices = choix_var
        )
        
      )
      
      
    })
  })
}
    
## To be copied in the UI
# mod_comp_pour_ui("comp_pour_1")
    
## To be copied in the server
# mod_comp_pour_server("comp_pour_1")
