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
#' @import bayesplot
#' @import shinyWidgets
#' @import shinyalert

mod_Multivarie_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidPage(
      titlePanel("Multivarié"),
      sidebarLayout(
        sidebarPanel(
          width = 4,
          radioButtons(
            ns("type_glm"), HTML(paste(h2("Type de regression"),text_aide("Choix type de régression multivarié"))),
            c(
              Linéaire = "lin",
              Binomial = "binom",
              Beta = "beta",
              Poisson = "poiss"
            ), "lin"
          ), 
          uiOutput(ns("choix_y")),
          uiOutput(ns("propositions_multi")),
          uiOutput(ns("refactorisation")),
          
          h3("Choix des priors"),
          actionButton(ns("ellicitation"), "Ellicitation"),
          
          text_aide("Texte Aide ellicitation multivarié "),
          h3("Seuils/Two IT ?"),text_aide("Texte Aide Two IT multivarié "),
          shinyWidgets::materialSwitch(ns("twit"), "", value =FALSE, status = "success", right = T),
          uiOutput(ns("twit_ui")),
          
          actionButton(ns("go"), "Go :")
        ),
        mainPanel(
          tags$head(tags$style(".butt{background-color:#E9967A;} .butt{color: black;}")),
          
          uiOutput(ns("result_multi"))
        
        ) # fin MainPanel
      ) # fin sidebarlayout
    ) # fin fluidpage
  )
}

#' Multivarie Server Functions
#'
#' @noRd
mod_Multivarie_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$choix_y <- renderUI({
      selectInput(ns("variable"), h2("Variable d'interêt :"),
        choices =
          r$noms
      )
    })


    
    # Choix seuil two_it
    output$twit_ui <- renderUI({
      if(input$twit){
        print(isolate(seuil_twoit()))
        #twitUi("id_i")
        twitUi(ns("id_i"))
        
      }})
    
    seuil_twoit<- twitServer("id_i")

    # Resultat table of model
    



    output$propositions_multi <- renderUI({
      liste_choix <- r$noms
      liste_choix <- liste_choix[-which(liste_choix == input$variable)]
      bucket_list(
        header = HTML("<h3>Variable explicatives :</h3>"),
        group_name = "bucket_list_group",
        orientation = "vertical",
        add_rank_list(
          text = "Noms des variables",
          labels = liste_choix,
          input_id = ns("choix_base")
        ),
        add_rank_list(
          text = HTML("<strong>Variables quantitatives</strong>"),
          labels = NULL,
          input_id = ns("list_quanti"),
          options = sortable_options(
            filter  = "Species",
            preventOnFilter=  T
          )
        ),
        add_rank_list(
          text =  HTML("<strong>Variables qualitatives</strong>"),
          labels = NULL,
          input_id = ns("list_quali")
        )
      )
    })



    output$result_multi<- renderUI({
      if (is.null(model_2())) {
        return()
      }
      
      type_model <-  names(which(c(
        Linéaire = "lin",
        Binomial = "binom",
        Beta = "beta",
        Poisson = "poiss"
      )=="input$type_glm"))
    
      
      fluidPage(
        
        h1(type_model),
        br(),
        fluidRow(h3(textOutput(ns("model_text")))),
        br(),
        h2("Prior :"),
        fluidRow(tableOutput(ns("model_prior"))),
        br(),
        uiOutput(ns("diag")),
        br(),
        h2("Résulats :"),
        fluidRow(tableOutput(ns("res_multi"))),
        br(),
        h2("Graphiques :"),
        fluidRow(plotOutput(ns("graph_model"))),
        br(),
        actionButton(ns("convergence"), "Analyse de convergence"),
        
      )
      
    })
    output$refactorisation<- renderUI({
      if (length(input$list_quali)==0) {
        return()
      }else{
        
       
        actionButton(ns("refact_button"), "Sélection des modalités de référence")
      }
      
    })
    
   
    observeEvent(input$refact_button, {

      nom_var_quali <-isolate(input$list_quali)
    
      showModal(
        modalDialog(title = "Séléction des références",
          tagList(lapply(1:length(nom_var_quali), function(i) {
            x <-  nom_var_quali[i]
            r$BDD[,x]<- as.factor(r$BDD[,x])
            var<- r$BDD[,x]
            var<- as.factor(var)
            noms_levels <- levels(var)
            
            
          list(
            radioButtons(inputId =ns(paste( "fact_", x, sep = "")), label= x ,choices =noms_levels,selected = noms_levels[1] )
             
            )
          })),
          footer = tagList(
            actionButton(ns("ok_fact"), "OK")

          )
        )
      )
      
  
      
  
     
    })
    
    observeEvent(input$ok_fact, {
      nom_var_quali <-isolate(input$list_quali)
      for (x in nom_var_quali) {
        
        
        r$BDD[,x]<-  relevel(r$BDD[,x], ref =   input[[paste( "fact_", x, sep = "")]])
        
      }
      removeModal()
    })
    
    randomVals <- eventReactive(input$go, {
      runif(n = 1)
    })
    
    
    
    output$model_text <- renderText({
      randomVals()

      
      formule_default(isolate(input$variable),isolate(input$list_quanti), isolate(input$list_quali))
    })

    
    
    model_2 <- reactiveVal(value = NULL)

    output$res_multi <- renderTable({
      if (is.null(model_2())) {
        return()
      }
      
      res<- model_2()$stan_summary%>%as.data.frame()%>%dplyr::select(moyenne=mean, median =`50%`, `2.5%`, `97.5%`)
      # tidyMCMC(model_2()$stanfit,
      #          conf.int = TRUE, conf.level = 0.95,
      #          robust = TRUE, rhat = TRUE, ess = TRUE
      # )%>% select(estimate,conf.low, conf.high)
    })
    
    output$graph_model <- renderPlot({
      
      if (is.null(model_2())) {
        return()
      }
      
      bayesplot::mcmc_areas(model_2() %>% as.matrix())+theme_light()
      
    })
    prior_lm <- reactiveValues(
      
      prior_intercept = NULL,
      prior_beta_scale = NULL,
      prior_beta_location = NULL
    )

    observeEvent(list(input$list_quanti, input$list_quali), {
      # reset the list
      
      prior_lm$prior_intercept = NULL
      prior_lm$prior_beta_scale = NULL
      prior_lm$prior_beta_location = NULL
    })
    
    # Button to lauch analysis
    observeEvent(input$go, {
      list_quanti = isolate(input$list_quanti)
      list_quali = isolate(input$list_quali)
      y = isolate(input$variable)
data =  r$BDD%>%select(y,list_quanti,list_quali)

if(length(list_quanti)>0) data = data%>%mutate_at(list_quanti,  ~as.numeric(as.character(.)))

if(length(list_quali)>0) data = data%>%  mutate_at(list_quali, as.factor)
      formule<- formule_default(y,list_quanti,list_quali)
      
          fit<- glm_Shiba(formule,
                          family = gaussian(link = "identity"),
                          data = r$BDD, refresh = 0,
                          prior = normal(scale = prior_lm$prior_beta_scale, location = prior_lm$prior_beta_location),#iter = 20
          )
        
      
      
     
      model_2(fit)

    })

    # Afficher les prior
    output$model_prior <- renderTable({
      if (is.null(model_2())) {
        return()
      }
      
      if(!is.null(isolate(prior_lm$prior_intercept))){
        nom_var_quali <- lapply(input$list_quali, function(x) paste(x, levels(factor(r$BDD[, x]))[-1], sep = "_")) %>% unlist()
        
        return(data.frame(Var =  c("intercept", input$list_quanti, nom_var_quali),
                   sd = c(prior_lm$prior_intercept[1],prior_lm$prior_beta_scale), 
                   mu = c(prior_lm$prior_intercept[2] ,prior_lm$prior_beta_location))
                   
        )
      }else{
        nom_var_quali <- lapply(input$list_quali, function(x) paste(x, levels(factor(r$BDD[, x]))[-1], sep = "_")) %>% unlist()
        
        return(data.frame(Var =  c("intercept", input$list_quanti, nom_var_quali),
                          sd = "default",
                          mu = "default")

        )
        
      }
     
 
    })

    
    # Afficher les diags de convergence : 
    output$diag <-  renderUI({
      if (is.null(model_2())) {
        return()
      }
      
 
     if(diag_convergence(model_2())){
      
      actionBttn(
        inputId = ns("convergence"),
        label = "Convergence", 
        style = "gradient",
        color = "success",
        icon = icon("check")
      )
      
     }else{
       
       actionBttn(
         inputId = ns("convergence"),
         label = "Convergence", 
         style = "gradient",
         color = "warning",
         icon = icon("exclamation")
       )
     }
      
      
    })
    
    
    # Action of Ellicitaion button
    observeEvent(input$ellicitation, ignoreInit = T, {
      # paste(isolate(input$variable),"~",paste(c(isolate(input$list_quanti), isolate(input$list_quali)), collapse = " + "))
      #
      # Default prior definition
      default_prior_intercept_def <- c(mean(r$BDD[, input$variable], na.rm = T), sd(r$BDD[, input$variable], na.rm = T))
      default_prior_beta_scale_def <- 2.5
      default_prior_beta_location_def <- 0

      prior_intercept_def <- ifelse_perso(is.null(prior_lm$prior_intercept), default_prior_intercept_def, prior_lm$prior_intercept)
      prior_beta_scale_def <- ifelse_perso(is.null(prior_lm$prior_beta_scale), default_prior_beta_scale_def, prior_lm$prior_beta_scale)
      prior_beta_location_def <- ifelse_perso(is.null(prior_lm$prior_beta_location), default_prior_beta_location_def, prior_lm$prior_beta_location)

      nom_var_quali <- lapply(input$list_quali, function(x) paste(x, levels(factor(r$BDD[, x]))[-1], sep = "_")) %>% unlist()

      
      
      
      
      showModal(
        modalDialog(
          tagList(lapply(1:length(c("intercept", input$list_quanti, nom_var_quali)), function(i) {
            x <- c("intercept", input$list_quanti, nom_var_quali)[i]
            prior_beta_location_def_i <- ifelse_perso(length(prior_beta_location_def) > 1, prior_beta_location_def[i - 1], prior_beta_location_def)
            prior_beta_scale_def_i <- ifelse_perso(length(prior_beta_scale_def) > 1, prior_beta_scale_def[i - 1], prior_beta_scale_def)
            list(
              plotOutput(width = 200, height = 100, ns(paste(x, "_courbe", sep = ""))),
              numericInput(ns(paste(x, "_mu_0", sep = "")), "A priori mu beta: ",
                min = -10, max = 10, value = ifelse(x == "intercept", prior_intercept_def[1], prior_beta_location_def_i)
              ),
              numericInput(ns(paste(x, "_sigma_0", sep = "")), "A priori écart-type beta : ",
                min = 0, max = 30, value = ifelse(x == "intercept", prior_intercept_def[2], prior_beta_scale_def_i)
              )
            )
          })),
          footer = tagList(
            actionButton(ns("ok"), "OK"),
            actionButton(ns("defaut"), "Défaut")
          )
        )
      )

      lapply(c("intercept", input$list_quanti, nom_var_quali), function(i) {
        output[[paste(i, "_courbe", sep = "")]] <- renderPlot({
          ggplot(data = data.frame(x = c(-20, 20)), aes(x)) +
            stat_function(fun = dnorm, args = list(mean = input[[paste(i, "_mu_0", sep = "")]], sd = input[[paste(i, "_sigma_0", sep = "")]])) +
            theme_light() +
            theme(
              axis.text.y = element_blank(),
              axis.ticks.y = element_blank(),
              axis.ticks.x = element_blank()
            ) +
            ylab("") +
            xlab(i)
        })
      })


    })
    
    observeEvent(input$defaut, {
      default_prior_intercept_def <- c(mean(r$BDD[, input$variable], na.rm = T), sd(r$BDD[, input$variable], na.rm = T))
      default_prior_beta_scale_def <- 2.5
      default_prior_beta_location_def <- 0
      
      prior_intercept_def <- ifelse_perso(is.null(prior_lm$prior_intercept), default_prior_intercept_def, prior_lm$prior_intercept)
      prior_beta_scale_def <- ifelse_perso(is.null(prior_lm$prior_beta_scale), default_prior_beta_scale_def, prior_lm$prior_beta_scale)
      prior_beta_location_def <- ifelse_perso(is.null(prior_lm$prior_beta_location), default_prior_beta_location_def, prior_lm$prior_beta_location)
      
      nom_var_quali <- lapply(input$list_quali, function(x) paste(x, levels(factor(r$BDD[, x]))[-1], sep = "_")) %>% unlist()
      
      for (x in c("intercept", input$list_quanti, nom_var_quali)) {
        updateNumericInput(session, paste(x, "_mu_0", sep = ""), value = ifelse(x == "intercept", default_prior_intercept_def[1], default_prior_beta_location_def))
        
        updateNumericInput(session, paste(x, "_sigma_0", sep = ""), value = ifelse(x == "intercept", default_prior_intercept_def[2], default_prior_beta_scale_def))
      }
    })
    
    observeEvent(input$ok, {
      nom_var_quali <- lapply(input$list_quali, function(x) paste(x, levels(factor(r$BDD[, x]))[-1], sep = "_")) %>% unlist()
      
      prior_mu <- unlist(lapply(c("intercept", input$list_quanti, nom_var_quali), function(i) {
        input[[paste(i, "_mu_0", sep = "")]]
      }))
      prior_sd <- unlist(lapply(c("intercept", input$list_quanti, nom_var_quali), function(i) {
        input[[paste(i, "_sigma_0", sep = "")]]
      }))
      
      removeModal()
      prior_lm$prior_intercept <- c(prior_mu[1], prior_sd[1])
      prior_lm$prior_beta_scale <- prior_sd[-1]
      prior_lm$prior_beta_location <- prior_mu[-1]
    })
    
    
  })
}



## To be copied in the UI
# mod_Multivarie_ui("Multivarie_1")

## To be copied in the server
# mod_Multivarie_server("Multivarie_1")
