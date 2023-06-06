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

mod_Multivarie_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidPage(
      titlePanel("Multivarié"),
      sidebarLayout(
        sidebarPanel(
          width = 4,
          radioButtons(
            ns("type_glm"), "Type de regression",
            c(
              Linéaire = "lin",
              Binomial = "binom",
              Beta = "beta",
              Poisson = "poiss"
            ), "lin"
          ), text_aide("Choix type de régression multivarié"),
          uiOutput(ns("choix_y")),
          uiOutput(ns("propositions_multi")),
          
          h3("Choix des priors"),
          actionButton(ns("ellicitation"), "Ellicitation"),
          
          text_aide("Texte Aide ellicitation multivarié "),
          h3("Two IT ?"),
          shinyWidgets::materialSwitch(ns("twit"), "", FALSE, status = "success", right = T),
          uiOutput(ns("twit_ui")), text_aide("Texte Aide Two IT multivarié "),
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
      selectInput(ns("variable"), "Variable d'interêt :",
        choices =
          r$noms
      )
    })



    # Resultat table of model
    



    output$propositions_multi <- renderUI({
      liste_choix <- r$noms
      liste_choix <- liste_choix[-which(liste_choix == input$variable)]
      bucket_list(
        header = "Choix des variables pour le modèle",
        group_name = "bucket_list_group",
        orientation = "vertical",
        add_rank_list(
          text = "Noms des variables",
          labels = liste_choix,
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
        h2("Résulats :"),
        fluidRow(tableOutput(ns("res_multi"))),
        br(),
        h2("Graphiques :"),
        fluidRow(plotOutput(ns("graph_model"))),
        br(),
        actionButton(ns("convergence"), "Analyse de convergence"),
        
      )
      
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
      tidyMCMC(model_2()$stanfit,
               conf.int = TRUE, conf.level = 0.95,
               robust = TRUE, rhat = TRUE, ess = TRUE
      )
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

    
    # Button to lauch analysis
    observeEvent(input$go, {
      print(prior_lm$prior_beta_scale)
      print(prior_lm$prior_beta_location)
      formule<- formule_default(isolate(input$variable),isolate(input$list_quanti), isolate(input$list_quali))
      
      if (is.null(prior_lm$prior_intercept)) {
        if (!is.null(prior_lm$prior_beta_scale) & !is.null(prior_lm$prior_beta_location)) {
          model_2(stan_glm(formule,
            family = gaussian(link = "identity"),
            data = r$BDD, refresh = 0,
            prior = normal(scale = prior_lm$prior_beta_scale, location = prior_lm$prior_beta_location)
          ))
        } else {
          model_2(stan_glm(formule,
            family = gaussian(link = "identity"),
            data = r$BDD, refresh = 0
          ))
        }
      } else if (!is.null(prior_lm$prior_beta_scale) & !is.null(prior_lm$prior_beta_location)) {
        model_2(stan_glm(formule,
          family = gaussian(link = "identity"),
          data = r$BDD, refresh = 0,
          prior_intercept = normal(prior_lm$prior_intercept[1], prior_lm$prior_intercept[2]),
          prior = normal(scale = prior_lm$prior_beta_scale, location = prior_lm$prior_beta_location)
        ))
      } else {
        model_2(stan_glm(formule,
          family = gaussian(link = "identity"),
          data = r$BDD, refresh = 0,
          prior_intercept = normal(location = prior_lm$prior_intercept[1], scale = prior_lm$prior_intercept[2])
        ))
      }
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

      observeEvent(input$defaut, {
        for (x in c("intercept", input$list_quanti, nom_var_quali)) {
          updateNumericInput(session, paste(x, "_mu_0", sep = ""), value = ifelse(x == "intercept", default_prior_intercept_def[1], default_prior_beta_location_def))

          updateNumericInput(session, paste(x, "_sigma_0", sep = ""), value = ifelse(x == "intercept", default_prior_intercept_def[2], default_prior_beta_scale_def))
        }
      })

      observeEvent(input$ok, {
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
  })
}

## To be copied in the UI
# mod_Multivarie_ui("Multivarie_1")

## To be copied in the server
# mod_Multivarie_server("Multivarie_1")
