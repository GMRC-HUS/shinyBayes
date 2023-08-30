#' infe_pour UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_infe_pour_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns('mod_inf_pour'))
    
     # fin fluidpage
  )
}
    
#' infe_pour Server Functions
#'
#' @noRd 
mod_infe_pour_server <-  function(id,r){
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    
    
    
    mod_inf_pour<- fluidPage(
      titlePanel(fluidRow("Inférence univarié, pour un pourcentage", text_aide("Texte Aide sur Inférence univarié "))),
      sidebarLayout(
        sidebarPanel(
          width = 4,
          h2("Variable proportion d'interêt :"),
          uiOutput(ns("vbl_prop")), 
          h3("Choix des priors"),
          uiOutput(ns("apriori")),
          actionButton(ns("defaut"), "Défaut"),
          sliderInput(ns("IC"),label = "Intervalle de Crédibilité en %",min = 80,max = 100,step = 1,animate = F,post = " %",value = 95),
          h3("Seuils/Two IT ?"), text_aide("Texte Aide Two IT multivarié "),
          # shinyWidgets::materialSwitch(ns("twit"), "", value =FALSE, status = "success", right = T),
          uiOutput(ns("twit_ui")),
          br(),
          actionButton(ns("go"), "Go :")
          
        ),
        mainPanel(
          tags$head(tags$style(".butt{background-color:#E9967A;} .butt{color: black;}")),
          fluidRow(
            h2(textOutput(ns("nameVariable"))),
            # column(6,
            uiOutput(ns("inferenceUni")), br() # ,  #tableOutput(ns("descvar"))
            #                                                            ),
            ,
            column(
              6,
              plotOutput(ns("plotinferenceUni"))
            )
          ) # fin fluid row du main panel
        ) # fin MainPanel
      ) # fin sidebarlayout
    )
    
    pasDeBase <- pasDeBase_ui()
    
    
    
    
    observe({
      output$mod_inf_pour <- renderUI({
        if (!r$BASEchargee) {
          do.call(tabPanel, pasDeBase)
        } else if(length(r$BDD%>%apply(2, function(x) nlevels(as.factor(x))==2)%>%which()%>%names)==0){
          do.call(tabPanel, pasDeBase_ui("Pas de variable de type proportion"))
          
          
        }else {
          do.call(tabPanel, mod_inf_pour)
        }
      })
    })
    
    ########################################################################################################################
    ####    OUTPUT : Inférence univarie
    ########################################################################################################################
    
    
    output$vbl_prop <- renderUI({
      choix_var <-r$BDD%>%apply(2, function(x) nlevels(as.factor(x))==2)%>%which()%>%names
      print(choix_var)
      if(length(choix_var)==0){ return(h3("Pas de variables avec 2 à 4 groupes dans la base"))}
      return(
        pickerInput(
          inputId = ns("var_prop"),
          label = "", 
          choices = choix_var
        )
        
      )
      
      
    })
    
    
    prior_prop <- reactiveValues(
      prior=NULL
    )
    
    observeEvent(c(input$var_prop), {
    
      var_prop <- r$BDD[,input$var_prop]
      prior_prop$prior<-list(nom=input$var_prop, alpha=0.5,beta=0.5)
      output[[paste(input$var_prop, "_courbe", sep = "")]] <- ui_ggplot_prior_dbeta(prior_prop$prior,input)
      
    })
    
    
    output$apriori <- renderUI({

  
      
      tagList(
        ui_choix_prior_dbeta( prior_prop$prior, ns)
        
      )
     
    })
    observeEvent(input$defaut, {
      
      var_prop <- r$BDD[,input$var_prop]
      prior_prop$prior<-list(nom=input$var_prop, alpha=0.5,beta=0.5)
      
      
      updateNumericInput(session, paste0(input$var_prop, "_alpha"), value =  prior_prop$prior$alpha)
      updateNumericInput(session, paste0(input$var_prop, "_beta"), value =  prior_prop$prior$beta)
      
      
    })
    
    
    
    ########## Seuil / Twit ########
    
    
    
    output$twit_ui <- renderUI({
      ui_twit("seuil_2it", ns)
    })
    #
    
    #
    #   # var_input2
    observeEvent(input$var_prop, {
      output$twit_ui <- renderUI({
        ui_twit("seuil_2it", ns)
      })
      
  
      
      var <-input$var_prop
      #seuil_twoit(twitServer_prop("id_i", var))
      twitServer_prop_infe("twit_seuil",twit_react,
                      seuil_react,
                      seuil_comp_prop)
    })
    

    
    observeEvent(input$seuil_2it, {
      
      
      output$twit_ui <- renderUI({
        actionBttn(
          inputId = ns("seuil_2it"),
          label = "Définition Seuil ou Two It",
          style = "gradient",
          color = "success",
          icon = icon("check")
        )
        
      })
      
      
      twitUi_prop_infe(ns("twit_seuil"))
      
    })
    
    
    twit_react <- reactiveValues(data = { 
      NULL
    }, var = NULL)
    
    seuil_react <- reactiveValues(data = { 
      NULL
    })
  
    seuil_comp_prop<-reactiveValues(type = { 
      NULL
    },
    plusieurs = {NULL})
    
    
    ############ A Trier ############# 
    
    
  
    
  
    

    
    
    observeEvent(input$go,{
      
      prior_prop<- list(nom = "nom",
                              alpha = input[[paste(prior_prop$prior$nom, "alpha", sep = "_")]],
                              beta = input[[paste(prior_prop$prior$nom, "beta", sep = "_")]])
      print(prior_prop)
      print(input$var_grp)
      print(seuil_react$data$seuil)
      
      
      print(Infe_prop2IT(r$BDD[,input$var_grp],
                         c(prior_prop$alpha, prior_prop$beta),
                         seuil_react$data$seuil,
                         twit_react,
                         type= seuil_comp_prop$type ))
      
      })
    
    
    
    
    
    
    
    
    
    
    
  
    
    
   
    
    
    output$plotinferenceUni <- renderPlot(plot(fitInference()))
    
    output$priorSigma <- renderPlot({
      ggplot(data = data.frame(x = c(0, 1)), aes(x)) +
        stat_function(fun = dgamma, n = 101, args = list(shape = input$alpha_0, rate = 1 / input$beta_0)) +
        theme_light() +
        theme(
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.ticks.x = element_blank()
        ) +
        ylab("") +
        xlab("")
    })
    
    
    
    
    output$plotinferenceUni <- renderPlot(plot(fitInference()))
    
    output$priorSigma <- renderPlot({
      ggplot(data = data.frame(x = c(0, input$alpha_0 * 4)), aes(x)) +
        stat_function(fun = dgamma, n = 101, args = list(shape = input$alpha_0, rate = input$beta_0)) +
        theme_light() +
        theme(
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.ticks.x = element_blank()
        ) +
        ylab("") +
        xlab("")
    })
    
    output$priorMean <- renderPlot({
      x <- r$BDD[, input$variable]
      x <- x[!is.na(x)]
      min_x <- min(x)
      max_x <- max(x)
      ggplot(data = data.frame(x = c(min_x - max_x, 2 * max_x)), aes(x)) +
        stat_function(fun = dnorm, n = 101, args = list(mean = input$mu0, sd = 1 / input$k0)) +
        theme_light() +
        theme(
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.ticks.x = element_blank()
        ) +
        ylab("") +
        xlab("")
    })
  })
}
    
## To be copied in the UI
# mod_infe_pour_ui("infe_pour_1")
    
## To be copied in the server
# mod_infe_pour_server("infe_pour_1")
