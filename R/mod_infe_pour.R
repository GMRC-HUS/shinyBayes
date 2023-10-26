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
      titlePanel(fluidRow("Inférence univariée, pour un pourcentage", text_aide("Texte Aide sur Inférence univarié "))),
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
         fluidPage(
           fluidRow( uiOutput(ns("res_infe_prop"))),
            br(),
            fluidRow(dropdownButton(
              
              
              fluidPage(
                
                awesomeRadio(
                  inputId = ns("prior_plot"),
                  label = "Afficher Prior :", 
                  choices = c("Oui", "Non"),
                  selected = "Oui",
                  inline = TRUE
                ),
              
              awesomeRadio(
                inputId = ns("Seuil_plot"),
                label = "Afficher Seuil ou Two It :", 
                choices = c("Oui", "Non"),
                selected = "Oui",
                inline = TRUE
              ),
              
              
            sliderInput(ns("line_size"), "Largeur des lignes",min = 0,max = 10,value = 1,step = 0.1),
      
              
              colourInput(
                ns("col1"), "Couleur 1", "#DE3163",
                showColour = "background"),
              
              colourInput(
                ns("col2"),  "Couleur 2", "#40E0D0",
                showColour = "background"),
              colourInput(
                ns("col3"),  "Couleur 3", "#EBC341",
                showColour = "background"),
            colourInput(
              ns("col4"),  "Couleur 4", "#9242A6",
              showColour = "background")),
              
              
              circle = TRUE,
              status = "primary",
              icon = icon("gear"), width = "300px",
              tooltip = tooltipOptions(title = "Modifier les pamètres graphiques")
            ),br(),
            plotOutput(ns("graph_inde_prop"))
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

  
      
      tagList(fluidRow(
        ui_choix_prior_dbeta( prior_prop$prior, ns, width = 12)
      )
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
    
    
    ############ Go ############# 
    
    
  
    
  
    

    
    res_infe <- reactiveVal(value = NULL)
    observeEvent(input$go,{
      
      prior_prop<- list(nom = "nom",
                              alpha = input[[paste(prior_prop$prior$nom, "alpha", sep = "_")]],
                              beta = input[[paste(prior_prop$prior$nom, "beta", sep = "_")]])
      print(prior_prop)
      print(r$BDD[,input$var_prop])
    
      
      
      res_infe(Infe_prop2IT(r$BDD[,input$var_prop],
                         c(prior_prop$alpha, prior_prop$beta),
                         seuil_react$data$seuil,
                         twit_react,IC= input$IC/100,
                         type= seuil_comp_prop$type ))
      
      res<-res_infe()
      
      output$res_infe_prop<- renderUI({tagList(
        lapply(1:length(res$df),function(x)  box(title = names(res$df)[x], DT::dataTableOutput(ns(make.names(names(res$df)[x]))))
               
        )
      )})
      
      lapply(1:length(res$df), function(i) {
        output[[make.names(names(res$df)[i]) ]] <- DT::renderDataTable({
          DT::datatable(res$df[[i]],options = list(dom="t",ordering=F))
        })
        
        
          
      })
      
    
      
      })
    
    
    output$graph_inde_prop <- renderPlot({
      input$go
      if (is.null(res_infe())) {
        return()
      }
      p <- res_infe()$graph
      
p<- p +
      scale_color_manual(name = "Distribution",
                         breaks = c("prior", "posterior"),
                         values = c("prior" =  input$col3, "posterior" =  input$col4) )+
  scale_fill_manual(name = "Hypothèse",
                    breaks = c("Acceptée", "Rejetée"),
                    values = c("Acceptée" =  input$col2, "Rejetée" =  input$col1))
    

p$layers[[1]]$aes_params$linewidth = input$line_size
p$layers[[2]]$aes_params$linewidth = input$line_size
if(input$Seuil_plot=="Non") {
  if(length(p$layers)>=3){
    for(i in length(p$layers):3){
      p$layers[[i]]<-NULL
    }
    
  }
}
  if( input$prior_plot=="Non"){p$layers[[1]]<-NULL}



      return(p)
    })
    
    
    
    
    
    
    
    
    
    
  
    
    
   
    
    
    
  })
}
    
## To be copied in the UI
# mod_infe_pour_ui("infe_pour_1")
    
## To be copied in the server
# mod_infe_pour_server("infe_pour_1")
