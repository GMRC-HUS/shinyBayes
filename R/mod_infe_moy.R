#' infe_moy UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_infe_moy_ui <- function(id){  ns <- NS(id)
tagList(
  uiOutput(ns('mod_inf_moy'))
  
  # fin fluidpage
)}
    
#' infe_moy Server Functions
#'
#' @noRd 
mod_infe_moy_server <- function(id,r){
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    
    
    
    mod_inf_moy<- fluidPage(
      titlePanel(fluidRow("Inférence univariée, pour une moyenne", text_aide("Texte Aide sur Inférence univarié "))),
      sidebarLayout(
        sidebarPanel(
          width = 4,
          h2("Variable moyenne d'interêt :"),
          uiOutput(ns("vbl_moy")), 
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
            fluidRow( uiOutput(ns("res_infe_moy"))),
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
            plotOutput(ns("graph_inde_moy"))
            )
          ) # fin fluid row du main panel
        ) # fin MainPanel
      ) # fin sidebarlayout
    )
    
    pasDeBase <- pasDeBase_ui()
    
    
    
    
    observe({
      output$mod_inf_moy <- renderUI({
        if (!r$BASEchargee) {
          do.call(tabPanel, pasDeBase)
        } else if(length(r$BDD%>%sapply( function(x) (is.numeric(x) & (length(unique(x))>10)))%>%which()%>%names)==0){
          do.call(tabPanel, pasDeBase_ui("Pas de variable de type quantitative"))
          
          
        }else {
          do.call(tabPanel, mod_inf_moy)
        }
      })
    })
    
    ########################################################################################################################
    ####    OUTPUT : Inférence univarie
    ########################################################################################################################
    
    
    output$vbl_moy <- renderUI({
      choix_var <-r$BDD%>%sapply( function(x) (is.numeric(x) & (length(unique(x))>10)))%>%which()%>%names
      print(choix_var)
      if(length(choix_var)==0){ return(h3("Pas de variable de type quantitative dans la base"))}
      return(
        pickerInput(
          inputId = ns("var_moy"),
          label = "", 
          choices = choix_var
        )
        
      )
      
      
    })
    
    
    prior_moy <- reactiveValues(
      prior=NULL
    )
    
    observeEvent(c(input$var_moy), {
      
      var_moy <- r$BDD[,input$var_moy]
      moy_tot = mean(r$BDD[,input$var_moy],na.rm=T)
      sd_tot = sd(r$BDD[,input$var_moy],na.rm=T)
      prior_moy$prior<-list(nom=input$var_moy,mu_mu = moy_tot, mu_sd = sd_tot,sd_shape = 1, sd_rate = 1)
      output[[paste(input$var_moy, "_courbe", sep = "")]] <-ui_ggplot_prior_norm2(prior_moy$prior,input)
      output[[paste(input$var_moy, "_courbe_gamma", sep = "")]] <- ui_ggplot_prior_dgamma(prior_moy$prior,input)
      
    })
    
    
    output$apriori <- renderUI({
      
      
      
      tagList(fluidRow(
        ui_choix_prior_norm2( prior_moy$prior, ns, width = 12),
        ui_choix_prior_dgamma(prior_moy$prior, ns, width = 12)
      )
      )
      
    })
    observeEvent(input$defaut, {
      
      var_moy <- r$BDD[,input$var_moy]
      moy_tot = mean(r$BDD[,input$var_moy],na.rm=T)
      sd_tot = sd(r$BDD[,input$var_moy],na.rm=T)
      prior_moy$prior<-list(nom=input$var_moy,mu_mu = moy_tot, mu_sd = sd_tot,sd_shape = 1, sd_rate = 1)
      
      

      updateNumericInput(session, paste0(input$var_moy, "_mu"), value = prior_moy$prior$mu_mu)
      updateNumericInput(session, paste0(input$var_moy, "_sd"), value = prior_moy$prior$mu_sd)
      
      updateNumericInput(session, paste0(input$var_moy, "_alpha"), value = prior_moy$prior$sd_shape)
      updateNumericInput(session, paste0(input$var_moy, "_beta"), value = prior_moy$prior$sd_rate)
      
      
    })
    
    
    
    ########## Seuil / Twit ########
    
    
    
    output$twit_ui <- renderUI({
      ui_twit("seuil_2it", ns)
    })
    #
    
    #
    #   # var_input2
    observeEvent(input$var_moy, {
      output$twit_ui <- renderUI({
        ui_twit("seuil_2it", ns)
      })
      
      
      
      var <-input$var_moy
      #seuil_twoit(twitServer_moy("id_i", var))
      
      
      #### A faire ici 
      twitServer_prop_infe("twit_seuil",twit_react,
                           seuil_react,
                           seuil_comp_moy,prop=F)
      
      
      
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
      
      ###  a faire
      twitUi_prop_infe(ns("twit_seuil"))
      
    })
    
    
    twit_react <- reactiveValues(data = { 
      NULL
    }, var = NULL)
    
    seuil_react <- reactiveValues(data = { 
      NULL
    })
    
    seuil_comp_moy<-reactiveValues(type = { 
      NULL
    },
    plusieurs = {NULL})
    
    
    ############ Go ############# 
    
    
    
    
    
    
    
    
    res_infe <- reactiveVal(value = NULL)
    observeEvent(input$go,{
      
      
      print(input)
      prior_moy<- list(nom = "nom",
                        mu_mu = input[[paste(prior_moy$prior$nom, "mu", sep = "_")]],
                       mu_sd = input[[paste(input$var_moy, "sd", sep = "_")]],
                       sd_shape = input[[paste(input$var_moy, "alpha", sep = "_")]],
                        sd_rate = input[[paste(input$var_moy, "beta", sep = "_")]])
      print(prior_moy)
      print(r$BDD[,input$var_moy])
      
      

      res_infe(Infe_moy2IT(r$BDD[,input$var_moy],
                           c(prior_moy$mu_mu,prior_moy$mu_sd,
                             prior_moy$sd_shape, prior_moy$sd_rate),
                            seuil_react$data$seuil,
                            twit_react,IC= input$IC/100,
                            type= seuil_comp_moy$type ))
print("cest ok")
      res<-res_infe()
      
      output$res_infe_moy<- renderUI({tagList(
        lapply(1:length(res$df),function(x)  box(title = names(res$df)[x], DT::dataTableOutput(ns(make.names(names(res$df)[x]))))
               
        )
      )})
      
      lapply(1:length(res$df), function(i) {
        output[[make.names(names(res$df)[i]) ]] <- DT::renderDataTable({
          DT::datatable(res$df[[i]],options = list(dom="t",ordering=F))
        })
        
        
        
      })
      
      
      
    })
    
    
    output$graph_inde_moy <- renderPlot({
      input$go
      if (is.null(res_infe())) {
        return()
      }
      p <- res_infe()$graph[[1]]
      
      p<- p +
        scale_color_manual(name = "Distribution",
                           breaks = c("prior", "posterior"),
                           values = c("prior" =  input$col3, "posterior" =  input$col4) )+
        scale_fill_manual(name = "Hypothèse",
                          breaks = c("Acceptée", "Rejetée"),
                          values = c("Acceptée" =  input$col2, "Rejetée" =  input$col1))
      
      
      # # p$layers[[1]]$aes_params$linewidth = input$line_size
      # # p$layers[[2]]$aes_params$linewidth = input$line_size
      # if(input$Seuil_plot=="Non") {
      #   if(length(p$layers)>=3){
      #     for(i in length(p$layers):3){
      #       p$layers[[i]]<-NULL
      #     }
      #     
      #   }
      # }
      # if( input$prior_plot=="Non"){p$layers[[1]]<-NULL}
      
      
      
      return(p)
    })
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
  })
}
    



## To be copied in the UI
# mod_infe_moy_ui("infe_moy_1")
    
## To be copied in the server
# mod_infe_moy_server("infe_moy_1")







