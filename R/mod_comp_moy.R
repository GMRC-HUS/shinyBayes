#' comp_moy UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_comp_moy_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns('mod_comp_moy'))
  )
}
    
#' comp_moy Server Functions
#'
#' @noRd 
mod_comp_moy_server <- function(id,r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    
    
    
    
    mod_comp_moy <- fluidPage(  
    
    ###################################################
    #####   PAGE 3.2     ###############################
    ###################################################
    
    
    titlePanel("Comparaison de Moyennes"),
    sidebarLayout(
      sidebarPanel(width = 3,
                   h2("Variable de groupe :"),
                   uiOutput(ns("vbl_grp")),
                   br(),
                   h2("Variable quantitative :"),
                   uiOutput(ns("vbl_moy")), 
                   h3("Choix des priors"),
                   actionButton(ns("ellicitation"), "Ellicitation"),
                   br(),
                   sliderInput(ns("IC"),label = "Intervalle de Crédibilité en %",min = 80,max = 100,step = 1,animate = F,post = " %",value = 95),
                   h3("Seuils/Two IT ?"), #text_aide("Texte Aide Two IT multivarié "),
                   # shinyWidgets::materialSwitch(ns("twit"), "", value =FALSE, status = "success", right = T),
                   uiOutput(ns("twit_ui")),
                   br(),
                   actionButton(ns("go"), "Go :")
                   
                   
                   
      ),
      
      mainPanel(
        
        
        # tags$head(tags$style(".butt{background-color:#E9967A;} .butt{color: black;}")),
        fluidPage(
          uiOutput(ns("res_crois_moy"))
        )
        # fluidRow(
        #   column(6,align="center", uiOutput(ns("descriptifUni")), br(), tableOutput(ns("descvar"))%>% withSpinner()),
        #   column(6,align="center", plotOutput(ns("plot1"))%>% withSpinner(), plotOutput(ns("plot2"))%>% withSpinner())
        # ) # fin fluid row du main panel
      ) # fin MainPanel
    ) # fin sidebarlayout
  )
 
  
  
  pasDeBase <- pasDeBase_ui()
  
  
  
  
  observe({
    output$mod_comp_moy <- renderUI({
      if (!r$BASEchargee) {
        do.call(tabPanel, pasDeBase)
      } else {
        do.call(tabPanel, mod_comp_moy)
      }
    })
  })
  
  output$vbl_grp <- renderUI({
    choix_var <-r$BDD%>%apply(2, function(x) between(nlevels(as.factor(x)),2,4))%>%which()%>%names
    if(length(choix_var)==0){ return(h3("Pas de variables avec 2 à 4 groupes dans la base"))}
    return(
      selectInput(
        inputId = ns("var_grp"),
        label = "", 
        choices = choix_var
      )
      
    )
    
  })
  #
  output$vbl_moy <- renderUI({
    choix_var <-r$BDD%>%sapply( function(x) (is.numeric(x) & (length(unique(x))>10)))%>%which()%>%names
    print(choix_var)
    if(length(choix_var)==0){ return(h3("Pas de variables numérique"))}
    return(
      selectInput(
        inputId = ns("var_moy"),
        label = "", 
        choices = choix_var
      )
      
    )
    
    
  })
  
  
  prior_moy <- reactiveValues(
    prior=NULL
  )
  
  observeEvent(c(input$var_grp,input$var_moy), {
    prior_moy$prior <- NULL
    
  })
  
  
  
  ############### Ellicitation #########
  observeEvent(input$ellicitation, ignoreInit = T, {
    
    if (is.null(prior_moy$prior)) {
      var_grp <- r$BDD[,input$var_grp]
      moy_tot = round(mean(r$BDD[,input$var_moy],na.rm=T),2)
      sd_tot = round(sd(r$BDD[,input$var_moy],na.rm=T),2)
      prior_moy$prior<-lapply(levels(as.factor(var_grp)), function(x) list(nom=paste(input$var_grp,x, sep="_"), 
                                                                           mu_mu = moy_tot, mu_sd = sd_tot,sd_shape = 1, sd_rate = 1))
    }
    print(prior_moy$prior)
    
    showModal(
      modalDialog(size = "l",
                  tagList(lapply(prior_moy$prior, function(i) ui_choix_prior_norm2(i, ns)
                  ),
                  lapply(prior_moy$prior, function(i) ui_choix_prior_dgamma(i, ns ))),
                  footer = tagList(
                    actionButton(ns("ok"), "OK"),
                    actionButton(ns("defaut"), "Défaut")
                  )
      )
    )
    
    lapply(prior_moy$prior, function(i) {
      output[[paste(i$nom, "_courbe", sep = "")]] <- ui_ggplot_prior_norm2(i,input)
      output[[paste(i$nom, "_courbe_gamma", sep = "")]] <- ui_ggplot_prior_dgamma(i,input)
    })
    
  })
  
  
  observeEvent(input$defaut, {
    
    var_grp <- r$BDD[,input$var_grp]
    moy_tot = round(mean(r$BDD[,input$var_moy],na.rm=T),2)
    sd_tot = round(sd(r$BDD[,input$var_moy],na.rm=T),2)
    prior_moy$prior<-lapply(levels(as.factor(var_grp)), function(x) list(nom=paste(input$var_grp,x, sep="_"),mu_mu = moy_tot, mu_sd = sd_tot,sd_shape = 1, sd_rate = 1))
    
    for (x in prior_moy$prior) {
      updateNumericInput(session, paste0(x$nom, "_mu"), value = x$mu_mu)
      updateNumericInput(session, paste0(x$nom, "_sd"), value = x$mu_sd)
      
      updateNumericInput(session, paste0(x$nom, "_alpha"), value = x$sd_shape)
      updateNumericInput(session, paste0(x$nom, "_beta"), value = x$sd_rate)
      
    }
  })
  
  observeEvent(input$ok, {
    
    prior_moy$prior<-  lapply( prior_moy$prior, function(i) {
      list(nom = i$nom,
           mu_mu = input[[paste(i$nom, "mu", sep = "_")]],
          mu_sd = input[[paste(i$nom, "sd", sep = "_")]],
           sd_shape = input[[paste(i$nom, "alpha", sep = "_")]],
          sd_rate=        input[[paste(i$nom, "alpha", sep = "_")]]
           )
    })
    removeModal()
    
  })
  
  
  ########### Seuil ...#########
  
  
  output$twit_ui <- renderUI({
    ui_twit("seuil_2it", ns)
  })
  #
  
  #
  #   # var_input2
  observeEvent(c(input$var_grp,input$var_moy), {
    output$twit_ui <- renderUI({
      ui_twit("seuil_2it", ns)
    })
   
    if (is.null(prior_moy$prior)) {
      var_grp <- r$BDD[,input$var_grp]
      moy_tot = round(mean(r$BDD[,input$var_moy],na.rm=T),2)
      sd_tot = round(sd(r$BDD[,input$var_moy],na.rm=T),2)
      prior_moy$prior<-lapply(levels(as.factor(var_grp)), function(x) list(nom=paste(input$var_grp,x, sep="_"), mu_mu = moy_tot, mu_sd = sd_tot,sd_shape = 1, sd_rate = 1))
      
    }
    
    var <- sapply(prior_moy$prior, function(x) x$nom)
    #seuil_twoit(twitServer_prop("id_i", var))

    twitServer_moy("twit_seuil",twit_react,
                   seuil_react,seuil_react_diff,group=levels(as.factor(r$BDD[,input$var_grp])),
                   seuil_comp_moy)
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
    
    
    twitUi_prop(ns("twit_seuil"))
    
  })
  
  
  twit_react <- reactiveValues(data = { 
    NULL
  }, var = NULL)
  seuil_react_diff<- reactiveValues(data = { 
    NULL
  })
  seuil_react <- reactiveValues(data = { 
    NULL
  })

  seuil_comp_moy<-reactiveValues(type = { 
    NULL
  },
  plusieurs = {NULL})
  
    ##### Analyse ####
  
  observeEvent(input$go,{
    dput(list(twit_react$data, twit_react$var))
    dput(list(seuil_comp_moy$type))
    dput(seuil_react$data)
    
    
    if (is.null(prior_moy$prior)) {
      var_grp <- r$BDD[,input$var_grp]
      moy_tot = round(mean(r$BDD[,input$var_moy],na.rm=T),2)
      sd_tot = round(sd(r$BDD[,input$var_moy],na.rm=T),2)
      prior_moy$prior<-lapply(levels(as.factor(var_grp)), function(x) list(nom=paste(input$var_grp,x, sep="_"), mu_mu = moy_tot, mu_sd = sd_tot,sd_shape = 1, sd_rate = 1))
      
    }
    
    priors =  prior_moy$prior%>%rbindlist()
   

    
    print(seuil_react_diff$data)
    res<-compare_moy_gibbs(r$BDD[,input$var_moy],  r$BDD[,input$var_grp],priors$mu_mu,priors$mu_sd,
                           priors$sd_shape,priors$sd_rate,
                           seuil_global=seuil_react$data, type = seuil_comp_moy$type, 
                           plusieurs = seuil_comp_moy$plusieurs,seuild = seuil_react_diff$data,twit = twit_react)
      
      print(res)
      
      # function(X, Y,mu0=NULL,s_m0 =NULL, s0=NULL, n_s0=NULL,
      #          type =  NULL, plusieurs = NULL, seuild=NULL, twit=NULL,seuil_global = NULL,
      #          n_sample =100000,IC=0.95, arr=3)

    lapply(1:length(res), function(x) print(names(res)[x]))
    output$res_crois_moy<- renderUI({tagList(
      lapply(1:length(res),function(x)  box(title = names(res)[x],width=12,
                                            div(  DT::dataTableOutput(ns(make.names(names(res)[x]))),style
                                                  = "overflow-y: auto;"))
             
      )
    )})
    lapply(1:length(res), function(i) {
      output[[make.names(names(res)[i]) ]] <- DT::renderDataTable({
        DT::datatable(res[[i]],options = list(dom="t",ordering=F))
      })
      
      
      
    })
    
    
    
    
  })
  
}
)}
    
## To be copied in the UI
# mod_comp_moy_ui("comp_moy_1")
    
## To be copied in the server
# mod_comp_moy_server("comp_moy_1")
