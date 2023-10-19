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
    
    
    titlePanel("Comparaison de Moyenne"),
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
                   h3("Seuils/Two IT ?"), text_aide("Texte Aide Two IT multivarié "),
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
      pickerInput(
        inputId = ns("var_grp"),
        label = "", 
        choices = choix_var
      )
      
    )
    
  })
  #
  output$vbl_moy <- renderUI({
    choix_var <-r$BDD%>%apply(2, function(x) (is.numeric(x) & length(unique(x))>10))%>%which()%>%names
    print(choix_var)
    if(length(choix_var)==0){ return(h3("Pas de variables numérique"))}
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
  
  observeEvent(c(input$var_grp,input$var_moy), {
    prior_moy$prior <- NULL
    
  })
  
  
  
  ############### Ellicitation #########
  observeEvent(input$ellicitation, ignoreInit = T, {
    
    if (is.null(prior_moy$prior)) {
      var_grp <- r$BDD[,input$var_grp]
      moy_tot = mean(r$BDD[,input$var_moy],na.rm=T)
      sd_tot = sd(r$BDD[,input$var_moy],na.rm=T)
      prior_moy$prior<-lapply(levels(as.factor(var_grp)), function(x) list(nom=paste(input$var_grp,x, sep="_"), mu=c(moy_tot,sd_tot),sd=c(1,1)))
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
    moy_tot = mean(r$BDD[,input$var_moy],na.rm=T)
    sd_tot = sd(r$BDD[,input$var_moy],na.rm=T)
    prior_moy$prior<-lapply(levels(as.factor(var_grp)), function(x) list(nom=paste(input$var_grp,x, sep="_"),mu=c(moy_tot,sd_tot),sd=c(1,1)))
    
    for (x in prior_moy$prior) {
      updateNumericInput(session, paste0(x$nom, "_mu"), value = x$mu_mu)
      updateNumericInput(session, paste0(x$nom, "_sd"), value = x$mu_sd)
      
      updateNumericInput(session, paste0(x$nom, "_alpha"), value = x$sd[1])
      updateNumericInput(session, paste0(x$nom, "_beta"), value = x$sd[2])
      
    }
  })
  
  observeEvent(input$ok, {
    
    prior_moy$prior<-  lapply( prior_moy$prior, function(i) {
      list(nom = i$nom,
           mu = c(input[[paste(i$nom, "mu", sep = "_")]],input[[paste(i$nom, "sd", sep = "_")]]),
           sd = c(input[[paste(i$nom, "alpha", sep = "_")]],input[[paste(i$nom, "alpha", sep = "_")]])
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
      moy_tot = mean(r$BDD[,input$var_moy],na.rm=T)
      sd_tot = sd(r$BDD[,input$var_moy],na.rm=T)
      prior_moy$prior<-lapply(levels(as.factor(var_grp)), function(x) list(nom=paste(input$var_grp,x, sep="_"), mu=c(moy_tot,sd_tot),sd=c(1,1)))
      
    }
    
    var <- sapply(prior_moy$prior, function(x) x$nom)
    #seuil_twoit(twitServer_prop("id_i", var))
    twitServer_prop("twit_seuil",twit_react,
                    seuil_react,seuil_react_diff,seuil_react_OR,seuil_react_RR, group=levels(as.factor(r$BDD[,input$var_grp])),
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
    
    
    twitUi_prop(ns("twit_seuil"))
    
  })
  
  
  twit_react <- reactiveValues(data = { 
    NULL
  }, var = NULL)
  
  seuil_react <- reactiveValues(data = { 
    NULL
  })
  seuil_react_diff<- reactiveValues(data = { 
    NULL
  })
  seuil_react_OR<-reactiveValues(data = { 
    NULL
  })
  
  seuil_react_RR<-reactiveValues(data = { 
    NULL
  })
  seuil_comp_mean<-reactiveValues(type = { 
    NULL
  },
  plusieurs = {NULL})
  
  
  
  
  
  
  
  ##### Analyse ####
  
  observeEvent(input$go,{
    print(twit_react$var)
    print(seuil_comp_prop$type)
    
    if (is.null(prior_moy$prior)) {
      var_grp <- r$BDD[,input$var_grp]
      moy_tot = mean(r$BDD[,input$var_moy],na.rm=T)
      sd_tot = sd(r$BDD[,input$var_moy],na.rm=T)
      prior_moy$prior<-lapply(levels(as.factor(var_grp)), function(x) list(nom=paste(input$var_grp,x, sep="_"), mu=c(moy_tot,sd_tot),sd=c(1,1)))
      
    }
    
    priors =  prior_moy$prior%>%rbindlist()%>%dplyr::select(mu, sd)%>%t
    print(r$BDD[,input$var_grp])
    prepa_prior = sapply(priors$prior_moy$prior
    res<-compare_moy_gibbs(r$BDD[,input$var_moy],  r$BDD[,input$var_grp],c(0,0),c(1,1),c(1,1),c(1,1),type="seuil",plusieurs =F,seuil_global = 0)
      
      
      
      
      Cpmultprop2IT(Y = r$BDD[,input$var_moy], Gr = r$BDD[,input$var_grp],priors = priors,
                        seuil_global = seuil_react$data,
                        seuild=seuil_react_diff$data,seuilr=seuil_react_RR$data, seuilo=seuil_react_OR$data, 
                        twit=twit_react, 
                        arr=3,M=100000, IC=0.95, type =  seuil_comp_prop$type, plusieurs = seuil_comp_prop$plusieurs
                        
    ))
    lapply(1:length(res), function(x) print(names(res)[x]))
    output$res_crois_moy<- renderUI({tagList(
      lapply(1:length(res),function(x)  box(title = names(res)[x], DT::dataTableOutput(ns(make.names(names(res)[x]))))
             
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
