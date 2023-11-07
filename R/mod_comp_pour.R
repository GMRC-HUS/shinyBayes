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
           uiOutput(ns("res_crois_prop"))
         )
          # fluidRow(
          #   column(6,align="center", uiOutput(ns("descriptifUni")), br(), tableOutput(ns("descvar"))%>% withSpinner()),
          #   column(6,align="center", plotOutput(ns("plot1"))%>% withSpinner(), plotOutput(ns("plot2"))%>% withSpinner())
          # ) # fin fluid row du main panel
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
        selectInput(
          inputId = ns("var_grp"),
          label = "", 
          choices = choix_var
        )
        
      )
      
    })
      #
      output$vbl_prop <- renderUI({
        choix_var <-r$BDD%>%apply(2, function(x) nlevels(as.factor(x))==2)%>%which()%>%names
        print(choix_var)
        if(length(choix_var)==0){ return(h3("Pas de variables avec 2 à 4 groupes dans la base"))}
        return(
          selectInput(
            inputId = ns("var_prop"),
            label = "", 
            choices = choix_var
          )
          
        )
        
        
      })
      
      
      prior_prop <- reactiveValues(
      prior=NULL
      )
      
      observeEvent(c(input$var_grp,input$var_prop), {
        prior_prop$prior <- NULL

      })
      
      
      
      ############### Ellicitation #########
 observeEvent(input$ellicitation, ignoreInit = T, {

        if (is.null(prior_prop$prior)) {
          var_grp <- r$BDD[,input$var_grp]
          prior_prop$prior<-lapply(levels(as.factor(var_grp)), function(x) list(nom=paste(input$var_grp,x, sep="_"), alpha=0.5,beta=0.5))
        }
        
   
          showModal(
            modalDialog(size = "l",
                        tagList(lapply(prior_prop$prior, function(i) ui_choix_prior_dbeta(i, ns )
                        )),
                        footer = tagList(
                          actionButton(ns("ok"), "OK"),
                          actionButton(ns("defaut"), "Défaut")
                        )
            )
          )

          lapply(prior_prop$prior, function(i) {
            output[[paste(i$nom, "_courbe", sep = "")]] <- ui_ggplot_prior_dbeta(i,input)
          })
        
          })
      
    
  observeEvent(input$defaut, {
    
    var_grp <- r$BDD[,input$var_grp]
    prior_prop$prior<-lapply(levels(as.factor(var_grp)), function(x) list(nom=paste(input$var_grp,x, sep="_"), alpha=0.5,beta=0.5))
    
    for (x in prior_prop$prior) {
    updateNumericInput(session, paste0(x$nom, "_alpha"), value = x$alpha)
    updateNumericInput(session, paste0(x$nom, "_beta"), value = x$beta)
    
  }
  })
  
  observeEvent(input$ok, {

    prior_prop$prior<-  lapply( prior_prop$prior, function(i) {
      list(nom = i$nom,
      alpha = input[[paste(i$nom, "alpha", sep = "_")]],
      beta = input[[paste(i$nom, "beta", sep = "_")]])
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
  observeEvent(c(input$var_grp,input$var_prop), {
    output$twit_ui <- renderUI({
      ui_twit("seuil_2it", ns)
    })
    
    if (is.null(prior_prop$prior)) {
      var_grp <- r$BDD[,input$var_grp]
      prior_prop$prior<-lapply(levels(as.factor(var_grp)), function(x) list(nom=paste(input$var_grp,x, sep="_"), alpha=0.5,beta=0.5))
    }
    
    var <- sapply(prior_prop$prior, function(x) x$nom)
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
    seuil_comp_prop<-reactiveValues(type = { 
      NULL
    },
    plusieurs = {NULL})

  
 
  
  
  
  
##### Analyse ####
  
  observeEvent(input$go,{
    print(twit_react$var)
    print(seuil_comp_prop$type)
    
    if (is.null(prior_prop$prior)) {
      var_grp <- r$BDD[,input$var_grp]
      prior_prop$prior<-lapply(levels(as.factor(var_grp)), function(x) list(nom=paste(input$var_grp,x, sep="_"), alpha=0.5,beta=0.5))
    }
    
    priors =  prior_prop$prior%>%rbindlist()%>%dplyr::select(alpha, beta)%>%t
    print(r$BDD[,input$var_grp])
    
    res<-(Cpmultprop2IT(Y = r$BDD[,input$var_prop], Gr = r$BDD[,input$var_grp],priors = priors,
                        seuil_global = seuil_react$data,
                        seuild=seuil_react_diff$data,seuilr=seuil_react_RR$data, seuilo=seuil_react_OR$data, 
                        twit=twit_react, 
                        arr=3,M=100000, IC=0.95, type =  seuil_comp_prop$type, plusieurs = seuil_comp_prop$plusieurs
                        
                        ))
    lapply(1:length(res), function(x) print(names(res)[x]))
    output$res_crois_prop<- renderUI({tagList(
      lapply(1:length(res),function(x)  box(title = names(res)[x], DT::dataTableOutput(ns(make.names(names(res)[x]))))
             
             )
    )})
    lapply(1:length(res), function(i) {
      output[[make.names(names(res)[i]) ]] <- DT::renderDataTable({
        DT::datatable(res[[i]],options = list(dom="t",ordering=F))
      })
        
      
      
    })
    
    
    
    
    })
  
  })
}
    
## To be copied in the UI
# mod_comp_pour_ui("comp_pour_1")
    
## To be copied in the server
# mod_comp_pour_server("comp_pour_1")
