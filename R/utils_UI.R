#' UI 
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd




choix_quali_quanti <- function(name_ui,x){
  renderUI({
  
  choice <- ifelse_perso(sum(is.na(as.numeric(as.character(x)))) > sum(is.na(x)),
                         "qual",
                         "quant")
  if(length(unique(x))<5)   choice <-"qual"
  radioButtons(
    name_ui,
    "Nature de la variable",
    c(Quantitative = "quant", Qualitative = "qual"),
    choice
  )
  
})
}


css_perso <- "
.tooltip {
  pointer-events: none;
}
.tooltip > .tooltip-inner {
  pointer-events: none;
  background-color: #ffffff;
  color: black;
  border: 1px solid lightgray;
  padding: 10px;
  font-size: 25px;
  font-style: italic;
  text-align: justify;
  margin-left: 0;
  max-width: 1000px;
}
.tooltip > .arrow::before {
  border-right-color: #73AD21;
}

.fas{font-size: 20px;}
.shiny-notification {
             position:fixed;
             top: calc(50%);
             left: calc(50%);
}
             
             
.default-sortable .rank-list {
  flex: 1 0 auto;
  -webkit-border-radius: 3px;
  border-radius: 5px;
  background-color: white;
  margin: 5px;
  min-height: 45px;
  overflow-y: scroll;
  max-height: 200px;
}

.box-header .box-title {
 
  font-weight: bold;
}
"

js <- "
$(function () {
  $('[data-toggle=tooltip]').tooltip()
})
"
text_aide <- function(text) {
  return(span(
    `data-toggle` = "tooltip", `data-placement` = "right",
    title = text,
    icon("info-circle")
  ))
}




ui_choix_prior_norm <-  function(i,variables, ns,prior_beta_location_def,prior_beta_scale_def) {
  x <-variables[i]
  prior_beta_location_def_i <- prior_beta_location_def[i]
  prior_beta_scale_def_i <- prior_beta_scale_def[i]
  
  box( title = x,
       plotOutput(width = "100%", height = 150, ns(paste(x, "_courbe", sep = ""))),
       numericInput(ns(paste(x, "_mu_0", sep = "")), "A priori mu : ",
                    value = prior_beta_location_def_i, step = 0.1
       ),
       numericInput(ns(paste(x, "_sigma_0", sep = "")), "A priori écart-type  : ",
                    step = 0.1, value = prior_beta_scale_def_i, min=00.0001
       )
  )
  
}

ui_choix_prior_exp <-  function(i,variables, ns,prior_beta_location_def,prior_beta_scale_def) {
  
  x <- variables[i]
  prior_beta_location_def_i <- prior_beta_location_def[i]
  prior_beta_scale_def_i <- prior_beta_scale_def[i]
  
  transfo_exp = norm_tomin_max_exp(prior_beta_location_def_i  , prior_beta_scale_def_i   )
  min_exp =transfo_exp[1]
  max_exp = transfo_exp[2]
  
  
  
  box(title = x,
    plotOutput(width = 200, height = 100, ns(paste(x, "_courbe", sep = ""))),
    numericInput(ns(paste(x, "_min_exp", sep = "")), "A priori min: ",
                 value = round(min_exp,3), step = 0.1, min =0.00001
    ),
      numericInput(ns(paste(x, "_max_exp", sep = "")), "A priori max : ",
                 step = 0.1, value = round(max_exp,3),min =0.00001
    )
  )
  
}


ui_ggplot_prior_norm <-  function(i, input, noms) {
  renderPlot({
    (ggplot(data = data.frame(x = c(0,1)), aes(x)) +
       stat_function(fun = dnorm, args = list(mean = input[[paste(noms[i], "_mu_0", sep = "")]], sd = input[[paste(noms[i], "_sigma_0", sep = "")]])) +
       theme_light() +
      
     
                                                                                              
       scale_x_continuous(limits=c(
         arrondi_echelle_inf(input[[paste(noms[i], "_mu_0", sep = "")]] - 3 * input[[paste(noms[i], "_sigma_0", sep = "")]]),
         arrondi_echelle_sup(input[[paste(noms[i], "_mu_0", sep = "")]] + 3 * input[[paste(noms[i], "_sigma_0", sep = "")]])
       )) +
       theme(
         axis.text.y = element_blank(),
         axis.ticks.y = element_blank(),
         axis.ticks.x = element_blank()
       ) +
       ylab("") +
       xlab(noms[i])) %>% ggfst_lst_label_bld
  })
}




ui_ggplot_prior_exp <-  function(i,input, noms) {
  renderPlot({
    trans<-(min_max_exp_to_norm(input[[paste(noms[i], "_min_exp", sep = "")]],input[[paste(noms[i], "_max_exp", sep = "")]]))
    
    
    ggplot(data = data.frame(x = c(0,1)), aes(x)) +
      geom_function(fun = function(x) (dnorm(log(x),mean = trans[1],
                                             sd = trans[2])))+
      theme_light() +
      xlim(c(
        arrondi_echelle_inf( exp(trans[1] - 3 * trans[2])),
        arrondi_echelle_sup( exp(trans[1] + 3 *  trans[2]))
      )) +
      theme(
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank()
      ) +
      ylab("") +
      xlab(noms[i])
  })
}




ui_twit<- function(nom,ns, color="primary",...){
  actionBttn(
    inputId = ns(nom),
    label = "Définition Seuil ou Two It",
    style = "gradient",
    color = color, ...
  )
}

ui_choix_prior_dbeta <-  function(i, ns, width = 4, height_figure = 100, width_figure ="100%") {

  
  
  box( title = i$nom,width=width,
       plotOutput(width = width_figure,height = 150,  ns(paste(i$nom, "_courbe", sep = "")))%>% withSpinner(),
       numericInput(ns(paste(i$nom, "_alpha", sep = "")), "A priori paramètre alpha : ",
                    value = i$alpha, step = 0.1, min=0
       ),
       numericInput(ns(paste(i$nom, "_beta", sep = "")), "A priori paramètre beta : ",
                    step = 0.1, value = i$beta, min=0
       )
  )
  
}


ui_ggplot_prior_dbeta <-  function(i, input) {
  renderPlot({
    (ggplot(data = data.frame(x = c(0,1)), aes(x)) +
       stat_function(fun = dbeta, args = list(shape1 = input[[paste0(i$nom, "_alpha")]], shape2 = input[[paste0(i$nom, "_beta")]])) +
       theme_light() +
       xlim(c(
         0,1
       )) +
       theme(
         axis.text.y = element_blank(),
         axis.ticks.y = element_blank(),
         axis.ticks.x = element_blank()
       ) +
       ylab("") +
       xlab(i$nom)) %>% ggfst_lst_label_bld
  })
}




ui_choix_prior_dgamma <-  function(i, ns, width = 4, height_figure = 100, width_figure ="100%") {
  
  
  
  box( title = paste(i$nom,"sd"),width=width,
       plotOutput(width = width_figure,height = 150,  ns(paste(i$nom, "_courbe_gamma", sep = "")))%>% withSpinner(),
       numericInput(ns(paste(i$nom, "_alpha", sep = "")), "A priori paramètre alpha : ",
                    value = i$sd_shape, step = 0.1, min=0
       ),
       numericInput(ns(paste(i$nom, "_beta", sep = "")), "A priori paramètre beta : ",
                    step = 0.1, value = i$sd_rate, min=0
       )
  )
  
}


ui_ggplot_prior_dgamma <-  function(i, input) {
  renderPlot({
    (ggplot(data = data.frame(x = c(0,1)), aes(x)) +
       stat_function(fun = dgamma, args = list(shape = input[[paste0(i$nom, "_alpha")]], rate = input[[paste0(i$nom, "_beta")]])) +
       theme_light() +
# Regarder pour mettre des echelles plus adapté
  scale_x_continuous(limits=c(
   0,
    arrondi_echelle_sup(input[[paste0(i$nom, "_alpha")]]/input[[paste0(i$nom, "_beta")]] + 
                          2 * (input[[paste0(i$nom, "_alpha")]]/input[[paste0(i$nom, "_beta")]] )^2)
  ))+
       theme(
         axis.text.y = element_blank(),
         axis.ticks.y = element_blank(),
         axis.ticks.x = element_blank()
       ) +
       ylab("") +
       xlab(i$nom)) %>% ggfst_lst_label_bld
  })
}




ui_choix_prior_norm2 <-  function(i, ns, width = 4, height_figure = 100, width_figure ="100%") {
  
  
  
  box( title = paste(i$nom,"mu"),width=width,
       plotOutput(width = width_figure,height = 150,  ns(paste(i$nom, "_courbe", sep = "")))%>% withSpinner(),
       numericInput(ns(paste(i$nom, "_mu", sep = "")), "A priori  mu : ",
                    value = i$mu_mu, step = 0.1, min=0
       ),
       numericInput(ns(paste(i$nom, "_sd", sep = "")), "A priori sd : ",
                    step = 0.1, value = i$mu_sd, min=0
       )
  )
  
}


ui_ggplot_prior_norm2 <-  function(i, input) {
  renderPlot({
    (ggplot(data = data.frame(x = c(0,1)), aes(x)) +
       stat_function(fun = dnorm, args = list(mean = input[[paste0(i$nom, "_mu")]], sd = input[[paste0(i$nom, "_sd")]])) +
       theme_light() +
       scale_x_continuous(limits=c(
         arrondi_echelle_inf(input[[paste0(i$nom, "_mu")]] - 3 * input[[paste0(i$nom, "_sd")]]),
         arrondi_echelle_sup(input[[paste0(i$nom, "_mu")]] + 3 * input[[paste0(i$nom, "_sd")]])
       )) +
       theme(
         axis.text.y = element_blank(),
         axis.ticks.y = element_blank(),
         axis.ticks.x = element_blank()
       ) +
       ylab("") +
       xlab(i$nom)) %>% ggfst_lst_label_bld
  })
}

