#' credit UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_credit_ui <- function(id){
  ns <- NS(id)
  fluidPage(align="center",
  
  tags$br(),
  img(src = "www/hex.png", width = "10%", style = "display: block; margin-left: auto; margin-right: auto;"),
  # tags$img(src="www/logo1.png"),
  
  tags$h1("SHIBA", align = "center", style = "color:#08088A; font-family: Georgia; font-size : 40px;"),
  HTML('<h2>Un outil combinant R <img src="www/logo.jpg"  height="25">, Shiny <img src="www/shiny.png" class="img-fluid" width="25"> et STAN <img src="www/stan_logo.png" class="img-fluid" width="25"> </br>pour faciliter la réalisation des analyses statistiques Bayésiennes.</h2>'),
  tags$br(),
  HTML('SHIBA est une application créée par le Groupe Méthodes en Recherche Clinique (GMRC) des Hôpitaux Universitaires de Strasbourg et les membres de l’équipe IMAGES – RaDoScauba du laboratoire iCUBE (CNRS UMR 7357).</br></br> Elle est entièrement gratuite, sous licence <img src="www/by-nc.eu.png" class="img-fluid" width="50">.</br></br>  
SHIBA est une interface graphique permettant de réaliser des analyses bayésiennes via le logiciel de statistiques R et les packages associés sans avoir à coder les instructions nécessaires pour obtenir ces analyses. 
</br></br></br></br>Cet outil est proposé <b>sans aucune garantie de validité sur les résultats.</b>
</br></br><b>Si vous utilisez cet outil pour traiter des données médicales et a fortiori relevant de la recherche médicale, vous devez le faire dans le respect de la réglementation en vigueur. Les concepteurs de cet outil ne sont pas responsables de ce que vous en faites . 
</b>'),
  tags$br(),
  tags$br(), tags$br(), tags$br(), tags$br(), tags$br(), tags$br(),
  tags$br(), tags$br(),
  HTML('<h3>Aide disponible sur le site: <a href="https://gmrc-hus.github.io/ShiBA/" target="_blank">gmrc-hus.github.io/ShiBA/</a></h3>'),
  HTML('
    <a href="https://github.com/GMRC-HUS/ShiBA" title="Source Code" target="_blank" ><img src="www/github.png"  height="25"></a>'),
  # tags$p("03 88 1(1 67 49)",style = "color:#08088A"),tags$br(),tags$br(),tags$br(),tags$br(),tags$br()
  # height = 400	, width = 492
  tags$br(), tags$br(), tags$br(), tags$br(), tags$br(), tags$br(), tags$br(), tags$br(), tags$br(), tags$br(), tags$br(),
  
  )
  
}
    
#' credit Server Functions
#'
#' @noRd 
mod_credit_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_credit_ui("credit_1")
    
## To be copied in the server
# mod_credit_server("credit_1")
