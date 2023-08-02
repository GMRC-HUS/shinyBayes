#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
#' @import dashboardthemes
#' @import utils
#' @import sortable
app_server <- function(input, output, session, BDD = NULL) {
  # Your application server logic
  r <- reactiveValues()
  pasDeBase <- fluidPage(
    h4("Aucune base n'a été chargée en mémoire, cet onglet n'est pas accessible."),
    p("Pour charger une base de données, rendez-vous sur l'onglet « Base de Données » dans la barre latérale.")
  )


  observeEvent(input$browser, {
    browser()
  })

  if (is.null(golem::get_golem_options("BDD"))) {
    mod_chargement_server("chargement_1", r)
  } else {
    BDD <- golem::get_golem_options("BDD")
    r$BDD <- BDD
    r$contentInput <- BDD
    r$BASEchargee <- T
    r$noms <- colnames(BDD)
    r$nbSujet <- dim(BDD)[1]
    D <- BDD
    Y <- as.data.frame(lapply(D, factor))
    z <- as.numeric(lapply(Y, nlevels))
    r$nbModeVariable <- z
    variableNum <- as.logical(lapply(D, is.numeric))
    r$variableNum <- variableNum
    ret <- rep(NA, ncol(D))
    ret[which(variableNum)] <- as.logical(lapply(as.data.frame(D[, which(variableNum)]), desctable::is.normal))
    r$variableNormale <- ret
  }
  # mod_Croisements_server("Croisements_1",r)
  # mod_Survie_server("Survie_1",r)
  # mod_Tests_server("Tests_1",r)
  # mod_Concordance_server("Concordance_1",r)
  mod_Accueil_server("Accueil_1")
  mod_Descriptifs_server("Descriptifs_1", r)
  mod_inferenceUni_server("inferenceUni_1", r)
  mod_Multivarie_server("multivarie_1", r)
  # mod_SaisieManuelle_server("SaisieManuelle_1")

  # callModule(mod_Descriptifs_server,id = "select",session = session, r = r)
}
