# Values: 

library(shiny)
BDD<- mtcars
r<-reactiveValues(
)
r$BASEchargee<-T
r$BDD<-BDD
r$noms<-colnames(BDD)
r$contentInput<-T
r$nbSujet<-dim(BDD)[1]
D <- BDD
Y<-as.data.frame(lapply(D, factor))
z<-as.numeric(lapply(Y, nlevels))
r$nbModeVariable<-z
r$variableNum<-as.logical(lapply(D, is.numeric))
a<-as.logical(lapply(D, is.numeric))
ret<-rep(NA, ncol(D))
ret[which(a)] <- as.logical(lapply(as.data.frame(D[,which(a)]), desctable::is.normal))
r$variableNormale<-ret
r$listeVariableNonNormale<-names(BDD)[!ret]
r$listeVariableNormale<-names(BDD)[ret]



# Set options here
options(golem.app.prod = FALSE) # TRUE = production mode, FALSE = development mode

# Detach all loaded packages and clean your environment
golem::detach_all_attached()
# rm(list=ls(all.names = TRUE))

# Document and reload your package
#undebug(golem::document_and_reload)
golem::document_and_reload()

library(tidyverse)


ui <- shiny::fluidPage(ShiBA:::mod_chargement2_ui(1))

server <- function(input, output, session) {
  
  ShiBA:::mod_chargement2_server(1,r=r)
}


shiny::shinyApp(ui, server)


