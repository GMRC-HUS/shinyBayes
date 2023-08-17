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