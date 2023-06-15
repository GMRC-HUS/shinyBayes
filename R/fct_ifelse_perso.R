#' ifelse_perso 
#'
#' @description A ifelse function
#' Usable for all object
#' @return 
#'
#' @noRd
ifelse_perso <- function(boolean, x, y ){
  if(boolean) return(x)
  return(y)
}
