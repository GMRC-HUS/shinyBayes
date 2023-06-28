#' glm_shiba 
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd



glm_Shiba <- function(data, formule, family , prior_intercept= NULL ,prior=NULL, refresh=0, chains =4,iter= 5000, keep_every = 3,  nb_repeat= 3, shiny = T,...){
  
  
  fit <- glm_prior(data, formule,prior,prior_intercept,iter=iter,chains=chains,keep_every = keep_every...)
  
  
  for(i in 1:nb_repeat) {
    print(i)
    checks <- diag_convergence(fit)
    if (checks) {
      return(fit)
    }
    fit <- glm_prior(data, formule,prior,prior_intercept,iter=iter*(i+1),keep_every=keep_every*(i+1),...)
 message <-  paste0(
   "Problème de convergence\n\nLe nombre d'itération a été augmenté à ",
   iter*(i+1),
   "\n Thining augmenté à ",
   keep_every*(i+1),
   "\n\n"
 )
    checks <- diag_convergence(fit)
    if (checks) {
      if (shiny) {
        
        shinyalert(
          "Warning!",
          message
          ,
          type = "warning"
        )
      } else{
        message(
          message
        )
      }
      return(fit)
    }
    
   
  
  }
  
  if (shiny) {
    shinyalert("Warning!",
               paste0("Problème de convergence, vérifier le modèle")
               ,
               type = "error")
  } else{
    message("Problème de convergence, vérifier le modèle")
  }
  return(fit)
}




glm_prior<- function(data, formule, family , prior_intercept= NULL ,prior=NULL,refresh=0, ...){
  
  if (is.null(prior_intercept$scale)|is.null(prior_intercept$location)) {
  if (!is.null(prior$scale) & !is.null(prior$location)) {
    fit<- stan_glm(formule,
                    family = gaussian(link = "identity"),
                    data = data, 
                    prior = prior,refresh=refresh,...)
    
    
    
  } else {
    fit<- (stan_glm(formule,
                    family = gaussian(link = "identity"),
                    data = data,refresh=refresh, ...
    ))
  }
} else if (!is.null(prior$scale) & !is.null(prior$location)) {
  fit<-stan_glm(formule,
                 family = gaussian(link = "identity"),
                 data = data, refresh = 0,
                 prior_intercept = prior_intercept,
                 prior = prior,refresh=refresh,...
  )
} else {
  fit<- (stan_glm(formule,
                  family = gaussian(link = "identity"),
                  data = r$BDD, refresh = 0,
                  prior_intercept = prior_intercept,refresh=refresh,...
  ))
}
  return(fit)
}


# Test 
# mtcars$mpg10 <- mtcars$mpg / 10
# fit <- glm_Shiba(
#   formule = mpg10 ~ wt + cyl + am,
#   data = mtcars,
#   QR = TRUE,
#   # for speed of example only (default is "sampling")
#   
#   refresh = 0,
#   iter = 20,seed = 42, 
#   keep_every=3, shiny = F
# )
# 
# 
# glm_prior
# 
# fit <- glm_prior(
#   formule = mpg10 ~ wt + cyl + am,
#   data = mtcars,
#  
#   # for speed of example only (default is "sampling")
#   
#   refresh = 0,
#   iter = 20,seed = 42, 
#   keep_every=1
# )
# 
# glm_prior(data, formula,prior,prior_intercept,iter=iter*(nb_repeat+1),keep_every=keep_every(keep_every+1)
# 
# summary(fit)
