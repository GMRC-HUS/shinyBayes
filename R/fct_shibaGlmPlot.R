#' shibaGlmPlot 
#'
#' @description A fct function
#' @import bayesplot
#' @import ggridges
#' @return The return value, if any, from executing the function.
#' 
#' @noRd



shibaGlmPlot<- function(fit, pars,seuilTwoIt=NULL,...){
  if(!is.null(pars)){
  p<-  mcmc_areas(fit %>% as.matrix(), pars = pars,...)+theme_light()
  if(is.null(seuilTwoIt)) return(p)
  
  if(!is.null(seuilTwoIt) ){
    nomsModel<- fit$coefficients%>%names
    
    if(seuilTwoIt$type=="seuil"){
      
      if(seuilTwoIt$plusieur_seuils){
        seuils<- c(NA,seuilTwoIt$val)
        seuils<-seuils[sapply(pars, function(x) which(x== nomsModel))]
        p1<- p + geom_ridgeline(aes(x=ifelse(x>seuils[parameter],x,0),height = .data$plotting_density,scale = 0.9 ), fill='#6ad02b', alpha=0.5)
        for( i in 1 : length(pars)){
          p1 <- p1+  geom_segment(x=seuils[length(pars)-i+1], xend=seuils[length(pars)-i+1],y=i, yend =i+1 ,linetype = "longdash")
          
        }
        return(p1)
        
      }else{
        seuils<- c(NA,rep(seuilTwoIt$val, length(nomsModel)-1))
        seuils<-seuils[sapply(pars, function(x) which(x== nomsModel))]
        p1<- p + geom_ridgeline(aes(x=ifelse(x>seuils[parameter],x,0),height = .data$plotting_density,scale = 0.9 ), fill='#6ad02b', alpha=0.5)
        for( i in 1 : length(pars)){
          p1 <- p1+  geom_segment(x=seuils[length(pars)-i+1], xend=seuils[length(pars)-i+1],y=i, yend =i+1 ,linetype = "longdash")
        }
        return(p1)
      }
    }else if(!is.null(seuilTwoIt$val$var)){
      
      list_param=seuilTwoIt$val

 
    y = length(pars)-which(list_param$var==pars)+1
    if(length(y)==0) return(p)
    p1 = p+annotate("rect", xmin =list_param$theta_P_min, 
             xmax = list_param$theta_P_max, ymin = y, 
             ymax = y+1, fill = "#40E0D0", alpha = 0.2) + 
      annotate("rect", xmin = list_param$theta_A_min, 
               xmax = list_param$theta_A_max, ymin = y, 
               ymax = y+1, fill = "#DE3163", alpha = 0.2)
    return(p1)
  }
  
  
  }else{
    return(p)

    
  }
  return(p)
  }
}

