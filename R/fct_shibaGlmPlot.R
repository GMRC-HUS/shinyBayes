#' shibaGlmPlot 
#'
#' @description A fct function
#' @import bayesplot
#' @import ggridges
#' @return The return value, if any, from executing the function.
#' 
#' @noRd



shibaGlmPlot<- function(fit,type_glm, pars,seuilTwoIt=NULL,...){
  if(!is.null(pars)){
    if(type_glm %in% c("poiss","binom")){
  p<-  mcmc_areas(fit %>% as.matrix(),transformations = "exp", pars = pars
                   ,...
                  )+theme_light()
    }else{
      p<-  mcmc_areas(fit %>% as.matrix(), pars = pars
                      ,...
      )+theme_light()
    }
  if(is.null(seuilTwoIt)) return(p)
  
  if(!is.null(seuilTwoIt) ){
      nomsModel<- fit$coefficients%>%names
    
    if(seuilTwoIt$type=="seuil"){
      
      
      if(seuilTwoIt$plusieur_seuils){
       
        if(!type_glm %in% c("poiss","binom")){
        seuils<- data.frame(seuils= c(NA,seuilTwoIt$val),
                            parameter=nomsModel )
        
        }else{
          seuils<- data.frame(seuils= c(NA,seuilTwoIt$val),
                              parameter=paste0("exp(",nomsModel,")") )   
        
          pars =   paste0("exp(",pars,")")
        }
        
        p$data<-p$data%>%left_join(seuils)
        
        
  
          p1<- p + geom_ridgeline(aes(x=x,y=parameter,height =if_else(x>seuils, .data$plotting_density,0), scale = 0.9 ), fill='#6ad02b', alpha=0.5)
        
         
        for( i in 1 : length(pars)){
          p1 <- p1+  geom_segment(x=(seuils%>%filter(parameter==rev(pars)[i]))$seuil,
                                  xend = (seuils%>%filter(parameter==rev(pars)[i]))$seuil,y=i, yend =i+1 ,linetype = "longdash")
        }
        return(p1)
        
      }else{
   
        seuils<- rep(seuilTwoIt$val, length(nomsModel))
        seuils[1]<-NA

        if(!type_glm %in% c("poiss","binom")){
          seuils<- data.frame(seuils= c(seuilTwoIt$val),
                              parameter=nomsModel )
        }else{
          seuils<- data.frame(seuils= c(seuilTwoIt$val),
                              parameter=paste0("exp(",nomsModel,")") )   
          pars =   paste0("exp(",pars,")")
          
        }
        p$data<-p$data%>%left_join(seuils)
        
        
        p1<- p +  geom_ridgeline(aes(x=x,y=parameter,height =if_else(x>seuils, .data$plotting_density,0), scale = 0.9 ), fill='#6ad02b', alpha=0.5)
        for( i in 1 : length(pars)){
          p1 <- p1+  geom_segment(x=(seuils%>%filter(parameter==rev(pars)[i]))$seuil, 
                                  xend=(seuils%>%filter(parameter==rev(pars)[i]))$seuil,y=i, yend =i+1 ,linetype = "longdash")
        }
        return(p1)
      }
    }else if(!is.null(seuilTwoIt$val$var)){
      
      list_param=seuilTwoIt$val

 
    y = length(pars)-which(list_param$var==pars)+1
    if(length(y)==0) return(p)
    limit_plot<- layer_scales(p)$x$get_limits()
    list_param$theta_P_max<-ifelse(between(list_param$theta_P_min,limit_plot[1],limit_plot[2])&list_param$theta_P_max>limit_plot[2],limit_plot[2]-0.01,list_param$theta_P_max )
    list_param$theta_P_min<-ifelse(between(list_param$theta_P_max,limit_plot[1],limit_plot[2])&list_param$theta_P_min<limit_plot[1],limit_plot[1]+0.01,list_param$theta_P_min )
    
    
    list_param$theta_A_max<-ifelse(between(list_param$theta_A_min,limit_plot[1],limit_plot[2])&list_param$theta_A_max>limit_plot[2],limit_plot[2]-0.01,list_param$theta_A_max )
    list_param$theta_A_min<-ifelse(between(list_param$theta_A_max,limit_plot[1],limit_plot[2])&list_param$theta_A_min<limit_plot[1],limit_plot[1]+0.01,list_param$theta_A_min )
    
    
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
  
