#' prop_comp 
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd


Cpmultprop2IT<-function(Y,Gr,priors,seuil_global = NULL,
                        seuild=NULL,seuilr=NULL,seuilo=NULL, twit=NULL, 
                        arr=3,M=100000, IC=0.95, type =  NULL, plusieurs = NULL){
  pourcent_IC2 = (1-IC)/2
  Gr <- as.factor(Gr)
  Y= as.factor(Y)
  levels_y = levels(Y)
  Ngroup<-nlevels(as.factor(Gr))
  print(Ngroup)
  noms<-levels(as.factor(Gr))
  long<-factorial(Ngroup)/(factorial(Ngroup-2)*2)


if(is.null(type)){
  
}else{
  if((!is.null(type)) &  "seuil" %in%type){
  
    if(!plusieurs & !is.null(seuil_global)){
      seuild = rep(seuil_global["Diff","seuil"],long)
      
      seuilr = rep(seuil_global["RR","seuil"],long)
      seuilo = rep(seuil_global["OR","seuil"],long)
    }else{
   
      seuild = unlist(seuild%>%unlist%>%na.omit()%>%as.vector())
      seuilr = unlist(seuilr%>%unlist%>%na.omit()%>%as.vector())
      seuilo = unlist(seuilo%>%unlist%>%na.omit()%>%as.vector())

    }
  }
  
}
  
  
  posteriors<-matrix(0,ncol=Ngroup,nrow=2)
  
  for(j in 1:Ngroup){
    posteriors[1,j]<-length(Y[Gr==noms[j]])
    posteriors[2,j]<-sum(Y[Gr==noms[j]]==levels_y[1])
  }
  
  table(Y,Gr)
  
  
  descriptif<-function(x, probs=c(pourcent_IC2,0.5, 1-pourcent_IC2 )){
    return(c(mean(x),quantile(x,probs=probs)))
  }
  
  calcor<-function(x1,x2){
    x1<-x1/100
    x2<-x2/100
    return((x1*(1-x2))/(x2*(1-x1)))
  }
  
  # a priori 
  
  xxprior<-NULL
  descrxxprior<-NULL
  for(j in 1:Ngroup){
    xxprior[[j]]<-rbeta(M,priors[1,j],priors[2,j])*100
    descrxxprior[[j]]<-descriptif(xxprior[[j]], probs = c(pourcent_IC2,0.5, 1-pourcent_IC2 ))
  }
  
  nomsx<-paste0("P",noms)
  
  diffprior<-list(NULL,NULL,NULL)
  nomsdiff<-NULL
  RRprior<-list(NULL,NULL,NULL)
  nomsRR<-NULL
  ORprior<-list(NULL,NULL,NULL)
  nomsOR<-NULL
  for(i in 1:Ngroup){
    for(j in 2:Ngroup){
      if(j<=i)next
      diffprior[[i]]<-append(diffprior[[i]],list(xxprior[[i]]-xxprior[[j]]))
      nomsdiff<-c(nomsdiff,paste0("Diff P(x=",noms[i],")-P(x=",noms[j],")"))
      RRprior[[i]]<-append(RRprior[[i]],list(xxprior[[i]]/xxprior[[j]]))
      nomsRR<-c(nomsRR,paste0("RR P(x=",noms[i],")/P(x",noms[j],")"))
      ORprior[[i]]<-append(ORprior[[i]],list(calcor(xxprior[[i]],xxprior[[j]])))
      nomsOR<-c(nomsOR,paste0("OR P(x=",noms[i],")/P(x",noms[j],")"))
    }
  }
  
  descrdiffprior<-list(NULL,NULL,NULL)
  descrRRprior<-list(NULL,NULL,NULL)
  descrORprior<-list(NULL,NULL,NULL)
  for(i in 1:length(diffprior)){
    descrdiffprior[[i]]<-lapply(diffprior[[i]],descriptif)
    descrRRprior[[i]]<-lapply(RRprior[[i]],descriptif)
    descrORprior[[i]]<-lapply(ORprior[[i]],descriptif)
  }
  
  # a posteriori
  
  xx<-NULL
  descrxx<-NULL
  for(j in 1:ncol(priors)){
    xx[[j]]<-rbeta(M,priors[1,j]+posteriors[1,j],priors[2,j]+posteriors[2,j])*100
    descrxx[[j]]<-descriptif(xx[[j]])
  }
  
  diff<-list(NULL,NULL,NULL)
  RR<-list(NULL,NULL,NULL)
  OR<-list(NULL,NULL,NULL)
  for(i in 1:Ngroup){
    for(j in 2:Ngroup){
      if(j<=i)next
      diff[[i]]<-append(diff[[i]],list(xx[[i]]-xx[[j]]))
      RR[[i]]<-append(RR[[i]],list(xx[[i]]/xx[[j]]))
      OR[[i]]<-append(OR[[i]],list(calcor(xx[[i]],xx[[j]])))
    }
  }
  
  descrdiff<-list(NULL,NULL,NULL)
  descrRR<-list(NULL,NULL,NULL)
  descrOR<-list(NULL,NULL,NULL)
  for(i in 1:length(diff)){
    descrdiff[[i]]<-lapply(diff[[i]],descriptif)
    descrRR[[i]]<-lapply(RR[[i]],descriptif)
    descrOR[[i]]<-lapply(OR[[i]],descriptif)
  }
  
  noms.tab<-c(nomsx,nomsdiff,nomsRR,nomsOR)
  
  resprior<-list(descrxxprior,unlist(descrdiffprior,recursive = F),unlist(descrRRprior,recursive = F),unlist(descrORprior,recursive=F))
  resprior<-unlist(resprior,recursive=F)
  resprior<-lapply(resprior,function(i){round(i,4)})
  resprior<-lapply(resprior,as.list)
  resprior<-rbindlist(resprior)
  resprior<-data.frame(resprior)
  colnames(resprior)<-c("moy", paste0(c(pourcent_IC2,0.5, 1-pourcent_IC2 )*100, "%") )
  rownames(resprior)<-c(noms.tab)
  
  res<-list(descrxx,unlist(descrdiff,recursive = F),unlist(descrRR,recursive = F),unlist(descrOR,recursive=F))
  res<-unlist(res,recursive=F)
  res<-lapply(res,function(i){round(i,4)})
  res<-lapply(res,as.list)
  res<-rbindlist(res)
  res<-data.frame(res)
  colnames(res)<-c("moy", paste0(c(pourcent_IC2,0.5, 1-pourcent_IC2 )*100, "%") )
  rownames(res)<-c(noms.tab)
  
  diff<-unlist(diff,recursive = F)
  RR<-unlist(RR,recursive = F)
  OR<-unlist(OR,recursive = F)
  
  
  TEST<-unlist(list(diff,RR,OR),recursive = F)
  if(is.null(type)){
  }else if(type =="seuil"){
  seuils<-c(seuild,seuilr,seuilo)
  tests<-NULL
  for(i in 1:length(TEST)){
    tests<-c(tests,round(mean(TEST[[i]]>seuils[i]),arr))
  }
  
  res$seuils<-c(rep(NA,Ngroup),seuils)
  res[,"Pr(X>seuil|D)"]<-c(rep(NA,Ngroup),tests)
  
  
  }else{
    
    sel<-which(apply(utils::combn(noms, 2),2, function(x) c(paste(x, collapse = "vs"),paste(rev(x), collapse = "vs")))%>%as.vector() ==twit$var )
    print(twit$var)
    print(sel)
    print(apply(utils::combn(noms, 2),2, function(x) c(paste(x, collapse = "vs"),paste(rev(x), collapse = "vs")))%>%as.vector() )
    lequel <- ceiling(sel/2)
    signe = 1-sel%%2
    data=twit$data
    data$`Pr Ha` = c(round(mean(between(TEST[[lequel]]* ifelse_perso(signe==1, 1, -1),data["Diff","min_effet_absent"],data["Diff","max_effet_absent"])),arr),
                     round(mean(between(TEST[[lequel]]^ifelse_perso(signe==1, 1, -1),data["RR","min_effet_absent"],data["RR","max_effet_absent"])),arr), 
                     round(mean(between(TEST[[lequel]]^ifelse_perso(signe==1, 1, -1),data["OR","min_effet_absent"],data["OR","max_effet_absent"])),arr))
                     
                     
    data$`Pr Hr` =  c(round(mean(between(TEST[[lequel]]* ifelse_perso(signe==1, 1, -1),data["Diff","min_effet_present"],data["Diff","max_effet_present"])),arr),
                      round(mean(between(TEST[[lequel]]^ifelse_perso(signe==1, 1, -1),data["RR","min_effet_present"],data["RR","max_effet_present"])),arr),
                      round(mean(between(TEST[[lequel]]^ifelse_perso(signe==1, 1, -1),data["OR","min_effet_present"],data["OR","max_effet_present"])),arr) )                

                    
    names(data)<-c("min Abs","max Abs","min pres","max pres",
                   "Pr(effet absent)","Pr(effet présent)")
    
    
    
  }

  prior<-  data.frame(Loi = "Beta", "Paramètre alpha" =priors[1,] , "Paramètre beta" = priors[2,],row.names = paste("Groupe",noms), check.names = F)



  laliste<-list(prior,resprior,res)
  names(laliste) <- c("prior","Valeurs a priori","Valeurs a posteriori" )
  if(is.null(type)){
    
  }else if(type =="twit" & !is.null(type)){
    res_twit= list(TwoIt = data)
    names(res_twit) = twit$var
    laliste<-append(laliste,res_twit)
  }
    
    
  
  return(laliste)
}




Infe_prop2IT<-function(Y,priors,seuil = NULL, twit=NULL, 
                       arr=3,M=100000, IC=0.95, type =  NULL, color_1 ="#DE3163" , color_2="#40E0D0",color_3 = "#EBC341" ,color_4="#9242A6"){
  pourcent_IC2 = (1-IC)/2
  
  
  
  Y=as.factor(Y)
  
  levels_Y= levels(Y)
  posteriors<-c(length(Y)-sum(Y==levels_Y[1]), sum(Y==levels_Y[1]))
  
  
  
  descriptif<-function(x, probs=c(pourcent_IC2,0.5, 1-pourcent_IC2 )){
    return(c(mean(x),quantile(x,probs=probs)))
  }
  
  
  
  # a priori 
  
  xxprior<-rbeta(M,priors[1],priors[2])*100
  descrxxprior<-descriptif(xxprior, probs = c(pourcent_IC2,0.5, 1-pourcent_IC2 ))
  
  print(paste("posterior: " ,posteriors ))
 p<- ggplot(data.frame(x=c(0,1)),aes(x=x))+ 
   
    stat_function(fun = dbeta,args=list(shape1 = priors[1],
                                                       shape2 = priors[2]),aes(color="prior"))+
    stat_function(fun = dbeta,args=list(shape1 = priors[1]+posteriors[1],
                                        shape2 = priors[2]+posteriors[2]),aes(color="posterior"))+
    
    theme_light() +
    xlim(c(0,1
    )) +
    theme(
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.ticks.x = element_blank()
    ) +
    ylab("") +
   scale_color_manual(name = "Distribution",
                      breaks = c("prior", "posterior"),
                      values = c("prior" = color_3, "posterior" = color_4) )
  
  
  xx<-rbeta(M,priors[1]+posteriors[1],priors[2]+posteriors[2])
  descrxx<-descriptif(xx)%>%round(arr)
  
  
  
  
  
  
  
  resprior<-descrxxprior%>%round(arr)
  
  
  resprior<-data.frame(resprior)%>%t%>%as.data.frame()
  colnames(resprior)<-c("moy", paste0(c(pourcent_IC2,0.5, 1-pourcent_IC2 )*100, "%") )
  rownames(resprior)<-c("Proportion")
  
  
  res<-data.frame(descrxx)%>%t%>%as.data.frame()
  res<-res*100
  colnames(res)<-c("moy", paste0(c(pourcent_IC2,0.5, 1-pourcent_IC2 )*100, "%") )
  rownames(res)<-c("Proportion")
  
  
  
  
  
  if(is.null(type)){
  }else if(type =="seuil"){
    tests <- round(mean(xx>seuil),arr)
    
    

    res$seuils<-c(seuil)
    res$`Pr(X>seuil|D)`<-c(tests)
    
p<- p+ geom_segment( aes(x=seuil, xend = seuil,y=-Inf,yend=Inf))
  }else{
    
    
    data=twit$data
    print(data)
    data$`Pr Ha` = round(mean(between(xx,data[,"min_effet_absent"],data[,"max_effet_absent"])),arr)
    
    
    data$`Pr Hr` =  round(mean(between(xx,data[,"min_effet_present"],data[,"max_effet_present"])),arr)
 
    print(xx)
    names(data)<-c("min Abs","max Abs","min pres","max pres",
                   "Pr(effet absent)","Pr(effet présent)")
    
    p<- p+ geom_rect( mapping= aes( xmin = data[,"min pres"],xmax = data[,"max pres"], ymin = -Inf,
                                                                        ymax = Inf, x=0, y=0,fill ="Acceptée" ), alpha = 0.2 )+
      geom_rect(aes( xmin = data[,"min Abs"],
                                                                 xmax = data[,"max Abs"], ymin = -Inf,
                                                                 ymax = Inf,x=0,y=0 ,fill ="Rejetée" ), alpha = 0.2 )+
      scale_fill_manual(name = "Hypothèse",
                        breaks = c("Acceptée", "Rejetée"),
                        labels =c("P : effet présent ","A : Effet absent"),
                        values = c("Acceptée" = color_2, "Rejetée" = color_1))
    
   
  }
  
  prior<-  data.frame(Loi = "Beta", "Paramètre alpha" =priors[1] , "Paramètre beta" = priors[2],row.names = ("Prior"), check.names = F)
  
  
  
  df<-list(prior,resprior,res)
  names(df) <- c("prior","Valeurs a priori","Valeurs a posteriori" )
  if(is.null(type)){
    
  }else if(type =="twit" & !is.null(type)){

    res_twit= list(TwoIt = data)
    names(res_twit) = twit$var
    df<-append(df,res_twit)
  }
  
  
  
  return(list(df=df, graph = p))
}





