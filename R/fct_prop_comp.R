#' prop_comp 
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd


Cpmultprop2IT<-function(Y,Gr,priors,
                        seuild=NULL,seuilr=NULL,seuilo=NULL,
                        arr=3,M=100000, IC=0.95){
  pourcent_IC2 = (1-IC)/2
  
  Ngroup<-nlevels(as.factor(Gr))
  noms<-levels(as.factor(Gr))
  
  if(Ngroup==1){
    stop(paste0("Un seul groupe :\n",paste0(noms,collapse=" ; ")))
  }
  
  if(Ngroup>4){
    stop(paste0("Plus de 4 groupes :\n",paste0(noms,collapse=" ; ")))
  }
  
  if(!is.matrix(priors)){
    dimension<-dim(priors)
    stop(paste0("Les priors doivent être une matrice de dimension 2 lignes x ",Ngroup,
                " colonnes (nombre de groupes) \n Ici la matrice fait ",dimension[1]," lignes x ",dimension[2]," colonnes"))
  }
  
  if(!all(dim(priors)==c(2,Ngroup))){
    dimension<-dim(priors)
    stop(paste0("Les priors doivent être une matrice de dimension 2 lignes x ",Ngroup,
                " colonnes (nombre de groupes) \n Ici la matrice fait ",dimension[1]," lignes x ",dimension[2]," colonnes"))
  }
  
  if(!is.null(seuild)&!is.vector(seuild)){
    long<-factorial(Ngroup)/(factorial(Ngroup-2)*2)
    if(length(seuild)!=long){
      stop(paste0("Les seuils de différences doivent être un vecteur de dimension ",long,
                  "\n Ici ce n'est pas un vecteur"))
    }
  }
  
  if(!is.null(seuild)){
    long<-factorial(Ngroup)/(factorial(Ngroup-2)*2)
    if(length(seuild)!=long){
      stop(paste0("Les seuils de différences doivent être un vecteur de dimension ",long,
                  "\n Ici le vecteur fait ",length(seuild)))
    }
  }
  
  if(!is.null(seuilr)&!is.vector(seuilr)){
    long<-factorial(Ngroup)/(factorial(Ngroup-2)*2)
    if(length(seuilr)!=long){
      stop(paste0("Les seuils de risques relatifs doivent être un vecteur de dimension ",long,
                  "\n Ici ce n'est pas un vecteur"))
    }
  }
  
  if(!is.null(seuilr)){
    long<-factorial(Ngroup)/(factorial(Ngroup-2)*2)
    if(length(seuilr)!=long){
      stop(paste0("Les seuils de risques relatifs doivent être un vecteur de dimension ",long,
                  "\n Ici le vecteur fait ",length(seuilr)))
    }
  }
  
  if(!is.null(seuilo)&!is.vector(seuilo)){
    long<-factorial(Ngroup)/(factorial(Ngroup-2)*2)
    if(length(seuilo)!=long){
      stop(paste0("Les seuils d'odds-ratio doivent être un vecteur de dimension ",long,
                  "\n Ici ce n'est pas un vecteur"))
    }
  }
  
  if(!is.null(seuilo)){
    long<-factorial(Ngroup)/(factorial(Ngroup-2)*2)
    if(length(seuilo)!=long){
      stop(paste0("Les seuils d'odds-ratio doivent être un vecteur de dimension ",long,
                  "\n Ici le vecteur fait ",length(seuilo)))
    }
  }
  
  posteriors<-matrix(0,ncol=Ngroup,nrow=2)
  
  for(j in 1:Ngroup){
    posteriors[1,j]<-sum(Y[Gr==noms[j]])
    posteriors[2,j]<-sum(Y[Gr==noms[j]]==0)
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
  
  nomsx<-paste0("P",1:length(descrxxprior))
  
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
      nomsdiff<-c(nomsdiff,paste0("Diff P",i,"-P",j))
      RRprior[[i]]<-append(RRprior[[i]],list(xxprior[[i]]/xxprior[[j]]))
      nomsRR<-c(nomsRR,paste0("RR P",i,"/P",j))
      ORprior[[i]]<-append(ORprior[[i]],list(calcor(xxprior[[i]],xxprior[[j]])))
      nomsOR<-c(nomsOR,paste0("OR P",i,"/P",j))
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
  seuils<-c(seuild,seuilr,seuilo)
  tests<-NULL
  for(i in 1:length(TEST)){
    tests<-c(tests,round(mean(TEST[[i]]>seuils[i]),arr))
  }
  
  res$seuils<-c(rep(NA,Ngroup),seuils)
  res[,"Pr(X>seuil|D)"]<-c(rep(NA,Ngroup),tests)
  
  cat("========================================================================================","\n")
  cat(" RAPPELS : ","\n")
  cat("========================================================================================","\n")
  prior<-  paste(" Prior du groupe 1 (=",noms[1],") : Beta(",priors[1,1],",",priors[2,1],")","\n",
                 " Prior du groupe 2 (=",noms[2],") : Beta(",priors[1,2],",",priors[2,2],")","\n",sep="")
  if(Ngroup>2){
     prior<-paste(prior," Prior du groupe 3 (=",noms[3],") : Beta(",priors[1,3],",",priors[2,3],")","\n",sep="")
  }
  if(Ngroup>3){
    prior<- paste(prior," Prior du groupe 4 (=",noms[4],") : Beta(",priors[1,4],",",priors[2,4],")","\n",sep="")
  }


  laliste<-list(prior,resprior,res)
  names(laliste) <- c("Valeurs a priori","Valeurs a posteriori")
  return(laliste)
}
