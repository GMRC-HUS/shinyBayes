#' compare_prop 
#'
#' @description A fct function
#' @import rstanarm
#' @import data.table
#' @return The return value, if any, from executing the function.
#'
#' @noRd


library(rstanarm)
library()



###################### fonction NM
Cp2prop2IT<-function(Y,Gr,alpha01,beta01,alpha02,beta02,
                     seuild=NULL,seuilr=NULL,seuilo=NULL,
                     HAdl=NULL,HAdu=NULL,HPdl=NULL,HPdu=NULL,
                     HAol=NULL,HAou=NULL,HPol=NULL,HPou=NULL,
                     HArl=NULL,HAru=NULL,HPrl=NULL,HPru=NULL,
                     arr=3,M=100000){
  
  
  if(nlevels(as.factor(Gr))==1){
    noms<-levels(as.factor(Gr))
    stop(paste0("Un seul groupe :\n",paste0(noms,collapse=" ; ")))
  }
  
  if(nlevels(as.factor(Gr))>2){
    noms<-levels(as.factor(Gr))
    stop(paste0("Plus de 2 groupes :\n",paste0(noms,collapse=" ; ")))
  }
  
  nom.gr1<-levels(as.factor(Gr))[1]
  nom.gr2<-levels(as.factor(Gr))[2]
  Gr<-ifelse(Gr==nom.gr2,1,0)
  
  alpha1<-sum(Y[Gr==1]) ## Gr Exp
  beta1<-length(which(Gr==1))-alpha1
  alpha2<-sum(Y[Gr==0]) ## Gr Ctrl                                             
  beta2<-length(which(Gr==0))-alpha2                                      
  
  respOR <- respRR <- respdi<-matrix(NA,nrow=5,ncol=1)
  rownames(respdi)<-c(paste("HA : Pr(",HAdl," < diff < ",HAdu,") ="),
                      paste("HP : Pr(",HPdl," < diff < ",HPdu,") ="),
                      paste("HA | D : Pr(",HAdl," < diff < ",HAdu," | D) ="),
                      paste("HP | D : Pr(",HPdl," < diff < ",HPdu," | D) ="),
                      "Pr(différence > seuil diff | D)")
  rownames(respRR)<-c(paste("HA : Pr(",HArl," < diff < ",HAru,") ="),
                      paste("HP : Pr(",HPrl," < diff < ",HPru,") ="),
                      paste("HA | D : Pr(",HArl," < diff < ",HAru," | D) ="),
                      paste("HP | D : Pr(",HPrl," < diff < ",HPru," | D) ="),
                      "Pr(RR > seuil RR | D)")
  rownames(respOR)<-c(paste("HA : Pr(",HAol," < diff < ",HAou,") ="),
                      paste("HP : Pr(",HPol," < diff < ",HPou,") ="),
                      paste("HA | D : Pr(",HAol," < diff < ",HAou," | D) ="),
                      paste("HP | D : Pr(",HPol," < diff < ",HPou," | D) ="),
                      "Pr(OR > seuil OR | D)")
  colnames(respdi)<-"" 
  colnames(respOR)<-""
  colnames(respRR)<-""
  
  # a priori 
  xx1prior<-rbeta(M,alpha01,beta01)
  xx2prior<-rbeta(M,alpha02,beta02)
  diffprior<-xx1prior-xx2prior
  RRprior<-xx1prior/xx2prior
  ORprior<-xx1prior*(1-xx2prior)/(xx2prior*(1-xx1prior))
  
  descriptifP1prior<-c(mean(xx1prior),quantile(xx1prior,probs=c(0.025,0.05,0.25,0.5,0.75,0.95,0.975)))
  descriptifP2prior<-c(mean(xx2prior),quantile(xx2prior,probs=c(0.025,0.05,0.25,0.5,0.75,0.95,0.975)))
  descriptifdiffprior<-c(mean(diffprior),quantile(diffprior,probs=c(0.025,0.05,0.25,0.5,0.75,0.95,0.975)))
  descriptifRRprior<-c(mean(RRprior),quantile(RRprior,probs=c(0.025,0.05,0.25,0.5,0.75,0.95,0.975)))
  descriptifORprior<-c(mean(ORprior),quantile(ORprior,probs=c(0.025,0.05,0.25,0.5,0.75,0.95,0.975)))
  
  # a posteriori
  xx1<-rbeta(M,alpha01+alpha1,beta01+beta1)
  xx2<-rbeta(M,alpha02+alpha2,beta02+beta2)
  diff<-xx1-xx2
  RR<-xx1/xx2
  OR<-xx1*(1-xx2)/(xx2*(1-xx1))
  
  descriptifP1<-c(mean(xx1),quantile(xx1,probs=c(0.025,0.05,0.25,0.5,0.75,0.95,0.975)))
  descriptifP2<-c(mean(xx2),quantile(xx2,probs=c(0.025,0.05,0.25,0.5,0.75,0.95,0.975)))
  descriptifdiff<-c(mean(diff),quantile(diff,probs=c(0.025,0.05,0.25,0.5,0.75,0.95,0.975)))
  descriptifRR<-c(mean(RR),quantile(RR,probs=c(0.025,0.05,0.25,0.5,0.75,0.95,0.975)))
  descriptifOR<-c(mean(OR),quantile(OR,probs=c(0.025,0.05,0.25,0.5,0.75,0.95,0.975)))
  
  resprior<-matrix(NA,nrow=5,ncol=8)
  colnames(resprior)<-c("moy","p_0.025","p_0.05","p_0.25","p_0.50","p_0.75","p_0.95","p_0.975")
  rownames(resprior)<-c("P1 ","P2" ,"Diff","RR  ","OR  ")
  resprior[1,]<-round(descriptifP1prior*100,arr)
  resprior[2,]<-round(descriptifP2prior*100,arr)
  resprior[3,]<-round(descriptifdiffprior*100,arr)
  resprior[4,]<-round(descriptifRRprior,arr)
  resprior[5,]<-round(descriptifORprior,arr)
  
  res<-matrix(NA,nrow=5,ncol=8)
  colnames(res)<-c("moy","p_0.025","p_0.05","p_0.25","p_0.50","p_0.75","p_0.95","p_0.975")
  rownames(res)<-c("P1 ","P2" ,"Diff","RR  ","OR  ")
  res[1,]<-round(descriptifP1*100,arr)
  res[2,]<-round(descriptifP2*100,arr)
  res[3,]<-round(descriptifdiff*100,arr)
  res[4,]<-round(descriptifRR,arr)
  res[5,]<-round(descriptifOR,arr)
  
  cat("========================================================================================","\n")
  cat(" RAPPELS : ","\n")
  cat("========================================================================================","\n")
  cat(" Prior du groupe 1 (=",nom.gr1,") : Beta(",alpha01,",",beta01,")","\n",sep="")
  cat(" Prior du groupe 2 (=",nom.gr2,") : Beta(",alpha02,",",beta02,")","\n",sep="")
  cat("========================================================================================","\n")
  
  
  if(is.null(seuild)){respdi<-NULL}else{
    respdi[1,1] <- round(sum(diffprior>(HAdl) & diffprior<(HAdu))/M,arr)
    respdi[2,1] <- round(sum(diffprior>(HPdl) & diffprior<(HPdu))/M,arr)
    respdi[3,1] <- round(sum(diff>(HAdl) & diff<(HAdu))/M,arr)
    respdi[4,1] <- round(sum(diff>(HPdl) & diff<(HPdu))/M,arr)
    respdi[5,1] <- round(sum(diff>seuild)/M,arr)
  }  
  
  if(is.null(seuilr)){respRR<-NULL}else{
    respRR[1,1] <- round(sum(RRprior>(HArl) & RRprior<(HAru))/M,arr)
    respRR[2,1] <- round(sum(RRprior>(HPrl) & RRprior<(HPru))/M,arr)
    respRR[3,1] <- round(sum(RR>(HArl) & RR<(HAru))/M,arr)
    respRR[4,1] <- round(sum(RR>(HPrl) & RR<(HPru))/M,arr)
    respRR[5,1] <- round(sum(RR>seuilr)/M,arr)
  }  
  
  if(is.null(seuilo)){respOR<-NULL}else{
    respOR[1,1] <- round(sum(ORprior>(HAol) & ORprior<(HAou))/M,arr)
    respOR[2,1] <- round(sum(ORprior>(HPol) & ORprior<(HPou))/M,arr)
    respOR[3,1] <- round(sum(OR>(HAol) & OR<(HAou))/M,arr)
    respOR[4,1] <- round(sum(OR>(HPol) & OR<(HPou))/M,arr)
    respOR[5,1] <- round(sum(OR>seuilo)/M,arr)
  }  
  
  
  laliste <- list(resprior,res,respdi,respRR,respOR)
  names(laliste) <- c("Valeurs a priori","Valeurs a posteriori","Différence","Risque Relatif","Odds-Ratio")
  laliste[sapply(laliste,is.null)]<- NULL
  return(laliste)
  
}

## test
set.seed(1492)
N=100
Y<-rbinom(N,1,0.5)
Gr<-rep(0:1,each=N/2)



Cp2prop2IT(Y,Gr,0.5,0.5,0.5,0.5,
           seuild=0,seuilo=1,seuilr=1,
           HAdl=-100,HAdu=0,HPdl=0,HPdu=25,
           HArl=0.8,HAru=1.2,HPrl=0.93,HPru=17,
           HAol=0.8,HAou=1.2,HPol=0.87,HPou=20.8,
           arr=4,M=100000)

Gr2<-factor(Gr,levels=c(0,1),labels=c("Controle","Traitement"))

Cp2prop2IT(Y,Gr2,0.5,0.5,0.5,0.5,
           seuild=0,seuilo=1,seuilr=1,
           HAdl=-100,HAdu=0,HPdl=0,HPdu=25,
           HArl=0.8,HAru=1.2,HPrl=0.93,HPru=17,
           HAol=0.8,HAou=1.2,HPol=0.87,HPou=20.8,
           arr=4,M=100000)

Gr3<-Gr+rbinom(100,1,0.2)
Gr3<-factor(Gr3)

Cp2prop2IT(Y,Gr3,0.5,0.5,0.5,0.5,
           seuild=0,seuilo=1,seuilr=1,
           HAdl=-100,HAdu=0,HPdl=0,HPdu=25,
           HArl=0.8,HAru=1.2,HPrl=0.93,HPru=17,
           HAol=0.8,HAou=1.2,HPol=0.87,HPou=20.8,
           arr=4,M=100000)




###################### fonction NM
Cpmultprop2IT<-function(Y,Gr,priors,
                        seuild=NULL,seuilr=NULL,seuilo=NULL,
                        arr=3,M=100000){
  
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
  
  
  descriptif<-function(x){
    return(c(mean(x),quantile(x,probs=c(0.025,0.05,0.25,0.5,0.75,0.95,0.975))))
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
    descrxxprior[[j]]<-descriptif(xxprior[[j]])
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
  colnames(resprior)<-c("moy","p_0.025","p_0.05","p_0.25","p_0.50","p_0.75","p_0.95","p_0.975")
  rownames(resprior)<-c(noms.tab)
  
  res<-list(descrxx,unlist(descrdiff,recursive = F),unlist(descrRR,recursive = F),unlist(descrOR,recursive=F))
  res<-unlist(res,recursive=F)
  res<-lapply(res,function(i){round(i,4)})
  res<-lapply(res,as.list)
  res<-rbindlist(res)
  res<-data.frame(res)
  colnames(res)<-c("moy","p_0.025","p_0.05","p_0.25","p_0.50","p_0.75","p_0.95","p_0.975")
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
  cat(" Prior du groupe 1 (=",noms[1],") : Beta(",priors[1,1],",",priors[2,1],")","\n",sep="")
  cat(" Prior du groupe 2 (=",noms[2],") : Beta(",priors[1,2],",",priors[2,2],")","\n",sep="")
  if(Ngroup>2){
    cat(" Prior du groupe 3 (=",noms[3],") : Beta(",priors[1,3],",",priors[2,3],")","\n",sep="")
  }
  if(Ngroup>3){
    cat(" Prior du groupe 4 (=",noms[4],") : Beta(",priors[1,4],",",priors[2,4],")","\n",sep="")
  }
  cat("========================================================================================","\n")
  
  # 
  # if(is.null(seuild)){respdi<-NULL}else{
  #   respdi[1,1] <- round(sum(diffprior>(HAdl) & diffprior<(HAdu))/M,arr)
  #   respdi[2,1] <- round(sum(diffprior>(HPdl) & diffprior<(HPdu))/M,arr)
  #   respdi[3,1] <- round(sum(diff>(HAdl) & diff<(HAdu))/M,arr)
  #   respdi[4,1] <- round(sum(diff>(HPdl) & diff<(HPdu))/M,arr)
  #   respdi[5,1] <- round(sum(diff>seuild)/M,arr)
  # }  
  # 
  # if(is.null(seuilr)){respRR<-NULL}else{
  #   respRR[1,1] <- round(sum(RRprior>(HArl) & RRprior<(HAru))/M,arr)
  #   respRR[2,1] <- round(sum(RRprior>(HPrl) & RRprior<(HPru))/M,arr)
  #   respRR[3,1] <- round(sum(RR>(HArl) & RR<(HAru))/M,arr)
  #   respRR[4,1] <- round(sum(RR>(HPrl) & RR<(HPru))/M,arr)
  #   respRR[5,1] <- round(sum(RR>seuilr)/M,arr)
  # }  
  # 
  # if(is.null(seuilo)){respOR<-NULL}else{
  #   respOR[1,1] <- round(sum(ORprior>(HAol) & ORprior<(HAou))/M,arr)
  #   respOR[2,1] <- round(sum(ORprior>(HPol) & ORprior<(HPou))/M,arr)
  #   respOR[3,1] <- round(sum(OR>(HAol) & OR<(HAou))/M,arr)
  #   respOR[4,1] <- round(sum(OR>(HPol) & OR<(HPou))/M,arr)
  #   respOR[5,1] <- round(sum(OR>seuilo)/M,arr)
  # }  
  # 
  # 
  # laliste <- list(resprior,res,respdi,respRR,respOR)
  # names(laliste) <- c("Valeurs a priori","Valeurs a posteriori","Différence","Risque Relatif","Odds-Ratio")
  # laliste[sapply(laliste,is.null)]<- NULL
  # return(laliste)
  # 
  laliste<-list(resprior,res)
  names(laliste) <- c("Valeurs a priori","Valeurs a posteriori")
  return(laliste)
}

set.seed(1492)
N=100
Y<-rbinom(N,1,0.5)
Gr<-rep(0:1,each=N/2)

Gr3<-round(runif(100)*2)+1

Gr4<-round(runif(100)*3)+1

Cpmultprop2IT(Y,Gr3,priors=matrix(0.5,ncol=3,nrow=2),
              seuild=c(10,-20,30),seuilo=c(1.1,0.7,1.5),seuilr=c(0.8,1.2,1.1),
              arr=4,M=100000)

Cpmultprop2IT(Y,Gr4,priors=matrix(0.5,ncol=4,nrow=2),
              seuild=rep(c(10,20,30),2),seuilo=rep(1.5,6),seuilr=rep(1.2,6),
              arr=4,M=100000)

