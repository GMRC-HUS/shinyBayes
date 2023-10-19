#' infe_moy 
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd


estim_moy_gibbs<-  function(x,mu0=NULL,s_m0 =NULL, s0=NULL, n_s0=NULL, n_sample =1000 ) {
  
  moy_data = mean(x,na.rm=T)
  var_data = var(x,na.rm = T)
  n = length(x)
  PHI<-matrix (nrow=n_sample, ncol=2)
  PHI[1,] <- phi <- c(moy_data,1/var_data)
  ###
  ###  Gibbs   sampling
  set.seed (1)
  for(s in 2:n_sample)   {
    # generate a new theta value from its full conditional
    mun <-(mu0/s_m0 + n*moy_data*phi[2])/(1/s_m0  + n*phi[2])
    t2n <- 1/(1/s_m0 + n*phi[2])
    phi[1]<-rnorm (1,mun,sqrt(t2n))
    # generate a new 1/sigmaˆ2 value   from its  full conditional
    nun <- n_s0+n
    s2n <- (n_s0*s0 + (n-1)*var_data  + n*(moy_data - phi[1])^2)/nun
    phi[2] <- rgamma(1,nun/2,nun*s2n/2)
    PHI[s,] <- phi
  }
  return(PHI)
}


compare_moy_gibbs <- function(X, Y,mu0=NULL,s_m0 =NULL, s0=NULL, n_s0=NULL,
                              type =  NULL, plusieurs = NULL, seuild=NULL, twit=NULL,seuil_global = NULL,
                              n_sample =100000,IC=0.95, arr=3) {
  Y = as.factor(Y)
  noms = levels(Y)
  Ngroup=nlevels(Y)
  pourcent_IC2 = (1-IC)/2
  long<-factorial(Ngroup)/(factorial(Ngroup-2)*2)
  
  if(is.null(type)){
    
  }else{
    if((!is.null(type)) &  "seuil" %in%type){
      
      if(!plusieurs & !is.null(seuil_global)){
        seuild = rep(seuil_global,long)
        
      }else{
        
        seuild = unlist(seuild%>%unlist%>%na.omit()%>%as.vector())
        
        
      }
    }
    
  }
  
  if(length(mu0) != Ngroup) stop('Prior mu0 n\' as pas la même nombre de groupe que la variable Y')
  if(length(s_m0) != Ngroup) stop('Prior s_m0 n\' as pas la même nombre de groupe que la variable Y')
  if(length(s0) != Ngroup) stop('Prior s0 n\' as pas la même nombre de groupe que la variable Y')
  if(length(n_s0) != Ngroup) stop('Prior n_s0 n\' as pas la même nombre de groupe que la variable Y')
  
  descriptif<-function(x, probs=c(pourcent_IC2,0.5, 1-pourcent_IC2 )){
    return(c(list(mean(x)),quantile(x,probs=probs)))
  }
  res_infe<-lapply(1:Ngroup, function(x) estim_moy_gibbs(X[which(Y==levels(Y)[x])],
                                                         mu0[x],s_m0[x],s0[x],n_s0[x], n_sample =n_sample
  ))
  res_prior<- lapply(1:Ngroup, function(x) rnorm(n_sample,
                                                 mu0[x],s0[x]
  ))
  
  diffprior<-diffpost<-diff<- list()
  
  for(i in 1:Ngroup){
    for(j in 2:Ngroup){
      if(j<=i)next
      diffprior[[i]]<-descriptif(res_prior[[i]]-res_prior[[j]])
      nomsdiff<-c(paste0("Diff Moy(groupe=",noms[i],")-Moy(Groupe=",noms[j],")"))
      diff[[i]]<-res_infe[[i]][,1]-res_infe[[j]][,1]
      diffpost[[i]]<-descriptif(res_infe[[i]][,1]-res_infe[[j]][,1])
    }
  }
  
  
  
  nomsx<-paste0("Moy groupe =",noms)
  noms.tab<-c(nomsx,nomsdiff)
  
  
  resprior<-c(lapply(res_prior,descriptif),diffprior)
  # resprior<-unlist(resprior,recursive=F)
  # resprior<-lapply(resprior,function(i){round(i,4)})
  resprior<-rbindlist(resprior)
  resprior<-resprior%>%mutate_all(function(i){round(i,arr)})
  
  # print(c("moy", paste0(c(pourcent_IC2,0.5, 1-pourcent_IC2 )*100, "%") ))
  print(res)
  print(noms.tab)
  colnames(resprior)<-c("moy", paste0(c(pourcent_IC2,0.5, 1-pourcent_IC2 )*100, "%") )
  rownames(resprior)<-c(noms.tab)
  # print(resprior)
  
  res<-c(lapply(res_infe,descriptif),diffpost)
  
  
  
  res<-rbindlist(res)
  
  res<-res%>%mutate_all(function(i){round(i,arr)})
  
  colnames(res)<-c("moy", paste0(c(pourcent_IC2,0.5, 1-pourcent_IC2 )*100, "%") )
 
  rownames(res)<-c(noms.tab)
  
  
  
  
  
  if(is.null(type)){
  }else if(type =="seuil"){
    seuils<-c(seuild)
    tests<-NULL
    for(i in 1:length(diff)){
      tests<-c(tests,round(mean(diff[[i]]>seuils[i]),arr))
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
    data$`Pr Ha` = c(round(mean(between(diff[[lequel]][,1]* ifelse_perso(signe==1, 1, -1),data["Diff","minHa"],data["Diff","maxHa"])),arr))
    
    
    data$`Pr Hr` =  c(round(mean(between(diff[[lequel]][,1]* ifelse_perso(signe==1, 1, -1),data["Diff","minHr"],data["Diff","maxHr"])),arr))              
    
    
    
    
  }
  
  # prior<-  data.frame(Loi = "Beta", "Parametre alpha" =priors[1,] , "Parametre beta" = priors[2,],row.names = paste("Groupe",noms), check.names = F)
  
  
  
  laliste<-list(
    # prior
    resprior,res)
  names(laliste) <- c(#"prior",
    "Valeurs a priori","Valeurs a posteriori" )
  if(is.null(type)){
    
  }else if(type =="twit" & !is.null(type)){
    res_twit= list(TwoIt = data)
    names(res_twit) = twit$var
    laliste<-append(laliste,res_twit)
  }
  
  
  
  return(laliste)
  
  res_infe
  
  
  
  
  
}


