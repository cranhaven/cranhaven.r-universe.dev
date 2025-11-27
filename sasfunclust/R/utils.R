
loglik <- function(parameters, data=NULL, X=NULL, timeindex=NULL,curve=NULL,grid=NULL, vars, FullS,W=NULL,AW_vec=NULL,P_tot=NULL,lambda_s=NULL,lambda_l=NULL){
  #Compute the log-likelihood and the penalized log-likelihood
  if(is.null(data)){

    if(length(dim(X))==2){
      n_obs<-dim(X)[2]
      n_t<-dim(X)[1]
      if(is.null(grid)) grid<-seq(0, 1, length.out = n_t)
      vec<-list(x=matrixcalc::vec(X),timeindex=rep(1:length(grid),n_obs),curve=rep(1:n_obs,each=length(grid)))
    }
    else if(is.null(dim(X))){

      if(is.null(grid)) stop("For irregularly sampled functional data grid must be provided")
      if(is.null(timeindex)) stop("For irregularly sampled functional timeindex grid must be provided")
      if(is.null(curve)) stop("For irregularly sampled functional timeindex curve must be provided")
      vec<-list(x=as.matrix(X),timeindex=timeindex,curve=curve)
    }
    else{
      stop("No data provided")
    }
    data=vec
  }

  CK=0
  perc_rankpapp2=NULL
  gamma <- vars$gamma
  gcov <- vars$gcov
  curve <- data$curve
  pi <- parameters$pi
  S <- FullS[data$timeindex,  ]
  N<-length(unique(data$curve))
  G <- dim(vars$gamma)[2]
  q <- dim(vars$gamma)[3]
  Gamma <- parameters$Gamma
  if(!is.null(perc_rankpapp2)){
    print(det(Gamma))
    print(2)
    gsvd <- svd(Gamma)
    p<-which(cumsum( gsvd$d)/sum( gsvd$d)>=perc_rankpapp2)[1]
    gsvd$d[ - (1:p)] <- 0
    Gamma <- gsvd$u %*% diag(gsvd$d) %*% t(gsvd$u)
  }
  if(is.null(parameters$mu))mu<-t(matrix(parameters$lambda.zero,q,G)+parameters$Lambda%*%t(parameters$alpha))
  else mu<-parameters$mu

  loglk <- 0
  for(i in 1:N){
    y <- data$x[data$curve==unique(data$curve)[i]]
    Si <- S[data$curve==unique(data$curve)[i],  ]
    ni <- dim(Si)[1]
    invvar <- diag(1/rep(parameters$sigma, ni))
    covx <- Si %*% Gamma %*% t(Si) +solve(invvar)
    covx_inv<-chol2inv(chol(covx))
    covx_det<-det(covx)
    temp<-sum(sapply(1:G,function(ll)pi[ll]*(2*base::pi)^(-ni/2)*covx_det^(-1/2)*exp(-(1/2)*t(y-Si%*%t(parameters$mu)[,ll])%*% covx_inv%*%(y-Si%*%t(parameters$mu)[,ll]))))
    if(temp==0)temp<-10^-20
    loglk=loglk+log(sum(temp))
  }

  if(!is.null(lambda_l)|!is.null(lambda_s)){
    p_l=lambda_l*t(AW_vec)%*%abs(P_tot%*%matrixcalc::vec(t(parameters$mu)))
    p_s=lambda_s*sum(sapply(1:G,function(ll)t(parameters$mu)[,ll]%*%W%*%t(parameters$mu)[,ll]))
    p_pi=CK*if(is.na(sum(sapply(1:G,function(ll)log(pi[ll]))))) is.na(sum(sapply(1:G,function(ll)log(pi[ll])))) else 0
    ploglk<-loglk-p_l-p_s+p_pi

    out<-round(c(loglk,ploglk[1,1]),digits = 2)


  }
  else{
    out<-loglk
  }
  return(out)
}
classify <- function(mod, data_new=NULL){
  #classification
  parameters<-mod$parameters
  vars<-mod$vars
  if(is.null(data_new))
    data=mod$data
  gamma <- vars$gamma
  gcov <- vars$gcov
  curve <- data$curve
  pi <- parameters$pi
  S <- mod$FullS[data$timeindex,  ]
  N<-length(unique(data$curve))
  G <- dim(vars$gamma)[2]
  q <- dim(vars$gamma)[3]
  Gamma <- parameters$Gamma
  po_pr<-matrix(0,N,G)
  for(i in 1:N){
    y <- data$x[data$curve==unique(data$curve)[i]]
    Si <- S[data$time[data$curve==unique(data$curve)[i]],  ]
    ni <- dim(Si)[1]
    invvar <- diag(1/rep(parameters$sigma, ni))
    covx<- Si %*% Gamma %*% t(Si) + solve(invvar)
    covx_inv<-chol2inv(chol(covx))
    covx_det<-det(covx)
    temp<-sum(sapply(1:G,function(ll)pi[ll]*(2*base::pi)^(-ni/2)*covx_det^(-1/2)*exp(-(1/2)*t(y-Si%*%t(parameters$mu)[,ll])%*% covx_inv%*%(y-Si%*%t(parameters$mu)[,ll]))))
    po_pr[i,]=temp/sum(temp)

  }
  if(is.null(data_new))po_pr<-vars$piigivej
  classes<-apply(po_pr,1,which.max)
  out<-list(classes=classes,po_pr=po_pr)
  return(out)
}
get_msdrule<-function(par,sds,comb_list,m1,m2,m3){
  #Component-wise cross validation procedure
  lambda_s_g=unique(comb_list[,2])
  lambda_l_g=unique(comb_list[,3])
  kk=1
  max_vec_nc<-sd_vec_nc<-numeric()
  new_comb_list<-matrix(0,length(lambda_s_g)*length(lambda_l_g),3)
  for (jj in 1:length(lambda_l_g)) {
    for (ii in 1:length(lambda_s_g)) {
      indexes<-which(comb_list[,2]==lambda_s_g[ii]&comb_list[,3]==lambda_l_g[jj])
      par_index<-par[indexes]
      sd_index<-sds[indexes]
      max<-which.max(par_index)
      onese<-which(par_index[1:(max)]>=par_index[max]-m1*sd_index[max])[1]
      max_vec_nc[kk]<-par_index[onese]
      sd_vec_nc[kk]<-sd_index[onese]
      new_comb_list[kk,]<-as.numeric(comb_list[indexes[onese],])
      kk=kk+1

    }
  }
  kk=1
  max_vec_s<-sd_vec_s<-numeric()
  new_comb_list2<-matrix(0,length(lambda_l_g),3)
  for (ii in 1:length(lambda_l_g)) {
    indexes<-which(new_comb_list[,3]==lambda_l_g[ii])
    par_index<-max_vec_nc[indexes]
    sd_index<-sd_vec_nc[indexes]
    max<-which.max(par_index)

    onese<-max(which(par_index>=par_index[max]-m2*sd_index[max]))
    max_vec_s[kk]<-par_index[onese]
    sd_vec_s[kk]<-sd_index[onese]
    new_comb_list2[kk,]<-as.numeric(new_comb_list[indexes[onese],])
    kk=kk+1
  }
  par_index<-max_vec_s
  sd_index<-sd_vec_s
  max<-which.max(par_index)
  if(m3*sd_index[max]>0.5*abs(max(par_index)-min(par_index)))lim=0.5*abs(max(par_index)-min(par_index)) else lim=m3*sd_index[max]
  onese<-max(which(par_index>=par_index[max]-lim))
  max_vec_l<-par_index[onese]
  sd_vec_l<-sd_index[onese]
  new_comb_list3<-as.numeric(new_comb_list2[onese,])
  num_clusters_opt<-new_comb_list3[1]
  lambda_s_opt<-new_comb_list3[2]
  lambda_l_opt<-new_comb_list3[3]

  return(c(num_clusters_opt=num_clusters_opt,
           lambda_s_opt=lambda_s_opt,
           lambda_l_opt=lambda_l_opt))
}
get_zero<-function(mod,mu_fd=NULL){
#Get Fraction of  portion of domain fused
  if(is.null(mu_fd)){
    K=dim(mod$parameters$mu)[1]
    if(K!=1){
    FullS <- mod$FullS
    mu<-mod$parameters$mu
    P<-matrix(0,((K-1)^2+(K-1))/2,K)
    ind<-c(1,K-1)
    for (ii in 1:(K-1)) {

      P[ind[1]:ind[2],ii]<-rep(1,length(ind[1]:ind[2]))
      if(length(ind[1]:ind[2])==1)
        aa<--1
      else
        aa<-diag(rep(-1,length(ind[1]:ind[2])))

      P[ind[1]:ind[2],(ii+1):K]<-aa
      ind<-ind+c((K-1)-(ii-1),(K-1)-(ii))
      ind<-c(min(((K-1)^2+(K-1))/2,ind[1]),min(((K-1)^2+(K-1))/2,ind[2]))
    }

      length(which(abs(t(FullS%*%t(P%*%mu)))<10^-4))/length(t(FullS%*%t(P%*%mu)))
    }
    else{
      NA
    }
  }
  else{
    grid<-seq(0,1,length.out = 60)
    mu<-fda::eval.fd(grid,mu_fd)
    K=dim(mu)[2]
    P<-matrix(0,((K-1)^2+(K-1))/2,K)
    ind<-c(1,K-1)
    for (ii in 1:(K-1)) {

      P[ind[1]:ind[2],ii]<-rep(1,length(ind[1]:ind[2]))
      if(length(ind[1]:ind[2])==1)
        aa<--1
      else
        aa<-diag(rep(-1,length(ind[1]:ind[2])))

      P[ind[1]:ind[2],(ii+1):K]<-aa
      ind<-ind+c((K-1)-(ii-1),(K-1)-(ii))
      ind<-c(min(((K-1)^2+(K-1))/2,ind[1]),min(((K-1)^2+(K-1))/2,ind[2]))
    }

    length(which(abs(P%*%t(mu))==0))/length(abs(P%*%t(mu)))
  }
}
