
# Simulate data -----------------------------------------------------------

#' @title Simulate data for Robust Functional ANOVA
#' @description Generate synthetic data as in the simulation study of Centofanti et al. (2021) with the addition of the case of bi-variate functional data. All the details are in  Centofanti et al. (2021).
#' @param scenario  A  character strings indicating the scenario considered. It could be "one-way", "two-way", "one-way surface" and "two-way surface".
#' @param mean A character strings indicating the type of mean function in one-way ANOVA. It could be "M1", "M2", and "M3".
#' @param con A character strings indicating the type of contamination function. It could be "C0", for no contamination, "C1", "C2", "C3", "C4", "C5", and "C6".
#' @param p The parameter related to the bernoulli variable in the contamination function.
#' @param M The contamination size constant.
#' @param n_i The number of observation for each group.
#' @param k_1 The number of level for the first main effect.
#' @param k_2 The number of level for the second main effect. For One-way ANOVA, it is ignored.
#' @param alpha The parameter a in the Two-way ANOVA scenarios. For One-way ANOVA, it is ignored.
#' @param beta The parameter b in the Two-way ANOVA scenarios. For One-way ANOVA, it is ignored.
#' @param sd The sigma parameter in the covariance of the error function.
#' @param grid The grid over which the functional data are observed.
#' @param err The direction of the dependence in the error function for the case of bi-variate functional data. It could be either "s", for dependence along the first dimension or "t" for dependence along the second dimension.
#' @return
#' A list containing the following arguments:
#'\itemize{
#' \item \code{X_fdata}: The generated functional data.
#'
#' \item\code{label_1}: The vector of containing group label corresponding to the first main effect.
#'
#'  \item\code{label_2}: The vector of containing group label corresponding to the second main effect. For one-way ANOVA, it is NULL.
#'}
#'@seealso \code{\link{rofanova}} \code{\link{fusem}} \code{\link{funmad}}
#'
#' @export
#' @references
#' Centofanti, F., Colosimo, B.M., Grasso, M.L., Menafoglio, A., Palumbo, B., Vantini, S. (2021).
#' Robust Functional ANOVA with Application to Additive Manufacturing.
#' \emph{arXiv preprint arXiv:2112.10643}.
#' @examples
#' library(rofanova)
#' data_out<-simulate_data(scenario="one-way")
#' label_1=data_out$label_1
#' X_fdata<-data_out$X_fdata
#' B=10
#' cores=1
#' per_list_median<-rofanova(X_fdata,label_1,B = B,family="median",cores=cores)
#' pvalue_median_vec<-per_list_median$pval_vec
#' per_list_huber<-rofanova(X_fdata,label_1,B = B,family="huber",cores=cores)
#' pvalue_huber_vec<-per_list_huber$pval_vec
#' per_list_bisquare<-rofanova(X_fdata,label_1,B = B,family="bisquare",cores=cores)
#' pvalue_bisquare_vec<-per_list_bisquare$pval_vec
#' per_list_hampel<-rofanova(X_fdata,label_1,B = B,family="hampel",cores=cores)
#' pvalue_hampel_vec<-per_list_hampel$pval_vec
#' per_list_optimal<-rofanova(X_fdata,label_1,B = B,family="optimal",cores=cores)
#' pvalue_optimal<-per_list_optimal$pval
simulate_data<-function(scenario="one-way",mean="M1",con="C0",p=0.1,M=1,n_i=25,k_1=3,k_2=3,alpha=0,beta=0,sd=0.01,grid=seq(0,1,length.out = 30),err="s"){

  if(scenario=="one-way")
    return(simulate_data_oneway(mean=mean,con=con,p=p,M=M,n_i=n_i,k=k_1,sd=sd,grid=grid))
  if(scenario=="two-way")
    return(simulate_data_twoway(con=con,p=p,M=M,n_i=n_i,k_1=k_1,k_2=k_2,alpha=alpha,beta=beta,sd=sd,grid=grid))
  if(scenario=="one-way surface")
    return(simulate_data_oneway_sur(mean=mean,con=con,p=p,M=M,n_i=n_i,k=k_1,sd=sd,grid=grid,err=err))
  if(scenario=="two-way surface")
    return(simulate_data_twoway_sur(con=con,p=p,M=M,n_i=n_i,k_1=k_1,k_2=k_2,alpha=alpha,beta=beta,sd=sd,grid=grid,err=err))
}
simulate_data_oneway<-function(mean="M1",con="C1",p=0.1,M=1,n_i=25,k=3,sd=0.01,grid=seq(0,1,length.out = 30)){

  print("Simulated data one-way")
  if(mean=="M1"){
    mean_function<-function(t,i)t*(1-t)
  }
  else if(mean=="M2"){
    mean_function<-function(t,i)(t^(i))*(1-t)^(6-i)
  }
  else if(mean=="M3"){
    mean_function<-function(t,i)t^(i/5)*(1-t)^(6-(i/5))
  }
  if(con=="C0"){
    cont_function<-function(n_g,p_g,M_g,ii)0
  }
  else if(con=="C1"){
    cont_function<-function(n_g,p_g,M_g,ii)M_g*matrix(stats::rbinom(n_g,1,p_g)*sample(c(-1,1),n_g,replace = T),length(grid),n_g,byrow = T)
  }
  else if(con=="C2"){
    cont_function<-function(n_g,p_g,M_g,ii){
      T_i<-stats::runif(n_g,0,0.75)
      matrix_old<-M_g*matrix(stats::rbinom(n_g,1,p_g)*sample(c(-1,1),n_g,replace = T),length(grid),n_g,byrow = T)
      matrix_new<-matrix(0,length(matrix_old[,1]),n_g)
      for (ii in 1:n_g) {
        ind<-(round(T_i[ii]*(length(matrix_old[,1])-1))+1)
        matrix_new[ind:length(matrix_old[,1]),ii]<-matrix_old[ind:length(matrix_old[,1]),ii]

      }
      return(matrix_new)
    }
  }
  else if(con=="C3"){
    cont_function<-function(n_g,p_g,M_g,ii){
      g<-mean_function(matrix(grid,length(grid),n_i),matrix(ll,length(grid),n_i))+t(rproc2fdata(n_i,t=grid,sigma="vexponential",par.list = list(scale=sd^2,theta=0.00001))$data)
      h<-mean_function(matrix(grid,length(grid),n_i),matrix(ll,length(grid),n_i))+t(rproc2fdata(n_i,t=grid,sigma="vexponential",par.list = list(scale=(sd+2)^2,theta=1))$data)
      ber<-matrix(stats::rbinom(n_g,1,p_g),length(grid),n_g,byrow = T)
      out<-(1-ber)*g+ber*h
      return(out)
    }
  }
  else if(con=="C4"){
    cont_function<-function(n_g,p_g,M_g,ii)(-1)^(ii)*M_g*matrix(stats::rbinom(n_g,1,p_g),length(grid),n_g,byrow = T)
  }
  else if(con=="C5"){
    cont_function<-function(n_g,p_g,M_g,ii){
      T_i<-stats::runif(n_g,0,0.75)
      matrix_old<-(-1)^(ii)*M_g*matrix(stats::rbinom(n_g,1,p_g),length(grid),n_g,byrow = T)
      matrix_new<-matrix(0,length(matrix_old[,1]),n_g)
      for (ll in 1:n_g) {
        ind<-(round(T_i[ll]*(length(matrix_old[,1])-1))+1)
        matrix_new[ind:length(matrix_old[,1]),ll]<-matrix_old[ind:length(matrix_old[,1]),ll]

      }
      return(matrix_new)
    }
  }
  else if(con=="C6"){
    cont_function<-function(n_g,p_g,M_g,ii){
      g<-mean_function(matrix(grid,length(grid),n_i),matrix(ll,length(grid),n_i))+t(rproc2fdata(n_i,t=grid,sigma="vexponential",par.list = list(scale=sd^2,theta=0.00001))$data)
      h<-mean_function(matrix(grid,length(grid),n_i),matrix(ll,length(grid),n_i))+t(rproc2fdata(n_i,t=grid,sigma="vexponential",par.list = list(scale=(sd+2+(-1)^(ii))^2,theta=1))$data)
      ber<-matrix(stats::rbinom(n_g,1,p_g),length(grid),n_g,byrow = T)
      out<-(1-ber)*g+ber*h
      return(out)
    }
  }
  if(con=="C3"|con=="C6"){
    data_list<-list()
    for (ll in 1:k) {

      data_list[[ll]]<-cont_function(n_i,p,M,ll)
    }

  }
  else{
    data_list<-list()
    for (ll in 1:k) {
      data_list[[ll]]<-mean_function(matrix(grid,length(grid),n_i),matrix(ll,length(grid),n_i))+t(rproc2fdata(n_i,t=grid,sigma="vexponential",par.list = list(scale=sd^2,theta=0.00001))$data)+cont_function(n_i,p,M,ll)
    }
  }

  data<-do.call("cbind", data_list)
  X_fdata<-fdata(t(data),argvals = grid)
  label<-rep(1:k,each=n_i)
  out=list(X_fdata=X_fdata,
           label_1=label,
           label_2=NULL)
  return(out)
}
simulate_data_twoway<-function(con="C1",n_i=25,k_1=3,k_2=3,p=0.1,M=1,alpha=0.05,beta=0.05,sd=0.01,grid=seq(0,1,length.out = 30)){

  print("Simulated data Two-way")
  mean_function<-function(t,i,j)t*(1-t)
  f1_function<-function(t,i,j,alpha,beta)alpha*(-1)^(i)*abs(sin(4*pi*t))
  f2_function<-function(t,i,j,alpha,beta)beta*(-1)^(j)*ifelse(t>0.5,1,0)
  int_function<-function(t,i,j,alpha,beta)-f1_function(t,i,j,alpha,beta)*f2_function(t,i,j,alpha,beta)*ifelse(alpha>=0.25,1,0)
  Y<-function(t,i,j,alpha,beta)mean_function(t,i,j)+f1_function(t,i,j,alpha,beta)+f2_function(t,i,j,alpha,beta)+int_function(t,i,j,alpha,beta)

  if(con=="C0"){
    cont_function<-function(n_g,p_g,M_g,ii,jj)0
  }
  else if(con=="C1"){
    cont_function<-function(n_g,p_g,M_g,ii,jj)M_g*matrix(stats::rbinom(n_g,1,p_g)*sample(c(-1,1),n_g,replace = T),length(grid),n_g,byrow = T)
  }
  else if(con=="C2"){
    cont_function<-function(n_g,p_g,M_g,ii,jj){
      T_i<-stats::runif(n_g,0,0.75)
      matrix_old<-M_g*matrix(stats::rbinom(n_g,1,p_g)*sample(c(-1,1),n_g,replace = T),length(grid),n_g,byrow = T)
      matrix_new<-matrix(0,length(matrix_old[,1]),n_g)
      for (ii in 1:n_g) {
        ind<-(round(T_i[ii]*(length(matrix_old[,1])-1))+1)
        matrix_new[ind:length(matrix_old[,1]),ii]<-matrix_old[ind:length(matrix_old[,1]),ii]

      }
      return(matrix_new)
    }
  }
  else if(con=="C3"){
    cont_function<-function(n_g,p_g,M_g,ii,jj){
      g<-Y(matrix(grid,length(grid),n_i),ii,jj,alpha,beta)+t(rproc2fdata(n_i,t=grid,sigma="vexponential",par.list = list(scale=sd^2,theta=0.00001))$data)
      h<-Y(matrix(grid,length(grid),n_i),ii,jj,alpha,beta)+t(rproc2fdata(n_i,t=grid,sigma="vexponential",par.list = list(scale=(sd+2)^2,theta=1))$data)
      ber<-matrix(stats::rbinom(n_g,1,p_g),length(grid),n_g,byrow = T)
      out<-(1-ber)*g+ber*h
      return(out)
    }
  }
  else if(con=="C4"){
    cont_function<-function(n_g,p_g,M_g,ii,jj)(-1)^(ii)*M_g*matrix(stats::rbinom(n_g,1,p_g),length(grid),n_g,byrow = T)
  }
  else if(con=="C5"){
    cont_function<-function(n_g,p_g,M_g,ii,jj){
      T_i<-stats::runif(n_g,0,0.75)
      matrix_old<-(-1)^(ii)*M_g*matrix(stats::rbinom(n_g,1,p_g),length(grid),n_g,byrow = T)
      matrix_new<-matrix(0,length(matrix_old[,1]),n_g)
      for (ll in 1:n_g) {
        ind<-(round(T_i[ll]*(length(matrix_old[,1])-1))+1)
        matrix_new[ind:length(matrix_old[,1]),ll]<-matrix_old[ind:length(matrix_old[,1]),ll]

      }
      return(matrix_new)
    }
  }
  else if(con=="C6"){
    cont_function<-function(n_g,p_g,M_g,ii,jj){
      g<-Y(matrix(grid,length(grid),n_i),ii,jj,alpha,beta)+t(rproc2fdata(n_i,t=grid,sigma="vexponential",par.list = list(scale=sd^2,theta=0.00001))$data)
      h<-Y(matrix(grid,length(grid),n_i),ii,jj,alpha,beta)+t(rproc2fdata(n_i,t=grid,sigma="vexponential",par.list = list(scale=(sd+2+(-1)^(ii))^2,theta=1))$data)
      ber<-matrix(stats::rbinom(n_g,1,p_g),length(grid),n_g,byrow = T)
      out<-(1-ber)*g+ber*h
      return(out)
    }
  }
  if(con=="C3"|con=="C6"){
    data_list<-list()
    kk=1
    for (ii in 1:k_1) {
      for (jj in 1:k_2) {
        data_list[[kk]]<-cont_function(n_i,p,M,ii,jj)
        kk=kk+1
      }
    }

  }
  else{
    data_list<-list()
    kk=1
    for (ii in 1:k_1) {
      for (jj in 1:k_2) {
        data_list[[kk]]<-Y(matrix(grid,length(grid),n_i),ii,jj,alpha,beta)+t(rproc2fdata(n_i,t=grid,sigma="vexponential",par.list = list(scale=sd^2,theta=0.00001))$data)+cont_function(n_i,p,M,ii,jj)
        kk=kk+1
      }


    }
  }

  data<-do.call("cbind", data_list)
  X_fdata<-fdata(t(data),argvals = grid)
  label_1<-rep(1:k_1,each=n_i*k_2)
  label_2<-rep(rep(1:k_2,each=n_i),k_1)
  out=list(X_fdata=X_fdata,
           label_1=label_1,
           label_2=label_2)
  return(out)
}
simulate_data_oneway_sur<-function(mean="M1",con="C1",p=0.1,M=1,n_i=10,k=3,sd=0.01,grid=seq(0,1,length.out = 30),err="s"){

  print("Simulated data one-way surface")
  if(mean=="M1"){
    mean_function<-function(s,t,i)t*(1-t)+s*(1-s)
  }
  else if(mean=="M2"){
    mean_function<-function(s,t,i)(t^(i))*(1-t)^(6-i)+(s^(i))*(1-s)^(6-i)
  }
  if(con=="C0"){
    cont_function<-function(n_g,p_g,M_g,ii)0
  }
  else if(con=="C1"){
    cont_function<-function(n_g,p_g,M_g,ii)M_g*matrix(stats::rbinom(n_g,1,p_g)*sample(c(-1,1),n_g,replace = T),length(grid),n_g,byrow = T)
  }
  expand_grid<-expand.grid(grid,grid)
  data<-array(0,c(k*n_i,length(grid),length(grid)))
  kk=1
  for (ll in 1:k) {

    for (mm in 1:n_i) {
      error<-sapply(1:length(grid),function(ii)rproc2fdata(1,t=grid,sigma="vexponential",par.list = list(scale=sd^2,theta=1))$data)
      mat_err<-if(err=="s")error else t(error)
      data[kk,,] <-matrix(mean_function(expand_grid[,1],expand_grid[,2],ll),length(grid),length(grid))+mat_err+matrix(cont_function(1,p,M,ll),length(grid),length(grid))
      kk=kk+1
    }
  }
  X_fdata<-fdata(data,argvals = list(grid,grid))
  label<-rep(1:k,each=n_i)
  out=list(X_fdata=X_fdata,
           label_1=label,
           label_2=NULL)
  return(out)
}
simulate_data_twoway_sur<-function(con="C1",n_i=10,k_1=3,k_2=3,p=0.1,M=1,alpha=0.05,beta=0.05,sd=0.01,grid=seq(0,1,length.out = 30),err="s"){


  print("Simulated data Two-way surface")
  mean_function<-function(s,t,i,j)t*(1-t)+s*(1-s)
  f1_function<-function(s,t,i,j,alpha,beta)alpha*(-1)^(i)*(abs(sin(4*pi*t))+abs(sin(4*pi*s)))
  f2_function<-function(s,t,i,j,alpha,beta)beta*(-1)^(j)*ifelse(t>0.5,1,0)*ifelse(s>0.5,1,0)
  int_function<-function(s,t,i,j,alpha,beta)-f1_function(s,t,i,j,alpha,beta)*f2_function(s,t,i,j,alpha,beta)*ifelse(alpha>=0.25,1,0)
  Y<-function(s,t,i,j,alpha,beta)mean_function(s,t,i,j)+f1_function(s,t,i,j,alpha,beta)+f2_function(s,t,i,j,alpha,beta)+int_function(s,t,i,j,alpha,beta)
  if(con=="C0"){
    cont_function<-function(n_g,p_g,M_g)0
  }
  else if(con=="C1"){
    cont_function<-function(n_g,p_g,M_g)M_g*matrix(stats::rbinom(n_g,1,p_g)*sample(c(-1,1),n_g,replace = T),length(grid),n_g,byrow = T)
  }
  expand_grid<-expand.grid(grid,grid)
  data<-array(0,c(k_1*k_2*n_i,length(grid),length(grid)))
  kk=1
  for (ii in 1:k_1) {
    for (jj in 1:k_2) {
      for (mm in 1:n_i) {
        error<-sapply(1:length(grid),function(ii)rproc2fdata(1,t=grid,sigma="vexponential",par.list = list(scale=sd^2,theta=0.00001))$data)
        mat_err<-if(err=="s")error else t(error)
        data[kk,,] <-matrix(Y(expand_grid[,1],expand_grid[,2],ii,jj,alpha,beta),length(grid),length(grid))+mat_err+matrix(cont_function(1,p,M),length(grid),length(grid))
        kk=kk+1
      }
    }
  }
  X_fdata<-fdata(data,argvals = list(grid,grid))
  label_1<-rep(1:k_1,each=n_i*k_2)
  label_2<-rep(rep(1:k_2,each=n_i),k_1)
  out=list(X_fdata=X_fdata,
           label_1=label_1,
           label_2=label_2)
  return(out)
}


# Robust location/scale estimation -----------------------------------------

#' @title The scale equivariant functional M-estimator
#' @description Compute the scale equivariant functional M-estimator as described in Centofanti et al. (2021).
#' @param sig0_g Estimate of the standard error of \code{X}. If NULL, the functional mean is used. Default is NULL.
#' @return
#' A list containing the following arguments:
#'\itemize{
#' \item \code{mu}: The scale equivariant functional M-estimator .
#'
#' \item\code{mu0_g}: \code{mu0_g}.
#'
#'  \item\code{sig0_g}: \code{sig0_g}.
#'}
#'@seealso \code{\link{rofanova}} \code{\link{funmad}}
#'
#' @export
#' @references
#' Centofanti, F., Colosimo, B.M., Grasso, M.L., Menafoglio, A., Palumbo, B., Vantini, S. (2021).
#' Robust Functional ANOVA with Application to Additive Manufacturing.
#' \emph{arXiv preprint arXiv:2112.10643}.
#' @inheritParams rofanova
#' @examples
#'
#' library(rofanova)
#' data_out<-simulate_data(scenario="one-way")
#' X_fdata<-data_out$X_fdata
#' per_list_median<-fusem(X_fdata)

fusem<-function (X, family = "bisquare", eff = 0.95, maxit = 50, tol = 1e-04,mu0_g=NULL,sig0_g=NULL){
  if(length(dim(X$data))==2)
    return(FlocScaleM_mon(X, family = family, eff = eff, maxit = maxit, tol = tol,mu0_g=mu0_g,sig0_g=sig0_g))
  if(length(dim(X$data))==3)
    return(FlocScaleM_sur(X, family = family, eff = eff, maxit = maxit, tol = tol,mu0_g=mu0_g,sig0_g=sig0_g))
}

FlocScaleM_mon<-function (X, family = "bisquare", eff = 0.95, maxit = 50, tol = 1e-04,mu0_g=NULL,sig0_g=NULL){
  x=X
  kpsi <- switch(family, bisquare = 1, huber = 2, optimal = 3, median=5,mean=6,hampel=7,8)
  if (kpsi == 8) {
    stop(paste0(family, " - No such rho function"))
  }
  else if (kpsi==6){
    mu0 = func.med.FM(x,trim=0.1)
    sig0 = sqrt(func.trimvar.FM(x,trim=0.2))
    mu=func.mean(x)
    resu <- list(mu = mu,mu0=func.med.FM(x,trim=0.1),sig0=sig0)
  }
  else if (kpsi==5){
    ktun=keff=1
    if(is.null(mu0_g)) mu0=func.mean(x)
    else mu0=mu0_g
    if(is.null(sig0_g)) sig0=sqrt(func.var(x))
    else sig0=sig0_g
    if (max(sig0) < 1e-10) {
      mu = 0
      sigma = 0
    }
    else {
      resi_start =norm_fdata_c((x-mu0)/sig0 )
      ww_start= wfun_c(resi_start, kpsi,ktun)
      if(all(ww_start==0))mu0=func.trim.FM(x,trim=0.5)
      mu=iteration(x,mu0,sig0,kpsi,ktun,tol, maxit)
    }
    resu <- list(mu = mu,mu0=mu0_g,sig0=sig0_g)
  }
  else {
    kBis = c(3.44, 3.88, 4.685)
    kHub = c(0.732, 0.981, 1.34)
    kHampel<-rbind(c(0.9539156, 1.9078312, 3.8156624),c(NA,NA,NA),c(1.3521, 3.1549, 7.2112))
    kOpt<-c(0.87,0.94,1.06)
    efis = c(0.85, 0.9, 0.95)
    keff = match(eff, efis)
    if(kpsi==7)ktun<-kHampel[keff,]
    else if(kpsi==1)ktun<-kBis[keff]
    else if(kpsi==2)ktun<-kHub[keff]
    else if(kpsi==3)ktun<-kOpt[keff]
    if(is.na(ktun[1])){
      print(c(eff, " No such eff"))
      keff = 0
    }
    else {
      if(is.null(mu0_g)) mu0=func.mean(x)
      else mu0=mu0_g
      if(is.null(sig0_g)) sig0=sqrt(func.var(x))
      else sig0=sig0_g
      if (max(sig0) < 1e-10) {
        mu = 0
        sigma = 0
      }
      else {
        resi_start =norm_fdata_c((x-mu0)/sig0 )
        ww_start= robustbase::Mwgt(resi_start, ktun, family)
        if(all(ww_start==0))mu0=func.trim.FM(x,trim=0.5)
        mu=iteration_ho(x,mu0,sig0,matrix(ktun),family,tol, maxit)
      }
      resu <- list(mu = mu,mu0=mu0_g,sig0=sig0_g)
    }
  }
  return(resu)
}
FlocScaleM_sur<-function (X, family = "bisquare", eff = 0.95, maxit = 50, tol = 1e-04,mu0_g=NULL,sig0_g=NULL){
  x=X
  kpsi <- switch(family, bisquare = 1, huber = 2, optimal = 3, median=5,mean=6,hampel=7,8)
  if (kpsi == 8) {
    stop(paste0(family, " - No such rho function"))
  }
  else if (kpsi==6){
    mu0 = func.med.FM(x,trim=0.1)
    sig0 = sqrt(func.trimvar.FM(x,trim=0.2))
    mu=func.mean(x)
    resu <- list(mu = mu,mu0=func.med.FM(x,trim=0.1),sig0=sig0)
  }
  else if (kpsi==5){
    ktun=keff=1
    if(is.null(mu0_g)) mu0=func.mean_sur(x)
    else mu0=mu0_g
    if(is.null(sig0_g)) sig0=sqrt(func.var_sur(x))
    else sig0=sig0_g
    if (max(sig0) < 1e-10) {
      mu = 0
      sigma = 0
    }
    else {
      n<-dim(x$data)[1]
      st<-standardize_sur(x,mu0,sig0)
      resi_start =norm_fdata_c_sur(st )
      ww_start= wfun_c(resi_start, kpsi,ktun)
      if(all(ww_start==0))mu0=func.trim.FM(x,trim=0.5)
      mu=iteration_sur(x,mu0,sig0,kpsi,ktun,tol, maxit)
    }
    resu <- list(mu = mu,mu0=mu0_g,sig0=sig0_g)
  }
  else {
    kBis = c(3.44, 3.88, 4.685)
    kHub = c(0.732, 0.981, 1.34)
    kHampel<-rbind(c(0.9539156, 1.9078312, 3.8156624),c(NA,NA,NA),c(1.3521, 3.1549, 7.2112))
    kOpt<-c(0.87,0.94,1.06)
    efis = c(0.85, 0.9, 0.95)
    keff = match(eff, efis)
    if(kpsi==7)ktun<-kHampel[keff,]
    else if(kpsi==1)ktun<-kBis[keff]
    else if(kpsi==2)ktun<-kHub[keff]
    else if(kpsi==3)ktun<-kOpt[keff]
    if(is.na(ktun[1])){
      print(c(eff, " No such eff"))
      keff = 0
    }
    else {
      if(is.null(mu0_g)) mu0=func.mean_sur(x)
      else mu0=mu0_g
      if(is.null(sig0_g)) sig0=sqrt(func.var_sur(x))
      else sig0=sig0_g
      if (max(sig0) < 1e-10) {
        mu = 0
        sigma = 0
      }
      else {
        n<-dim(x$data)[1]
        st<-standardize_sur(x,mu0,sig0)
        resi_start =norm_fdata_c_sur(st)
        ww_start= robustbase::Mwgt(resi_start, ktun, family)
        if(all(ww_start==0))sig0=sqrt(func.var_sur(x))
        mu=iteration_ho_sur(x,mu0,sig0,matrix(ktun),family,tol, maxit )
      }
      resu <- list(mu = mu,mu0=mu0_g,sig0=sig0_g)
    }
  }
  return(resu)
}
rfun<-function (x,rho="bisquare",eff=0.95){
  kpsi <- switch(rho, bisquare = 1, huber = 2, optimal = 3,
                 median=5,mean=6,hampel=7,8)
  if (kpsi == 8) {
    stop(paste0(rho, " - No such rho function"))
  }
  else if(kpsi == 5)r=abs(x)
  else if(kpsi == 6)r=x^2
  else{
    kBis = c(3.44, 3.88, 4.685)
    kHub = c(0.732, 0.981, 1.34)
    kHampel<-rbind(c(0.9539156, 1.9078312, 3.8156624),c(NA,NA,NA),c(1.3521, 3.1549, 7.2112))
    kOpt<-c(0.87,0.94,1.06)
    efis = c(0.85, 0.9, 0.95)
    keff = match(eff, efis)
    if(kpsi==7)ktun<-kHampel[keff,]
    else if(kpsi==1)ktun<-kBis[keff]
    else if(kpsi==2)ktun<-kHub[keff]
    else if(kpsi==3)ktun<-kOpt[keff]
    if(is.na(ktun[1])){
      print(c(eff, " No such eff"))
      keff = 0
    }
    else {
      r=robustbase::Mpsi(x, cc = ktun, psi = rho, deriv = -1)
    }
  }
  return(r)
}


#' @title The functional normalized median absolute deviation estimators
#' @description Compute  the functional normalized median absolute deviation (FuNMAD) estimator  as described in Centofanti et al. (2021).
#' @param ... Additional argument to be passed to \code{fusem}.
#' @return The FuNMAD estimator.
#'@seealso \code{\link{rofanova}} \code{\link{fusem}}
#'
#' @export
#' @references
#' Centofanti, F., Colosimo, B.M., Grasso, M.L., Menafoglio, A., Palumbo, B., Vantini, S. (2021).
#' Robust Functional ANOVA with Application to Additive Manufacturing.
#' \emph{arXiv preprint arXiv:2112.10643}.
#' @inheritParams rofanova
#' @examples
#'
#' library(rofanova)
#' data_out<-simulate_data(scenario="one-way")
#' X_fdata<-data_out$X_fdata
#' per_list_median<-funmad(X_fdata)
funmad<-function(X,...){
  if(length(dim(X$data))==2)
    return(scale_fun_pw(X,...))
  if(length(dim(X$data))==3)
    return(scale_fun_pw_sur(X,...))
}
scale_fun_pw<-function(X,...){
  x=X
  med<-FlocScaleM_mon(x, family = "median",...)$mu
  diff<-abs(x-med)
  MAD<-(1/0.675)*pw_median(diff)
  return(MAD)
}
scale_fun_pw_sur<-function(x,...){
  med<-FlocScaleM_sur(x, family = "median", ...)$mu
  diff<-abs(standardize_sur(x,med))
  MAD<-(1/0.675)*pw_median_sur(diff)
  return(MAD)
}
pw_median<-function(x){
  data<-x$data
  nvar<-dim(data)[2]
  grid<-x$argvals
  data_new<-sqrt((sapply(1:nvar, function(ii)stats::median(data[,ii])^2)))
  fdata(data_new,argvals = grid)
}
pw_median_sur<-function(x){
  grid<-x$argvals
  data_new<-apply(x$data , c(2,3), stats::median)
  fdata(data_new,argvals = grid)
}

scale_res_oneway_pw<-function(x,label,...){
  k=length(unique(label))
  grid<-x$argval
  sig0<-fdata(rep(1,length(x$data[1,])),argvals = x$argvals)
  res_list<-lapply(1:k,function(ii)x[label==ii]-FlocScaleM_mon(x[label==ii], family = "median", sig0_g=sig0,  ...)$mu )
  res_data<-Reduce("rbind",lapply(1:k, function(ii)res_list[[ii]]$data))
  res<-fdata(res_data,argvals =grid )
  med<-(1/0.675)*pw_median(abs(res))
  return(med)
}
scale_res_oneway_pw_sur<-function(x,label,...){
  k=length(unique(label))
  grid<-x$argvals
  sig0<-fdata(array(1,dim = c(1,dim(x$data)[2],dim(x$data)[3])),argvals = x$argvals)
  res_list<-lapply(1:k,function(ii)center_sur(ex_fdata(x,label==ii),FlocScaleM_sur(ex_fdata(x,label==ii), family = "median", sig0_g=sig0,...)$mu ))
  res_data<-abind::abind(lapply(1:k, function(ii)res_list[[ii]]$data),along = 1)
  res<-fdata(res_data,argvals =grid )
  med<-(1/0.675)*pw_median_sur(abs(res))
  return(med)
}
scale_res_twoway_pw<-function(x,label_1,label_2,...){
  k_1=length(unique(label_1))
  k_2=length(unique(label_2))
  sig0<-fdata(rep(1,length(x$data[1,])),argvals = x$argvals)
  grid<-x$argvals
  group_mean_ij<-list()
  for (ii in 1:k_1) {
    group_mean_ij[[ii]]<-lapply(1:k_2,function(jj)FlocScaleM_mon(x[label_1==ii&label_2==jj],family = "median", sig0_g=sig0,...)$mu  )
  }
  kkk=1
  res_list<-list()
  for (ii in 1:k_1) {
    for (jj in 1:k_2) {
      res_list[[kkk]]<-x[label_1==ii&label_2==jj]-group_mean_ij[[ii]][[jj]]
      kkk=kkk+1
    }
  }
  res_data<-Reduce("rbind",lapply(1:(k_1*k_2), function(ii)res_list[[ii]]$data))
  res<-fdata(res_data,argvals =grid )
  med<-(1/0.675)*pw_median(abs(res))
  return(med)
}
scale_res_twoway_pw_sur<-function(x,label_1,label_2,...){
  k_1=length(unique(label_1))
  k_2=length(unique(label_2))
  grid<-x$argvals
  sig0<-fdata(array(1,dim = c(1,dim(x$data)[2],dim(x$data)[3])),argvals = x$argvals)
  group_mean_ij<-list()
  for (ii in 1:k_1) {
    group_mean_ij[[ii]]<-lapply(1:k_2,function(jj)FlocScaleM_sur(ex_fdata(x,label_1==ii&label_2==jj),family = "median", sig0_g = sig0)$mu  )
  }
  kkk=1
  res_list<-list()
  for (ii in 1:k_1) {
    for (jj in 1:k_2) {
      res_list[[kkk]]<-center_sur(ex_fdata(x,label_1==ii&label_2==jj),group_mean_ij[[ii]][[jj]])
      kkk=kkk+1
    }
  }
  res_data<-abind::abind(lapply(1:(k_1*k_2), function(ii)res_list[[ii]]$data),along=1)
  res<-fdata(res_data,argvals =grid )
  med<-(1/0.675)*pw_median_sur(abs(res))
  return(med)
}

# RoFanova ----------------------------------------------------------------

#' @title Robust Functional Analysis of Variance
#' @description Robust Functional Analysis of Variance (RoFANOVA) allows identifying the presence of significant differences, in terms of
#' functional mean, among groups of a functional data by being robust against the presence of outliers  (Centofanti et al., 2021).
#' @param X Either an object of class  \code{fdata} for monodimensional functional data  or an object of class \code{fdata2d} for bi-dimensional functional data.
#' @param label_1 A vector of containing group label corresponding to the first main effect.
#' @param label_2 A vector of containing group label corresponding to the second main effect. If it is NULL, the one-way RoFANOVA is performed.
#'  Otherwise, the two-way RoFANOVA with interaction is employed. Default is NULL.
#' @param B  The number of permutations used to approximate the p-value in the permutation test. Default is 1000.
#' @param family The family of loss function for the calculation of the equivariant functional M-estimator. The values allowed are
#'  "bisquare" for the bisquare or Tukey's biweight family of loss functions;  "huber" for the the Huber's family of loss functions;
#'    "optimal" for the  optimal family of loss functions; "hampel" for the the Hampel's family of loss functions; "median" for the median loss function.
#'    A non-robust functional estimator of the mean based on the standard least squares loss function is used with the value "mean". Default is "bisquare".
#' @param eff Asymptotic efficiency of the equivariant functional M-estimator. When \code{family} is either "mean" or "median", \code{eff} is ignored.
#' @param mu0_g Initial estimate  used in re-weighted least-squares algorithm to compute the equivariant functional M-estimator.
#' If NULL the standard non-robust functional mean is used. Default is NULL.
#' @param scale Estimate of the standard error of \code{X}. If NULL, the functional normalized median absolute deviation estimator is used. Default is NULL.
#' @param maxit The maximum number of iterations allowed in the re-weighted least-squares algorithm to compute the equivariant functional M-estimator.
#' @param tol The tolerance for the stopping condition of the re-weighted least-squares algorithm to compute the equivariant functional M-estimator.
#' The algorithm stops when the relative variation of the weighted norm sum between two consecutive iterations is less than \code{tol}.
#' @param cores If \code{cores}>1, then parallel computing is used, with \code{cores} cores. Default is 1.
#' @return
#'  \code{pval_vec} Vector of p-value of corresponding to the test of significance of the whole model, the main effects and the interaction.  For one-way RoFANOVA, it is the p-value corresponding to the test of the main effect.
#'
#'  \code{Tr_obs} The observed value of the test statistic.
#'
#'  \code{Tr_perm} The values of the test statistic for each permutation.
#'
#'   \code{mod} A list containing the following arguments:
#'\itemize{
#' \item \code{Tr}: The observed value of the test statistic.
#'
#' \item\code{global_mean}: The robust estimate of functional grand mean.
#'
#' \item\code{group_mean_1}: The robust estimate of the first functional main effect.
#'
#' \item\code{group_mean_2}: The robust estimate of the second functional main effect. For one-way RoFANOVA, it is NULL.
#'
#' \item\code{group_mean_ij}: The robust estimate of the group functional mean. For one-way, it RoFANOVA is NULL.
#'
#' \item\code{scale}: The robust estimate of functional standard deviation.
#'
#' \item\code{scale_1}: The robust estimate of functional standard deviation corresponding to the first functional main effect.
#'
#' \item\code{scale_2}: The robust estimate of functional standard deviation corresponding to the second functional main effect. For one-way RoFANOVA, it is NULL.
#'
#' \item\code{scale_re}: The robust estimate of the  functional standard deviation of the error distribution. For one-way RoFANOVA, it is NULL.
#'
#' \item\code{X}: The variable \code{X}.
#'
#' \item\code{label_1}: The vector of containing group label corresponding to the first main effect.
#'
#'  \item\code{label_2}: The vector of containing group label corresponding to the second main effect. For one-way RoFANOVA, it is NULL.
#'
#'  \item\code{family}: The family of loss function for the calculation of the equivariant functional M-estimator.
#'}
#'@seealso \code{\link{fusem}} \code{\link{funmad}}
#'
#' @export
#' @references
#' Centofanti, F., Colosimo, B.M., Grasso, M.L., Menafoglio, A., Palumbo, B., Vantini, S. (2021).
#' Robust Functional ANOVA with Application to Additive Manufacturing.
#' \emph{arXiv preprint arXiv:2112.10643}.
#' @examples
#' library(rofanova)
#' data_out<-simulate_data(scenario="one-way")
#' label_1=data_out$label_1
#' X_fdata<-data_out$X_fdata
#' B=10
#' cores=1
#' per_list_median<-rofanova(X_fdata,label_1,B = B,family="median",cores=cores)
#' pvalue_median_vec<-per_list_median$pval_vec
#' per_list_huber<-rofanova(X_fdata,label_1,B = B,family="huber",cores=cores)
#' pvalue_huber_vec<-per_list_huber$pval_vec
#' per_list_bisquare<-rofanova(X_fdata,label_1,B = B,family="bisquare",cores=cores)
#' pvalue_bisquare_vec<-per_list_bisquare$pval_vec
#' per_list_hampel<-rofanova(X_fdata,label_1,B = B,family="hampel",cores=cores)
#' pvalue_hampel_vec<-per_list_hampel$pval_vec
#' per_list_optimal<-rofanova(X_fdata,label_1,B = B,family="optimal",cores=cores)
#' pvalue_optimal<-per_list_optimal$pval
rofanova<-function(X,label_1,label_2=NULL,B=100,cores=1,family="bisquare",eff=0.95,mu0_g=NULL,scale=NULL,maxit = 50, tol = 1e-04){

  if(length(dim(X$data))==2){
    if(is.null(label_2)[1]){
      return(rofanova_oneway_perm(X,label_1,B=B,eff=eff,family=family,mu0_g=mu0_g,scale=scale,maxit = maxit, tol = tol,cores=cores))
    }
    else{
      return(rofanova_twoway_perm(X,label_1,label_2,B=B,eff=eff,family=family,mu0_g=mu0_g,scale=scale,maxit = maxit, tol = tol,cores=cores))
    }
  }
  else if(length(dim(X$data))==3){
    if(is.null(label_2)[1]){
      return(rofanova_oneway_perm_sur(X,label_1,B=B,eff=eff,family=family,mu0_g=mu0_g,scale=scale,maxit = maxit, tol = tol,cores=cores))
    }
    else{
      return(rofanova_twoway_perm_sur(X,label_1,label_2,B=B,eff=eff,family=family,mu0_g=mu0_g,scale=scale,maxit = maxit, tol = tol,cores=cores))
    }
  }

}
rofanova_oneway<-function(X,label,family="bisquare",eff = 0.95, maxit = 100, tol = 1e-30,scale=NULL,mu0_g=NULL){

  k=length(unique(label))
  n=length(label)
  grid<-X$argvals
  if(is.null(scale))scale<-scale_res_oneway_pw(X,label,eff = eff,tol=tol, maxit = maxit,mu0_g=mu0_g )
  global_mean<-FlocScaleM_mon(X,family = family, eff = eff, maxit = maxit, tol =tol,sig0_g = scale,mu0_g=mu0_g)$mu
  group_mean<-lapply(1:k,function(ii)FlocScaleM_mon(X[label==ii],family=family,eff = eff, maxit = maxit, tol = tol,sig0_g = scale,mu0_g=mu0_g)$mu  )
  sum_1_i<-stdandar(X,global_mean,scale)
  norm_tot<-norm_fdata_c(sum_1_i)
  sum_1<-sum(sapply(1:n, function(ii)rfun(norm_tot[ii],rho = family,eff = eff)))

  res_list<-lapply(1:k,function(ii)X[label==ii]-group_mean[[ii]])
  red_list<-list()
  for (ll in 1:k) {
    norm_g<-norm_fdata_c(res_list[[ll]]/scale)
    red_list[[ll]]<-sapply(1:length(which(label==ll)), function(ii)rfun(norm_g[ii],rho = family,eff = eff))
  }
  sum_2<-sum(unlist(red_list))
  Tr<-(1/(k-1))*(sum_1-sum_2)

  out<-list(Tr=Tr,
            global_mean=global_mean,
            group_mean_1=group_mean,
            group_mean_2=NULL,
            group_mean_ij=NULL,
            scale=scale,
            scale_1=NULL,
            scale_2=NULL,
            scale_re=NULL,
            X=X,
            label_1=label,
            label_2=NULL,
            family=family)
  return(out)

}
rofanova_oneway_perm<-function(X,label,B=100,cores=1,eff=0.95,family="bisquare",mu0_g=NULL,scale=NULL,maxit = 50, tol = 1e-04){

  print("One-way RoFANOVA")
  n<-length(label)
  mod_rofanova<-rofanova_oneway(X,label,eff=eff,family=family,mu0_g=mu0_g,scale=scale,maxit = maxit, tol = tol)
  Tr_obs<-mod_rofanova$Tr
  perm_mat<-t(sapply(1:B,function(ii)sample(1:n,replace = F)))
  per_fun<-function(kkk){
    perm_comb<-perm_mat[kkk,]
    X_per<-X[perm_comb]
    return(rofanova_oneway(X_per,label,eff=eff,family=family,mu0_g=mu0_g,scale=scale,maxit = maxit, tol = tol)$Tr)
  }
  if(cores==1){
    per_list<-lapply(1:B,per_fun)
  }
  else{
    if(.Platform$OS.type=="unix")
      per_list<-parallel::mclapply(1:B,per_fun,mc.cores = cores)
    else{
      cl <- parallel::makeCluster(cores)
      parallel::clusterExport(cl, c("X","perm_mat","label","eff","family","mu0_g","scale","maxit", "tol"),envir = environment())
      parallel::clusterEvalQ(cl, library(rofanova))
      per_list<-parallel::parLapply(cl,1:B,per_fun)
      parallel::stopCluster(cl)
    }
  }
  Tr_perm<-unlist(per_list)
  pval<-sum(Tr_perm>=Tr_obs)/B
  out<-list(pval_vec=pval,
            Tr_obs=Tr_obs,
            Tr_perm=Tr_perm,
            mod=mod_rofanova)
  return(out)

}
rofanova_twoway<-function(X,label_1,label_2=NULL,family="bisquare",eff = 0.95, maxit = 100, tol = 1e-30,scale=NULL,mu0_g=NULL){

  k_1=length(unique(label_1))
  k_2=length(unique(label_2))
  n=length(label_1)
  grid<-X$argvals
  scale_res<-scale_res_twoway_pw(X,label_1,label_2,eff = eff,tol=tol, maxit = maxit,mu0_g=mu0_g )
  scale_1<-scale_res_oneway_pw(X,label_1,eff = eff,tol=tol, maxit = maxit,mu0_g=mu0_g  )
  scale_2<-scale_res_oneway_pw(X,label_2,eff = eff,tol=tol, maxit = maxit,mu0_g=mu0_g  )
  if(is.null(scale))scale<-scale_fun_pw(X,eff = eff,tol=tol, maxit = maxit,mu0_g=mu0_g)
  global_mean<-FlocScaleM_mon(X,family = family, eff = eff, maxit = maxit, tol =tol,sig0_g = scale,mu0_g=mu0_g)$mu
  group_mean_1<-lapply(1:k_1,function(ii)FlocScaleM_mon(X[label_1==ii],family = family, eff = eff, maxit = maxit, tol =tol,sig0_g = scale_1,mu0_g=mu0_g)$mu )
  group_mean_2<-lapply(1:k_2,function(ii)FlocScaleM_mon(X[label_2==ii],family = family, eff = eff, maxit = maxit, tol =tol,sig0_g = scale_2,mu0_g=mu0_g)$mu  )
  group_mean_ij<-list()
  for (ii in 1:k_1) {
    group_mean_ij[[ii]]<-lapply(1:k_2,function(jj)FlocScaleM_mon(X[label_1==ii&label_2==jj],family = family, eff = eff, maxit = maxit, tol =tol,sig0_g = scale_res,mu0_g=mu0_g)$mu  )
  }

  kkk=1
  sum_full_list<-list()
  for (ii in 1:k_1) {
    for (jj in 1:k_2) {
      sum_full_list[[kkk]]<-(X[label_1==ii&label_2==jj]-group_mean_ij[[ii]][[jj]])/scale_res
      kkk=kkk+1
    }
  }
  sum_full_data<-Reduce("rbind",lapply(1:(k_1*k_2), function(ii)sum_full_list[[ii]]$data))
  sum_full_i<-fdata(sum_full_data,argvals =grid )
  norm_full<-norm_fdata_c(sum_full_i)
  sum_full<-sum(sapply(1:n, function(ii)rfun(norm_full[ii],rho = family,eff = eff)))

  ####FULL
  kkk=1
  sum_red_list<-list()
  for (ii in 1:k_1) {
    for (jj in 1:k_2) {
      sum_red_list[[kkk]]<-(X[label_1==ii&label_2==jj]-global_mean)/scale_res
      kkk=kkk+1
    }
  }
  sum_red_data<-Reduce("rbind",lapply(1:(k_1*k_2), function(ii)sum_red_list[[ii]]$data))
  sum_red_i<-fdata(sum_red_data,argvals =grid )
  norm_red<-norm_fdata_c(sum_red_i)
  sum_red<-sum(sapply(1:n, function(ii)rfun(norm_red[ii],rho = family,eff = eff)))
  Tr_full<-(1/((k_1-1)*(k_2-1)+(k_1-1)+(k_2-1)))*(sum_red-sum_full)

  ####Interaction
  kkk=1
  sum_red_list<-list()
  for (ii in 1:k_1) {
    for (jj in 1:k_2) {
      sum_red_list[[kkk]]<-(X[label_1==ii&label_2==jj]-group_mean_1[[ii]]-group_mean_2[[jj]]+global_mean)/scale_res
      kkk=kkk+1
    }
  }
  sum_red_data<-Reduce("rbind",lapply(1:(k_1*k_2), function(ii)sum_red_list[[ii]]$data))
  sum_red_i<-fdata(sum_red_data,argvals =grid )
  norm_red<-norm_fdata_c(sum_red_i)
  sum_red<-sum(sapply(1:n, function(ii)rfun(norm_red[ii],rho = family,eff = eff)))
  Tr_int<-(1/((k_1-1)*(k_2-1)))*(sum_red-sum_full)

  ####Factor_1
  kkk=1
  sum_red_list<-list()
  for (ii in 1:k_1) {
    for (jj in 1:k_2) {
      sum_red_list[[kkk]]<-(X[label_1==ii&label_2==jj]-global_mean-group_mean_ij[[ii]][[jj]]+group_mean_1[[ii]])/scale_res
      kkk=kkk+1
    }
  }
  sum_red_data<-Reduce("rbind",lapply(1:(k_1*k_2), function(ii)sum_red_list[[ii]]$data))
  sum_red_i<-fdata(sum_red_data,argvals =grid )
  norm_red<-norm_fdata_c(sum_red_i)
  sum_red<-sum(sapply(1:n, function(ii)rfun(norm_red[ii],rho = family,eff = eff)))
  Tr_f1<-(1/((k_1-1)))*(sum_red-sum_full)

  ####Factor_2
  kkk=1
  sum_red_list<-list()
  for (ii in 1:k_1) {
    for (jj in 1:k_2) {
      sum_red_list[[kkk]]<-(X[label_1==ii&label_2==jj]-global_mean-group_mean_ij[[ii]][[jj]]+group_mean_2[[jj]])/scale_res
      kkk=kkk+1
    }
  }
  sum_red_data<-Reduce("rbind",lapply(1:(k_1*k_2), function(ii)sum_red_list[[ii]]$data))
  sum_red_i<-fdata(sum_red_data,argvals =grid )
  norm_red<-norm_fdata_c(sum_red_i)
  sum_red<-sum(sapply(1:n, function(ii)rfun(norm_red[ii],rho = family,eff = eff)))
  Tr_f2<-(1/((k_2-1)))*(sum_red-sum_full)

  Tr<-rbind(Tr_full,Tr_f1,Tr_f2,Tr_int)
  rownames(Tr)<-c("MOD","F1","F2","INT")
  out<-list(Tr=Tr,
            global_mean=global_mean,
            group_mean_1=group_mean_1,
            group_mean_2=group_mean_2,
            group_mean_ij=group_mean_ij,
            scale=scale,
            scale_1=scale_1,
            scale_2=scale_2,
            scale_re=scale_res,
            X_fdata=X,
            label_1=label_1,
            label_2=label_2,
            family=family)
  return(out)
}
rofanova_twoway_perm<-function(X,label_1,label_2=NULL,B=100,cores=1,eff=0.95,family="bisquare",mu0_g=NULL,scale=NULL,maxit = 50, tol = 1e-04){

  print("Two-way RoFANOVA")
  n<-length(label_1)
  mod_rofanova<-rofanova_twoway(X,label_1,label_2,eff=eff,family=family,mu0_g=mu0_g,scale=scale,maxit = maxit, tol = tol)
  Tr_obs<-mod_rofanova$Tr

  per_fun<-function(kkk){
    perm_comb<-sample(1:n,replace = F)
    X_per<-X[perm_comb]
    return(rofanova_twoway(X_per,label_1,label_2,eff=eff,family=family,mu0_g=mu0_g,scale=scale,maxit = maxit, tol = tol)$Tr)
  }
  if(cores==1){
    per_list<-lapply(1:B,per_fun)
  }
  else{
    if(.Platform$OS.type=="unix")
      per_list<-parallel::mclapply(1:B,per_fun,mc.cores = cores)
    else{
      cl <- parallel::makeCluster(cores)
      parallel::clusterExport(cl, c("n","X","label_1","label_2","eff","family","mu0_g","scale","maxit", "tol"),envir = environment())
      parallel::clusterEvalQ(cl, library(rofanova))
      per_list<-parallel::parLapply(cl,1:B,per_fun)
      parallel::stopCluster(cl)
    }
  }

  pval_vec<-Tr_obs
  Tr_perm<-list()
  for (ii in 1:nrow(pval_vec)) {
    Tr_perm[[ii]]<-sapply(1:B,function(ll)per_list[[ll]][ii])
    pval_vec[ii]<-sum(Tr_perm[[ii]]>=Tr_obs[ii])/B
  }

  out<-list(pval_vec=pval_vec,
            Tr_obs=Tr_obs,
            Tr_perm=Tr_perm,
            mod=mod_rofanova)
  return(out)

}

# RoFanova Surface----------------------------------------------------------------
rofanova_oneway_sur<-function(X,label,family="bisquare",eff = 0.95, maxit = 100, tol = 1e-30,scale=NULL,mu0_g=NULL){

  k=length(unique(label))
  n=length(label)
  grid<-X$argvals
  if(is.null(scale))scale<-scale_res_oneway_pw_sur(X,label,eff = eff,tol=tol, maxit = maxit,mu0_g=mu0_g )
  global_mean<-FlocScaleM_sur(X,family = family, eff = eff, maxit = maxit, tol =tol,sig0_g = scale,mu0_g=mu0_g)$mu
  group_mean<-lapply(1:k,function(ii)FlocScaleM_sur(ex_fdata(X,label==ii),family=family,eff = eff, maxit = maxit, tol = tol,sig0_g = scale,mu0_g=mu0_g)$mu  )
  sum_1_i<-stdandar_sur(X,global_mean,scale)
  norm_tot<-norm_fdata_c_sur(sum_1_i)
  sum_1<-sum(sapply(1:n, function(ii)rfun(norm_tot[ii],rho = family,eff = eff)))

  red_list<-list()
  for (ll in 1:k) {
    red_list[[ll]]<-stdandar_sur(ex_fdata(X,label==ll),group_mean[[ll]],scale)
  }
  sum_red_data<-abind::abind(lapply(1:(k), function(ii)red_list[[ii]]$data),along = 1)
  sum_red_i<-fdata(sum_red_data,argvals =grid )
  norm_red<-norm_fdata_c_sur(sum_red_i)
  sum_red<-sum(sapply(1:n, function(ii)rfun(norm_red[ii],rho = family,eff = eff)))
  Tr<-(1/((k-1)))*(sum_1-sum_red)

  out<-list(Tr=Tr,
            global_mean=global_mean,
            group_mean_1=group_mean,
            group_mean_2=NULL,
            group_mean_ij=NULL,
            scale=scale,
            scale_1=NULL,
            scale_2=NULL,
            scale_re=NULL,
            X=X,
            label_1=label,
            label_2=NULL,
            family=family)
  return(out)
}
rofanova_oneway_perm_sur<-function(X,label,B=100,cores=1,eff=0.95,family="bisquare",mu0_g=NULL,scale=NULL,maxit = 50, tol = 1e-04){

  print("One-way bivariate RoFANOVA")
  n<-length(label)
  mod_rofanova<-rofanova_oneway_sur(X,label,eff=eff,family=family,mu0_g=mu0_g,scale=scale,maxit = maxit, tol = tol)
  Tr_obs<-mod_rofanova$Tr
  perm_mat<-t(sapply(1:B,function(ii)sample(1:n,replace = F)))
  per_fun<-function(kkk){
    perm_comb<-sample(1:n,replace = F)
    X_per<-ex_fdata(X,perm_comb)
    return(rofanova_oneway_sur(X_per,label,eff=eff,family=family,mu0_g=mu0_g,scale=scale,maxit = maxit, tol = tol)$Tr)
  }
  if(cores==1){
    per_list<-lapply(1:B,per_fun)
  }
  else{
    if(.Platform$OS.type=="unix")
      per_list<-parallel::mclapply(1:B,per_fun,mc.cores = cores)
    else{
      cl <- parallel::makeCluster(cores)
      parallel::clusterExport(cl, c("n", "X","label","eff","family","mu0_g","scale","maxit", "tol"),envir = environment())
      parallel::clusterEvalQ(cl, library(rofanova))
      per_list<-parallel::parLapply(cl,1:B,per_fun)
      parallel::stopCluster(cl)
    }
  }

  Tr_perm<-unlist(per_list)
  pval<-sum(Tr_perm>=Tr_obs)/B
  out<-list(pval=pval,
            Tr_obs=Tr_obs,
            Tr_perm=Tr_perm)
  return(out)

}
rofanova_twoway_perm_sur<-function(X,label_1,label_2=NA,B=100,cores=1,eff=0.95,family="bisquare",mu0_g=NULL,scale=NULL,maxit = 50, tol = 1e-04){

  print("Two-way bivariate RoFANOVA")
  n<-length(label_1)
  mod_rofanova<-rofanova_twoway_sur(X,label_1,label_2,eff=eff,family=family,mu0_g=mu0_g,scale=scale,maxit = maxit, tol = tol)
  Tr_obs<-mod_rofanova$Tr

  per_fun<-function(kkk){
    perm_comb<-sample(1:n,replace = F)
    X_per<-ex_fdata(X,perm_comb)
    return(rofanova_twoway_sur(X_per,label_1,label_2,eff=eff,family=family,mu0_g=mu0_g,scale=scale,maxit = maxit, tol = tol)$Tr)
  }
  if(cores==1){
    per_list<-lapply(1:B,per_fun)
  }
  else{
    if(.Platform$OS.type=="unix")
      per_list<-parallel::mclapply(1:B,per_fun,mc.cores = cores)
    else{
      cl <- parallel::makeCluster(cores)
      parallel::clusterExport(cl, c("n","X","label_1","label_2","eff","family","mu0_g","scale","maxit", "tol"),envir = environment())
      parallel::clusterEvalQ(cl, library(rofanova))
      per_list<-parallel::parLapply(cl,1:B,per_fun)
      parallel::stopCluster(cl)
    }
  }

  pval_vec<-Tr_obs
  Tr_perm<-list()
  for (ii in 1:nrow(pval_vec)) {
    Tr_perm[[ii]]<-sapply(1:B,function(ll)per_list[[ll]][ii])
    pval_vec[ii]<-sum(Tr_perm[[ii]]>=Tr_obs[ii])/B
  }


  out<-list(pval_vec=pval_vec,
            Tr_obs=Tr_obs,
            Tr_perm=Tr_perm)
  return(out)
}
rofanova_twoway_sur<-function(X,label_1,label_2=NA,family="bisquare",eff = 0.95, maxit = 100, tol = 1e-30,scale=NULL,mu0_g=NULL){

  k_1=length(unique(label_1))
  k_2=length(unique(label_2))
  n=length(label_1)
  grid =X$argvals
  scale_res<-scale_res_twoway_pw_sur(X,label_1,label_2,eff = eff,tol=tol, maxit = maxit,mu0_g=mu0_g  )#5
  if(is.null(scale))scale<-scale_fun_pw_sur(X,eff = eff,tol=tol, maxit = maxit,mu0_g=mu0_g  )#5
  scale_1<-lapply(1:k_1,function(ii)scale_fun_pw_sur(ex_fdata(X,label_1==ii),eff = eff,tol=tol, maxit = maxit,mu0_g=mu0_g  ))#4)
  scale_2<-lapply(1:k_2,function(ii)scale_fun_pw_sur(ex_fdata(X,label_2==ii),eff = eff,tol=tol, maxit = maxit,mu0_g=mu0_g  ))#4
  scale_res_ij<-list()
  for (ii in 1:k_1) {
    scale_res_ij[[ii]]<-lapply(1:k_2,function(jj)scale_fun_pw_sur(ex_fdata(X,label_1==ii&label_2==jj), eff = eff, maxit = maxit, tol =tol)  )
  }
  global_mean<-FlocScaleM_sur(X,family = family, eff = eff, maxit = maxit, tol =tol,sig0_g=scale,mu0_g=mu0_g)$mu
  group_mean_1<-lapply(1:k_1,function(ii)FlocScaleM_sur(ex_fdata(X,label_1==ii),family = family, eff = eff, maxit = maxit, tol =tol,sig0_g = scale_1[[ii]],mu0_g=mu0_g )$mu )
  group_mean_2<-lapply(1:k_2,function(ii)FlocScaleM_sur(ex_fdata(X,label_2==ii),family = family, eff = eff, maxit = maxit, tol =tol,sig0_g = scale_2[[ii]],mu0_g=mu0_g )$mu  )
  group_mean_ij<-list()
  for (ii in 1:k_1) {
    group_mean_ij[[ii]]<-lapply(1:k_2,function(jj)FlocScaleM_sur(ex_fdata(X,label_1==ii&label_2==jj),family = family, eff = eff, maxit = maxit, tol =tol,sig0_g = scale_res_ij[[ii]][[jj]],mu0_g=mu0_g )$mu  )
  }

  kkk=1
  sum_full_list<-list()
  for (ii in 1:k_1) {
    for (jj in 1:k_2) {
      sum_full_list[[kkk]]<-stdandar_sur(ex_fdata(X,label_1==ii&label_2==jj),group_mean_ij[[ii]][[jj]],scale_res)
      kkk=kkk+1
    }
  }
  sum_full_data<-abind::abind(lapply(1:(k_1*k_2), function(ii)sum_full_list[[ii]]$data),along = 1)
  sum_full_i<-fdata(sum_full_data,argvals =grid )
  norm_full<-norm_fdata_c_sur(sum_full_i)
  sum_full<-sum(sapply(1:n, function(ii)rfun(norm_full[ii],rho = family,eff = eff)))

  ####FULL
  kkk=1
  sum_red_list<-list()
  for (ii in 1:k_1) {
    for (jj in 1:k_2) {
      sum_red_list[[kkk]]<-stdandar_sur(ex_fdata(X,label_1==ii&label_2==jj),global_mean,scale_res)
      kkk=kkk+1
    }
  }
  sum_red_data<-abind::abind(lapply(1:(k_1*k_2), function(ii)sum_red_list[[ii]]$data),along = 1)
  sum_red_i<-fdata(sum_red_data,argvals =grid )
  norm_red<-norm_fdata_c_sur(sum_red_i)
  sum_red<-sum(sapply(1:n, function(ii)rfun(norm_red[ii],rho = family,eff = eff)))
  Tr_full<-(1/((k_1-1)*(k_2-1)+(k_1-1)+(k_2-1)))*(sum_red-sum_full)

  ####Interaction
  kkk=1
  sum_red_list<-list()
  for (ii in 1:k_1) {
    for (jj in 1:k_2) {
      diff_ele<-diff_fdata_sur(sum_fdata_sur(group_mean_1[[ii]],group_mean_2[[jj]]),global_mean)
      sum_red_list[[kkk]]<-stdandar_sur(ex_fdata(X,label_1==ii&label_2==jj),diff_ele,scale_res)
      kkk=kkk+1
    }
  }
  sum_red_data<-abind::abind(lapply(1:(k_1*k_2), function(ii)sum_red_list[[ii]]$data),along = 1)
  sum_red_i<-fdata(sum_red_data,argvals =grid )
  norm_red<-norm_fdata_c_sur(sum_red_i)
  sum_red<-sum(sapply(1:n, function(ii)rfun(norm_red[ii],rho = family,eff = eff)))
  Tr_int<-(1/((k_1-1)*(k_2-1)))*(sum_red-sum_full)

  ####Factor_1
  kkk=1
  sum_red_list<-list()
  for (ii in 1:k_1) {
    for (jj in 1:k_2) {
      diff_ele<-diff_fdata_sur(sum_fdata_sur(global_mean,group_mean_ij[[ii]][[jj]]),group_mean_1[[ii]])
      sum_red_list[[kkk]]<-stdandar_sur(ex_fdata(X,label_1==ii&label_2==jj),diff_ele,scale_res)
      kkk=kkk+1
    }
  }
  sum_red_data<-abind::abind(lapply(1:(k_1*k_2), function(ii)sum_red_list[[ii]]$data),along = 1)
  sum_red_i<-fdata(sum_red_data,argvals =grid )
  norm_red<-norm_fdata_c_sur(sum_red_i)
  sum_red<-sum(sapply(1:n, function(ii)rfun(norm_red[ii],rho = family,eff = eff)))
  Tr_f1<-(1/((k_1-1)))*(sum_red-sum_full)

  ####Factor_2
  kkk=1
  sum_red_list<-list()
  for (ii in 1:k_1) {
    for (jj in 1:k_2) {
      diff_ele<-diff_fdata_sur(sum_fdata_sur(global_mean,group_mean_ij[[ii]][[jj]]),group_mean_2[[jj]])
      sum_red_list[[kkk]]<-stdandar_sur(ex_fdata(X,label_1==ii&label_2==jj),diff_ele,scale_res)
      kkk=kkk+1
    }
  }
  sum_red_data<-abind::abind(lapply(1:(k_1*k_2), function(ii)sum_red_list[[ii]]$data),along = 1)
  sum_red_i<-fdata(sum_red_data,argvals =grid )
  norm_red<-norm_fdata_c_sur(sum_red_i)
  sum_red<-sum(sapply(1:n, function(ii)rfun(norm_red[ii],rho = family,eff = eff)))
  Tr_f2<-(1/((k_2-1)))*(sum_red-sum_full)

  Tr<-rbind(Tr_full,Tr_f1,Tr_f2,Tr_int)
  rownames(Tr)<-c("MOD","F1","F2","INT")
  out<-list(Tr=Tr,
            global_mean=global_mean,
            group_mean_1=group_mean_1,
            group_mean_2=group_mean_2,
            group_mean_ij=group_mean_ij,
            scale=scale,
            scale_1=scale_1,
            scale_2=scale_2,
            scale_re=scale_res,
            X_fdata=X,
            label_1=label_1,
            label_2=label_2,
            family=family)
  return(out)
}
