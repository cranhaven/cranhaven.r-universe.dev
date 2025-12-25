

#' @title Smooth LASSO estimator for the function-on-function linear regression model
#' @description The smooth LASSO (S-LASSO) method for the function-on-function linear regression model provides interpretable coefficient function estimates that are both locally sparse and smooth (Centofanti et al., 2020).
#' @param Y_fd An object of class fd corresponding to the response functions. 
#' @param X_fd An object of class fd corresponding to the covariate functions. 
#' @param basis_s B-splines basis along the \code{s}-direction of class basisfd. 
#' @param basis_t B-splines basis along the \code{t}-direction of class basisfd. 
#' @param lambda_L Regularization parameter of the functional LASSO penalty.
#' @param lambda_s Regularization parameter of the smoothness penalty along the \code{s}-direction. 
#' @param lambda_t Regularization parameter of the smoothness penalty along the \code{t}-direction. 
#' @param B0 Initial estimator of the basis coefficients matrix of the coefficient function. Should have dimensions in accordance with the basis dimensions of \code{basis_s} and \code{basis_t}.
#' @param ... Other arguments to be passed to the Orthant-Wise Limited-memory Quasi-Newton optimization function. See the \code{lbfgs} help page of the package \code{lbfgs}.
#' 
#' @return   A list containing the following arguments:
#' \itemize{
#' \item \code{B}: The basis coefficients matrix estimate of the coefficient function.
#'
#' \item \code{Beta_hat_fd}: The coefficient function estimate of class bifd.
#'
#' \item \code{alpha}: The intercept function estimate.
#'
#' \item \code{lambdas_L}: Regularization parameter of the functional LASSO penalty.
#'
#' \item \code{lambda_s}: Regularization parameter of the smoothness penalty along the \code{s}-direction. 
#'
#' \item \code{lambda_t}: Regularization parameter of the smoothness penalty along the \code{t}-direction. 
#'
#' \item \code{Y_fd}: The response functions.
#'
#' \item \code{X_fd}: The covariate functions. 
#'
#' \item \code{per_0}: The fraction of domain where the coefficient function is zero.
#'
#' \item \code{type}: The output type.
#'}
#'@seealso \code{\link{slasso.fr_cv}}
#'
#' @export
#' @references
#' Centofanti, F., Fontana, M., Lepore, A., & Vantini, S. (2020).
#' Smooth LASSO Estimator for the Function-on-Function Linear Regression Model.
#' \emph{arXiv preprint arXiv:2007.00529}.
#' @examples
#' library(slasso)
#' data<-simulate_data("Scenario II",n_obs=150)
#' X_fd=data$X_fd
#' Y_fd=data$Y_fd
#' domain=c(0,1)
#' n_basis_s<-30
#' n_basis_t<-30
#' breaks_s<-seq(0,1,length.out = (n_basis_s-2))
#' breaks_t<-seq(0,1,length.out = (n_basis_t-2))
#' basis_s <- fda::create.bspline.basis(domain,breaks=breaks_s)
#' basis_t <- fda::create.bspline.basis(domain,breaks=breaks_t)
#' mod_slasso<-slasso.fr(Y_fd = Y_fd,X_fd=X_fd,basis_s=basis_s,basis_t=basis_t,
#' lambda_L = -1.5,lambda_s =-8,lambda_t = -7,B0 =NULL,invisible=1,max_iterations=10)
slasso.fr<-function(Y_fd,X_fd,basis_s,basis_t,
                    lambda_L,lambda_s,lambda_t,B0=NULL,...){
  
  n_obs<-dim(X_fd$coefs)[2]
  W_X<-fda::eval.penalty(basis_s)
  W_Y<-fda::eval.penalty(basis_t)
  R_X<-fda::eval.penalty(basis_s,2)
  R_Y<-fda::eval.penalty(basis_t,2)
  domain_s<-basis_s$rangeval
  domain_t<-basis_t$rangeval
  n_basis_s<-basis_s$nbasis
  n_basis_t<-basis_t$nbasis
  orders<-n_basis_s-length(basis_s$params)
  ordert<-n_basis_t-length(basis_t$params)
  breaks_s<-basis_s$params
  breaks_t<-basis_t$params
  ext_break_s<-c(rep(domain_s[1],orders),breaks_s,rep(domain_s[2],orders))
  ext_break_t<-c(rep(domain_s[1],ordert),breaks_t,rep(domain_s[2],ordert))
  weights_s<-diff(ext_break_s,lag=orders)/orders
  weights_t<-diff(ext_break_t,lag=ordert)/ordert
  weights_mat<-weights_s%o%weights_t
  weights_vec<-matrixcalc::vec(weights_mat)
  
  X_mean<-fda::mean.fd(X_fd)
  Y_mean<-fda::mean.fd(Y_fd)
  X_fd_cen<-fda::center.fd(X_fd)
  Y_fd_cen<-fda::center.fd(Y_fd)
  
  X_new<-fda::inprod(X_fd_cen,basis_s)
  Y_new<-fda::inprod(Y_fd_cen,basis_t)
  
  env <- new.env()
  env[["n_basis_x"]] <- n_basis_s
  env[["n_basis_y"]] <- n_basis_t
  env[["Y_newc"]] <- Y_new
  env[["X_newc"]] <- X_new
  env[["W_X"]] <-W_X
  env[["W_Y"]] <-W_Y
  env[["R_X"]] <-R_X
  env[["R_Y"]] <-R_Y
  
  if(is.null(B0)){
    B_basis<-MASS::ginv(t(X_new)%*%X_new)%*%t(X_new)%*%Y_new%*%solve(W_Y)
  }
  else{
    B_basis<-B0
  }
  
  
  env[["lambda_x_opt"]] <- lambda_s
  env[["lambda_y_opt"]] <-lambda_t
  cat("SLASSO:",c(lambda_L, lambda_s, lambda_t),"     ")

  cx_o <- cxxfunplus::grab.cxxfun(objective) 
  cx_g <- cxxfunplus::grab.cxxfun(gradient) 
  
  output <- lbfgsw(cx_o(), cx_g(), B_basis, environment=env,lambda = lambda_L,weights = weights_vec,...)
  B_par<-matrix(output$par,nrow=n_basis_s,ncol = n_basis_t)
  Beta_hat_fd<-fda::bifd(B_par,basis_s,basis_t)
  
  X_mean_new<-fda::inprod(X_mean,basis_s)
  alpha<-Y_mean-fda::fd(t(X_mean_new%*%B_par),basis_t)
  
  
  per_0<-get_per_0(Beta_hat_fd)
  out<-list(B=B_par,
            Beta_hat_fd=Beta_hat_fd,
            alpha=alpha,
            lambda_L=lambda_L,
            lambda_s=lambda_s,
            lambda_t=lambda_t,
            Y_fd=Y_fd,
            X_fd=X_fd,
            per_0=per_0)
  class(out)<-"slasso"
  return(out)
  
}

#' @title Cross-validation for the S-LASSO estimator
#' @description K-fold cross-validation procedure to choose the tuning parameters for the S-LASSO estimator (Centofanti et al., 2020).
#' @inheritParams slasso.fr
#' @param lambda_L_vec Vector of regularization parameters of the functional LASSO penalty.
#' @param lambda_s_vec Vector of regularization parameters of the smoothness penalty along the \code{s}-direction. 
#' @param lambda_t_vec Vector of regularization parameters of the smoothness penalty along the \code{t}-direction. 
#' @param K Number of folds. Default is 10.
#' @param kss_rule_par Parameter of the \code{k}-standard error rule. If \code{kss_rule_par=0} the tuning parameters that minimize the estimated prediction error are chosen.  Default is 0.5.
#' @param ncores If \code{ncores}>1, then parallel computing is used, with \code{ncores} cores. Default is 1.
#' 
#' @return   A list containing the following arguments:
#' \itemize{
#' \item \code{lambda_opt_vec}: Vector of optimal tuning parameters.
#'
#' \item \code{CV}:  Estimated prediction errors.
#' 
#' \item \code{CV_sd}:  Standard errors of the estimated prediction errors.
#'
#' \item \code{per_0}: The fractions of domain where the coefficient function is zero for all the tuning parameters combinations.
#'
#' \item \code{comb_list}: The combinations of \code{lambda_L},\code{lambda_s} and \code{lambda_t} explored.
#'
#' \item \code{Y_fd}: The response functions.
#'
#' \item \code{X_fd}: The covariate functions. 
#'}
#' @export
#' @references
#' Centofanti, F., Fontana, M., Lepore, A., & Vantini, S. (2020).
#' Smooth LASSO Estimator for the Function-on-Function Linear Regression Model.
#' \emph{arXiv preprint arXiv:2007.00529}.
#' @seealso \code{\link{slasso.fr}}
#' @examples
#' library(slasso)
#' data<-simulate_data("Scenario II",n_obs=150)
#' X_fd=data$X_fd
#' Y_fd=data$Y_fd
#' domain=c(0,1)
#' n_basis_s<-60
#' n_basis_t<-60
#' breaks_s<-seq(0,1,length.out = (n_basis_s-2))
#' breaks_t<-seq(0,1,length.out = (n_basis_t-2))
#' basis_s <- fda::create.bspline.basis(domain,breaks=breaks_s)
#' basis_t <- fda::create.bspline.basis(domain,breaks=breaks_t)
#' mod_slasso_cv<-slasso.fr_cv(Y_fd = Y_fd,X_fd=X_fd,basis_s=basis_s,basis_t=basis_t,
#' lambda_L_vec=seq(0,1,by=1),lambda_s_vec=c(-9),lambda_t_vec=-7,B0=NULL,
#' max_iterations=10,K=2,invisible=1,ncores=1)
slasso.fr_cv<-function(Y_fd,X_fd,basis_s,basis_t,K=10,kss_rule_par=0.5,
                       lambda_L_vec=NULL,lambda_s_vec=NULL,lambda_t_vec=NULL,B0=NULL,ncores=1,...){
  
  
  if(is.null(lambda_L_vec)||is.null(lambda_s_vec)||is.null(lambda_t_vec)) stop("Wrong lambdas!")
  
  n_obs<-dim(X_fd$coefs)[2]
  W_X<-fda::eval.penalty(basis_s)
  W_Y<-fda::eval.penalty(basis_t)
  R_X<-fda::eval.penalty(basis_s,2)
  R_Y<-fda::eval.penalty(basis_t,2)
  domain_s<-basis_s$rangeval
  domain_t<-basis_t$rangeval
  n_basis_s<-basis_s$nbasis
  n_basis_t<-basis_t$nbasis
  orders<-n_basis_s-length(basis_s$params)
  ordert<-n_basis_t-length(basis_t$params)
  breaks_s<-basis_s$params
  breaks_t<-basis_t$params
  ext_break_s<-c(rep(domain_s[1],orders),breaks_s,rep(domain_s[2],orders))
  ext_break_t<-c(rep(domain_s[1],ordert),breaks_t,rep(domain_s[2],ordert))
  weights_s<-diff(ext_break_s,lag=orders)/orders
  weights_t<-diff(ext_break_t,lag=ordert)/ordert
  weights_mat<-weights_s%o%weights_t
  weights_vec<-matrixcalc::vec(weights_mat)

  X_mean<-fda::mean.fd(X_fd)
  Y_mean<-fda::mean.fd(Y_fd)
  X_fd_cen<-fda::center.fd(X_fd)
  Y_fd_cen<-fda::center.fd(Y_fd)
  
  X_new<-fda::inprod(X_fd_cen,basis_s)
  Y_new<-fda::inprod(Y_fd_cen,basis_t)
  
  env <- new.env()
  env[["n_basis_x"]] <- n_basis_s
  env[["n_basis_y"]] <- n_basis_t
  env[["Y_newc"]] <- Y_new
  env[["X_newc"]] <- X_new
  env[["W_X"]] <-W_X
  env[["W_Y"]] <-W_Y
  env[["R_X"]] <-R_X
  env[["R_Y"]] <-R_Y
  
  if(is.null(B0)){
    B_basis<-MASS::ginv(t(X_new)%*%X_new)%*%t(X_new)%*%Y_new%*%solve(W_Y)
  }
  else{
    B_basis<-B0
  }
  
  comb_list<-expand.grid(lambda_L_vec,lambda_s_vec,lambda_t_vec)
  
  parr_func<-function(ii){
    
    lambdas<-comb_list[ii,]
    lambda_L<-as.numeric(lambdas[1])
    lambda_s<-as.numeric(lambdas[2])
    lambda_t<-as.numeric(lambdas[3])
    env[["lambda_x_opt"]] <-lambda_s
    env[["lambda_y_opt"]] <-lambda_t
    ran_seq<-sample(seq(1, n_obs), n_obs, replace=FALSE)
    split_vec<-base::split(ran_seq,cut(seq(1,n_obs),breaks=K,labels=FALSE))
    inpr_vec<-numeric()
    
    cx_o <- cxxfunplus::grab.cxxfun(objective) 
    cx_g <- cxxfunplus::grab.cxxfun(gradient) 
    
    for (ll in 1:K) {
      Y_i<-Y_fd_cen[split_vec[[ll]]]
      X_i<-X_new[split_vec[[ll]],]
      Y_minus<-Y_new[-split_vec[[ll]],]
      X_minus<-X_new[-split_vec[[ll]],]
      env[["Y_newc"]] <- Y_minus
      env[["X_newc"]] <- X_minus
     
      output <- lbfgsw(cx_o(), cx_g(), B_basis, lambda = lambda_L,weights = weights_vec,environment=env,...)
      
      B_par<-matrix(output$par,n_basis_s,n_basis_t)
      Y_hat<-fda::fd(t(X_i%*%B_par),basis_t)
      inpr_vec[ll]<-base::mean(diag(fda::inprod(Y_i-Y_hat,Y_i-Y_hat)))
      
    }
    per_0<-get_per_0(fda::bifd(B_par,basis_s,basis_t))
    mean<-base::mean(inpr_vec)
    sd<-stats::sd(inpr_vec)/sqrt(K)
    out<-as.numeric(list(mean=mean,
                         sd=sd,
                         per_0=per_0))
    return( out)
  }
  
  if(ncores==1){
    vec_par<-lapply(seq(1,length(comb_list[,1])),parr_func)
  }
  else{
    if(.Platform$OS.type=="unix")
      vec_par<-parallel::mclapply(seq(1,length(comb_list[,1])),parr_func,mc.cores = ncores)
    else{
      cl <- parallel::makeCluster(ncores)
      parallel::clusterExport(cl, c( "comb_list","n_obs","env","K","Y_fd_cen","X_new","Y_new","B_basis","weights_vec", "n_basis_s","n_basis_t","basis_t","basis_s","..."),envir = environment())
      parallel::clusterEvalQ(cl, {
        library(slasso)
        library(fda)
        invisible(source('R/utils.R'))
        })
      vec_par<-parallel::parLapply(cl,seq(1,length(comb_list[,1])),parr_func)
      parallel::stopCluster(cl)
    }
  }
  par<-sapply(vec_par,"[[",1)
  sd<-sapply(vec_par,"[[",2)
  per_0<-sapply(vec_par,"[[",3)
  
  if(kss_rule_par==0){
    lambda_opt<-as.numeric(comb_list[max(which(par<=min(par))),])
  }
  else{
    lambdas_opt<-as.numeric(comb_list[max(which(par<=min(par))),])
    len_s<-length(lambda_s_vec)
    len_t<-length(lambda_t_vec)
    len_L<-length(lambda_L_vec)
    
    lambda_opt_i<-matrix(0,length(kss_rule_par),3)
    for(kkk in 1:length(kss_rule_par)){
      mat_CV<-matrix(0,len_s*len_t,len_L)
      mat_CV_sd<-matrix(0,len_s*len_t,len_L)
      for(ii in 1:len_L){
        mat_CV[,ii]<-par[which(comb_list[,1]==lambda_L_vec[ii])]
        mat_CV_sd[,ii]<-sd[which(comb_list[,1]==lambda_L_vec[ii])]
      }
      min_CV_L<-matrixStats::colMins(mat_CV)
      mat_st<-list()
      for(ii in 1:len_L){
        mat_st[[ii]]<-comb_list[which(par==min_CV_L[ii]),]
      }
      sd_CV_L<-matrixStats::colMins(mat_CV_sd)
      ind_opt_L<-max(which(abs(min_CV_L-min(min_CV_L))<=kss_rule_par[kkk]*sd[max(which(par==min(min_CV_L)))]))
      lambda_opt_i[kkk,]<-as.numeric(mat_st[[ind_opt_L]])
    }
    lambda_opt<-lambda_opt_i
  }
  
  out<-list(lambda_opt_vec=lambda_opt,
            CV=par,
            CV_sd=sd,
            per_0=per_0,
            comb_list=comb_list,
            X_fd=X_fd,
            Y_fd=Y_fd)
  class(out)<-"slasso_cv"
  return(out)
  
}


#' @title Simulate data through the function-on-function linear regression model
#' @description Generate synthetic data as in the simulation study of Centofanti et al. (2020).
#' @param scenario A  character strings indicating the scenario considered. It could be "Scenario I", "Scenario II", "Scenario III", and "Scenario IV".
#' @param n_obs Number of observations.
#' @param type_x  Covariate generating mechanism, either Bspline or Brownian.
#' @return   A list containing the following arguments:
#'
#'  \code{X}: Covariate matrix, where  the rows  correspond to argument values and columns to replications.
#'
#'  \code{Y}: Response matrix, where  the rows  correspond to argument values and columns to replications.
#'
#'  \code{X_fd}: Coavariate functions.
#'
#'  \code{Y_fd}: Response functions.
#'
#'  \code{clus}: True cluster membership vector.
#'
#' @export
#' @references
#' Centofanti, F., Fontana, M., Lepore, A., & Vantini, S. (2020).
#' Smooth LASSO Estimator for the Function-on-Function Linear Regression Model.
#' \emph{arXiv preprint arXiv:2007.00529}.
#' @examples
#' library(slasso)
#' data<-simulate_data("Scenario II",n_obs=150)
simulate_data<-function(scenario,n_obs=3000,type_x="Bspline") {
  
  length_tot<-500
  grid_s<-grid_t<-seq(0,1,length.out = length_tot)
  
  # generate X --------------------------------------------------------------
  domain<-c(0,1)
  if(type_x=="Bspline"){
    n_basis_x<-32     #random chosen between 10 and 50
    X_basis<-fda::create.bspline.basis(domain,norder = 4,nbasis = n_basis_x)
    X_coef<-matrix(stats::rnorm(n_obs*n_basis_x),nrow = n_basis_x,ncol = n_obs )
    X_fd<-fda::fd(X_coef,X_basis)
    X<-fda::eval.fd(grid_s,X_fd)
  }
  else if(type_x=="Brownian"){
    X<- as.matrix(t(as.matrix(fda.usc::rproc2fdata(n_obs,t=grid_s+1,sigma="brownian",par.list=list("scale"=16))[[1]])))
  }
  
  # Generate ERROR ----------------------------------------------------------
  n_basis_eps<-20 #random chosen between 10 and 50
  eps_basis<-fda::create.bspline.basis(domain,norder = 4,nbasis = n_basis_eps)
  eps_coef<-matrix(stats::rnorm(n_obs*n_basis_eps),nrow = n_basis_eps,ncol = n_obs )
  eps_fd<-fda::fd(eps_coef,eps_basis)
  Eps<-t(fda::eval.fd(grid_t,eps_fd))
  
  # Define beta -----------------------------------------------------------
  if(scenario=="Scenario I"){
    cat("Scenario I")
    beta<-function(s,t){
      if(length(s)!=1){d=expand.grid(s,t)
      colnames(d)=c('s','t')
      z_matrix<-matrix(0,nrow=length(s),ncol = length(t),byrow=TRUE)
      z_matrix}
      else{
        z_matrix<-matrix(0,nrow=length(s),ncol = length(t),byrow=TRUE)
        z_matrix}
    }
  }
  if(scenario=="Scenario II"){
    cat("Scenario II")
    beta<-function(s,t){
      a=0.25
      b=0.25
      if(length(s)!=1){d=expand.grid(s,t)
      colnames(d)=c('s','t')
      z<- -(((d$s-0.5)/a)^2 + ((d$t-0.5)/b)^2) +1
      z[z<0]<-0
      z_matrix<-matrix(z,nrow=length(s))
      z_matrix}
      else{
        z<- -(((s)/a)^2 + ((d)/b)^2) + 1
        z[z<0]<-0
        z}
    }
  }
  if(scenario=="Scenario III"){
    cat("Scenario III")
    beta<-function(s,t){
      a<-0.05
      b<-0.5
      f_1<-function(s,t){b*(1-s)*sin(10*pi*(s-a-1+sqrt(1-(t-0.5)^2)))}
      f_2<-function(s,t){b*sin(10*pi*(s+a+1-sqrt(1-(t-0.5)^2)))}
      
      z<-matrix(0,length_tot,length_tot)
      for (ii in 1:length(grid_t)) {
        t<-grid_t[ii]
        s_0_1<-grid_s[grid_s>( a+1-sqrt(1-(t-0.5)^2))&grid_s<0.5]
        s_0_2<-grid_s[grid_s<( -a+sqrt(1-(t-0.5)^2))&grid_s>=0.5]
        s_n0_1<-grid_s[grid_s<=(a+1-sqrt(1-(t-0.5)^2))&grid_s<0.5]
        s_n0_2<-grid_s[grid_s>=(-a+sqrt(1-(t-0.5)^2))&grid_s>0.5]
        z_i<-c(f_1(s_n0_1,t),rep(0,length(s_0_1)),rep(0,length(s_0_2)),f_2(s_n0_2,t))
        z[ii,]=z_i
      }
      return(t(z))
    }
  }
  
  if(scenario=="Scenario IV"){
    cat("Scenario IV")
    beta<-function(s,t){
      a=0.5
      b=0.5
      c=0.5
      d=0.5
      f_1<-function(s,t){((t-0.5)/c)^3+((s-0.5)/d)^3+((t-0.5)/b)^2 + ((s-0.5)/a)^2+5}
      z<- outer(s,t,f_1)
      z
    }
  }
  if(type_x=="Bspline"){
    G<-(1/length(grid_s))*t(fda::eval.basis(grid_s,X_basis))%*%beta(grid_s,grid_t)
    Y_parz<-t(X_coef)%*%G
  }
  else if(type_x=="Brownian"){
    Y_parz<-(1/length(grid_s))*t(X)%*%beta(grid_s,grid_t)
  }
  signal_to_noise_ratio<-4
  if(scenario=="Scenario I"){Y = Y_parz + Eps%*%diag(matrixStats::colVars(Eps)^(-1/2))}
  else{
    k <- sqrt((matrixStats::colVars(Y_parz)+max(matrixStats::colVars(Y_parz)))/(signal_to_noise_ratio*matrixStats::colVars(Eps)))
    Y = Y_parz + Eps%*%diag(k)
  }
  domain=c(0,1)
  n_basis_x<-min(80,length_tot)
  n_basis_y<-min(80,length_tot)
  breaks_x<-seq(0,1,length.out = (n_basis_x-2))
  breaks_y<-seq(0,1,length.out = (n_basis_y-2))
  basis_x <- fda::create.bspline.basis(domain,breaks=breaks_x)
  basis_y <- fda::create.bspline.basis(domain,breaks=breaks_y)
  X_fd <- fda::smooth.basis(grid_s,X,basis_x)$fd
  Y_fd <- fda::smooth.basis(grid_s,t(Y),basis_y)$fd
  out<-list(X=X,
            Y=t(Y),
            X_fd=X_fd,
            Y_fd=Y_fd)
  
  return(out)
}


get_per_0<-function(mod,tol=10^-6){
  length_grid<-500
  if(is.null(mod$Beta_hat_fd$sbasis$rangeval)){
    rangevals<-mod$sbasis$rangeval
    rangevalt<-mod$tbasis$rangeval
    beta<-mod
  }
  else{
    rangevals<-mod$Beta_hat_fd$sbasis$rangeval
    rangevalt<-mod$Beta_hat_fd$tbasis$rangeval
    beta<-mod$Beta_hat_fd
  }
  grid_s<-seq(rangevals[1],rangevals[2],length.out = length_grid)
  grid_t<-seq(rangevalt[1],rangevalt[2],length.out = length_grid)
  A=fda::eval.bifd(seq(rangevals[1],rangevals[2],length.out = 500),seq(rangevalt[1],rangevalt[2],length.out = 500),beta)
  out<-length(which(abs(A)<tol))/(500*500)
  return(out)
}
