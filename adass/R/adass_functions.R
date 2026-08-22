#' @title Simulate data through the function-on-function linear regression model
#' @description Generate synthetic data as in the simulation study of Centofanti et al. (2020).
#' @param scenario A  character strings indicating the scenario considered. It could be "Scenario HAT", "Scenario DAMP", or "Scenario RCHANGE".
#' @param n_obs Number of observations.
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
#'  \code{Beta_vero_fd}: The true coefficient function.
#'
#' @export
#' @references
#' Centofanti, F., Lepore, A., Menafoglio, A., Palumbo, B., Vantini, S. (2023).
#' Adaptive Smoothing Spline Estimator for the Function-on-Function Linear Regression Model.
#' \emph{Computational Statistics 38(1), 191–216}.
#' @examples
#' library(adass)
#' data<-simulate_data("Scenario HAT",n_obs=100)
simulate_data<-function(scenario,n_obs=3000) {

  length_tot<-500
  grid_s<-grid_t<-seq(0,1,length.out = length_tot)

  # generate X --------------------------------------------------------------
  domain<-c(0,1)
  n_basis_x<-32     #random chosen between 10 and 50
  X_basis<-fda::create.bspline.basis(domain,norder = 4,nbasis = n_basis_x)
  X_coef<-matrix(stats::rnorm(n_obs*n_basis_x),nrow = n_basis_x,ncol = n_obs )
  X_fd<-fda::fd(X_coef,X_basis)
  X<-fda::eval.fd(grid_s,X_fd)

  # Generate ERROR ----------------------------------------------------------
  n_basis_eps<-20 #random chosen between 10 and 50
  eps_basis<-fda::create.bspline.basis(domain,norder = 4,nbasis = n_basis_eps)
  eps_coef<-matrix(stats::rnorm(n_obs*n_basis_eps),nrow = n_basis_eps,ncol = n_obs )
  eps_fd<-fda::fd(eps_coef,eps_basis)
  Eps<-t(fda::eval.fd(grid_t,eps_fd))

  # Define beta -----------------------------------------------------------
  if(scenario=="Scenario HAT"){
    cat("Scenario HAT")
    beta<-function(grid_s,grid_t){
      eval_point<-expand.grid(grid_s,grid_t)
      eval_pdf<-mvtnorm::dmvnorm(eval_point,mean = c(0.6,0.6), sigma = diag(c(0.001,0.001)))
      eval<--1+as.matrix(eval_point)%*%c(1.5,1.5)+0.05*eval_pdf
      Z<-matrix(eval,length(grid_s),length(grid_t))
    }
  }
  if(scenario=="Scenario DAMP"){
    cat("Scenario DAMP")
    beta<-function(grid_s,grid_t){
      eval_point<-as.matrix(expand.grid(grid_s,grid_t))
      eval<-5*exp(-5*eval_point%*%c(1,1))*cos(10*pi*eval_point%*%c(1,0))+5*exp(-5*eval_point%*%c(1,1))*cos(10*pi*eval_point%*%c(0,1))+1

      Z<-matrix(eval,length(grid_s),length(grid_t))
    }
  }
  if(scenario=="Scenario RCHANGE"){
    cat("Scenario RCHANGE")
    beta<-function(grid_s,grid_t){
      eval_point<-as.matrix(expand.grid(grid_s,grid_t))
      eval<-1-5/(1+exp(10*(eval_point%*%c(1,1)-0.2)))+5/(1+exp(75*(eval_point%*%c(1,1)-0.8)))

      Z<-matrix(eval,length(grid_s),length(grid_t))
    }
  }
  G<-(1/length(grid_s))*t(fda::eval.basis(grid_s,X_basis))%*%beta(grid_s,grid_t)
  Y_parz<-t(X_coef)%*%G
  signal_to_noise_ratio<-4
  k <- sum(sqrt(Rfast::colVars(Y_parz)))/sum((signal_to_noise_ratio*Rfast::colVars(Eps)))
  Y = Y_parz + Eps%*%diag(rep(k,length(Y_parz[1,])))

  n_basis_x<-min(80,length_tot)
  n_basis_y<-min(80,length_tot)
  basis_x <- fda::create.bspline.basis(domain,nbasis = n_basis_x)
  basis_y <- fda::create.bspline.basis(domain,nbasis = n_basis_y)
  X_fd <- fda::smooth.basis(grid_s,X,basis_x)$fd
  Y_fd <- fda::smooth.basis(grid_t,t(Y),basis_y)$fd
  inter_basis<-fda::create.bspline.basis(domain,nbasis = length_tot,norder = 3)
  beta_matrix=beta(grid_s,grid_t)
  Beta_vero_fd<-fda::bifd(beta_matrix,inter_basis,inter_basis)

  out<-list(X=X,
            Y=t(Y),
            X_fd=X_fd,
            Y_fd=Y_fd,
            Beta_vero_fd=Beta_vero_fd)
  return(out)
}


#' @title Evolutionary algorithm for the adaptive smoothing spline estimator (EAASS).
#' @description EAASS algorithm to choose the tuning parameters for the AdaSS estimator (Centofanti et al., 2020).
#' @inheritParams adass.fr
#' @param X_fd_test  Test set covariate functions. Default is NULL.
#'  If \code{X_fd_test} and \code{Y_fd_test} are both provided the prediction error on the test set is used as performance metric in place of the cross-validation prediction error.
#' @param Y_fd_test Test set response functions. Default is NULL.
#' If \code{X_fd_test} and \code{Y_fd_test} are both provided the prediction error on the test set is used as performance metric in place of the cross-validation prediction error.
#' @param rand_search_par List containing the initial population ranges for the tuning parameters.
#' @param popul_size Initial population size.
#' @param iter_num Algorithm iterations.
#' @param r Truncation parameter in the exploitation phase.
#' @param pert_vec Perturbation parameters in the exploration phase.
#' @param progress If TRUE a progress bar is printed. Default is TRUE.
#' @param ncores If \code{ncores}>1, then parallel computing is used, with \code{ncores} cores. Default is 1.
#' @return   A list containing the following arguments:
#' \itemize{
#' \item \code{tun_par_opt}: Vector of optimal tuning parameters.
#'
#' \item \code{CV}:  Estimated prediction errors.
#'
#' \item \code{CV_sd}:  Standard errors of the estimated prediction errors.
#'
#' \item \code{comb_list}: The combinations of tuning parameters explored.
#'
#' \item \code{Y_fd}: The response functions.
#'
#' \item \code{X_fd}: The covariate functions.
#'}
#' @seealso \code{\link{adass.fr_eaass}}
#'
#' @export
#' @references
#' Centofanti, F., Lepore, A., Menafoglio, A., Palumbo, B., Vantini, S. (2023).
#' Adaptive Smoothing Spline Estimator for the Function-on-Function Linear Regression Model.
#' \emph{Computational Statistics 38(1), 191–216}.
#' @examples
#' library(adass)
#' data<-simulate_data("Scenario HAT",n_obs=100)
#' X_fd=data$X_fd
#' Y_fd=data$Y_fd
#' basis_s <- fda::create.bspline.basis(c(0,1),nbasis = 5,norder = 4)
#' basis_t <- fda::create.bspline.basis(c(0,1),nbasis = 5,norder = 4)
#' mod_smooth <-adass.fr(Y_fd,X_fd,basis_s = basis_s,basis_t = basis_t,tun_par=c(10^-6,10^-6,0,0,0,0))
#' grid_s<-seq(0,1,length.out = 5)
#' grid_t<-seq(0,1,length.out = 5)
#' beta_der_eval_s<-fda::eval.bifd(grid_s,grid_t,mod_smooth$Beta_hat_fd,sLfdobj = 2)
#' beta_der_eval_t<-fda::eval.bifd(grid_s,grid_t,mod_smooth$Beta_hat_fd,tLfdobj = 2)
#' mod_adsm<-adass.fr_eaass(Y_fd,X_fd,basis_s,basis_t,
#'                         beta_ders=beta_der_eval_s, beta_dert=beta_der_eval_t,
#'                         rand_search_par=list(c(-8,4),c(-8,4),c(0,0.1),c(0,4),c(0,0.1),c(0,4)),
#'                         grid_eval_ders=grid_s, grid_eval_dert=grid_t,
#'                         popul_size = 1,ncores=1,iter_num=1)
#'
adass.fr_eaass <-function(Y_fd, X_fd, basis_s, basis_t,
                          beta_ders = NULL,beta_dert = NULL, grid_eval_ders=NULL, grid_eval_dert=NULL,
                          rand_search_par = list(c(-4, 4), c(-4, 4), c(0,1,5,10, 15), c(0,1,2,3,4), c(0,1,5,10, 15), c(0,1,2,3,4)),
                          popul_size = 12, iter_num = 10, r=0.2,pert_vec=c(0.8, 1.2),
                          X_fd_test = NULL, Y_fd_test = NULL, progress=TRUE, ncores=1,K=10) {

    inf_lam_s<-rand_search_par[[1]][1]
    sup_lam_s<-rand_search_par[[1]][2]
    inf_lam_t<-rand_search_par[[2]][1]
    sup_lam_t<-rand_search_par[[2]][2]
    inf_sum_s<-rand_search_par[[3]][1]
    sup_sum_s<-rand_search_par[[3]][2]
    inf_ind_s<-rand_search_par[[4]][1]
    sup_ind_s<-rand_search_par[[4]][2]
    inf_sum_t<-rand_search_par[[5]][1]
    sup_sum_t<-rand_search_par[[5]][2]
    inf_ind_t<-rand_search_par[[6]][1]
    sup_ind_t<-rand_search_par[[6]][2]


    list_l<-lapply(1:popul_size,function(ii){c(10^stats::runif(1,inf_lam_s,sup_lam_s),10^stats::runif(1,inf_lam_t,sup_lam_t),
                                          stats::runif(1,inf_sum_s,sup_sum_s),stats::runif(1,inf_ind_s,sup_ind_s),
                                          stats::runif(1,inf_sum_t,sup_sum_t),stats::runif(1,inf_ind_t,sup_ind_t))})

    ## Evaluation ----
    comb_list_eval<-do.call("rbind",list_l)
    mean_par<-NULL
    pb = utils::txtProgressBar(min = 0, max = iter_num, initial = 0,style = 3)
    if(progress)
      utils::setTxtProgressBar(pb,0)
    for (iter in 1:iter_num) {
      parr_func <- function(ii) {
        lambda_s_i <- comb_list_eval[ii, 1]
        lambda_t_i <- comb_list_eval[ii, 2]
        sum_s_i <- comb_list_eval[ii, 3]
        ind_s_i <- comb_list_eval[ii, 4]
        sum_t_i <- comb_list_eval[ii, 5]
        ind_t_i <- comb_list_eval[ii, 6]

        mod_i <-adass.fr(Y_fd, X_fd, basis_s = basis_s,basis_t = basis_t,
                         tun_par = c(lambda_s_i,lambda_t_i,sum_s_i,ind_s_i,sum_t_i,ind_t_i),
                         beta_ders = beta_ders, beta_dert = beta_dert,
                         X_fd_test = X_fd_test,Y_fd_test = Y_fd_test,
                         grid_eval_ders=grid_eval_ders,grid_eval_dert=grid_eval_dert, CV = TRUE,K=K)

        a<-mod_i$CV
        b<-mod_i$CV_sd
        out <- list(a, b)
        return(out)
      }
      if(ncores==1){
        vec_par<-lapply(seq(1,length(comb_list_eval[,1])),parr_func)
      }
      else{
        if(.Platform$OS.type=="unix"){
          vec_par<-parallel::mclapply(seq(1,length(comb_list_eval[,1])),parr_func,mc.cores = ncores)
        }else{
          cl <- parallel::makeCluster(ncores)
          parallel::clusterExport(cl, c( "comb_list_eval","X_fd","Y_fd","basis_s","basis_t",
                                         "beta_ders","beta_dert","X_fd_test","Y_fd_test",
                                         "grid_eval_ders","grid_eval_dert"),envir = environment())
          parallel::clusterEvalQ(cl, {
            library(adass)
          })
          vec_par<-parallel::parLapply(cl,seq(1,length(comb_list_eval[,1])),parr_func)
          parallel::stopCluster(cl)
        }
      }
      mean_eval <- sapply(vec_par, "[[", 1)
      sd_eval <- sapply(vec_par, "[[", 2)

      ##Exploit (Truncation method) ----
      if (is.null(mean_par)) {
        mean_tot <- mean_eval
        sd_tot <- sd_eval
        comb_list_tot<-comb_list_eval
      }
      else{
        mean_tot <- c(mean_par, mean_eval)
        sd_tot <- c(sd_par, sd_eval)
      }
      if(progress&iter==iter_num){
        utils::setTxtProgressBar(pb,iter)
        break
      }
      rank_vec <- rank(mean_tot)
      treshold <- length(mean_tot) * (1-r)
      in_vec <- which(rank_vec <= treshold)
      out_vec <- which(rank_vec > treshold)
      in_comb <- comb_list_tot[in_vec, ]
      out_comb <- comb_list_tot[out_vec, ]
      ns_out <- length(out_vec)
      ind_new_comb <- sample(1:length(in_vec), size = ns_out, replace = TRUE)
      new_comb <- in_comb[ind_new_comb, ]
      mean_par <- mean_tot[in_vec]
      sd_par <- sd_tot[in_vec]

      mean_tot <- c(mean_par, mean_par[ind_new_comb])

      ## Exploration (perturbation) ----
      fac_vec <- pert_vec
      ind_fac <- sample(1:2, length(new_comb), replace = T)
      fac_comb <- sapply(ind_fac, function(ii) fac_vec[ii])
      fac_mat <- matrix(fac_comb, ns_out, 6)
      new_comb <- new_comb * fac_mat
      comb_list_eval <- new_comb
      comb_list_tot <- rbind(in_comb, new_comb)
      if(progress)
        utils::setTxtProgressBar(pb,iter)
    }

    ind_opt=which(mean_tot==min(mean_tot))
    lambda_s_opt <- comb_list_tot[ind_opt, 1]
    lambda_t_opt <- comb_list_tot[ind_opt, 2]
    sum_s_opt <- comb_list_tot[ind_opt, 3]
    ind_s_opt <- comb_list_tot[ind_opt, 4]
    sum_t_opt <- comb_list_tot[ind_opt, 5]
    ind_t_opt <- comb_list_tot[ind_opt, 6]
    tun_par_opt<-c(lambda_s_opt,lambda_t_opt,sum_s_opt,ind_s_opt,sum_t_opt,ind_t_opt)
    out<-list(tun_par_opt=tun_par_opt,
              CV=mean_tot,
              CV_sd=sd_tot,
              comb_list_tot=comb_list_tot,
              X_fd=X_fd,
              Y_fd=Y_fd)
    class(out)<-"adass_eaass"
    return(out)
  }




#' @title Adaptive smoothing spline estimator for the function-on-function linear regression model
#' @description The adaptive smoothing spline (AdaSS) estimator for the function-on-function linear regression proposed in Centofanti et al., 2020.
#' @param Y_fd An object of class fd corresponding to the response functions.
#' @param X_fd An object of class fd corresponding to the covariate functions.
#' @param basis_s B-splines basis along the \code{s}-direction of class basisfd.
#' @param basis_t B-splines basis along the \code{t}-direction of class basisfd.
#' @param beta_ders Initial estimate of the partial derivative of the coefficient function along the \code{s}-direction.
#'  Either a matrix or a class basisfd object. If NULL no adaptive penalty is  used along the \code{s}-direction.
#' @param beta_dert Initial estimate of the partial derivative of the coefficient function along the \code{t}-direction.
#'  Either a matrix or a class basisfd object. If NULL no adaptive penalty is used along the \code{t}-direction.
#' @param grid_eval_ders Grid of evaluation of the partial derivatives along the \code{s}-direction.
#' @param grid_eval_dert Grid of evaluation of the partial derivatives along the \code{t}-direction.
#' @param tun_par Vector of tuning parameters.
#' @param CV If TRUE the \code{K}-fold cross-validation prediction error is calculated. Default is FALSE.
#' If \code{X_fd_test} and \code{Y_fd_test} are both provided the prediction error on the test set is calculated in place of the cross-validation prediction error when \code{CV} is TRUE.
#' @param K Number of folds. Default is 10.
#' @param X_fd_test  Test set covariate functions. Default is NULL.
#' @param Y_fd_test Test set response functions. Default is NULL.
#' @return   A list containing the following arguments:
#' \itemize{
#' \item \code{B}: The basis coefficients matrix estimate of the coefficient function.
#'
#' \item \code{Beta_hat_fd}: The coefficient function estimate of class bifd.
#'
#' \item \code{alpha}: The intercept function estimate.
#'
#' \item \code{tun_par}: Vector of tuning parameters.
#'
#' \item \code{CV}: Estimated prediction error.
#'
#' \item \code{CV_sd}: Standard error of the estimated prediction error.
#'
#' \item \code{Y_fd}: The response functions.
#'
#' \item \code{X_fd}: The covariate functions.
#'}
#'@seealso \code{\link{adass.fr_eaass}}
#'
#' @export
#' @references
#' Centofanti, F., Lepore, A., Menafoglio, A., Palumbo, B., Vantini, S. (2023).
#' Adaptive Smoothing Spline Estimator for the Function-on-Function Linear Regression Model.
#' \emph{Computational Statistics 38(1), 191–216}.
#' @examples
#' library(adass)
#' data<-simulate_data("Scenario HAT",n_obs=100)
#' X_fd=data$X_fd
#' Y_fd=data$Y_fd
#' basis_s <- fda::create.bspline.basis(c(0,1),nbasis = 10,norder = 4)
#' basis_t <- fda::create.bspline.basis(c(0,1),nbasis = 10,norder = 4)
#' mod_smooth <-adass.fr(Y_fd,X_fd,basis_s = basis_s,basis_t = basis_t,tun_par=c(10^-6,10^-6,0,0,0,0))
#' grid_s<-seq(0,1,length.out = 10)
#' grid_t<-seq(0,1,length.out = 10)
#' beta_der_eval_s<-fda::eval.bifd(grid_s,grid_t,mod_smooth$Beta_hat_fd,sLfdobj = 2)
#' beta_der_eval_t<-fda::eval.bifd(grid_s,grid_t,mod_smooth$Beta_hat_fd,tLfdobj = 2)
#' mod_adass <-adass.fr(Y_fd, X_fd, basis_s = basis_s, basis_t = basis_t,
#'                      tun_par=c(10^-6,10^-6,0,1,0,1),beta_ders = beta_der_eval_s,
#'                      beta_dert = beta_der_eval_t,grid_eval_ders=grid_s,grid_eval_dert=grid_t )


adass.fr<-function(Y_fd,X_fd,basis_s,basis_t,
                  beta_ders=NULL,beta_dert=NULL,grid_eval_ders=NULL,
                  grid_eval_dert=NULL,tun_par=c(lambda_s=10^4,lambda_t=10^4,delta_s=0,gamma_s=1,delta_t=0,delta_t=1),
                  CV=FALSE,K=10,X_fd_test=NULL,Y_fd_test=NULL){

  n_obs<-dim(X_fd$coefs)[2]
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
  W_t<-fda::eval.penalty(basis_t)
  ##Centering data
  X_mean<-fda::mean.fd(X_fd)
  Y_mean<-fda::mean.fd(Y_fd)
  X_fd_cen<-fda::center.fd(X_fd)
  Y_fd_cen<-fda::center.fd(Y_fd)

  X_new<-fda::inprod(X_fd_cen,basis_s)
  Y_new<-fda::inprod(Y_fd_cen,basis_t)

  ##Eval weight function
  if(is.null(beta_ders)){
    beta_der_eval_s<-matrix(1,1,1)
    tun_par[3]=0
    tun_par[4]=0
    grid_eval_ders=domain_s
  }
  else if(class(beta_ders)[1]=="bifd" ){
    if(is.null(grid_eval_ders)|is.null(grid_eval_dert))
      stop("Please set the grid to evaluate the partial derivatives!")
    beta_der_eval_s<-fda::eval.bifd(grid_eval_ders,grid_eval_dert,beta_ders)
  }
  else{
    beta_der_eval_s<-beta_ders
    if(is.null(grid_eval_ders)|is.null(grid_eval_dert)) stop("Please provide the grids to evaluate the partial derivatives!")
    if(sum(range(grid_eval_ders)!=domain_s)|sum(range(grid_eval_dert)!=domain_t))
      stop("Grid domains to evaluate the partial derivatives different from the functional data domains!")
    if((length(grid_eval_ders)!=dim(beta_der_eval_s)[1])|(length(grid_eval_dert)!=dim(beta_der_eval_s)[2]))
      stop("Wrong dimensions between grids and partial derivatives evaluation!")
  }
  if(is.null(beta_dert)){
    beta_der_eval_t<-matrix(1,1,1)
    tun_par[5]=0
    tun_par[6]=0
    grid_eval_dert=domain_t
  }
  else if(class(beta_dert)[1]=="bifd" ){
    if(is.null(grid_eval_ders)|is.null(grid_eval_dert))
      stop("Please provide the grid to evaluate the partial derivatives!")
    beta_der_eval_t<-fda::eval.bifd(grid_eval_ders,grid_eval_dert,beta_dert)
  }
  else{
    beta_der_eval_t<-beta_dert
    if(is.null(grid_eval_ders)|is.null(grid_eval_dert)) stop("Please provide the grids to evaluate the partial derivatives!")
    if(sum(range(grid_eval_ders)!=domain_s)|sum(range(grid_eval_dert)!=domain_t))
      stop("Grid domains to evaluate the partial derivatives different from the functional data domains!")
    if((length(grid_eval_ders)!=dim(beta_der_eval_t)[1])|(length(grid_eval_dert)!=dim(beta_der_eval_t)[2]))
      stop("Wrong dimensions between grids and partial derivatives evaluation!")
  }

  W_si<-R_si<-W_ti<-R_ti<-list()
  for (ii in 1:(length(grid_eval_ders)-1)) {
    W_si[[ii]]<-fda::eval.penalty(basis_s,rng =c(grid_eval_ders[ii],grid_eval_ders[ii+1]) )
    R_si[[ii]]<-fda::eval.penalty(basis_s,2,rng =c(grid_eval_ders[ii],grid_eval_ders[ii+1]))
  }

  for (ii in 1:(length(grid_eval_dert)-1)) {
    W_ti[[ii]]<-fda::eval.penalty(basis_t,rng =c(grid_eval_dert[ii],grid_eval_dert[ii+1]) )
    R_ti[[ii]]<-fda::eval.penalty(basis_t,2,rng =c(grid_eval_dert[ii],grid_eval_dert[ii+1]) )
  }

  ## Calculation C1 and C2
  comb_list_mat<-expand.grid(1:(length(grid_eval_ders)-1),1:(length(grid_eval_dert)-1))
  ##Cs ------
  par_Cs <- function(ll) {
    ii <- comb_list_mat[ll, 1]
    jj <- comb_list_mat[ll, 2]
    SparseM::as.matrix.csr(W_ti[[jj]])%x% SparseM::as.matrix.csr(R_si[[ii]])*(1 / (abs(beta_der_eval_s[ii, jj]) +tun_par[3]*max(abs(beta_der_eval_s))))^tun_par[4]
  }
  Cs_list<-lapply(seq(1,length(comb_list_mat[,1])),par_Cs)
  Cs<-Reduce("+",Cs_list)
  ##Ct ------
  par_Ct <- function(ll) {
    ii <- comb_list_mat[ll, 1]
    jj <- comb_list_mat[ll, 2]
    SparseM::as.matrix.csr(R_ti[[jj]])%x% SparseM::as.matrix.csr(W_si[[ii]])*(1/(abs(beta_der_eval_t[ii, jj]) +tun_par[5]*max(abs(beta_der_eval_t))))^tun_par[6]
  }
  Ct_list<-lapply(seq(1,length(comb_list_mat[,1])),par_Ct)
  Ct<-Reduce("+",Ct_list)

  if(CV){
    if(is.null(X_fd_test)){
      fun<-function(ii){
        ran_seq<-sample(seq(1, n_obs), n_obs, replace=FALSE)
        split_vec<-split(ran_seq,cut(seq(1,n_obs),breaks=K,labels=FALSE))
        mean_vec<-numeric()
        for(ll in 1:K){
          Y_i_fd<-Y_fd[split_vec[[ll]]]
          X_i<-X_new[split_vec[[ll]],]
          X_minus<-X_new[-split_vec[[ll]],]
          Y_minus<-Y_new[-split_vec[[ll]],]
          A<-W_t%x%(t(X_minus)%*%X_minus)+tun_par[1]*Cs+tun_par[2]*Ct
          B_s<-matrixcalc::vec(t(X_minus)%*%Y_minus)
          b_vec<-SparseM::solve(A,B_s)
          B<-matrix(b_vec,basis_s$nbasis,basis_t$nbasis)
          Beta_hat_fd<-fda::bifd(B,basis_s,basis_t)
          Y_hat<-fda::fd(t(X_i%*%B),basis_t)
          mean_vec[ll]<-mean(diag(fda::inprod(Y_i_fd-Y_hat,Y_i_fd-Y_hat)))
        }
        mean<-mean(mean_vec)
        sd<-sd(mean_vec)/sqrt(K)
        out<- list(mean=mean,
                   sd=sd)
        return(out)
      }
    }
    else{
      fun<-function(ii){
        A<-W_t%x%(t(X_new)%*%X_new)+tun_par[1]*Cs+tun_par[2]*Ct
        B_s<-matrixcalc::vec(t(X_new)%*%Y_new)
        b_vec<-SparseM::solve(A,B_s)
        B<-matrix(b_vec,basis_s$nbasis,basis_t$nbasis)
        Beta_hat_fd<-fda::bifd(B,basis_s,basis_t)
        mean<-get_PMSE(Y_fd_test,X_fd_test,Beta_hat_fd)
        out<- list(mean=mean,
                   sd=0)
        return(out)
      }
    }
    vec_par<-lapply(1,fun)
    CV<-sapply(vec_par,"[[",1)
    CV_sd<-sapply(vec_par,"[[",2)

  }
  A<-W_t%x%(t(X_new)%*%X_new)+tun_par[1]*Cs+tun_par[2]*Ct
  B_s<-matrixcalc::vec(t(X_new)%*%Y_new)
  b_vec<-SparseM::solve(A,B_s)
  B<-matrix(b_vec,basis_s$nbasis,basis_t$nbasis)
  Beta_hat_fd<-fda::bifd(B,basis_s,basis_t)

  if(CV==FALSE){
    CV=CV_sd=NA
  }
  X_mean_new<-fda::inprod(X_mean,basis_s)
  alpha<-Y_mean-fda::fd(t(X_mean_new%*%B),basis_t)
  out<-list(B=B,
            Beta_hat_fd=Beta_hat_fd,
            alpha=alpha,
            tun_par=tun_par,
            CV=CV,
            CV_sd=CV_sd,
            X_fd=X_fd,
            Y_fd=Y_fd)
  class(out)<-"adass"
  return(out)
}
get_PMSE<-function(Y_fd_test,X_fd_test,Beta){

  length_grid<-1000
  grid<-seq(0,1,length.out = length_grid)
  delta<-1/length_grid
  X_fd_eval<-t(fda::eval.fd(grid,X_fd_test))
  Y_fd_eval<-t(fda::eval.fd(grid,Y_fd_test))
  Beta_mat<-fda::eval.bifd(grid,grid,Beta)
  Y_hat<-delta*X_fd_eval%*%Beta_mat
  PMSE<-mean(delta*rowSums((Y_fd_eval-Y_hat)^2))

  return(PMSE)
}

