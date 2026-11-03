

#' The test statistic for the RE test with survey weights
#'
#' This is an internal function used in the function test to compute the test statistic with survey weights.
#'
#' By default, the test is implemented without covariates. To perform the test with covariates, one has to indicate in X a non-constant vector or matrix. Also, one can perform the « generalized » tests allowing for aggregate shocks by using the dummy variable generalized. Survey weights can be added. The user can modify the number of cores used by R to reduce the computational time. Tuning parameters used in the test can also be modified.
#'
#' @param Y_tilde the vector stacking the realisations y then the anticipated values psi of respective sizes n_y and n_p.
#' @param X the matrix of covariates. Set to a vector of 1 by default (in which case the test without covariates is performed).
#' @param D the vector stacking the dummies for the dataset of realisation : n_y ones then n_p zeros
#' @param data_test the matrix of sample moments
#' @param epsilon the parameter epsilonon inSection 3
#' @param B  the number of bootstrap samples
#' @param N3 a parameter equal to 1 if no covariates, to N otherwise
#' @param c the parameter c in  Section 3
#' @param kappa the parameter kappapa in  Section 3
#' @param p  the parameter p in  Section 3. Equals 0.0 if generalized RE test.
#' @param N total number of observations
#' @param weights the vector of survey weights. Uniform by default.
#'
#' @return a list containing, in order:
#'
#'  -  T_n : the test statistic
#'
#'  - phi_n: the vector of coresponding GMS functions
#'
#'  - M_bar : the matrix of M_bar  in  Section 3
#' @references
#' D’Haultfoeuille X, Gaillac C, Maurel A (2018). “Rationalizing Rational Expectations? Tests and Deviations.” CREST Working paper
#'
#' Andrews D, Shi X (2017). “Inference Based on Many Conditional Moment Inequalities.” Journal of Econometrics, 196(2), 275–287.
#'
#' Andrews DW, Kim W, Shi X (2017). “Commands for testing conditional moment inequalities and equalities.” The Stata journal, 17(1).
#'
#' @export
#'
test_base <- function(Y_tilde,X,D,data_test,epsilon,B,N3,c,kappa,p,N,weights){

  out <-  vector("list")
  # N = length(Y_tilde)	#	// # of sample
  DX = dim(X)[2] #		// dimension of regressors
  # // calculate kappa_n and B_n if unspecified
  kappa_n = sqrt(kappa*log(N))
  B_n = sqrt(c*log(N)/log(log(N)))
  X_mean = colMeans(X)
  Sigma_hat = var(X)
  if(max(Sigma_hat) ==0){Sigma_hat =1}
  # X_adj = pnorm((X-X_mean)*sqrt(invsym(diag(diagonal(Sigma_hat)))))
  X_adj = pnorm((X-X_mean)%*%sqrt(Sigma_hat^(-1)))
  # // (STEP 1.(b)) Specify the functions g - countable cubes
  # // and (STEP 1.(c)) Specify the weight function Q_AR

  # // Check whether r_n is specified
  r_n = ceiling((N3/2)^(1/2/DX)/2)

  # // g_col : the number of cubes for each r (r_n x 1 vector)
  # // G_X   : function g for countable cubes (N x sum(g_col) matrix)
  # // Q_AR  : weight function (1 x sum(g_col) vector)
  res <- c_cube(X_adj, N, DX, r_n)

  g_col <- res[[3]]
  Q_AR <- res[[4]]
  G_X <- res[[5]]
  #   N_g is the number of instrumental function h_{a,r} in DGM_paper p12
  N_g =dim(G_X)[2]
  N_k = dim(data_test)[2]
  if(  is.null(N_k)){
    N_k = 1
  }

  # sig <- var(  Y_tilde)/sqrt(N)
  sig <- cov.wt(as.matrix(Y_tilde), as.numeric(weights))$cov
  if(  N_k ==1){
    sigma_1_hat =  sig
  }else{
    sigma_1_hat =  diag(rep(sig,N_k ))
  }
  #N_g_b is the number of instrumental function h_{a,r} in DGM_paper p12
  S_m = matrix(0,N_g,1)
  M_g = matrix(0,N,N_g * N_k)
  M_bar = matrix(0,N_k*N_g,1)
  Sigma_bar = matrix(0,N_k*N_g,N_k*N_g)

  # //!!!!!!!!!*** S1, S2 and S3 index =1
  ### for each instrumental function h_{a,r} in DGM paper, we compute the moments m_n( h_{a,r}, y) (in M_temp)
  for (index in 1:N_g){
    # 		  /*
    # 		    M_temp :
    # 		    sigma_n_hat :
    # 		    sigma_n_bar :
    # 		    */

    M_temp = data_test * G_X[,index]
    M_g[,((index-1)*N_k+1):(index*N_k)] = M_temp
    ### we multiply by sqrt N and define the matrix overline Sigma
    M_bar[(N_k*(index-1)+1):(N_k*index),1] =  sqrt(N)*cov.wt(M_temp,as.numeric(weights))$center
    # sigma_n_hat = var(M_temp)
    sigma_n_hat = cov.wt(M_temp,as.numeric(weights))$cov
    if( sum(diag(sigma_1_hat)==0)>0 ){
      sigma_n_bar = diag(diag(sigma_n_hat)) +  epsilon * diag(diag(sigma_1_hat)+epsilon )
    }else{
      if(  N_k ==1){
        sigma_n_bar = sigma_n_hat + epsilon * sigma_1_hat
      }else{
        sigma_n_bar = diag(diag(sigma_n_hat)) + epsilon * sigma_1_hat
      }
    }
    Sigma_bar[(N_k*(index-1)+1):(N_k*index),(N_k*(index-1)+1):(N_k*index)] = sigma_n_bar

    ### this computes the values of the brackets in the expression of T_n p12 in DGM
    S_m[index,1] = S1(sqrt(N)*cov.wt(M_temp,as.numeric(weights))$center,sigma_n_bar,1,N_k,p)
  }

  ## this sum the brackets in the expression of T_n in DGM p12, where  Q_AR puts the required weights.
  T_n = Q_AR%*%S_m #// CvM statistic, keep



  ## this step  compute the GMS function as define in 1) at the end of p12 in DGM
  if(  N_k >1){
    D_n = matrix(diag(Sigma_bar),N_k*N_g,1)
    si_1 = 1 / sqrt(diag(Sigma_bar))
  }else{
    D_n = matrix(Sigma_bar,N_k*N_g,1)
    si_1 = 1 / sqrt(diag(Sigma_bar))
  }
  si_n = (M_bar * si_1) / kappa_n
  phi_n = (si_n >= 1) * sqrt(D_n) * B_n
  if(  N_k>1){
    for (index in 1:N_g) {

      phi_n[((index-1)*N_k+2):(index*N_k),] = matrix(0,1,1)

    }
  }


  out[[1]] <-   T_n
  out[[2]] <- phi_n
  out[[3]] <- M_bar
  out[[4]] <- si_1
  return(out)
}

