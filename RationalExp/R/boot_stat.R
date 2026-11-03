
#' Compute the bootstrap test statistic for parallel implementation
#'
#' This is an internal function to separately compute the bootsrap test statsitic.
#'
#' By default, the test is implemented without covariates. To perform the test with covariates, one has to indicate in X a non-constant vector or matrix. Also, one can perform the « generalized » tests allowing for aggregate shocks by using the dummy variable generalized. Survey weights can be added. The user can modify the number of cores used by R to reduce the computational time. Tuning parameters used in the test can also be modified.
#'
#' @param u bootstrap index;
#' @param Y_tilde the vector stacking the realisations y then the anticipated values psi of respective sizes n_y and n_p.
#' @param X the matrix of covariates.  Set to a vector of 1 by default (in which case the test without covariates is performed).
#' @param D the vector stacking the dummies for the dataset of realisation : n_y ones then n_p zeros
#' @param epsilon the parameter epsilonon in  Section 3 of DGM. Default value is 0.05.
#' @param N3 equals to N if covariates, to 1 other wise.
#' @param p the parameter p in  Section 3 of DGM.  Default is  0.05.
#' @param prec the number of points to be tested. Default is 30.
#' @param N the total numeber of obs
#' @param sample_mat matrix of bootrap indexes
#' @param generalized "Add" if additive shocks for the generalized test
#' @param weights  survey weights
#' @param y_grid the grid points. Default is quantile(Y_tilde,seq(0,1,length.out=30)).
#' @param phi_n the GMS function in DGM
#' @param M_bar the quantilty bar m in section 2 of  DGM
#' @param DX the total number of covariates
#'
#' @return
#' @export
boot_stat <- function(u,Y_tilde,X,D,epsilon,N3,p,prec,N,sample_mat,generalized,weights,y_grid,phi_n,M_bar,DX){

T_reps = matrix(0,prec,1)
# // (Step 2.(b) in MCMI) Generate Bootstrap Samples
# b_sample = ceiling ( N * runif(N,0,1))
# b_sample = sample(1:N,N,replace = TRUE, prob =   weights /sum(weights)  )
b_sample = sample_mat[u,]
T_n_grid_b<- matrix(0,prec,1)
D_b <- D[ b_sample]
W_b <-  ((1-D_b)/sum((1-D_b)) - D_b / sum(D_b))*N
Y_tilde_b <-Y_tilde[ b_sample]


if( generalized =="Add"  |  generalized =="Mult"){
  yy_b <- Y_tilde_b[D_b==1]
  ypsi_b <- Y_tilde_b[D_b==0]
  if( generalized =="Add" ){
    alpha_b <-mean(yy_b)-mean(ypsi_b)
    Y_tilde_b[D_b==1] <- Y_tilde_b[D_b==1]-alpha_b
  }else if( generalized =="Mult"){
    alpha_b <-mean(yy_b)/mean(ypsi_b)
    Y_tilde_b[D_b==1] <- Y_tilde_b[D_b==1]/alpha_b
  }
}

for (i in 1:prec){

  ### create the moments m_j^b (j=1 and 2)
  y <-  y_grid[i]
  #### equality
  eq_1 <- W_b*Y_tilde_b
  ## inequality
  ineq_1 <- - W_b*(y-Y_tilde_b)*(Y_tilde_b <= y)

  data_test <- cbind(ineq_1,eq_1)
  N_k = dim(data_test)[2]
  if(  N_k >1){
    M_ineq_b = data_test[,1]
    M_eq_b = data_test[,2]
  }else{
    M_ineq_b = data_test
  }
  X_b = as.matrix(X[b_sample,] )

  # // (Step 2.(c)) Without X this step is trivial (should be a normalisation but there is none as N3=1, thus r_n=1)
  # and there is non sum in the definition of T_n^b
  X_mean_b = colMeans(X_b)
  Sigma_hat_b = var(X_b)
  if(max(Sigma_hat_b) ==0){Sigma_hat_b =1}

  # // Check whether r_n is specified// r_n is of no use here (without X)
  r_n = ceiling((N3/2)^(1/2/DX)/2)
  X_adj_b = pnorm((X_b-X_mean_b)%*%sqrt(Sigma_hat_b^(-1)))

  ### to compute the weights in the definition of the test statistic T (2r)^{-d_X}/(r^2+100) times the indicator of this
  ## function h_{a,r} entering this specific moment.
  res <- c_cube(X_adj_b, N, DX, r_n)


  g_col_b <- res[[3]]
  Q_AR_b <- res[[4]]
  G_X_b <- res[[5]]
  # // (Step 2.(d))
  N_g_b = dim(G_X_b)[2]

  if(  N_k >1){
    m_n_b = cbind(M_ineq_b,M_eq_b)
  }else{
    m_n_b = M_ineq_b
  }
  N_k_b = dim(data_test)[2]
  if(  is.null(  N_k_b)){
    N_k_b = 1
  }

  y_b<- Y_tilde[b_sample]
  D_b <- D[b_sample]
  sig <- var(  y_b)
  #### create the small sigma^b to later form overline sigma as in the equation below (3) p12
  if(  N_k ==1){
    sigma_1_hat_b =   sig
  }else{
    sigma_1_hat_b =  diag(rep(sig,N_k ))
  }

  ## *N_g_b is the number of instrumental function h_{a,r} in DGM_paper p12
  S_m_b = matrix(0,N_g_b,1)
  M_g_b = matrix(0,N,N_g_b * N_k_b)
  M_bar_b = matrix(0,N_k_b*N_g_b,1) ## matrix  for overline m in DGM paper
  Sigma_bar_b = matrix(0,N_k_b*N_g_b,N_k_b*N_g_b)
  # index=1
  # //!!!!!!!!!*** S1, S2 and S3
  ### for each instrumental function h_{a,r} in DGM paper, we compute the moments m_n^b( h_{a,r}, y) (in M_temp_b)
  for (index in 1:N_g_b){
    # 		                                                  /*
    # 		                                                  M_temp :
    # 		                                                  sigma_n_hat :
    # 		                                                  sigma_n_bar :
    # 		                                                  */

    M_temp_b = m_n_b * G_X_b[,index]
    M_g_b[,((index-1)*N_k_b+1):(index*N_k_b)] = M_temp_b
    ### we multiply by sqrt N and define the matrix overline Sigma
    M_bar_b[(N_k*(index-1)+1):(N_k_b*index),1] = sqrt(N)*colMeans(M_temp_b)
    sigma_n_hat_b = var(M_temp_b)
    if( sum(diag(sigma_1_hat_b)==0)>0 ){
      sigma_n_bar_b = diag(diag(sigma_n_hat_b)) +  epsilon * diag(diag(sigma_1_hat_b)+epsilon )
    }else{
      if(  N_k_b ==1){
        sigma_n_bar_b = sigma_n_hat_b + epsilon * sigma_1_hat_b
      }else{
        sigma_n_bar_b = diag(diag(sigma_n_hat_b)) + epsilon * diag(diag(sigma_1_hat_b))
      }
    }
    Sigma_bar_b[(N_k_b*(index-1)+1):(N_k_b*index),(N_k_b*(index-1)+1):(N_k_b*index)] = sigma_n_bar_b
  }

  ###
  ##### this is the boot strap statitic, where we first compute the overline m using
  # the Generalized Moment Selection (GMS) method, as  in the last equation of p12.
  # phi_n[,i] is the  Generalized Moment Selection function
  M_boot = (M_bar_b-M_bar[,i]) + phi_n[,i]

  ## this compute the test statistic using the overline Sigma, and the bootstraped moments M_boot
  T_reps[i,1] = T_stat(M_boot,Sigma_bar_b,Q_AR_b,N_g_b,N_k_b,p)


}
# T_reps[,b_rep]
  ## this compute the test statistic using the overline Sigma, and the bootstraped moments M_boot
 return( as.numeric(max(T_reps[,1])) )
}


