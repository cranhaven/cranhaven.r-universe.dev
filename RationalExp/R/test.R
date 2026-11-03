
#' Implementation of the RE test with possible survey weights (direct and with parallel computing)
#'
#'This function performs the test of rational expectations described in Section 3 of D’Haultfoeuille et al. (2018). By default, the test is implemented without covariates. To perform the test with covariates, one has to indicate in X a non-constant vector or matrix. Also, one can perform the « generalized » tests allowing for aggregate shocks by using the dummy variable generalized. Survey weights can be added. The user can modify the number of cores used by R to reduce the computational time. Tuning parameters used in the test can also be modified.
#'
#' @param Y_tilde the vector stacking the realisations y then the anticipated values psi of respective sizes n_y and n_p.
#' @param D the vector stacking the dummies for the dataset of realisation : n_y ones then n_p zeros
#' @param X the matrix of covariates.  Set to a vector of 1 by default (in which case the test without covariates is performed).
#' @param weights the vector of survey weights. Uniform by default.
#' @param generalized whether a generalized test should be performed or not: "Add" for additive shocks (default),  "Mult" for multiplicative shocks. Set by default to "No" (no generalized test).
#' @param nbCores the number of cores used by the program. To reduce the computational time, this function can use several cores, in which case the library snowfall should be loaded first. By default nbCores is set to 1.
#' @param tuningParam a dictionnary (see the example below for modification of the default parameters) containing:
#'
#'      - the parameter p  in  Section 3 of DGM.  Default is0.05.
#'
#'      - epsilon the parameter epsilonon in  Section 3 of DGM. Default value is 0.05 and p is set to 0 if a generalized test is performed.
#'
#'      - B the number of bootstrap samples. Default value is 500.
#'
#'      - grid_y: the number of points to be tested.
#'
#'        Default is quantile(Y_tilde,seq(0,1,length.out=30)).
#'
#'      - c:  the parameter c inSection 3 of DGM. Default is 0.3.
#'
#'      - kappa : the parameter kappapa in  Section 3 of DGM. Default is  0.001.
#'
#' Default values are associated with the test without covariates.
#'@return a list containing, in order:
#'
#'  - N, the number of observations
#'
#'  - cv01, the 1\% critical value
#'
#'  - cv05, the 5\% critical value
#'
#'  - cv10, the 10\% critical value
#'
#'  - T_n, the Test ststistic
#'
#'  - B, the number of bootstrap samples
#'
#'  - p_value, the p-value
#'
#'  - T_reps, the vector of bootstraped test statitics.
#' @references
#' D’Haultfoeuille X, Gaillac C, Maurel A (2018). “Rationalizing Rational Expectations? Tests and Deviations.” NBER Working paper <doi:10.3386/w25274>
#'
#' Andrews D, Shi X (2017). “Inference Based on Many Conditional Moment Inequalities.” Journal of Econometrics, 196(2), 275–287.
#'
#' Andrews DW, Kim W, Shi X (2017). “Commands for testing conditional moment inequalities and equalities.” The Stata journal, 17(1).
#'
#' @export
#' @examples ## The RE test without covariates
#'n_p=600
#'n_y=n_p
#'N <- n_y + n_p
#'rho <-0.29
#'sig=0.1
#'u=1
#'b=0.10
#'a=2
#'
### Data generating process
#'psi <-rnorm(n_p,0,u)
#'pp_y <- runif(n_y,0,1)
#'zeta <- rnorm(n_y,a,sig)
#'zeta1 <- rnorm(n_y,-a,sig)
#'pp1_y <- 1*(pp_y <b)
#'pp2_y <- 1*(pp_y >1-b)
#'pp3_y <- 1*(pp_y <=(1-b) & pp_y >=b)
#'psi_y <-rnorm(n_y,0,u)
#'y =  rho*psi_y+ pp1_y*zeta + pp2_y*zeta1
#'
#'
#'D <- rbind(matrix(1,n_y,1),matrix(0,n_p,1))
#'Y_tilde <- rbind(matrix(y,n_y,1),matrix(psi,n_p,1))
#'
#'#res <- test(Y_tilde ,D)
#'
#'
test <-  function(Y_tilde,D,X =matrix(1,length(Y_tilde),1),weights=rep(1/length(Y_tilde),length(Y_tilde)),generalized= "No",nbCores=1, tuningParam=NULL){
  n_y = sum(D)
   ## if no parameters: test without X by default.
  if(is.null(X)){
    X= matrix(1,length(Y_tilde),1)
  }
  if(is.null(weights)){
    weights= as.matrix(rep(1/length(Y_tilde),length(Y_tilde)))
  }
  if(is.null( generalized)){
    generalized= "No"
  }
  if(is.null(nbCores)){
    nbCores=1
  }
  ## if no parameters: test without X by default.
  if(is.null(tuningParam)){
    # X<- matrix(1,length(Y_tilde),1)
    p= 0.05
    epsilon=0.05
    B=500
    prec=30
    y_grid=quantile(Y_tilde,seq(0,1,length.out=prec))
    c=0.3
    kappa=0.001
  }else{
    # X<-tuningParam[["X"]]
    p=tuningParam[["p"]]
    epsilon=tuningParam[["epsilon"]]
    B=tuningParam[["B"]]
    y_grid=tuningParam[["y_grid"]]
    c=tuningParam[["c"]]
    kappa=tuningParam[["kappa"]]
  }
  # generalized= "Mult"
  ## N3 for r_n =1 => no covariates in the test
  ## N2 for impacting kappapa and B_n
  #### number of points to test on the support of (Y,\psi), namely \mathcal{Y} and \mathcal{\psi} in 3 Statistical tests p11
  prec = length(y_grid)
  out <-  vector("list")
  N = length(Y_tilde)  #	// # of sample
  Sigma0 = var(X)
  if(max(  Sigma0) ==0){N3 =1}else{N3=min(N,50)}
  if( generalized =="Add"  |  generalized =="Mult"){
    yy <- Y_tilde[D==1]
    ypsi <- Y_tilde[D==0]
    if(  generalized =="Add" ){
      p= 0
      alpha <-sum(yy*weights[D==1]/sum(weights[D==1]))-sum(ypsi*weights[D==0]/sum(weights[D==0]))
      Y_tilde[D==1] <- Y_tilde[D==1]-alpha
    }
    if( generalized =="Mult" ){
      p= 0
      alpha <-sum(yy*weights[D==1]/sum(weights[D==1]))/sum(ypsi*weights[D==0]/sum(weights[D==0]))
      Y_tilde[D==1] <- Y_tilde[D==1]/alpha
    }
  }

  DX = dim(X)[2] #		// dimension of regressors
  # // calculate kappa_n and B_n if unspecified
  kappa_n = sqrt( kappa*log(N))
  B_n = sqrt(c*log(N)/log(log(N)))


  ### generic weights W in the paper
  W <-  ((1-D)/sum((1-D)) - D / sum(D))*N

  ########
  phi_n <- NULL
  M_bar <- NULL
  T_n_grid<- matrix(0,prec,1)
  ## loop on the points y to test, create the moments
  for (i in 1:prec){

    y <-  y_grid[i]
    ## create the moments m_j j=1 and 2 in equation (3) p12
    #### equality  (j=2)
    eq_1 <- W*Y_tilde
    ## inequality (j=1)
    ineq_1 <- - W*(y-Y_tilde)*(Y_tilde <= y)
    data_test <- cbind(ineq_1,eq_1)

    #### Make the test
    res <- test_base(Y_tilde,X,D,data_test,epsilon,B, N3,c,kappa,p,N,weights)

    ## stock the value
    T_n_grid[i,1] <- res[[1]]   ## the test statistics for y
    phi_n <- cbind(phi_n,res[[2]] )
    M_bar <- cbind(M_bar,res[[3]] )
  }

  ## take the maximum of all values as in the expression of T inthe equation in the middle of p12
  T_n  <- max( T_n_grid)
  # // Using Bootstrap ctirical value
  b_num =B# // number of bootstrap samples
  T_reps = matrix(0,prec,B)

  sample_mat <- matrix(0, b_num,N)
  for (k in 1: b_num ){
    sample_mat[k,] <- sample(1:N,N,replace = TRUE, prob =   weights /sum(weights)  )
  }

  T_reps2 = matrix(0,1,B)
  #### if one core, then direct work
  if(nbCores==1){
    ######## Bootstrap test  b_rep=1
    for (b_rep in 1:b_num){
      T_reps2[1,b_rep] <-boot_stat(b_rep,Y_tilde,X,D,epsilon,N3,p,prec,N,sample_mat,generalized,weights,y_grid,phi_n,M_bar,DX)
    }
  }else{
    # nbCores=4
    sfInit(parallel=TRUE, cpus=nbCores, type="SOCK")
    # sfExportAll( except=c() )
    sfExportAll()
    #sfExport("c_cube")
    #sfExport("S1")
    #sfExport("T_stat")
    #sfExport("sample_mat")
    # sfClusterSetupRNG('RNGstream')
    T_reps2 <-sfLapply( 1:B,boot_stat,Y_tilde,X,D,epsilon,N3,p,prec,N,sample_mat,generalized,weights,y_grid,phi_n,M_bar,DX)
    sfStop()
    T_reps2 <- unlist(  T_reps2)
  }

  # // (STEP 2.(f)) compute the pvalue of the test.
  T_reps = sort(T_reps2)
  p_index = 0
  for (p_rep in 1:B){
    if (T_reps[p_rep] < T_n) {
      p_index = p_index + 1
    }
  }
  if(p_index == 0){
    p_index = 1
  }
  p_value = 1-(p_index - 1) / (B - 1)

  cv01 =quantile( T_reps, 0.99, na.rm = TRUE)
  cv05 =quantile( T_reps, 0.95, na.rm = TRUE)
  cv10 =quantile( T_reps, 0.90, na.rm = TRUE)

  out[[1]] <- N
  out[[2]] <- cv01
  out[[3]] <- cv05
  out[[4]] <- cv10
  out[[5]] <- T_n
  out[[6]] <- B
  out[[7]] <- p_value
  out[[8]] <- T_reps
  cat("Conditional Moment Inequalities Test   Number of obs : ", N ,"\nTest Statistic : ", T_n ,"\n ")
  cat("Critical Value (1%) " ,cv01 ,"\n")
  cat("Critical Value (5%) " ,cv05 ,"\n")
  cat("Critical Value (10%) " ,cv10,"\n")
  cat("p-value  : ",p_value)
  return(out)
}
