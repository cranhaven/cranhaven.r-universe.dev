
# Constraint function
constraint <- function(alpha_x,alpha_1n,r, sig.lv){
  rr <- base::sqrt(r)
  n_dim <- length(r)
  mat <- rr%*%(1/t(rr))
  mat[upper.tri(mat)]<- t(mat)[upper.tri(mat)]
  diag(mat) <- rep(1, n_dim)
  corr <- mat
  return(1 - mnormt::pmnorm(x  = c(stats::qnorm(1-alpha_1n),stats::qnorm(1-alpha_x)), mean = rep(0,length(r)), varcov = corr)[1]-sig.lv)
}


# Calculate the significant level for the n-th sub-population
alpha_kernel <- function(alpha_tol,r, sig.lv){
  aa <- try(stats::uniroot(constraint,interval = c(0,0.025),alpha_1n = alpha_tol,r=r,sig.lv = sig.lv,tol = 1e-10,maxiter = 10000, trace = 2),silent = TRUE)
  if (typeof(aa)=='list' ){
    alpha_x <- aa$root
    return(alpha_x)
  }
}

