#####----------------------------------------------------------------------------------------------------------------
#####  prolongation matrix I_{g-1}^{g} (coarse to fine)
prolongation_matrix <- function(K_coarse, K_fine, q){
  
  g <- q+1
  values <- (1/(2^q))*choose(g, 0:g)   # Pascals Triangle
  k_zeros <- K_fine+2*q-length(values)
  S <- sapply(1:K_coarse, function(j)  c( rep(0,2*(j-1)) , values , rep(0,k_zeros-(2*(j-1))) ) )
  P <- S[g:(K_fine+q),]
  return(P)
  
}


#####----------------------------------------------------------------------------------------------------------------
#####  restriction matrix I_{g}^{g-1} (fine to coarse)
restriction_matrix <- function(K_fine, K_coarse, q){
  
  R <- t( prolongation_matrix(K_coarse, K_fine, q) )
  return(R)
  
}


#####--------------------------------------------
##### matrix-free PCG method with diagonal preconditioner
solve_PCG <- function(tPhi_list, Psi_list, lambda, b, pen_type = "curve", tolerance = 1e-8){
  
  ### diagonal preconditioner
  diag_spline <- diag_khatrirao_rcpp(tPhi_list)
  if(pen_type == "curve"){
    diag_pen <- rowSums(sapply( 1:length(Psi_list), function(j) diag_kronecker_rcpp(Psi_list[[j]]) ) )
  }
  if(pen_type == "diff"){
    Pj <- length(Psi_list)
    Jj <- sapply(1:Pj, function(p) dim(Psi_list[[p]])[2] )
    Kj <- prod(Jj)
    n_left <- c(1, sapply(1:(Pj-1), function(p) prod(Jj[1:p]) ))
    n_right <- c(rev( sapply(1:(Pj-1), function(p) prod(rev(Jj)[1:p]) ) ), 1)
    if(Pj==1) n_left <- n_right <- 1
    diag_pen <- rowSums( sapply(1:Pj, function(p)  rep(1,n_left[p]) %x% diag(Psi_list[[p]]) %x% rep(1,n_right[p]) ) )
  }
  diag_inv <- 1 / (diag_spline + lambda*diag_pen )
  K <- length(diag_inv)

  ###
  norm_b <- sqrt(sum(b^2))
  alpha <- rep(0, K)
  r <- b
  d <- r
  z <- diag_inv*r
  d <- z
  rz <- as.numeric( crossprod(r,z) )
  #if(is.null(K_max)){
  K_max <- K
  #}
  
  for(i in 1:K_max){         # loop of the PCG iteration
    
    Ad <- MVP_spline(tPhi_list, d) + lambda*MVP_penalty(Psi_list, d, pen_type = pen_type)
    t <- as.numeric( rz / crossprod(d, Ad) )
    alpha <- alpha+t*d
    rz_old <- rz
    r <- r-t*Ad
    if(sqrt(sum(r^2)) / norm_b <= tolerance){
      break
    }
    z <- diag_inv*r
    rz <- crossprod(r,z)
    beta <-  as.numeric( rz / rz_old )
    d <- z + beta*d
    
  }
  return(alpha)
  
}


#####----------------------------------------------------------------------------------------------------------------
#####  matrix-free Jacobi as smoothing iteration
smooth_jacobi <- function(tPhi_list, Psi_list, lambda, b, k_max, w = 1, a = rep(0,length(b)), tol = 1e-8 ){
  
  # Parameter
  diag_A <- diag_khatrirao_rcpp(tPhi_list) + lambda*rowSums(sapply( 1:length(Psi_list), function(j) diag_kronecker_rcpp(Psi_list[[j]]) ) )
  invD <- 1/diag_A
  W <- w*invD
  norm_b <- sqrt(sum(b^2))
  nreg <- length(Psi_list)
  
  # Jacobi Iteration
  if(sqrt(sum(a^2))!=0){
    Aa <- MVP_spline(tPhi_list, a) + lambda*MVP_penalty(Psi_list, a)
    res <- b-Aa
  } else{
    res <- b
  }
  for(k in 1:k_max){
    a <- a + W*res
    Aa <- MVP_spline(tPhi_list, a) + lambda*MVP_penalty(Psi_list, a)
    res <- b-Aa
    if(sqrt(sum(res^2))/norm_b < tol){
      break
    }
  }
  return(a)

}


#####----------------------------------------------------------------------------------------------------------------
#####  matrix-free v-cycle (with Jacobi smoother and CG coarse grid solver)
v_cycle <- function(tPhi_list, Psi_list, Rest, Prol, lambda, b, nu, U_chol, w = 1, a = rep(0,length(b)) ){
  
  g <- length(tPhi_list)
  if(g==1){
    if(is.logical(U_chol)){    
      z <- solve_PCG(tPhi_list[[g]], Psi_list[[g]], lambda, b)
      return(z)
    } else{
      z <- forwardsolve(t(U_chol), as.vector(b) )
      return( backsolve(U_chol, z) )
    }
  } else{
    a <- smooth_jacobi(tPhi_list[[g]], Psi_list[[g]], lambda, b, nu[1], w, a )
    Aa <- MVP_spline(tPhi_list[[g]], a) + lambda*MVP_penalty(Psi_list[[g]], a)
    e <- b - Aa
    r <- MVP_kronecker_rcpp(Rest[[g-1]], e)
    e <- v_cycle(tPhi_list[1:(g-1)], Psi_list[1:(g-1)], Rest[1:(g-1)], Prol[1:(g-1)], lambda, r, nu, U_chol, w )
    a <- a + MVP_kronecker_rcpp( Prol[[g-1]], e )
    a <- smooth_jacobi(tPhi_list[[g]], Psi_list[[g]], lambda, b, nu[2], w, a )
  }
  return(as.vector(a))
  
}

