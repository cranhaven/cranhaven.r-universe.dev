#####--------------------------------------------
#####  B-spline Basis Matrix (with equidistant knots)
bspline_matrix <- function(x, m, q, Omega = NULL ){
  
  if(is.null(Omega)){
    Omega <- c(min(x), max(x))
  }
  trunc_pol <- function(x, t, q) (x-t)^q * (x>t)
  h <- (Omega[2] - Omega[1]) / (m+1)
  knots <- seq(Omega[1]-q*h, Omega[2]+q*h, by = h)
  H <- outer(x, knots, trunc_pol, q)
  D <- diff(diag(dim(H)[2]), diff = q+1) / (gamma(q+1)*h^q)
  Phi <- (-1)^(q+1) * tcrossprod(H,D)
  Phi[Phi < 1e-10] <- 0
  return(Phi)
  
}


#####--------------------------------------------
##### computes the product (Phi^T%*%(Phi%*%alpha))
MVP_spline <- function(tPhi_list, alpha){
  
  v <- MVP_khatrirao_trans_rcpp(tPhi_list, alpha)
  base <- MVP_khatrirao_rcpp(tPhi_list, v)
  return(base)
  
}

#####--------------------------------------------
##### computes the product Lambda%*%alpha
MVP_penalty <- function(pen_list, alpha, pen_type = "curve"){
  
  if(pen_type == "curve"){
    pen <- rowSums( sapply( 1:length(pen_list), function(p) MVP_kronecker_rcpp(pen_list[[p]], alpha) ) )
    return(pen)
  }
  if(pen_type == "diff"){
    Pj <- length(pen_list)
    Jj <- sapply(1:Pj, function(p) dim(pen_list[[p]])[1])
    n_left <- c(1, sapply(1:(Pj-1), function(p) prod(Jj[1:p]) ))
    n_right <- c(rev( sapply(1:(Pj-1), function(p) prod(rev(Jj)[1:p]) ) ), 1)
    if(Pj==1) n_left <- n_right <- 1
    pen <- rowSums( sapply( 1:Pj, function(p) MVP_normalfactor_rcpp(pen_list[[p]], n_left[p], n_right[p], alpha) ) )
    return(as.vector(pen))
  }
  
}


#####----------------------------------------------------------------------------------------------------------------
#####  Gramian Matrix of varphi_{-q,q}^l,...,varphi_{m,q}^l (equidistant knots)
#' @importFrom statmod gauss.quad
L2norm_matrix <- function(m, q, d, Omega){
  
  # Parameter
  K <- m+q+1
  h <- (Omega[2]-Omega[1]) / (m+1)
  
  # Gauss-Quadratur
  n_gauss <- q+1
  gauss_legendre <- statmod::gauss.quad(n_gauss)
  w_ref <- gauss_legendre$weights[order(gauss_legendre$nodes)]
  points_ref <- (h/2)*sort(gauss_legendre$nodes) + (Omega[1]+(h/2))
  points <- as.vector( sapply( 1:(m+1), function(j) points_ref+(j-1)*h ) )
  w <- rep(w_ref,(m+1))
  Phi <- bspline_matrix(points, m, (q-d), Omega)
  G <- (h/2)*crossprod(Phi, Phi*w)
  
  # Gramian Matrix
  if(d==0){
    Psi <- G
  } else{
    Delta <- diff(diag(K),diff=d)
    Psi <- (1 / (h^(2*d))) * crossprod(Delta,G%*%Delta)
  }
  return(Psi)
  
}

####----------------------------------------------------------------------------------------------------------------
#####  List of Gramian Matrices of each spatial dimension
L2norm_matrix_list <- function(m, q, d, Omega){
  
  P <- length(q)
  Reg_list <- lapply(1:P, function(p) L2norm_matrix(m[p],q[p],d[p],Omega[[p]]) )
  return(Reg_list) 
  
}

#####----------------------------------------------------------------------------------------------------------------
#####  Curvature Penalty
#' @importFrom combinat permn
curvature_penalty <- function(m, q, Omega){
  
  P <- length(m)
  d_mat <- 2*diag(P)
  if(P>1){
    vec <- c(1,1,rep(0,P-2))
    M <- matrix( unlist( unique(combinat::permn(vec)) ), nrow=P, byrow=F )
    d_mat <- cbind(d_mat,M)
  }
  Psi_list <- lapply(1:dim(d_mat)[2], function(p) L2norm_matrix_list(m,q,d_mat[,p],Omega) )
  nreg <- length(Psi_list)
  w_Psi <- sapply(1:nreg, function(j) 2/prod(factorial(d_mat[,j])) )
  for(j in 1:nreg){
    Psi_list[[j]][[1]] <- w_Psi[j]*Psi_list[[j]][[1]]
  }
  return(Psi_list)
  
}
