#' Function to invert model, so amenities, wages, productivities, and development density
#'  are chosen to match model to data.
#'
#' @param N Integer - Number of locations.
#' @param L_i Nx1 matrix - Number of residents in each location.
#' @param L_j Nx1 matrix - Number of workers in each location. 
#' @param Q Nx1 matrix - Floorspace prices
#' @param K Nx1 matrix - Land area
#' @param t_ij NxN matrix - Travel times across all possible locations.
#' @param alpha Float - Utility parameter that determines preferences for
#'     consumption.
#' @param beta Float - Output elasticity wrt labor
#' @param theta Float - Commuting elasticity and migration elasticity.
#' @param delta Float - Decay parameter agglomeration
#' @param rho Float - Decay parameter congestion
#' @param lambda Float - Agglomeration force
#' @param epsilon Float - Parameter that transforms travel times to commuting costs
#' @param mu Float - Floorspace prod function: output elast wrt capital, 1-mu wrt land.     
#' @param eta Float - Congestion force
#' @param nu_init Float - Convergence parameter to update wages.
#'     Default nu=0.01.
#' @param tol Int - tolerance factor
#' @param maxiter Integer - Maximum number of iterations for convergence.
#'     Default maxiter=1000.
#'
#' @return Equilibrium values.
#' @export
#'
#' @examples
#' N=5
#' L_i = c(63, 261, 213, 182, 113)
#' L_j = c(86, 278, 189, 180, 99)
#' Q = c(2123, 1576, 1371, 1931, 1637)
#' K = c(0.44, 1.45, 1.15, 0.87, 0.58)
#' t_ij = rbind(c(0.0, 6.6, 5.5, 5.6, 6.4),
#'              c(6.7, 0.0, 3.9, 4.6, 4.4),
#'              c(5.5, 3.9, 0.0, 2.8, 3.0),
#'              c(5.6, 4.6, 2.8, 0.0, 2.7),
#'              c(6.4, 4.4, 3.0, 2.7, 0.0))
#' 
#' inversionModel(N=N,
#'                L_i=L_i,
#'                L_j=L_j,
#'                Q=Q,
#'                K=K,
#'                t_ij=t_ij)
#'                
inversionModel = function(N,
                          L_i,
                          L_j,
                          Q,
                          K,
                          t_ij,
                          alpha=0.7,
                          beta=0.7,
                          theta=7,
                          delta=0.3585,
                          rho=0.9094,
                          lambda=0.01,
                          epsilon=0.01,
                          mu=0.3,
                          eta=0.1548,
                          nu_init=0.005,
                          tol=10^-10,
                          maxiter=1000){
  
  # Formatting of input data
  if(is.data.frame(L_i)){
    L_i = array(unlist(L_i), dim(L_i))
  } else if(is.null(dim(L_i))){
    L_i = array(L_i, dim=c(N,1))
  }
  
  if(is.data.frame(L_j)){
    L_j = array(unlist(L_j), dim(L_j))
  } else if(is.null(dim(L_j))){
    L_j = array(L_j, dim=c(N,1))
  }
  if(is.data.frame(K)){
    K = array(unlist(K), dim(K))  
  } else if(is.null(dim(K))){
    K = array(K, dim=c(N,1))
  }
  if(is.data.frame(Q)){
    Q = array(unlist(Q), dim(Q))
  } else if(is.null(dim(Q))){
    Q = array(Q, dim=c(N,1))
  }
  t_ij = array(unlist(t_ij), dim(t_ij))  
  
  # Normalize L_i to have the same size as L_j
  L_i=L_i*sum(L_j)/sum(L_i)
  
  # Initialization
  w_init=array(1, dim=c(N,1))
  
  # Transformation of travel times to trade costs
  D = commuting_matrix(t_ij=t_ij, 
                       epsilon=epsilon)
  tau = D$tau
  
  # Finding the wages that match the data
  WI = wages_inversion(N=N,
                       w_init=w_init,
                       theta=theta,
                       tau=tau,
                       L_i=L_i,
                       L_j=L_j,
                       nu_init=nu_init,
                       tol=tol,
                       maxiter=maxiter)
  
  # Equilibrium wages
  w = WI$w
  w_tr = WI$w_tr
  W_i = WI$W_i
  lambda_ij_i = WI$lambda_ij_i
  
  # Average income
  Inc = av_income_simple(lambda_ij_i=lambda_ij_i,
                         w_tr = w_tr
  )
  y_bar = Inc$y_bar
  
  
  #Density of development
  DensD = density_development(Q=Q,
                              K=K,
                              w=w,
                              L_j=L_j,
                              y_bar=y_bar,
                              L_i=L_i,
                              beta=beta,
                              alpha=alpha,
                              mu=mu
  )
  Q_mean = DensD$Q_mean
  Q_norm = DensD$Q_norm
  FS_f = DensD$FS_f
  FS_r = DensD$FS_r
  FS = DensD$FS
  varphi = DensD$varphi
  ttheta = FS_f/FS
  
  #Productivities
  Prod = productivity(N=N,
                      Q=Q,
                      w=w,
                      L_j=L_j,
                      K=K,
                      t_ij = t_ij,
                      delta=delta,
                      lambda=lambda,
                      beta=beta
  )
  A = Prod$A
  a = Prod$a
  
  # Amenities
  AM = living_amenities_simple(theta=theta,
                               N=N,
                               L_i=L_i,
                               W_i=W_i,
                               Q=Q,
                               K=K,
                               alpha=alpha,
                               t_ij=t_ij,
                               rho=rho,
                               eta=eta
  )
  
  B = AM$B
  b = AM$b  
  
  # Save and export
  Q_alpha = Q_norm^(1-alpha)
  u = array_operator(array_operator(W_i,Q_alpha,'/'),B,'*')
  U = (sumDims(u,1))^(1/theta)
  
  return(list(A=A, a=a, u=u, B=B, b=b, w=w, varphi=varphi, U=U, Q_norm=Q_norm, ttheta=ttheta))
}