#' Computes residential and commercial floorspace supply and equilibrium prices.
#'
#' @param Q Nx1 array - Floorspaces prices.
#' @param K Nx1 array - Land supply.
#' @param w NxS - Wages in each location in each sector.
#' @param L_j Nx1 matrix - Number of workers in each location.
#' @param y_bar - Average income in each location.
#' @param L_i Nx1 matrix - Number of residents in each location.
#' @param beta Float - Cobb-Douglas parameter output elasticity wrt labor.
#' @param alpha Float - Utility parameter that determines preferences for
#'     consumption.
#' @param mu Float - Floorspace prod function: output elast wrt capita, 1-mu wrt land.     
density_development = function(Q,
                               K,
                               w,
                               L_j,
                               y_bar,
                               L_i,
                               beta,
                               alpha,
                               mu){
  
  Q_mean = exp(mean(log(Q)));
  Q_norm = Q/Q_mean;
  FS_f = ((1-beta)/beta)*(array_operator(array_operator(w, L_j, '*'), Q_norm, '/'));
  FS_r = (1-alpha)*(array_operator(array_operator(y_bar, L_i, '*'),Q_norm,'/'));
  FS = FS_f+FS_r;
  varphi = array_operator(FS, K^(1-mu), '/'); 
  return(list(Q_mean = Q_mean, Q_norm = Q_norm, FS_f = FS_f, FS_r = FS_r, FS = FS, varphi=varphi))
}