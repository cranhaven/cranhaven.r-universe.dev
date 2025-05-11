generate_Y_mean.function <- function(true_mean, theta, ..., true_X, n){
  return(f2ftheta(true_mean, true_X, n)(theta))
}
