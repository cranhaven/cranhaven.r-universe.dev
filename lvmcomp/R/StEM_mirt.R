#' Stochastic EM algorithm for solving multivariate item response theory model
#' 
#' @param response N by J matrix containing 0/1 responses, where N is the number of respondents and J is the number of items.
#' @param Q J by K matrix containing 0/1 entries, where J is the number of items and K is the number of latent traits. Each entry indicates whether an item measures a certain latent trait.
#' @param A0 J by K matrix, the initial value of loading matrix, satisfying the constraints given by Q.
#' @param d0 Length J vector, the initial value of intercept parameters.
#' @param theta0 N by K matrix, the initial value of latent traits for each respondent.
#' @param sigma0 K by K matrix, the initial value of correlations among the latent traits.
#' @param m The length of Markov chain window for choosing burn-in size with a default value 200.
#' @param TT The batch size with a default value 20.
#' @param max_attempt The maximum number of batches before stopping.
#' @param tol The tolerance of geweke statistic used for determining burn-in size with a default value 1.5.
#' @param precision The precision value for determining the stopping of the algorithm with a default value 1e-2.
#' @param parallel Whether or not enable the parallel computing with a default value FALSE.
#' 
#' @return The function returns a list with the following components:
#' \describe{
#'   \item{A_hat}{The estimated loading matrix.}
#'   \item{d_hat}{The estimated value of intercept parameters.}
#'   \item{sigma_hat}{The estimated value of correlation matrix of latent traits.}
# #'   \item{theta0}{N by J matrix, the estimated posterior mean of latent traits.}
#'   \item{burn_in_T}{The length of burn-in size.}
#' }
#' 
#' @references 
#' Zhang, S., Chen, Y. and Liu, Y. (2018). An Improved Stochastic EM Algorithm for Large-Scale Full-information Item Factor Analysis. \emph{British Journal of Mathematical and Statistical Psychology}. To appear.
#' D.C. Liu and J. Nocedal. On the Limited Memory Method for Large Scale Optimization (1989), Mathematical Programming B, 45, 3, pp. 503-528.
#' 
#' @examples
#' # run a toy example based on the M2PL model
#' 
#' # load a simulated dataset
#' attach(data_sim_mirt)
#' 
#' # generate starting values for the algorithm
#' A0 <- Q
#' d0 <- rep(0, J)
#' theta0 <- matrix(rnorm(N*K, 0, 1),N)
#' sigma0 <- diag(1, K) 
#' 
#' # do the confirmatory MIRT analysis
#' # to enable multicore processing, set parallel = T
#' mirt_res <- StEM_mirt(response, Q, A0, d0, theta0, sigma0)
#' 
#' @importFrom coda geweke.diag mcmc
#' @importFrom stats sd cor
#' @export StEM_mirt
StEM_mirt <- function(response, Q, A0, d0, theta0, sigma0, m = 200, TT = 20, max_attempt = 40,
                           tol = 1.5, precision = 0.01, parallel=FALSE){
  N <- nrow(response)
  K <- ncol(A0)
  J <- nrow(A0)
  if(!all(response %in% c(0,1)))
    stop("response data should only contain 0/1! \n")
  if(!all(Q %in% c(0,1)))
    stop("input Q should only contains 0/1 (True/False)! \n")
  if(ncol(response)!=J || nrow(Q)!=J || ncol(Q)!=K || nrow(theta0)!=N ||
     ncol(theta0)!=K || nrow(sigma0)!=K || ncol(sigma0)!=K)
    stop("The input argument dimension is incorrect. Please check!\n")
  burn_in_T <- 0 
  temp <- stem_mirtc(response, Q, A0, d0, theta0, sigma0, m, parallel=parallel)
  full_window <- result.window <- temp$res
  flag1 <- FALSE
  flag2 <- FALSE
  seg_all <- m / TT
  for(attempt_i in 1:max_attempt){
    cat("attemp: ",attempt_i,", ")
    if(!flag1){
      z.value <- abs(geweke.diag(mcmc(result.window), frac1 = 0.2)$z)
      z.value <- (sum(z.value[1:(K^2)]^2, na.rm = T)/2 + sum(z.value[(K^2+1):length(z.value)]^2, na.rm = T)) / ((K^2-K)/2+sum(Q) + J)
      cat("Geweke stats: ", z.value,"\n")
      if(z.value < tol && attempt_i > 1){
        flag1 <- TRUE
        cat("Burn in finished.\n")
      }
      else{
        A0 <- temp$A0
        d0 <- temp$d0
        theta0 <- temp$theta0
        sigma0 <- temp$sigma0
        temp <- stem_mirtc(response, Q, A0, d0, theta0, sigma0, TT, parallel=parallel)
        result.window <- rbind(result.window[(TT+1):m,], temp$res)
        full_window <- rbind(full_window, temp$res)
        burn_in_T <- burn_in_T + 1
      }
    }
    if(flag1 && !flag2){
      res.sd <- NULL
      for(seg_i in 1:seg_all){
        res.sd <- rbind(res.sd,colMeans(result.window[((seg_i-1)*TT+1):(seg_i*TT),]))
      }
      prop.precision <- apply(res.sd, 2, sd) / sqrt(seg_all)
      cat("max sd: ", max(prop.precision), "\n")
      if(max(prop.precision) < precision*50/sqrt(N)){
        flag2 <- TRUE
        cat("Precision reached!\n")
      }
      else{
        seg_all <- seg_all + 1
        A0 <- temp$A0
        d0 <- temp$d0
        theta0 <- temp$theta0
        sigma0 <- temp$sigma0
        temp <- stem_mirtc(response, Q, A0, d0, theta0, sigma0, TT, parallel=parallel)
        result.window <- rbind(result.window, temp$res)
        full_window <- rbind(full_window, temp$res)
      }
    }
    if(flag1 && flag2){
      res_tmp <- colMeans(full_window)
      sigma_hat <- matrix(res_tmp[1:(K^2)], K)
      A_hat <- matrix(res_tmp[(K^2+1):(K^2+J*K)], J)
      d_hat <- matrix(res_tmp[(K^2+J*K+1):(K^2+J*K+J)])
      return(list("A_hat"= A_hat,
                  "d_hat"= d_hat,
                  "sigma_hat" = sigma_hat,
                  "burn_in_T" = burn_in_T+1))
    }
  }
  cat("Warning! The iteration limit has been reached. The result may not be valid. \n")
  res_tmp <- colMeans(full_window)
  sigma_hat <- matrix(res_tmp[1:(K^2)], K)
  A_hat <- matrix(res_tmp[(K^2+1):(K^2+J*K)], J)
  d_hat <- matrix(res_tmp[(K^2+J*K+1):(K^2+J*K+J)])
  return(list("A_hat"= A_hat,
              "d_hat"= d_hat,
              "sigma_hat" = sigma_hat,
              "burn_in_T" = burn_in_T+1))
}