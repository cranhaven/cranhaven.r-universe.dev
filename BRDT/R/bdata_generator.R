#' @title Data Generation Function for Binomial RDT Design
#'
#' @description  Define the function to generate the dataset based on the design settings (for Binomial RDT).
#'
#' @param Cf Fixed costs of RDT
#' @param Cv Variable unit costs of RDT
#' @param nvec Vector of test sample size
#' @param G Reliabilty growth cost
#' @param Cw Average cost per warranty claim
#' @param N Sales volume
#' @param Rvec Vector of lower level reliability requirements
#' @param cvec Vector of maximum allowable failures
#' @param pi Failure probability
#' @param par Specify which columns to return. Default is all columns.The columns include c('n', 'R', 'c', 'CR', 'AP',
#' 'RDT Cost', 'RG Cost', 'RG Cost Expected',
#' 'WS Cost', 'WS Failure Probability', 'WS Cost Expected',  'Overall Cost')
#' @param option Options to get different datasets. Default is 'optimal'.
#' If option = 'all', get all test plans data for all combinations of n, c, R;
#' If option = 'optimal', get test plans data with optimal test sample size for every combination of c, R.
#' @param thres_CR Threshold (acceptable level) of consumer's risk
#' @return Matrix of the dataset
#' @export
#' @importFrom stats pbinom
#' @examples
#' \donttest{
#' nvec <- seq(0, 10, 1)
#' Rvec <- seq(0.8, 0.85, 0.01)
#' cvec <- seq(0, 2, 1)
#' pi <- pi_MCSim_beta(M = 5000, seed = 10, a = 1, b = 1)
#' bdata_generator(Cf = 10, Cv = 10, nvec = nvec, G = 10000, Cw = 10,
#' N = 100, Rvec = Rvec, cvec = cvec, pi = pi,
#' par = c('n', 'R', 'c', 'CR', 'AP'), option = c("optimal"), thres_CR = 0.05)
#' }
#' @seealso
#' \code{\link{boptimal_cost}} for getting the optial test plan with minimum overall cost;
#' \code{\link{boptimal_n}} for getting the optial test sample size;


bdata_generator <- function(Cf, Cv, nvec, G, Cw, N, Rvec, cvec, pi,
                            par = all(), option = c('optimal'), thres_CR){
  #To get all test plans data for different combinations of n, c, R
  if (option == c('all')){
    #exapnd all possible combinations of n, c, R
    n_R_c_vecs <- expand.grid(nvec, Rvec, cvec)
    colnames(n_R_c_vecs) <- c('n', 'R', 'c')

    #create empty vectors
    emptyvec <- rep(NA, length(1:dim(n_R_c_vecs)[1]))
    CRvec <- emptyvec
    APvec <- emptyvec
    RDTcostvec <- emptyvec
    RGcostvec_expected <- emptyvec
    WScostvec <- emptyvec
    WScostvec_failureprob <- emptyvec
    WScostvec_expected <- emptyvec
    costvec <- emptyvec

    for(i in 1:dim(n_R_c_vecs)[1]){
      n <- n_R_c_vecs[i, 'n']
      R <- n_R_c_vecs[i, 'R']
      c <- n_R_c_vecs[i, 'c']
      tmp <- pbinom(c, n, pi)
      #CR
      CRvec[i] <- 1 - t(matrix(tmp)) %*% matrix(sapply(pi, bIndicator, R)) / sum(tmp)
      #AP
      APvec[i] <- sum(tmp) / length(tmp)
      #RDT cost
      RDTcostvec[i] <- Cf + Cv * n
      #Expected RG cost
      RGcostvec_expected[i] <- G * (1 - APvec[i])
      #Expected WS cost
      WScostvec_failureprob[i] <- sum(tmp * pi) / sum(tmp)
      WScostvec[i] <- Cw * N * WScostvec_failureprob[i]
      WScostvec_expected[i] <- WScostvec[i] * APvec[i]
      #
      costvec[i] <- RDTcostvec[i] + RGcostvec_expected[i] + WScostvec_expected[i]
    }

    result <- cbind(n_R_c_vecs, CRvec, APvec, RDTcostvec, G, RGcostvec_expected,
                    WScostvec, WScostvec_failureprob, WScostvec_expected, costvec)
    colnames(result) <- c('n', 'R', 'c', 'CR', 'AP',
                          'RDT Cost', 'RG Cost', 'RG Cost Expected',
                          'WS Cost', 'WS Failure Probability', 'WS Cost Expected',  'Overall Cost')
    return(result[ , par])
  }
  #To get all test plans data for different combinations of n, c, R
  else if (option == c('optimal')){
    #exapnd all possible combinations of n, c, R
    R_c_vecs <- expand.grid(Rvec, cvec)
    colnames(R_c_vecs) <- c('R', 'c')

    #create empty vectors
    emptyvec <- rep(NA, length(1:dim(R_c_vecs)[1]))
    CRvec <- emptyvec
    APvec <- emptyvec
    RDTcostvec <- emptyvec
    RGcostvec_expected <- emptyvec
    WScostvec <- emptyvec
    WScostvec_failureprob <- emptyvec
    WScostvec_expected <- emptyvec
    costvec <- emptyvec
    nvec_optimal <- emptyvec
    for(i in 1:dim(R_c_vecs)[1]){
      R <- R_c_vecs[i, 'R']
      c <- R_c_vecs[i, 'c']

      #obtain the optimal n
      nvec_optimal[i] <- boptimal_n(c, pi, R, thres_CR)
      n <- nvec_optimal[i]

      tmp <- pbinom(c, n, pi)
      #CR
      CRvec[i] <- 1 - t(matrix(tmp)) %*% matrix(sapply(pi, bIndicator, R)) / sum(tmp)
      #AP
      APvec[i] <- sum(tmp) / length(tmp)
      #RDT cost
      RDTcostvec[i] <- Cf + Cv * n
      #Expected RG cost
      RGcostvec_expected[i] <- G * (1 - APvec[i])
      #Expected WS cost
      WScostvec_failureprob[i] <- sum(tmp * pi) / sum(tmp)
      WScostvec[i] <- Cw * N * WScostvec_failureprob[i]
      WScostvec_expected[i] <- WScostvec[i] * APvec[i]
      #
      costvec[i] <- RDTcostvec[i] + RGcostvec_expected[i] + WScostvec_expected[i]
    }

    result <- cbind(nvec_optimal, R_c_vecs, CRvec, APvec, RDTcostvec, G, RGcostvec_expected,
                    WScostvec, WScostvec_failureprob, WScostvec_expected, costvec)
    colnames(result) <- c('n', 'R', 'c', 'CR', 'AP',
                          'RDT Cost', 'RG Cost', 'RG Cost Expected',
                          'WS Cost', 'WS Failure Probability', 'WS Cost Expected',  'Overall Cost')
    return(result[ , par])
  }
}


















