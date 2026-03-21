#' @title Probability Core for Multi-state RDT with Multiple Periods and Criteria for Separate Periods
#'
#' @description Define the summed core function inside of the integration which gets the probability of passing the test given specific failure probabilities.
#' The maximum allowable failures for each separate period need to be satisfied to pass the test (for Multi-state RDT, Multiple Periods, Scenario II).
#'
#' @param n RDT sample size
#' @param cvec Maximum allowable failures for each separate period
#' @param pivec Failure probability for each seperate period
#' @return Core probability of passing the test given specific failure probabilities
#' @examples
#' #Example for two periods
#' pi <- pi_MCSim_dirichlet(M = 1000, seed = 10, par = c(1, 1, 1))
#' MPSep_core(n = 10, cvec = c(1, 1), pivec = pi[1, ]);
#' #The function also works for more than two periods, however, may increase the computation cost.
#' #Example for three periods
#' pi <- pi_MCSim_dirichlet(M = 1000, seed = 10, par = c(1, 1, 1, 1))
#' MPSep_core(n = 10, cvec = c(1, 1, 1), pivec = pi[1, ]);
#' @export
#' @importFrom stats dmultinom



######define the inside sum core of integration fucntion
MPSep_core<-function(n, cvec, pivec){
  if(n <= sum(cvec)){
    return(0)
  }
  else {
    if(length(cvec) == 1){
      return(sum(apply(cbind(seq(0, cvec, 1), n - seq(0, cvec, 1)), 1, dmultinom, prob = pivec)))
    }
    else if(length(cvec) == 2){
      sum2 <- rep(NA, length(0:(cvec[2])))
      if(cvec[1] == 0 & cvec[2] == 0){
        return(dmultinom(c(cvec[1], cvec[2], n - cvec[1] - cvec[2]), prob = pivec))
      }
      else
        if(cvec[1] == 0 & cvec[2] != 0){
          for(j in 0:(cvec[2])){
            sum2[j+1] <- dmultinom(c(cvec[1], j, n - cvec[1] - j), prob = pivec)
          }
          return(sum(sum2))
        }
      else
        if(cvec[1] != 0){
          sum1 <- rep(NA, length(0:(cvec[1])))
          for (i in 0:cvec[1]){
            sum2 <- rep(NA, length(0:(cvec[2])))
            for(j in 0:(cvec[2])){
              sum2[j+1] <- dmultinom(c(i, j, n - i - j), prob = pivec)
            }
            sum1[i+1] <- sum(sum2)
          }
          return(sum(sum1))
        }
    }
    else{
      tmp <- expand.grid(lapply(cvec, seq, from = 0))
      return(sum(apply(cbind(tmp, n - rowSums(tmp)), 1, dmultinom, prob = pivec)))
    }
  }

}
