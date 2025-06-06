#' @title 
#' Inverse probability of censoring weights
#'
#' @description Computes inverse probability of censoring weights.
#' 
#'
#' @export
#' @importFrom survival survfit Surv
#' @param y numerical vector with right-censored follow-up times
#' @param delta numerical vector, same length as y, 1 indicates an event while 0 indicates censoring
#' 
#' @details 
#' Inverse probability of censoring weights are calculated by dividing the event indicator by the Kaplan-Meier estimator of the censoring time.
#' This leads to zero weights for censored observations, while every uncensored event receives a weight larger than 1, representing several censored observations.
#' In the redistribute-to-the-right approach, the last observation always receives a positive weight such that no weight will be lost.
#' Further details can be found in Seipp et al. (2021).
#' 
#' @returns A data frame with 2 coloumns. The first column consists of usual inverse probability of censoring weights. For the second column, IPC weights modified in a redistribute-to-the-right approach are given.
#' 
#' @references Seipp, A., Uslar, V., Weyhe, D., Timmer, A., & Otto-Sobotka, F. (2021). Weighted expectile regression for right-censored data. Statistics in Medicine, 40(25), 5501-5520.
#' 
#' @examples 
#' data(colcancer)
#' kw <- weightsKM(colcancer$logfollowup, colcancer$death)


weightsKM <-
function(y, delta){
  
  if(!is.numeric(y) || !is.numeric(delta)) stop("y and delta have to be numeric")
  if(length(y) != length(delta)) stop("y and delta have to be the same length")
  if(!all(delta == 1 | delta == 0)) stop("values of delta have to be 0 or 1")
  
  km <- survfit(Surv(y, 1 - delta) ~ 1,  type="kaplan-meier")
  Wsurv <- km$surv[match(y, km$time)]
  
  # if match does not work because of runding errors
  if(sum(is.na(Wsurv)) > 0){
    
    na <- which(is.na(Wsurv))
    
    KMsurv <- c(1, km$surv)
    KMtime <- c(min(y), km$time)
    Wsurv.na <- sapply(1:length(na), function(i) KMsurv[which.min(c(KMtime <= y[na[i]], FALSE)) - 1])
    Wsurv[na] <- Wsurv.na
    
  }
  
  # weightsIPC - standard case
  # if last element is censored
  Wsurv_Null <- Wsurv
  Wsurv_Null[Wsurv_Null == 0] <- 1
  weightsIPC <- delta / Wsurv_Null
  
  # weightsIPC_RR - last observation is definitely uncensored
  second_highest <- max(y[y != max(y)])
  Wsurv_uncensored <- Wsurv
  Wsurv_uncensored[y == max(y) & delta == 0] <- Wsurv_uncensored[y == second_highest][1]
  delta_uncensored <- delta
  delta_uncensored[y == max(y)] <- 1
  weightsIPC_RR <- delta_uncensored / Wsurv_uncensored

  data.frame(weightsIPC, weightsIPC_RR)
}
