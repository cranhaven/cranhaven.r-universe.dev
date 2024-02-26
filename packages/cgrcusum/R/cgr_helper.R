#' Continuous time Generalized Rapid response CUSUM (CGR-CUSUM) helper -
#' single time point
#'
#' @description This function calculates the value of the CGR-CUSUM at one
#' specified timepoint
#'
#'
#'
#' @param data data frame containing the following named columns:
#' \code{entrytime} (numeric - time of entry into study), \code{survtime}
#' (numeric - time from entry until event), \code{censorid} (integer - censoring
#' indicator: 0 - right censored, 1 - observed), \code{cause} (factor - cause of
#' event - competing risks).
#' @param ctime construction time (single) at which the value of the chart
#' should be determined.
#' @param coxphmod a cox proportional hazards regression model as produced by
#' the function \code{\link[survival:coxph]{coxph}}. Obtained using:
#' \code{coxph(Surv(survtime, censorid) ~ covariates, data = data)}.
#' Alternatively, a list with $formula (~ covariates)
#' and $coefficients (named vector specifying risk adjustment coefficients
#' for covariates - names must be the same as in $formula and colnames).
#' @param cbaseh a function which returns the non risk-adjusted cumulative
#' baseline hazard \eqn{h_0(t)}. If \code{cbaseh} is missing but
#' \code{coxphmod} has been
#' specified as a survival object, this baseline hazard rate will be determined
#' using the provided \code{coxphmod}.
#' @param displaypb (optional) boolean indicating whether a progress bar should be
#' displayed
#'
#' @return A list containing the following:
#' \itemize{
#'   \item $val value of CGR-CUSUM at specified time point
#'   \item $theta value at corresponding time of the MLE \eqn{\hat{\theta}_t}
#'   \item $starttime time from which individuals contribute to the chart \eqn{S_\nu}
#' }
#'
#' @importFrom utils txtProgressBar
#' @importFrom utils setTxtProgressBar
#' @export
#'
#' @author Daniel Gomon
#'
#' @seealso \code{\link{bkcusum}},
#' \code{\link{bercusum}} (step 2)
#'
#' @examples
#' #TO-DO

cgr_helper <- function(data, ctime, coxphmod, cbaseh,
                       displaypb = FALSE){
  entrytime <- NULL
  #check whether a construction time has been specified, otherwise take max
  if(missing(ctime)){
    ctime <- max(data$otime)
  }
  if(!"censorid" %in% colnames(data)){
    cat("No censoring mechanism specified. Assuming data is uncensored.")
    data$censorid <- rep(1, nrow(data))
  }
  #Calculate subject specific failure risk (exp(\beta Z))
  riskdat <- calc_risk(data, coxphmod)
  #Indices for maximum achieved value, starting time and value of \theta_t
  maxval <- 0
  startidx <- 0
  thetaval <- 0
  #helperstimes - helper starting times. Sorted times of entry of subjects
  helperstimes <- sort(unique(data$entrytime))
  if(displaypb){
    pb <- txtProgressBar(min= 0, max = length(helperstimes), style = 3)
  }
  #i is indicator for considered patients starting time, S_i \geq i
  for(i in seq_along(helperstimes)){
    #display progress bar?
    if(displaypb){
      setTxtProgressBar(pb, value = i)
    }
    #Consider only patients with starting time larger than helperstimes[i]
    tdat <- subset(data, entrytime >= helperstimes[i] & entrytime <= ctime)
    tdatidx <- which(data$entrytime >= helperstimes[i] & data$entrytime <= ctime)
    #Determine amount of (relevant) failures in this subset
    NDT <- length(which(tdat$censorid == 1 & tdat$otime <= ctime))
    #Contribution of active subjects to A(t)
    activecbaseh <- cbaseh(ifelse(tdat$otime < ctime, tdat$otime, ctime)-tdat$entrytime)
    #Error check for empty contribution
    activecbaseh[is.na(activecbaseh)] <- 0
    #Determine A(t) (have to risk-adjust)
    AT <- sum(riskdat[tdatidx] * activecbaseh)
    #Determine \hat{\theta}_t
    thetat <- log(NDT/AT)
    if (is.finite(thetat)){
      thetat <- max(0,thetat)
    } else {thetat <- 0}
    #Determine value of CGI-CUSUM using only patients with S_i > helperstimes[i]
    CGIvalue <- thetat* NDT - (exp(thetat)- 1) * AT
    #Check whether this value is the largest value possible for CGR
    if(CGIvalue > maxval){
      maxval <- CGIvalue
      startidx <- helperstimes[i]
      thetaval <- thetat
    }
  }
  if(displaypb){
    close(pb)
  }
  #return list of relevant values
  return(list(val = maxval,
              theta = thetaval,
              starttime = startidx))
}




