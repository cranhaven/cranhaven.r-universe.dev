#' Continuous time Generalized Rapid response CUSUM (CGR-CUSUM) helper - matrix
#' formulation of the problem - version 2
#'
#' @description This function calculates the value of the CGR-CUSUM using a
#' matrix formulation of the problem - this can require a lot of available RAM.
#'
#' @inheritParams cgrcusum
#' @param data \code{data.frame} containing the following named columns:
#' \itemize{
#' \item \code{entrytime} numeric - time of entry into study,
#' \item \code{otime} numeric - time from entry until event,
#' \item \code{censorid} integer - (optional) censoring indicator (0 = right censored, 1 = observed),
#\item \code{cause} factor - cause of event - competing risks.
#' } and optionally additional covariates used for risk-adjustment.
#' @param displaypb boolean Display a progress bar?
#'
#' @return A matrix with 4 named columns:
#' \itemize{
#'   \item $time time at which the value of the CGR-CUSUM was determined
#'   \item $value value at corresponding time of the CGR-CUSUM
#'   \item $exp_theta_t value at corresponding time of the MLE \eqn{\hat{\theta}_t}{\theta_t}
#'   \item $S_nu time from which individuals contribute to the chart \eqn{S_\nu}{S_\nu}
#' }
#'
#' @importFrom utils txtProgressBar
#' @importFrom utils setTxtProgressBar
#' @export
#'
#' @author Daniel Gomon
#'
#' @seealso \code{\link{cgrcusum}}
#'
#' @examples
#' \dontrun{
#' require(survival)
#' tdat <- subset(surgerydat, Hosp_num == 1)
#' tdat$otime <- tdat$entrytime + tdat$survtime
#' tcbaseh <- function(t) chaz_exp(t, lambda = 0.01)
#' varsanalysis <- c("age", "sex", "BMI")
#' exprfit <- as.formula(paste("Surv(survtime, censorid) ~" ,paste(varsanalysis, collapse='+')))
#' tcoxmod <- coxph(exprfit, data= surgerydat)
#' #Alternatively, cbaseh can be left empty when specifying coxphmod through coxph()
#' cgr2 <- cgr_helper_mat_2(data = tdat, ctimes = unique(tdat$entrytime + tdat$survtime),
#'                          coxphmod = tcoxmod, cbaseh = tcbaseh, displaypb = TRUE)
#' }

cgr_helper_mat_2 <- function(data, ctimes, coxphmod, cbaseh, displaypb = FALSE){
  #Calculate Lambda_i for a single person at times
  Lambdafun <- function(entrytime, otime, times, cbaseh){
    lamval <- sapply(times, FUN = function(x) {if(x >= entrytime){cbaseh(min(x, otime) - entrytime)}else{0}})
  }


  #---------NEW FCT BODY-------------
  #Calculate subject specific failure risk (exp(\beta Z))
  riskdat <- calc_risk(data, coxphmod)
  #ctimes are already pre-sorted in cgrcusum
  #Create matrix containing Lambda_i(t) per patient. Rows represent patient i
  lambdamat <- t(mapply(FUN = Lambdafun, entrytime = data$entrytime, otime = data$otime, MoreArgs = list( times = ctimes, cbaseh = cbaseh)))
  lambdamat <- lambdamat * as.vector(riskdat)
  #Determine times from which to construct the CGR
  helperstimes <- sort(unique(data$entrytime))

  #Function used for maximizing over starting points (patients with starting time >= k)
  maxoverk <- function(helperstime, ctime, ctimes, data, lambdamat){
    if(helperstime <= ctime){
      matsub <- which(data$entrytime >= helperstime & data$entrytime <= ctime)
      AT <- sum(lambdamat[matsub, which(ctimes == ctime)])
      #THIS COULD BE SLOW, OTHERWISE ASSIGN TDAT <- subset(data, matsub)
      NDT <- length(which(data[matsub, ]$censorid == 1 & data[matsub,]$otime <= ctime))
      thetat <- log(NDT/AT)
      if (is.finite(thetat)){
        thetat <- max(0,thetat)
      } else {thetat <- 0}
      #Determine value of CGI-CUSUM using only patients with S_i > helperstimes[i]
      CGIvalue <- thetat* NDT - (exp(thetat)- 1) * AT
      return(c(CGIvalue, thetat))
    } else{
      return(c(0,0))
    }
  }
  #Function to calculate maximum value at one ctime
  if(displaypb){
    pb <- txtProgressBar(min= 0, max = length(ctimes), style = 3)
    maxoverj <- function(y){
      setTxtProgressBar(pb, value = match(y, ctimes))
      a <- sapply(helperstimes, function(x) maxoverk(helperstime = x,  ctime = y, ctimes = ctimes, data = data, lambdamat = lambdamat))
      tidmax <- which.max(a[1,])
      atemp <- a[,tidmax]
      return(c(atemp, tidmax))
    }
  } else{
    maxoverj <- function(y){
      a <- sapply(helperstimes, function(x) maxoverk(helperstime = x,  ctime = y, ctimes = ctimes, data = data, lambdamat = lambdamat))
      tidmax <- which.max(a[1,])
      atemp <- a[,tidmax]
      return(c(atemp, tidmax))
    }
  }

  #Calculate maxoverk for every construction time ctimes
  fin <- sapply(ctimes, maxoverj)
  if(displaypb){
    close(pb)
  }
  Gt <- as.data.frame(t(fin))
  Gt[,2] <- exp(Gt[,2])
  Gt[,3] <- helperstimes[Gt[,3]]
  Gt <- cbind(ctimes, Gt)
  colnames(Gt) <- c("time", "value", "exp_theta_t", "S_nu")

  #return list of relevant values
  return(Gt)
}
