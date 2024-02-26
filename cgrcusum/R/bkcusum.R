#' @title Continuous time BK-CUSUM
#'
#' @description This function performs the BK-CUSUM procedure based on the
#' Biswas & Kalbfleisch (2008) CUSUM. For detection purposes, it is sufficient
#' to only determine the value of the chart at the times of failure. This can be
#'  achieved by leaving \code{ctimes} empty.
#'
#' @references Biswas P. and Kalbfleisch J.D. (2008), A risk-adjusted CUSUM in
#' continuous time based on the Cox Model, \doi{doi:10.1002/sim.3216}
#'
#' @details The BK-CUSUM can be used to test the hypothesis of an instant change
#' of fixed size \eqn{e^\theta}{exp(\theta)}
#' in the subject specific hazard rate from \eqn{h_i(t)}{h_i(t)} to
#' \eqn{h_i(t) e^\theta}{h_i(t) exp(\theta)}. The parameter \code{C} can be used
#' to ignore information provided by subjects C time units after their entry
#' into the study.
#' The BK-CUSUM is constructed as:
#' \deqn{G(t) = \max_{0 \leq k \leq t} \left( \theta N(k,t) - \left( e^\theta -1  \right) \Lambda(k,t)  \right)}{G(t) = max_{0 <= k <= t} (\theta N(k,t) - (e^\theta -1) \Lambda(k,t))}
#' with \eqn{\theta}{\theta} the ln(expected hazard ratio) and \deqn{N(k,t) = N(t) - N(k)}{N(k,t) = N(t)-N(k)}
#' with \eqn{N(t)}{N(t)} the counting process of all failures at time t and \deqn{\Lambda(k,t) = \Lambda(t) - \Lambda(k)}{\Lambda(k,t) = \Lambda(t) - \Lambda(k)} the
#' with \eqn{\Lambda(t)}{\Lambda(t)} the summed cumulative intensity of all subjects at time t.
#'
#' @param data \code{data.frame} containing the following named columns:
#' \itemize{
#' \item \code{entrytime} numeric - time of entry into study,
#' \item \code{survtime} numeric - time from entry until event,
#' \item \code{censorid} integer - (optional) censoring indicator (0 = right censored, 1 = observed),
#\item \code{cause} factor - cause of event - competing risks.
#' } and optionally additional covariates used for risk-adjustment.
#' @param theta expected ln(hazard ratio) \eqn{\theta}{\theta}
#' @param coxphmod (optional) a cox proportional hazards regression model as produced by
#' the function \code{\link[survival:coxph]{coxph()}}. Standard practice: \cr
#' \code{coxph(Surv(survtime, censorid) ~ covariates, data = data)}. \cr
#' Alternatively, a list with:
#' \itemize{
#' \item $formula (~ covariates)
#' \item $coefficients (named vector specifying risk adjustment coefficients
#' for covariates - names must be the same as in $formula and colnames of \code{data}).
#' }
#' @param cbaseh a function which returns the non risk-adjusted cumulative
#' baseline hazard \eqn{H_0(t)}{H_0(t)}. If \code{cbaseh} is missing but
#' \code{coxphmod} has been
#' specified as a survival object, this baseline hazard rate will be determined
#' using the provided \code{coxphmod}.
#' @param ctimes (optional) vector of construction times at which the value of the chart should be
#' determined. When not specified, the chart is constructed at all failure times.
#' @param h (optional) value of the control limit. The chart will only be
#' constructed until the value of the control limit has been reached or
#' surpassed.
#' @param stoptime (optional) time after which the value of the chart should no
#' longer be determined. Default = max(failure time). Useful when ctimes
#' has not been specified.
#' @param C (optional) a numeric value indicating how long after entering the study
#' patients should no longer influence the value of the chart. This is
#' equivalent to right-censoring every observation at time \code{entrytime} + C.
#' @param pb (optional) boolean indicating whether a progress bar should be shown.
#' Default = FALSE
#'
#' @return An object of class "bkcusum" containing:
#' \itemize{
#' \item \code{BK}: list containing
#' \itemize{
#' \item $time (times at which chart is constructed),
#' \item $value (value of the chart at corresponding times),
#' }
#' \item \code{stopind}: indicator for whether the chart was stopped by the control limit
#' \item \code{call}: the call used to obtain output
#' } There are \code{\link[cgrcusum:plot.bkcusum]{plot}} and
#'  \code{\link[cgrcusum:runlength.bkcusum]{runlength}} methods for "bkcusum" objects.
#'
#' @import survival
#' @importFrom stats loess
#' @importFrom stats predict
#' @importFrom utils txtProgressBar
#' @importFrom utils setTxtProgressBar
#' @export
#'
#' @author Daniel Gomon
#' @family qcchart
#'
#' @seealso \code{\link[cgrcusum]{plot.bkcusum}}, \code{\link[cgrcusum]{runlength.bkcusum}}
#'
#'
#' @examples
#' require(survival)
#' tdat <- subset(surgerydat, Hosp_num == 14)
#' tcbaseh <- function(t) chaz_exp(t, lambda = 0.01)
#' varsanalysis <- c("age", "sex", "BMI")
#' exprfit <- as.formula(paste("Surv(survtime, censorid) ~" ,paste(varsanalysis, collapse='+')))
#' tcoxmod <- coxph(exprfit, data= surgerydat)
#' #Alternatively, cbaseh can be left empty when specifying coxphmod through coxph()
#' bk <- bkcusum(data = tdat, theta = log(2), coxphmod = tcoxmod, cbaseh = tcbaseh, pb = TRUE)
#' plot(bk)


bkcusum <- function(data, theta, coxphmod, cbaseh, ctimes, h, stoptime,
                    C, pb = FALSE){
  call = match.call()
  #----------INPUT CHECKS-------------
  # check data.frame and column names
  if(!is.data.frame(data)){
    warning("Data provided is not a data frame, attempting to convert.",
            immediate. = TRUE)
    data <- as.data.frame(data)
  }
  if(!"entrytime" %in% colnames(data)){
    stop("Entry time missing for subjects. Please specify them as named column
        'entrytime' in your data frame.")
  }
  if(!"survtime" %in% colnames(data)){
    stop("Survival time missing for subjects. Please specify them as named
          column 'survtime' in your data frame.")
  }
  if(!"censorid" %in% colnames(data)){
    cat("No censoring mechanism specified. Assuming data is uncensored.")
    data$censorid <- rep(1, nrow(data))
  }
  compriskcheck <- "cause" %in% colnames(data)
  if(compriskcheck){
    cat("Competing risks specified.")
  }
  # determine chronological failure times
  data$otime <- data$entrytime + data$survtime
  if(!missing(C)){
    tempidx <- which(data$otime < data$entrytime + C)
    data[tempidx,]$otime <- data$entrytime + C
    data[tempidx,]$censorid <- rep(length(tempidx), 0)
  }
  # determine the default construction times (all failtimes + entrytimes), if none specified
  if(missing(ctimes)){
    ctimes <- union(unique(data$otime), unique(data$entrytime))
  } else{
    ctimes <- union(ctimes, unique(data$otime))
  }
  if(missing(stoptime)){
    stoptime <- max(data$otime[is.finite(data$otime)])
  }
  checkcbase <- FALSE
  if(missing(coxphmod)){
    coxphmod <- NULL
  } else if(inherits(coxphmod, "coxph")){
    if(missing(cbaseh)){
      checkcbase <- TRUE
      cat("Missing cumulative baseline hazard. Determining using provided Cox PH model.")
      cbasetemp <- basehaz(coxphmod, centered = FALSE)
      cbaselo <- loess(cbasetemp$hazard~cbasetemp$time)
      cbaseh <- function(x) predict(cbaselo, x)
    }
  } else if(is.list(coxphmod)){
    if(all(c("formula", "coefficients") %in% names(coxphmod))){
      checkcoxlist <- TRUE
    } else{
      stop("coxphmod does not contain $formula and/or $coefficients.")
    }
  } else{ stop("coxphmod is not a list or survival object.")}
  if(missing(cbaseh)){
    if(!checkcbase){
      stop("Please specify cbaseh (function) or coxphmod as Survival object.")
    }
  }
  if(missing(theta)){
    stop("Please specify a value for theta (ln(expected hazard ratio)).")
  }


  #----------FUNCTION BODY-------------------
  ctimes <- sort(ctimes[which(ctimes <= stoptime)])
  Gt <- matrix(0, nrow =1, ncol = 2)
  Gt[1,1] <- min(data$entrytime)
  startval <- 0
  stopind <- FALSE
  if(pb){
    pb2 <- txtProgressBar(min= 0, max = length(ctimes), style = 3)
  }
  riskdat <- calc_risk(data = data, coxphmod = coxphmod)
  for(j in seq_along(ctimes)){
    if(pb){
      setTxtProgressBar(pb2, value = j)
    }
    if(j == 1){
      dNDT <- length(which(data$otime <= ctimes[j] & data$censorid == 1))
      Gt[j, 2] <- theta*dNDT
    } else{
      #Determine dUt from ctimes[j-1] to ctimes[j]
      active <- which(data$entrytime < ctimes[j] & data$otime > ctimes[j-1])
      tdat <- data[active,]
      #Determine amount of (relevant) failures in this subset
      dNDT <- length(which(tdat$otime <= ctimes[j] & tdat$censorid == 1 ))
      #Contribution of active subjects to A(t)
      activecbaseh <- cbaseh(ifelse(tdat$otime < ctimes[j], tdat$otime, ctimes[j])-ctimes[j-1])
      #Error check for empty contribution
      activecbaseh[is.na(activecbaseh)] <- 0
      #Determine dA(t) (have to risk-adjust)
      dAT <- sum(riskdat[active] * activecbaseh)
      #dUt <- theta * dNDT - (exp(theta) - 1) * dAT
      #Determine cusum value and rbind to previous values
      newval <- max(0, Gt[j-1,2] - (exp(theta)-1)* dAT)
      newval <- newval + theta*dNDT
      Gt <- rbind(Gt, c(ctimes[j], newval))
    }
    if (!missing(h)){if(Gt[j,2] >= h) {stopind = TRUE; break}}
  }
  colnames(Gt) <- c("time", "value")
  if(pb){
    close(pb2)
  }
  Gt <- as.data.frame(Gt)
  bkcus <- list(BK = Gt,
              stopind = stopind,
              call = call)
  if(!missing(h)){bkcus$h <- h}
  class(bkcus) <- "bkcusum"
  bkcus
}
