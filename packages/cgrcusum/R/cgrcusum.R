#' @title Continuous time Generalized Rapid response CUSUM (CGR-CUSUM)
#'
#' @description This function performs the CGR-CUSUM procedure
#' described in ARTICLE UNDER REVIEW FOR PUBLICATION. For detection purposes, it is sufficient
#' to only determine the value of the chart at the times of failure. This can be
#'  achieved by leaving \code{ctimes} empty.
#'
#' @details The CGR-CUSUM can be used to test for a change of unknown positive fixed size \eqn{\theta}{\theta}
#'  in the subject-specific hazard rate from \eqn{h_i(t)}{h_i(t)} to \eqn{h_i(t) e^\theta}{h_i(t) exp(\theta)}
#'  starting from some unknown patient \eqn{\nu}{\nu}. The starting time of the first patient
#'  which had an increase in failure rate as well as the estimated increase in the
#'  hazard rate are also given in the output.
#'  The CGR-CUSUM is determined as:
#' \deqn{\max_{1 \leq \nu \leq n} \left( \hat{\theta}_{\geq \nu}(t) N_{\geq \nu}(t) - \left( \exp\left( \hat{\theta}_{\geq \nu}(t) \right) - 1 \right) \Lambda_{\geq \nu}(t)\right)}{max{1<=\nu<=n} (\theta_{>=\nu}(t)N_{>=\nu}(t)) - (exp(\theta_{>=\nu}(t))-1) \Lambda_{>=\nu}(t))}
#' with  \deqn{N(\geq \nu)(t) = \sum_{i \geq \nu} N_i(t)}{N_{>=\nu}(t) = \sum_{i>=\nu} N_i(t)}
#' with \eqn{N_i(t)}{N_i(t)} the counting process for the failure at time t of subject i
#' and \deqn{\Lambda_{\geq \nu}(t) = \sum_{i \geq \nu} \Lambda_i(t)}{\Lambda_{>=\nu}(t) = \sum_{i>=\nu}\Lambda_i(t)} the
#' with \eqn{\Lambda_i(t)}{\Lambda_i(t)} the cumulative intensity of subject i at time t.
#'
#' @param data \code{data.frame} containing the following named columns:
#' \itemize{
#' \item \code{entrytime} numeric - time of entry into study,
#' \item \code{survtime} numeric - time from entry until event,
#' \item \code{censorid} integer - (optional) censoring indicator (0 = right censored, 1 = observed),
#\item \code{cause} factor - cause of event - competing risks.
#' } and optionally additional covariates used for risk-adjustment.
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
#' @param cmethod One of the following:
#' \itemize{
#' \item "memory2" (default) Matrix formulation of the problem (faster for high volume/long time construction - less RAM than "memory")
#' \item "CPU" Calculates the value of the CGR-CUSUM for every time point from scratch. Recommended for small data volume (lower initialization time).
#' \item "memory" (outdated) Matrix formulation of the problem (faster for high volume/long time construction - may require much RAM)
#' }
#'
#' @return An object of class "cgrcusum" containing:
#' \itemize{
#' \item \code{CGR}: a data.frame with named columns:
#' \itemize{
#' \item $time (time of construction),
#' \item $value (value of the chart at $time),
#' \item $exp_theta_t (value of MLE \eqn{e^{\theta_t}}{e^(\theta_t)}),
#' \item $S_nu (time from which patients are considered for constructing the chart)
#' }
#' \item \code{call}: Contains the \code{call} used to obtain output;
#' \item \code{stopind}: (only if h specified) Boolean indicating whether the chart was stopped by the provided value of h;
#' \item \code{h}: Specified value for the control limit;
#' }There are \code{\link[cgrcusum:plot.cgrcusum]{plot}} and
#'  \code{\link[cgrcusum:runlength.cgrcusum]{runlength}} methods for "cgrcusum" objects.
#'
#' @importFrom stats loess
#' @importFrom stats predict
#' @importFrom utils txtProgressBar
#' @importFrom utils setTxtProgressBar
#' @export
#'
#' @author Daniel Gomon
#' @family qcchart
#' @seealso \code{\link[cgrcusum]{plot.cgrcusum}}, \code{\link[cgrcusum]{runlength.cgrcusum}}
#'
#'
#' @examples
#' require(survival)
#' tdat <- subset(surgerydat, Hosp_num == 1)
#' tcbaseh <- function(t) chaz_exp(t, lambda = 0.01)
#' varsanalysis <- c("age", "sex", "BMI")
#' exprfit <- as.formula(paste("Surv(survtime, censorid) ~" ,paste(varsanalysis, collapse='+')))
#' tcoxmod <- coxph(exprfit, data= surgerydat)
#' #Alternatively, cbaseh can be left empty when specifying coxphmod through coxph()
#' cgr <- cgrcusum(data = tdat, coxphmod = tcoxmod, cbaseh = tcbaseh, pb = TRUE)
#' plot(cgr)






cgrcusum <- function(data, coxphmod, cbaseh, ctimes, h, stoptime,
                     C, pb = FALSE, cmethod = "memory2" ){
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
    message("No censoring mechanism specified. Assuming data is uncensored.")
    data$censorid <- rep(1, nrow(data))
  }
#  compriskcheck <- "cause" %in% colnames(data)
#  if(compriskcheck){
#    message("Competing risks specified.")
#  }
  # determine chronological failure times
  data$otime <- data$entrytime + data$survtime
  if(!missing(C)){
    tempidx <- which(data$otime < data$entrytime + C)
    data[tempidx,]$otime <- data$entrytime + C
    data[tempidx,]$censorid <- rep(length(tempidx), 0)
  }
  # determine the default construction times (all failtimes), if none specified
  if(missing(ctimes)){
    ctimes <- unique(data$otime)
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

  #----------FUNCTION BODY-------------------
  ctimes <- sort(ctimes[which(ctimes <= stoptime)])

  if(cmethod == "CPU"){
    Gt <- matrix(0, nrow =1, ncol = 4)
    startval <- 0
    stopind <- FALSE
    if(pb){
      pb2 <- txtProgressBar(min= 0, max = length(ctimes), style = 3)
    }
    for(j in seq_along(ctimes)){
      if(pb){
        setTxtProgressBar(pb2, value = j)
      }
      temcgr <- cgr_helper(data = data, ctime = ctimes[j],
                           coxphmod = coxphmod, cbaseh = cbaseh)
      Gt <- rbind(Gt, c(ctimes[j], temcgr$val, exp(temcgr$theta), temcgr$starttime))
      if (!missing(h)){if(temcgr$val >= h) {stopind = TRUE; break}}
    }
    colnames(Gt) <- c("time", "value", "exp_theta_t", "S_nu")
    if(pb){
      close(pb2)
    }
    cgr <- list(CGR = Gt,
                stopind = stopind,
                call = call)
  } else if(cmethod == "memory"){
    Gt <- cgr_helper_mat(data = data, ctimes = ctimes, coxphmod = coxphmod, cbaseh = cbaseh, displaypb = pb)
    cgr <- list(CGR = Gt,
                call = call)
  } else if(cmethod == "memory2" & missing(h)){
    Gt <- cgr_helper_mat_2(data = data, ctimes = ctimes, coxphmod = coxphmod, cbaseh = cbaseh, displaypb = pb)
    cgr <- list(CGR = Gt,
                call = call)
  } else if(cmethod == "memory2" & !missing(h)){
    Gt <- cgr_helper_mat_3(data = data, ctimes = ctimes, coxphmod = coxphmod, cbaseh = cbaseh, h=h, displaypb = pb)
    cgr <- list(CGR = Gt$Gt,
                stopind = Gt$stopind,
                call = call)
  }
  if(!missing(h)){cgr$h <- h}
  class(cgr) <- "cgrcusum"
  cgr
}


