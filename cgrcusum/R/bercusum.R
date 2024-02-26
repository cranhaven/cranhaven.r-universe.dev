#' @title Risk-adjusted Bernoulli CUSUM
#'
#' @description This function can be used to construct a risk-adjusted Bernoulli
#' CUSUM chart on survival data.
#' Specify one of the following combinations for the parameters:
#' \itemize{
#' \item glmmod + theta
#' \item p0 + theta
#' \item p0 + p1
#' }
#'
#' @details The Bernoulli CUSUM chart is given by:
#' \deqn{S_n = \max(0, S_{n-1} + W_n)}{S_n = max(0, S_{n-1} + W_n)} with
#' \deqn{W_n = X_n \ln \left( \frac{p_1 (1-p_0)}{p_0(1-p_1)}  \right) + \ln \left( \frac{1-p_1}{1-p_0} \right)}{W_n = X_n ln((p_1 * (1-p_0))/(p_0 * (1-p_1))) + ln((1-p_1)/(1-p_0))}
#' where X_n is the outcome of the n-th (chronological) subject in the data.
#' Instead of displaying patient numbering on the x-axis, the time of outcome is displayed.
#'
#' @param data \code{data.frame} containing the following named columns:
#'  \itemize{
#' \item \code{entrytime} numeric - time of entry into study,
#' \item \code{survtime} numeric - time from entry until event,
#' \item \code{censorid} integer - (optional) censoring indicator (0 = right censored, 1 = observed),
# \item \code{cause} factor - cause of event - competing risks
#' }
#' @param followup The followup time for every individual. At what time after entry do we consider the outcome?
#' @param glmmod Generalized linear regression model used for risk-adjustment as produced by
#' the function \code{\link[stats:glm]{glm}}. Standard practice: \cr
#' \code{glm(as.formula(paste("(survtime <= followup) & (censorid == 1)~" ,paste(covariates, collapse='+'))), data = data)}. \cr
#' Alternatively, a list containing:
#' \itemize{
#' \item $formula (~ covariates),
#' \item $coefficients (named vector specifying risk adjustment coefficients
#' for covariates - names must be the same as in $formula and colnames of \code{data}).
#' }
#' @param theta \eqn{e^\theta}{e^\theta} is the odds ratio under the alternative hypothesis.
#' Note that: \deqn{p_1 = \frac{p_0 e^\theta}{(1-p_0) (1+p_0 e^\theta)}}{p1 = (p0 * e^\theta)/((1-p0) * (1+p0 e^\theta))}
#' @param p0 The baseline failure probability at entrytime + followup for individuals.
#' @param p1 The alternative hypothesis failure probability at entrytime + followup for individuals.
#' @param h (optional) Control limit to be used for the procedure
#' @param stoptime (optional) Time after which the value of the chart should no longer be determined
#'
#' @return An object of class \code{bercusum} containing:
#' \itemize{
#' \item \code{CUSUM}: A \code{data.frame} containing:
#' \itemize{
#' \item $time (times at which chart is constructed),
#' \item $value (value of the chart at corresponding times),
#' \item $numobs (number of observations at corresponding times)
#' }
#' \item \code{call}: the call used to obtain output
#' \item \code{glmmod}: glm coefficients used for risk-adjustment, if specified
#' \item \code{stopind}: indicator for whether the chart was stopped by the control limit
#' } There are \code{\link[cgrcusum:plot.bercusum]{plot}} and
#'  \code{\link[cgrcusum:runlength.bercusum]{runlength}} methods for "bercusum" objects.

#'
#' @importFrom stats predict.glm
#' @export
#'
#' @author Daniel Gomon
#' @family qcchart
#'
#' @seealso \code{\link[cgrcusum]{plot.bercusum}}, \code{\link[cgrcusum]{runlength.bercusum}}
#'
#'
#' @examples
#' varsanalysis <- c("age", "sex", "BMI")
#' exprfitber <- as.formula(paste("(entrytime <= 365) & (censorid == 1)~",
#'  paste(varsanalysis, collapse='+')))
#' surgerydat$instance <- surgerydat$Hosp_num
#' glmmodber <- glm(exprfitber, data = surgerydat, family = binomial(link = "logit"))
#' bercus <- bercusum(data = subset(surgerydat, Hosp_num == 14), glmmod = glmmodber,
#'  followup = 100, theta = log(2))
#' plot(bercus)




bercusum <- function(data, followup, glmmod, theta, p0, p1, h, stoptime){
  entrytime <- otime <- NULL
  call <- match.call()
  #exp(theta) is the Odds Ratio under the alternative hypothesis
  #Supply either of the following combinations:
  #1. glmmodel + theta  2. p0 + theta   3. p0 + p1
  #Relationship between p1 and theta: p1 = (p0*exp(theta))/((1-p0)*(1+p0*exp(theta)))
  #First perform a logistic regression model on in-control data to obtain RA probs
  #dat must contain entrytime, survtime and the covariates to RA on
  #glmmodel must be either of class glm or contain $formula and $coefficients
  #stoptime is the time until which the CUSUM chart should be constructed
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
    warning("No censoring mechanism specified. Assuming data is uncensored.")
    data$censorid <- rep(1, nrow(data))
  }
#  compriskcheck <- "cause" %in% colnames(data)
#  if(compriskcheck){
#    message("Competing risks specified.")
#  }

  if(!missing(stoptime)){
    data <- subset(data, entrytime + followup <= stoptime)
  }
  stopind = FALSE
  hnull <- missing(h)
  data <- data[order(data$entrytime),]
  data$outcome <- as.integer((data$survtime <= followup) & (data$censorid == 1))
  data$otime <- data$entrytime + followup
  Gt <- data.frame(time = double(), value = double(), numobs = double())
  Gtval <- 0
  j <- 1
  numobs <- 0
  if(!missing(p1)){
    OR = (p1*(1-p0))/(p0*(1-p1))
  }
  for(i in unique(data$otime)){
    tempdata <- subset(data, otime == i)
    numobs <- numobs + nrow(tempdata)
    if(!missing(glmmod)){
      if(inherits(glmmod, "glm")){
        tempprobs <-  predict(glmmod, newdata = tempdata, type = "response")
      } else{
        mmatrix <- model.matrix(glmmod$formula, tempdata)
        coeffs <- glmmod$coefficients[colnames(mmatrix)]
        tempprobs <- c(1/(1 + exp(-mmatrix %*% coeffs)))
      }
      tempsecondval <- sum(log(1/(1-tempprobs + exp(theta)*tempprobs)))
    }else if(!missing(p0)){
      if(!missing(theta)){
        tempsecondval <- log((1/(1-p0 + exp(theta)*p0))^(nrow(tempdata)))
      } else if(!missing(p1)){
        tempsecondval <- log(((1-p1)/(1-p0))^(nrow(tempdata)))
      }
    } else{ stop("Please supply a value of theta or p1 or a glmmod")}
    if(!missing(theta)){
      Wn <- sum(tempdata$outcome)*theta +  tempsecondval
    } else{
      Wn <- sum(tempdata$outcome)*log(OR) +  tempsecondval
    }
    Gtval <- max(0, Gtval + Wn)
    Gt <- rbind(Gt, c(i, Gtval, numobs))
    if(!hnull){if(Gtval >= h){Ber <- list(CUSUM = Gt,
                                          call = call,
                                          h = h,
                                          stopind = TRUE)}
      if(!missing(glmmod)){
        Ber$glmmod <- glmmod$coefficients
      }
      class(Ber) <- "bercusum"
      return(Ber)
      }
    j <- j+1
  }
  colnames(Gt) = c("time", "value", "numobs")
  Ber <- list(CUSUM = Gt,
              call = call,
              stopind = stopind)
  if(!missing(glmmod)){
    Ber$glmmod <- glmmod$coefficients
  }
  class(Ber) <- "bercusum"
  Ber
}

