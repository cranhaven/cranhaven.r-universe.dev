#' Risk-adjusted funnel plot
#'
#' @description This function can be used to construct a risk-adjusted funnel plot.
#'
#'
#' @param data \code{data.frame} containing the following named columns:
#' \itemize{
#' \item \code{entrytime} numeric - time of entry into study,
#' \item \code{survtime} numeric - time from entry until event,
#' \item \code{censorid} integer - (optional) censoring indicator (0 = right censored, 1 = observed),
# \item \code{cause} (factor - cause of event - competing risks)
#' \item \code{instance} integer or character - indicating which instance (f.e. hospital) the observation belongs to
#' } and optionally additional covariates used for risk-adjustment.
#' @param ctime construction time at which the funnel plot
#' should be determined. Constructed over whole data when not specified
#' @param p0 The baseline failure probability at entrytime + followup for individuals.
#' If not specified, average failure proportion over whole data is used instead.
#' @param glmmod a generalized linear regression model as produced by
#' the function \code{\link[stats:glm]{glm()}}. Standard practice: \cr
#' \code{glm(as.formula(paste("(survtime <= followup) & (censorid == 1)~" ,paste(covariates, collapse='+'))), data = data)}. \cr
#' Alternatively, a list with:
#' \itemize{
#' \item $formula (~ covariates)
#' \item $coefficients (named vector specifying risk adjustment coefficients
#' for covariates - names must be the same as in $formula and colnames of \code{data}).
#' }
#' @param followup The followup time for every individual. At what time after subject entry do we consider the outcome?
#' @param conflev A vector of confidence levels of interest. Default is c(0.95, 0.99).
#'
#' @return An object of class "funnelplot" containing:
#' \itemize{
#' \item \code{data}: A \code{data.frame} containing:
#' \itemize{
#' \item $instance instance number
#' \item $observed observed number of failures at instance
#' \item $expected expected (risk-adjusted) number of failures at instance
#' \item $numtotal total number of individuals considered at this instance
#' \item $p (risk-adjusted) proportion of failure at instance
#' \item $conflevels worse/normal/better performance than expected at this confidence level
#' }
#' \item \code{call}: the call used to obtain output
#' \item \code{plotdata}: data used for plotting confidence intervals
#' \item \code{conflev}: specified confidence level(s)
#' }There are \code{\link[cgrcusum:plot.funnelplot]{plot}} and
#'  \code{\link[cgrcusum:summary.funnelplot]{summary}} methods for "funnelplot" objects.
#'
#' @importFrom stats predict.glm
#' @importFrom stats qnorm
#' @export
#'
#' @author Daniel Gomon
#' @family qcchart
#' @seealso \code{\link[cgrcusum]{plot.funnelplot}}, \code{\link[cgrcusum]{summary.funnelplot}}
#'
#'
#' @examples
#' varsanalysis <- c("age", "sex", "BMI")
#' exprfitfunnel <- as.formula(paste("(entrytime <= 365) & (censorid == 1)~",
#'  paste(varsanalysis, collapse='+')))
#' surgerydat$instance <- surgerydat$Hosp_num
#' glmmodfun <- glm(exprfitfunnel, data = surgerydat, family = binomial(link = "logit"))
#' funnel <- funnelplot(data = surgerydat, ctime = 3*365, glmmod = glmmodfun, followup = 100)
#' plot(funnel)






funnelplot <- function(data, ctime, p0, glmmod, followup, conflev = c(0.95, 0.99)){
  entrytime <- instance <- NULL
  call <- match.call()

  #First perform a logistic regression to obtain coefficients for the Risk-adjustment, then specify them later
  #dat has to be a dataframe containing at least entrytime (time of entry), survtime (time until failure), and additionally the    covariates to RA   on, it also has to contain $instance indicating the hospital in question
  #glmmodel is the risk-adjustment model, either an object of class "glm" or $formula and $coefficients
  #followup is time until which we consider outcomes, usually 365 (1 year) as we consider 1 year post transplant
  #Specify institute name or number in dat$instance
  #conflev indicates the confidence levels at which to plot the boundaries
  #time is the chronological time at which the FUNNEL chart should be constructed, we remove non-qualifying cases
  if(!missing(ctime)){
    newdata <- subset(data, entrytime + followup <= ctime)
  } else{
    newdata <- data
  }
  if(missing(p0)){
    warning("No value provided for null (hypothesis) failure probability. Determining using average over whole data set.", immediate. = TRUE)
    p0 <- length(which((newdata$survtime <= followup) & (newdata$censorid == 1)))/length(newdata$survtime)
  }
  plotframe <- data.frame(instance = character(), observed = double(), expected = double(), numcases = double())
  for(j in unique(newdata$instance)){
    tempdata <- subset(newdata, instance == j)
    tempnum <- length(tempdata$survtime)
    if(!missing(glmmod)){
      if(inherits(glmmod, "glm")){
        tempprobs <-  predict(glmmod, newdata = tempdata, type = "response")
        tempexpec <- sum(tempprobs)
      } else{
        mmatrix <- model.matrix(glmmod$formula, tempdata)
        coeffs <- glmmod$coefficients[colnames(mmatrix)]
        tempexpec <- sum(c(1/(1 + exp(-mmatrix %*% coeffs))))
      }
    } else{
      tempexpec <- tempnum * p0
    }
    tempexpec <- ifelse(tempexpec > nrow(tempdata), nrow(tempdata), tempexpec)
    tempobs <- length(which((tempdata$survtime <= followup) & (tempdata$censorid == 1)))
    temprow <- data.frame(as.character(j), tempobs, tempexpec,  tempnum)
    plotframe <- rbind(plotframe, temprow)
  }
  colnames(plotframe) = c("instance", "observed", "expected", "numtotal")

  plotframe$p <- plotframe$observed/plotframe$expected * p0
  plotframe$p <- ifelse(plotframe$p > 1, 1, plotframe$p)
  boundplotframe <- data.frame(number = double(),conflev = double(),lower = double(), upper = double())
  plotseq <- seq(max(1, min(plotframe$numtotal)-10), max(plotframe$numtotal) +10, by = 1)
  findbounds <- function(t, conflev){
    return(c(p0 - qnorm(conflev) * sqrt((p0*(1-p0))/t),p0 + qnorm(conflev) * sqrt((p0*(1-p0))/t)))
  }
  for(k in 1:length(conflev)){
    temprow2 <- data.frame(plotseq, rep(conflev[k], length(plotseq)), t(sapply(plotseq, function(t) findbounds(t, conflev[k]))))
    boundplotframe <- rbind(boundplotframe, temprow2)
    tempchar <- character(length = nrow(plotframe))
    for(i in 1:nrow(plotframe)){
      tempbounds <- findbounds(plotframe$numtotal[i], conflev = conflev[k])
      if(plotframe$p[i] > tempbounds[2]){
        tempchar[i] <- "worse"
      } else if(plotframe$p[i] < tempbounds[1]){
        tempchar[i] <- "better"
      } else{
        tempchar[i] <- "normal"
      }
    }
    plotframe <- cbind(plotframe, tempchar)
    colnames(plotframe)[ncol(plotframe)] <- as.character(conflev[k])
  }
  colnames(boundplotframe) = c("numtotal", "conflev", "lower", "upper")
  funnelp <- list(data = plotframe,
              call = call,
              plotdata = boundplotframe,
              conflev = conflev)
  class(funnelp) <- "funnelplot"
  funnelp
}







