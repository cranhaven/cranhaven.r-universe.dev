#' @title Calculate treatment effect confidence intervals by inverting the randomization test
#' 
#' @description
#' Using an output object from \code{seqblock} or any other matrix or dataframe that includes a treatment and an outcome variable for multiple units, as well as blocking and non-blocking variables for the respective unit(s), \code{invertRIconfInt} calculates treatment effect confidence intervals by inverting the randomization inference test.
#' 
#' @details
#'\code{invertRIconfInt} takes a data matrix (or data frame) containing names and values of different blocking and non-blocking variables, as well as each unit's treatment assignment and outcome as input and returns a list of treatment effect confidence intervals.
#'
#'Apart from specifying the treatment and outcome variable, the user can set all other arguments to \code{seqblock} when running \code{invertRIconfInt}. The function will then calculate the confidence intervals by employing a method described in Ho and Imai (2006), which inverts Fisher's exact test. The resulting confidence intervals are distribution-free, nonparametric and have accurate coverage probabilities. 
#'
#' @param dat a matrix or dataframe containing the names and values of the different blocking and non-blocking variables, as well as each unit's treatment assignment and outcome
#' @param outcome.var a string specifying the name of the outcome variable
#' @param tr.var a string specifying the name of the treatment variable
#' @param tau.abs.min lower bound of the range across which the confidence intervals will be computed
#' @param tau.abs.max upper bound of the range across which the confidence intervals will be computed
#' @param tau.length the number of (evenly spaced) possible treatment effects across the range specified by \code{tau.abs.min} and \code{tau.abs.max} for which location inside or outside the confidence intervals will be computed
#' @param n.sb.p the number of times that sequential blocking will be performed on the dataset
#' @param id.vars see the \code{seqblock} documentation
#' @param id.vals see the \code{seqblock} documentation
#' @param exact.vars see the \code{seqblock} documentation
#' @param exact.vals see the \code{seqblock} documentation
#' @param exact.restr see the \code{seqblock} documentation
#' @param exact.alg see the \code{seqblock} documentation
#' @param covar.vars see the \code{seqblock} documentation
#' @param covar.vals see the \code{seqblock} documentation
#' @param covar.restr see the \code{seqblock} documentation
#' @param covars.ord see the \code{seqblock} documentation
#' @param n.tr see the \code{seqblock} documentation
#' @param tr.names see the \code{seqblock} documentation
#' @param assg.prob see the \code{seqblock} documentation
#' @param seed see the \code{seqblock} documentation
#' @param seed.dist see the \code{seqblock} documentation
#' @param assg.prob.stat see the \code{seqblock} documentation
#' @param trim see the \code{seqblock} documentation
#' @param assg.prob.method see the \code{seqblock} documentation
#' @param assg.prob.kfac see the \code{seqblock} documentation
#' @param distance see the \code{seqblock} documentation
#' @param file.name see the \code{seqblock} documentation
#' @param query see the \code{seqblock} documentation
#' @param verbose see the \code{seqblock} documentation
#' 
#' @return 
#' A list with elements
#' \itemize{
#'   \item \strong{ci95}: vector of treatment effects within the 95\% confidence interval
#'   \item \strong{ci90}: vector of treatment effects within the 90\% confidence interval
#'   \item \strong{ci80}: vector of treatment effects within the 80\% confidence interval
#' }
#' 
#' @examples
#' # Create an example data matrix with 50 observations that contains an ID variable, 
#' # a dummy variable indicating gender, an age variable (between 18 and 55), a 
#' # treatment variable and an outcome variable (between 15 and 20). 
#' # id <- seq(1, 50, 1)
#' # gender <- sample(c(1, 2), 50, replace = TRUE)
#' # age <- sample(seq(18, 55, 1), 50, replace = TRUE)
#' # treat <- sample(c(1, 2), 50, replace = TRUE)
#' # out <- treat + sample(seq(15, 20, 1), 50, replace = TRUE)
#' # df <- cbind(id, gender, age, out, treat)
#' 
#' # Check summary statistics for the created data
#' # aggregate(out ~ treat, df, mean)
#' 
#' # Run invertRIconfInt()
#' # invertRIconfInt(data, outcome.var="out", tr.var="treat", tau.abs.min = -3, 
#' #                 tau.abs.max = 3, id.vars = "id", id.vals = "id", 
#' #                 exact.vars = c("gender", "age"), exact.vals = c("gender", "age"))
#' 
#' @references 
#' Moore, Ryan T. and Sally A. Moore. 2013. "Blocking for Sequential Political Experiments." \emph{Political Analysis} 21(4): 507-523. 
#' 
#' Ho, Daniel E., and Kosuke Imai. 2006. "Randomization inference with natural experiments: An analysis of ballot effects in the 2003 California recall election." \emph{Journal of the American Statistical Association} 101(475): 888-900.
#' 
#' @seealso \code{\link{seqblock}}
#' 
#' @author Ryan T. Moore \email{rtm@american.edu} and Jonathan Homola \email{homola@wustl.edu}
#' 
#' @keywords design
#' 
#' @export 

invertRIconfInt <- function(dat, outcome.var, tr.var, tau.abs.min = -1, tau.abs.max = 1, tau.length = 10, n.sb.p = 100, id.vars, id.vals, exact.vars = NULL, exact.vals = NULL, exact.restr = NULL, exact.alg = "single", covar.vars = NULL, covar.vals = NULL, covar.restr = NULL, covars.ord = NULL, n.tr = 2, tr.names = NULL, assg.prob = NULL, seed = NULL, seed.dist, assg.prob.stat = NULL, trim = NULL, assg.prob.method = NULL, assg.prob.kfac = NULL, distance = "mahalanobis", file.name = "sbout.RData", query = FALSE, verbose = TRUE){
  
  taus <- seq(tau.abs.min, tau.abs.max, length = tau.length)
  ci95 <- ci90 <- ci80 <- array(NA)

  for(i in 1:length(taus)){
    
    tau <- taus[i]
    y <- dat[, outcome.var] 
    t <- dat[, tr.var]	
    
    # given dat X/Y/Tr, calc W_{\tau_0}^D(T) [see Ho and Imai 2006: 892-893].
    wdt <- sum(t * y)/sum(t) - sum((1 - t) * y)/sum(1 - t)
    
    wdT.dist <- array(NA)
    
    for(j in 1:n.sb.p){
      
      # given dat X, do an SB.
      sb1 <- seqblock(id.vars = id.vars, id.vals = dat[1, id.vals], exact.vars = exact.vars, exact.vals = dat[1, exact.vals], exact.restr = exact.restr, exact.alg = exact.alg, covar.vars = covar.vars, covar.vals = dat[1, covar.vals], covar.restr = covar.restr, covars.ord = covars.ord, n.tr = n.tr, tr.names = tr.names, assg.prob = assg.prob, seed = seed, seed.dist = seed.dist, assg.prob.stat = assg.prob.stat, trim = trim, assg.prob.method = assg.prob.method, assg.prob.kfac = assg.prob.kfac, file.name = file.name, verbose = FALSE)
  
      for(n.idx in 2:nrow(dat)){
        sb2k <- seqblock(object = file.name, id.vals = dat[n.idx, id.vals], exact.vals = dat[n.idx, exact.vals], exact.restr = exact.restr, exact.alg = exact.alg, covar.vals = dat[n.idx, covar.vals], covar.restr = covar.restr, covars.ord = covars.ord, n.tr = n.tr, tr.names = tr.names, assg.prob = assg.prob, seed = seed, seed.dist = seed.dist, assg.prob.stat = assg.prob.stat, trim = trim, assg.prob.method = assg.prob.method, assg.prob.kfac = assg.prob.kfac, file.name = file.name, verbose = FALSE)
      }
        
      t.tmp <- sb2k$orig[, "Tr"]
      t.tmp <- as.numeric(as.factor(t.tmp)) - 1
      
      # t.tmp is "this rerandomized t". t is the original randomization.
      wdT.dist[j] <- sum(t.tmp*(y + (1 - t) * tau))/sum(t.tmp) - sum((1 - t.tmp) * (y - t * tau))/sum(1 - t.tmp)
    }
    
    if(.025 <= mean(wdT.dist >= wdt) & mean(wdT.dist >= wdt) <= .975){
      ci95 <- append(ci95, tau)
    }
    if(.05 <= mean(wdT.dist >= wdt) & mean(wdT.dist >= wdt) <= .95){
      ci90 <- append(ci90, tau)
    }
    if(.1 <= mean(wdT.dist >= wdt) & mean(wdT.dist >= wdt) <= .9){
      ci80 <- append(ci80, tau)
    }
    
  }
  
  ci95 <- ci95[2:length(ci95)]
  ci90 <- ci90[2:length(ci90)]
  ci80 <- ci80[2:length(ci80)]
  
  return(list(ci95 = ci95, ci90 = ci90, ci80 = ci80))
  
}