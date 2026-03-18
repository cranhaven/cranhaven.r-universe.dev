#' Natural-Logarithm-Transformed Odds Ratio (lnOR): Monte Carlo Based Heterogeneity Test for Between-study Heterogeneity in Random- or Mixed- Effects Model
#'
#' \code{mc.lnOR} returns the Monte Carlo based tests of the residual heterogeneity in random- or mixed- effects model of natural-logarithm-transformed observed odds ratio (lnOR).
#'
#' For odds ratio, its standard error will be infinite if any one of the four cells in the contingency tables is zero. In this case, Haldane and Anscombe correction is used by adding 0.5 to each cell value (Anscombe, 1956; Haldane, 1940).

#' This function returns the test statistics as well as their p-value and significances using (1) Q-test, (2) Monte Carlo Based Heterogeneity Test with Maximum Likelihood (ML), and (3) Monte Carlo Based Heterogeneity Test with Restricted Maximum Likelihood (REML).
#'
#' The results of significances are classified as "sig" or "n.s" based on the cutoff p-value (i.e., alpha level). "sig" means that the between-study heterogeneity is significantly different from zero whereas "n.s" means the between-study heterogeneity is not significantly different from zero. The default alpha level is 0.05.
#'
#' @param n_00 a vector of number of participants who score negatively on both Y1 and Y2 (e.g., mortality cases in the control group).
#' @param n_01 a vector of number of participants who score negatively on Y1 and positively on Y2  (e.g., recovery cases in the control group).
#' @param n_10 a vector of number of participants who score positively on Y1 and negatively on Y2 (e.g., mortality cases in the experimental group).
#' @param n_11 a vector of number of participants who score positively on both Y1 and Y2 (e.g., recovery cases in the experimental group).
#' @param lnOR a vector of natural-logarithm-transformed odds ratio in the included studies, which is calculated as ln(n11*n00/n01/n10)
#' @param model choice of random- or mixed- effects models. Can only be set to \code{"random"}, or \code{"mixed"}.
#' @param mods optional argument to include one or more moderators in the model. \code{mods} is NULL for random-effects model and a dataframe for mixed-effects model. A single moderator can be given as a vector of length \eqn{k} specifying the values of the moderator. Multiple moderators are specified by giving a matrix with \eqn{k} rows and as many columns as there are moderator variables. See \code{\link[metafor:rma.uni]{rma}} for more details.
#' @param nrep number of replications used in Monte Carlo Simulations. Default to 10^4.
#' @param p_cut cutoff for p-values, which is the alpha level. Default to 0.05.
#' @param mc.include if true, Monte Carlo simulation results are included in the output (e.g., Monte Carlo critical values).
#' @importFrom metafor rma
#' @importFrom metafor fitstats

#' @references Anscombe, F. J. (1956). On estimating binomial response relations. Biometrika, 43(3/4), 461–464.
#' @references Haldane, J. (1940). The mean and variance of| chi 2, when used as a test of homogeneity, when expectations are small. Biometrika, 31(3/4), 346–355.
#' @references Viechtbauer, W. (2010). Conducting meta-analyses in R with the metafor package. Journal of Statistical Software, 36(3), 1-48. URL: https://www.jstatsoft.org/v36/i03/
#' @source C. Silagy (2003), Nicotine replacement therapy for smoking cessation (Cochrane Review). The Cochrane Library, 4, John Wiley \& Sons, Chichester.

#' @examples
#' # A meta-analysis consists of 26 studies on nicotine replacement therapy for smoking cessation
#' library(HSAUR3)
#' data(smoking)
#' # Y1: receive treatment; Y2: stop smoking
#' n_00 <- smoking$tc - smoking$qc  # not receive treatement yet not stop smoking
#' n_01 <- smoking$qc # not receive treatement but stop smoking
#' n_10 <- smoking$tt - smoking$qt # receive treatement but not stop smoking
#' n_11 <- smoking$qt # receive treatement and stop smoking
#' lnOR <- log(n_11*n_00/n_01/n_10)
#' \dontrun{
#' mc.run <- mc.lnOR(n_00, n_01, n_10, n_11, model = 'random', p_cut = 0.05)
#' }
#' ## Note: this mc.lnOR() function will soon be deprecated
#' ## and replaced by \link[boot.heterogeneity]{boot.lnOR} in
#' ## package [boot.heterogeneity](https://CRAN.R-project.org/package=boot.heterogeneity).
#' @export

mc.lnOR <- function(n_00, n_01, n_10, n_11, model = 'random', mods = NULL, nrep = 10^4, p_cut = 0.05, mc.include = FALSE) {

  #########################################################################
  if (!model %in% c('random', 'mixed')){
    stop("The meta-analytical model must be either random- or mixed- effects model!")
  }
  if (model == 'random' & !is.null(mods)){
    stop("No moderators should be included for random-effects model!")
  }
  if (model == 'mixed' & is.null(mods)){
    stop("Moderators need be included for mixed-effects model!")
  }

  #########################################################################
  # zero count correction
  df <- cbind(n_00, n_01, n_10, n_11)
  if(any(df == 0)){
    df <- df + 0.5*(df==0)
    n_00 <- df$n_00
    n_01 <- df$n_01
    n_10 <- df$n_10
    n_11 <- df$n_11
    }
  #########################################################################

  n <- n_00 + n_01 + n_10 + n_11
  lnOR <- log(n_11*n_00/n_01/n_10)
  vi <- 1/n_00+1/n_01+1/n_10+1/n_11

  model.f1<-try(metafor::rma(lnOR, vi, mods = mods, tau2=0, method="ML"))
  model.f2<-try(metafor::rma(lnOR, vi, mods = mods, tau2=0, method="REML"))
  model.r1<-try(metafor::rma(lnOR, vi, mods = mods, method="ML"))
  model.r2<-try(metafor::rma(lnOR, vi, mods = mods, method="REML"))


  #if (class(model.r2)!="try-error" ){
  if (sum(!class(model.r2)!="try-error")==0){

  bs <- model.r2$beta[,1]
  lnOR_overall <- apply(cbind(1, mods), 1, function(x) sum(bs*x))
  #get predicted effect size for each study #for w/ and w/o moderators

  find.c <- matrix(NA, 2, nrep)
  pb <- utils::txtProgressBar(min = 0, max = nrep, style = 3)
  for(i in 1:nrep){
    Sys.sleep(0.01)
    utils::setTxtProgressBar(pb, i)
    find.c[,i] = simulate.OR(i, lnOR_overall, vi, n, n_00, n_01, mods)
  }
  err.catcher <- sum(colSums(is.na(find.c))!=0)/nrep
  if (err.catcher >0.05){
    warning("Noncovergence rate in simulations is larger than 5%!")
  }

  ML.sim <- stats::na.omit(unlist(find.c)[ c(TRUE,FALSE) ])
  REML.sim <- stats::na.omit(unlist(find.c)[ c(FALSE,TRUE) ])
  ML.c<-stats::quantile(ML.sim, 0.95)
  REML.c<-stats::quantile(REML.sim, 0.95)

  if (sum(!class(model.r1)!="try-error" , !class(model.f1)!="try-error")==0){
    lllr1<-(metafor::fitstats(model.r1)-metafor::fitstats(model.f1))[1]*2
    p_lr1<-sum(ML.sim>=lllr1)/nrep
    res_lr1<-ifelse(lllr1>ML.c, 'sig', 'n.s')
  } else {
    lllr1<-NA; p_lr1<-NA; res_lr1<-NA
  }

  if (sum(!class(model.r2)!="try-error" , !class(model.f2)!="try-error")==0){
    lllr2<-(metafor::fitstats(model.r2)-metafor::fitstats(model.f2))[1]*2
    p_lr2<-sum(REML.sim>=lllr2)/nrep
    res_lr2<-ifelse(lllr2>REML.c, 'sig', 'n.s')
  } else {
    lllr2<-NA; p_lr2<-NA; res_lr2<-NA
  }

  Q <- model.f1$QE
  Qp <- model.r2$QEp
  Qres<-ifelse(Qp< p_cut, 'sig', 'n.s') ### vary the size
  } else {
    Q<-NA
    Qp<-NA
    Qres<-NA
    lllr1<-NA
    p_lr1<-NA
    res_lr1<-NA
    lllr2<-NA
    p_lr2<-NA
    res_lr2<-NA
  }

  out <- data.frame(stat = c(Q, lllr1, lllr2), p_value = c(Qp, p_lr1, p_lr2), Heterogeneity = c(Qres, res_lr1, res_lr2))
  #(Q, Qp, Qres, lllr1, p_lr1, res_lr1, lllr2, p_lr2, res_lr2)
  #colnames(out) <- c('QE', 'QEp', 'QEres', 'ML', 'mc.MLp', 'mc.MLres', 'REML', 'mc.REMLp', 'REMLp')
  rownames(out) <- c('Qtest', 'mc.ML', 'mc.REML')

  if(mc.include){
    out <- list(results = out, ML.crit = ML.c, REML.crit = REML.c)
  }

  return(out)
}

