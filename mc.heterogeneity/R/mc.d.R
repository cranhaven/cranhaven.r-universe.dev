#' Standardized Mean Differences (d): Monte Carlo Based Heterogeneity Test for Between-study Heterogeneity in Random- or Mixed- Effects Model
#'
#' \code{mc.d} returns the Monte Carlo based tests of the residual heterogeneity in random- or mixed- effects model of standardized mean differences (d).
#'
#' For standardized mean difference, if the biased estimates (i.e., g values) are provided, \code{adjust=TRUE} can be specified to obtain the corresponding unbiased estimates.
#'
#' This function returns the test statistics as well as their p-value and significances using (1) Q-test, (2) Monte Carlo Based Heterogeneity Test with Maximum Likelihood (ML), and (3) Monte Carlo Based Heterogeneity Test with Restricted Maximum Likelihood (REML).
#'
#' The results of significances are classified as "sig" or "n.s" based on the cutoff p-value (i.e., alpha level). "sig" means that the between-study heterogeneity is significantly different from zero whereas "n.s" means the between-study heterogeneity is not significantly different from zero. The default alpha level is 0.05.
#'
#' @param n1 a vector of sample sizes from group 1 in each of the included studies.
#' @param n2 a vector of sample sizes from group 2 in each of the included studies.
#' @param est a vector of unbiased estimates of standardized mean differences.
#' @param adjust if biased estimates (i.e., g values) are provided, \code{adjust} must be set to \code{TRUE} to compensate for small sample bias. By default, \code{adjust} is set to \code{FALSE}.
#' @param model choice of random- or mixed- effects models. Can only be set to \code{"random"}, or \code{"mixed"}.
#' @param mods optional argument to include one or more moderators in the model. \code{mods} is NULL for random-effects model and a dataframe for mixed-effects model. A single moderator can be given as a vector of length \eqn{k} specifying the values of the moderator. Multiple moderators are specified by giving a matrix with \eqn{k} rows and as many columns as there are moderator variables. See \code{\link[metafor:rma.uni]{rma}} for more details.
#' @param nrep number of replications used in Monte Carlo Simulations. Default to 10^4.
#' @param p_cut cutoff for p-values, which is the alpha level. Default to 0.05.
#' @param mc.include if true, Monte Carlo simulation results are included in the output (e.g., Monte Carlo critical values).

#' @importFrom metafor rma
#' @importFrom metafor fitstats

#' @references Hedges, L. V. (1981). Distribution theory for glass’s estimator of effect size and related estimators. Journal of Educational and Behavioral Statistics, 6(2), 107–128.
#' @references Hedges, L. V., Giaconia, R. M., & Gage, N. L. (1981). Meta-analysis of the effect of open and traditional instruction. Stanford, CA: Stanford University, Program on Teaching Effectiveness.
#' @references Viechtbauer, W. (2010). Conducting meta-analyses in R with the metafor package. Journal of Statistical Software, 36(3), 1-48. URL: https://www.jstatsoft.org/v36/i03/

#' @examples
#' # A meta-analysis of 18 studies in which the effect of open versus traditional
#' # education on students' self-concept was studied (Hedges et al., 1981).
#' selfconcept <- mc.heterogeneity:::selfconcept
#' # n1 and n2 are lists of samples sizes in two groups
#' n1 <- selfconcept$n1
#' n2 <- selfconcept$n2
#' # g is a list of biased estimates of standardized mean differences in the meta-analytical study
#' g <- selfconcept$g
#' cm <- (1-3/(4*(n1+n2-2)-1)) #correct factor to compensate for small sample bias (Hedges, 1981)
#' d <- cm*g
#' \dontrun{
#' mc.run <- mc.d(n1, n2, est = d, model = 'random', p_cut = 0.05)
#' # is equivalent to:
#' mc.run2 <- mc.d(n1, n2, est = g, model = 'random', adjust = TRUE, p_cut = 0.05)
#' }
#'
#'# A hypothetical meta-analysis of 15 studies with 3 moderators.
#' hypo_moder <- mc.heterogeneity:::hypo_moder
#' \dontrun{
#' mc.run3 <- mc.d(n1 = hypo_moder$n1, n2 = hypo_moder$n2, est = hypo_moder$d, model = 'mixed',
#' mods = cbind(hypo_moder$cov.z1, hypo_moder$cov.z2, hypo_moder$cov.z3), p_cut = 0.05)
#' }
#' ## Note: this mc.d() function will soon be deprecated
#' ## and replaced by \link[boot.heterogeneity]{boot.d} in
#' ## package [boot.heterogeneity](https://CRAN.R-project.org/package=boot.heterogeneity).
#' @export

mc.d <- function(n1, n2, est, model = 'random', adjust = FALSE, mods = NULL, nrep = 10^4, p_cut = 0.05, mc.include = FALSE) {

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
  # adjustment for bias
  if(adjust){
    cm <- (1-3/(4*(n1+n2-2)-1)) #correct factor to compensate for small sample bias (Hedges, 1981)
    est <- cm*est
  }
  #########################################################################
  vi<-(n1+n2)/n1/n2+est^2/(2*(n1+n2))

  model.f1<-try(metafor::rma(est, vi, mods = mods, tau2=0, method="ML"))
  model.f2<-try(metafor::rma(est, vi, mods = mods, tau2=0, method="REML"))
  model.r1<-try(metafor::rma(est, vi, mods = mods, method="ML"))
  model.r2<-try(metafor::rma(est, vi, mods = mods, method="REML"))


  #if (class(model.r2)!="try-error" ){
  if (sum(!class(model.r2)!="try-error")==0 ){

  bs <- model.r2$beta[,1]
  d_overall <- apply(cbind(1, mods), 1, function(x) sum(bs*x))
  #get predicted effect size for each study #for w/ and w/o moderators

  find.c <- matrix(NA, 2, nrep)
  pb <- utils::txtProgressBar(min = 0, max = nrep, style = 3)
  for(i in 1:nrep){
    Sys.sleep(0.01)
    utils::setTxtProgressBar(pb, i)
    find.c[,i] = simulate.d(i, d_overall, vi, n1, n2, mods)
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

