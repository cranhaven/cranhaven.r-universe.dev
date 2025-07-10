#' Generate operating characteristics for Bayesian two-stage trial design
#' of ordinal endpoints without proportional odds assumption
#'
#' @description
#' Obtain operating characteristics (OC) of the Bayesian two-stage trial
#' design with ordinal endpoints while the proportional odds assumption are
#' violated.
#'
#' @param alpha the desired type I error to be controlled
#' @param pro_ctr distribution of clinical categories for the
#' control group
#' @param U the desirability of each outcome level
#' @param fixed_ss fixed sample size when simulates the OC for various effect
#' size
#' @param ors a user-defined matrix, each row denotes the various scenarios,
#' the number of columns depend on the number of outcome scales.
#' @param nmax the maximum sample size when simulates the OC for different sample size, the increment is 50 and
#' the initial sample size is 50 for each arm each stage.
#' @param fixed_es fixed effect size when simulate the OC for various sample
#' size
#' @param ntrial the number of simulated trials
#' @param method whether the statistical test for interim/final analysis is Bayesian or
#' Frequentist. method = "Frequentist" for Frequentist approach; method = "Bayesian"
#' for Bayesian approach
#'
#' @details
#'   Grid search of sample size is used for guarantee a desirable type I error rate.
#'   The upper limitation is 400, and lower limitation default is sample size 50
#'   for the control and treatment groups at each stage. Default increment of the
#'   sequence is 50.
#'
#'   For the parameter estimation section, we have two options, and can be selected using
#'   the method argument.Two following options are available: (i) method = "Frequentist",
#'   (ii) method = "Bayesian". If method = "Frequentist", parameters are estimated via package
#'   ordinal, which is based on frequentist method, while method = "Bayesian", parameters are
#'   estimated through Bayesian model.
#'
#'   Specifically, the numerical utilities U reflect the desirability of each outcome
#'   level. To do this, in our example, we first set U[1] = 100 and U[5] = 0, and then
#'   asked physicians to specify numerical values for the intermediate levels, that
#'   reflect their desirability relative to the best and worst levels.
#'
#'   Function provides two types of operating characteristics via simulation. If user
#'   specifies the value of ors and fixed_ss, function will calculate the design's power
#'   in terms of effect size. If user specifies the value of nmax and fixed_es, function
#'   will calculate the design's power in terms of sample size, and nmax is the upper
#'   limitation of sample size for the treatment and control groups at each stage, the lower
#'   limitation is 50, the default increment of the sequence is 10.
#'
#'   Please note, in our example, argument ntrial =  5 is for the time saving purpose.
#'
#'
#'
#' @return get_oc_NPO() returns the operating characteristics of design as a
#' table, including (1) user-defined value, either sample size or effect size
#' (2) corresponding power (3) average sample size
#' @export
#' @import rjmcmc
#' @import stats
#' @importFrom ggplot2 ggplot
#' @importFrom ordinal clm
#' @import superdiag
#' @import rjags
#' @import R2jags
#' @import gsDesign
#' @import schoolmath
#' @importFrom madness det
#' @importFrom graphics matplot
#' @importFrom stats predict.lm
#' @importFrom methods is
#'
#' @examples
#'
#' ors = matrix(c(1.5,1.5,1,1,1,1.5,1.5,1.1,1.1,1.1), nrow=2, ncol=5, byrow=TRUE)
#'
#' get_oc_NPO(alpha = 0.05, pro_ctr = c(0.58,0.05,0.17,0.03,0.04,0.13),
#'            U = c(100,80,65,25,10,0), fixed_ss = 200, ors, ntrial = 5,
#'            method = "Frequentist")
#'
#' set.seed(123)
#' get_oc_NPO(alpha = 0.05, pro_ctr = c(0.58,0.05,0.17,0.03,0.04,0.13),
#'            U = c(100,80,65,25,10,0), nmax = 100, fixed_es = c(1.5,1.3,1,1,1),
#'            ntrial = 5, method = "Frequentist")
#'


get_oc_NPO = function(alpha, pro_ctr, U, fixed_ss, ors, nmax, fixed_es,
                       ntrial, method){
  N = 100

  if (missing(nmax))
    nmax = NULL

  if (missing(ors))
    ors = NULL

  if (missing(fixed_es))
    fixed_es = NULL

  if (missing(fixed_ss))
    fixed_ss = NULL

  if (is.numeric(nmax) & is.numeric(ors)) {
    stop("nmax and ors can not be specified at the same time.")
  }

  if (is.numeric(fixed_es) & is.numeric(fixed_ss)) {
    stop("fixed_es and fixed_ss can not be specified at the same time.")
  }


  or       = rep(1, length(pro_ctr)-1)
  or.mat   = matrix(rep(or, ntrial),nrow = ntrial, ncol = length(or),
                  byrow = TRUE)

  # calculate threshold through grid search
  cf_grid        = 0.2#seq(0.5, 0.7, by=0.1)
  threshold_grid = seq(0.8, 0.95, by=0.05)

  output = c()

  for (cf in cf_grid){
    for (threshold in threshold_grid){

      out = multiple_trial_npo(or.mat, sd = 0.2, pro_ctr, U, n = N, cf=cf,
                                 threshold=threshold, method = method)
      rr = c(cf, threshold, out)
      output = rbind(output, rr)
      colnames(output) = c("cf", "threshold", "PET(%)", "alpha", "Avg SS")
      results = as.data.frame(output)
    }
  }
  index = min(which(abs(results$alpha-alpha)==min(abs(results$alpha-alpha))))
  vec = c(results[index,c(1,2,4)])
  thrsh = c(vec$cf, vec$threshold)
  names(thrsh) = c("futility", "superiority")


  output = c()

  if(is.numeric(fixed_es) & is.numeric(nmax)){
    or.mat = matrix(rep(fixed_es, ntrial), nrow = ntrial,
                    ncol = length(fixed_es), byrow = TRUE)

    n_grid = seq(50, nmax, by = 50)
    for (n in n_grid){

      out = multiple_trial_npo(or.mat, sd = 0.2, pro_ctr, U, n=n, cf=vec$cf,
                               threshold = vec$threshold, method = method)

      rr = c(n, out)
      output = rbind(output, rr)
      colnames(output) = c("Sample Size", "PET(%)", "Power(%)", "Avg SS")
    }

  }else if(is.numeric(fixed_ss)&is.numeric(ors)){

    for (i in 1:dim(ors)[1]){
      or = ors[i,]

      p2 = pro_trt_cal(or, pro_ctr)
      dif_utility = round(mean_u(U,pro_ctr,p2)[3],digits = 2)

      or.mat = matrix(rep(or, ntrial),nrow = ntrial, ncol = length(or),
                      byrow = TRUE)

      out = multiple_trial_npo(or.mat, sd = 0.2, pro_ctr, U, n = fixed_ss, cf=vec$cf,
                                 threshold = vec$threshold, method = method)

      rr = c(dif_utility, out)
      output = rbind(output, rr)
      colnames(output) = c("Effect Size", "PET(%)", "Power(%)", "Avg SS")

    }
  }

  output[,3] = output[,3]*100
  rownames(output) = paste0("Scenario ", 1:dim(output)[1])
  results = list()
  results$design = output
  results$threshold = thrsh
  results$typeIerror = round(vec$alpha,digits = 3)
  return(results)
}

