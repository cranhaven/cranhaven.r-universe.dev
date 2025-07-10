#' Generate operating characteristics for Bayesian two-stage trial design
#' of ordinal endpoints with proportional odds assumption
#'
#' @description
#' Obtain operating characteristics (OC) of the Bayesian two-stage trial
#' design of ordinal endpoints with proportional odds assumption.
#'
#' @param alpha the desirable type I error rate to be controlled
#' @param pro_ctr distribution of clinical categories for the
#' control group
#' @param nmax the maximum sample size for operating characteristics
#' @param fixed_es fixed effect size when simulate the OC for various sample
#' size
#' @param ormax the maximum effect size for OC
#' @param fixed_ss fixed sample size when simulate the OC for various effect
#' size
#' @param ntrial the number of simulated trials
#' @param method whether the statistical test for interim/final analysis is Bayesian or
#' Frequentist. method = "Frequentist" for Frequentist approach; method = "Bayesian"
#' for Bayesian approach
#'
#'
#' @details
#'   Grid search of sample size is used for guarantee a desirable type I error rate.
#'   The upper limitation is 200, and lower limitation default is sample size 50
#'   for the control and treatment groups at each stage. Default increment of the
#'   sequence is 10.
#'
#'   For the parameter estimation section, we have two options, and can be selected using
#'   the method argument.Two following options are available: (i) method = "Frequentist",
#'   (ii) method = "Bayesian". If method = "Frequentist", parameters are estimated via package
#'   ordinal, which is based on frequentist method, while method = "Bayesian", parameters are
#'   estimated through Bayesian model.
#'
#'   Two types of operating characteristics can be implemented through this function.
#'
#'   Please note, in our example, argument ntrial = 5 is for the time saving purpose.
#'
#' @return get_oc_PO() returns the operating characteristics of design as a
#' table, including: (1) user-defined value, either sample size or effect size
#' (2) corresponding power (3) average sample size
#' @export
#'
#'
#' @examples
#'
#' get_oc_PO(alpha = 0.05, pro_ctr = c(0.58,0.05,0.17,0.03,0.04,0.13),
#'           ormax = 1.5, fixed_ss = 150,
#'           ntrial = 5, method = "Frequentist")
#'
#'
#' get_oc_PO(alpha = 0.05, pro_ctr = c(0.58,0.05,0.17,0.03,0.04,0.13),
#'           nmax = 200, fixed_es = 1.5,
#'           ntrial = 5, method = "Frequentist")
#'

get_oc_PO = function(alpha, pro_ctr, nmax, fixed_es, ormax, fixed_ss,
                      ntrial, method){
  N = 200

  if (missing(nmax))
    nmax = NULL

  if (missing(ormax))
    ormax = NULL

  if (missing(fixed_es))
    fixed_es = NULL

  if (missing(fixed_ss))
    fixed_ss = NULL

  if (is.numeric(nmax) & is.numeric(ormax)) {
    stop("namx and ormax can not be specified at the same time.")
  }

  if (is.numeric(fixed_es) & is.numeric(fixed_ss)) {
    stop("fixed_es and fixed_ss can not be specified at the same time.")
  }

  output = c()

  # search cutoff points
  cf_grid        = 0.2#seq(0.6, 0.7, by=0.1)
  threshold_grid = seq(0.99, 0.999, by=0.001)

  log_or = rnorm(ntrial, log(1), sd = 0.2)
  or = exp(log_or)
  or.mat = matrix(rep(or, each = length(pro_ctr)-1,
                      times=1), ncol = length(pro_ctr)-1, byrow = TRUE)

  for (cf in cf_grid){
    for (threshold in threshold_grid){
      out = multiple_trial_po(sim_runs = ntrial, or.mat, pro_ctr = pro_ctr, n = N,
                              cf = cf, threshold = threshold, method = method)
      rr = c(cf, threshold, out)
      output = rbind(output, rr)
      colnames(output) = c("cf", "threshold", "PET(%)", "alpha", "Avg SS")
      results = as.data.frame(output)
    }
    index = min(which(abs(results$alpha-alpha)==min(abs(results$alpha-alpha))))
    vec = c(results[index,c(1,2,4)])
  }

  thrsh = c(vec$cf, vec$threshold)
  names(thrsh) = c("futility", "superiority")

  output = c()
  if (is.numeric(fixed_es) & is.numeric(nmax)){
    log_or = rnorm(ntrial, log(fixed_es), sd = 0.2)
    or = exp(log_or)
    or.mat = matrix(rep(or, each = length(pro_ctr)-1, times=1),
                    ncol = length(pro_ctr)-1, byrow = TRUE)

    n_grid = seq(50, nmax, by = 50)
    for (n in n_grid){
      out = multiple_trial_po(sim_runs = ntrial, or.mat, pro_ctr = pro_ctr, n,
                              cf  = vec$cf, threshold = vec$threshold,
                              method = method)
      rr = c(2*n, out)
      output = rbind(output, rr)
      colnames(output) = c("Sample Size", "PET(%)", "Power(%)", "Avg SS")
    }
  }else if (is.numeric(fixed_ss)&is.numeric(ormax)){

    or_seq = seq(1, ormax, by = 0.5)

    for (i in 1:length(or_seq)){
      or = or_seq[i]

      log_or = rnorm(ntrial, log(or), sd = 0.2)
      or.mat = matrix(rep(exp(log_or), each = length(pro_ctr)-1, times = 1),
                  ncol = length(pro_ctr)-1, byrow = TRUE)
      prob = multiple_trial_po(sim_runs =  ntrial, or.mat, pro_ctr = pro_ctr, n = fixed_ss,
                               cf = vec$cf, threshold = vec$threshold,
                               method = method)
      rr = c(or, prob)
      output = rbind(output, rr)
      colnames(output) = c("Effect Size",  "PET(%)", "Power(%)", "Avg SS")
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
