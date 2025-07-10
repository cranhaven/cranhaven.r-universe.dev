#' Determine the sample size for Bayesian two-stage trial design
#' of ordinal endpoints with proportional odds assumption
#'
#' @description
#' Obtain estimated sample size based on user-specified type I
#' error, power and effect size defined by the odds ratio between
#' the treatment and control groups, under the proportional
#' odds (PO) assumption.
#'
#' @param or_alt effect size to be detected (under H_1)
#' in terms of odds ratio
#' @param pro_ctr distribution of clinical categories for the
#' control group
#' @param alpha the desirable type I error rate to be controlled
#' @param power the desirable power to be achieved
#' @param nmax the maximum sample size for searching to get the desirable power
#' @param ntrial the number of simulated trials
#' @param method whether the statistical test for interim/final analysis is Bayesian or
#' Frequentist. method = "Frequentist" for Frequentist approach; method = "Bayesian"
#' for Bayesian approach
#'
#'
#' @return ss_po() returns recommended sample size for each of
#' two groups for the interim and final stages, by assuming 1:1 equal
#' randomization for the two groups at each stage; and the corresponding power.
#' @export
#' @details
#'   Grid search of sample size is used for guarantee a desirable type I error rate.
#'   The upper limitation is 200, and lower limitation default is sample size 50
#'   for the control and treatment groups at each stage. Default increment of the
#'   sequence is 50.
#'
#'   For the parameter estimation section, we have two options, and can be selected using
#'   the method argument.Two following options are available: (i) method = "Frequentist",
#'   (ii) method = "Bayesian". If method = "Frequentist", parameters are estimated via package
#'   ordinal, which is based on frequentist method, while method = "Bayesian", parameters are
#'   estimated through Bayesian model.
#'
#'   Please note, in our example, argument ntrial =  5 is for the time saving purpose.
#'
#'
#' @examples
#' ss_po(or_alt = 1.5, pro_ctr = c(0.58,0.05,0.17,0.03,0.04,0.13), alpha = 0.05,
#'       power = 0.8, nmax = 100, ntrial = 5, method ="Frequentist")
#'

## sample size calculator
ss_po = function(or_alt, pro_ctr, alpha, power, nmax, ntrial,method){


  N = 200 # maximum sample size
  # under null, calculate thresholds
  cf_grid        = 0.2#seq(0.6, 0.7, by=0.1)
  threshold_grid = seq(0.99, 0.999, by=0.001)


  log_or = rnorm(ntrial, log(1), sd = 0.2)
  or = exp(log_or)
  or.mat = matrix(rep(or, each = length(pro_ctr)-1,
                      times=1),ncol = length(pro_ctr)-1,byrow = TRUE)

  output = c()

  for (cf in cf_grid){
    for (threshold in threshold_grid){
      out = multiple_trial_po(sim_runs = ntrial, or.mat, pro_ctr = pro_ctr,
                              n = N, cf = cf,
                           threshold = threshold, method = method)
      rr = c(cf, threshold, out)
      output = rbind(output, rr)
      colnames(output) = c("cf", "threshold", "PET(%)", "alpha", "avgss")
      results = as.data.frame(output)
    }
  }
  index = min(which(abs(results$alpha-alpha)==min(abs(results$alpha-alpha))))
  vec = c(results[index,c(1,2,4)])
  thrsh = c(vec$cf, vec$threshold)
  names(thrsh) = c("futility", "superiority")

  output = c()

  log_or = rnorm(ntrial, log(or_alt), sd = 0.2)
  or = exp(log_or)
  or.mat = matrix(rep(or, each = length(pro_ctr)-1, times=1),
                  ncol = length(pro_ctr)-1,byrow = TRUE)

  n_grid = seq(50, nmax, by = 25)

  for (n in n_grid){
    out = multiple_trial_po(sim_runs = ntrial, or.mat, pro_ctr = pro_ctr, n,
                         cf = vec$cf, threshold = vec$threshold,
                         method = method)
    rr = c(n, out)
    output = rbind(output, rr)
    colnames(output) = c("sample size", "PET(%)", "Power(%)", "avgss")

  }

  results = list()
  index = min(which(output[,3] >= power))
  results$total_sample_size_for_each_group = output[index, 1]
  results$power = 100*output[index, 3]
  results$threshold = thrsh
  results$typeIerror = vec$alpha
  return(results)
}



