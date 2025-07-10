#' Determine the sample size for Bayesian two-stage trial design for
#' ordinal endpoints based on switch model
#'
#' @description
#' When there lacks of sufficient information to determine which of these two
#' models (PO or NPO) is more appropriate, PO/NPO switch model-based design is
#' utilized to obtain estimated sample size based on user specified type I
#' error, power and expected effect.
#'
#' @returns ss_switch() returns recommended sample size for each group at every
#' interim look, with assumption that the sample size in the control arm of the
#' study is same as in the treatment arm, and the sample size at each interim
#' look is same.
#' @export
#' @param alpha the desirable type I error rate to be controlled
#' @param power the desirable power to be achieved
#' @param n_po sample size for the treatment and control groups, at each stage
#' based on PO model
#' @param n_npo sample size for the treatment and control groups, at each stage
#' based on NPO model
#' @param or_alt expected treatment efficacy effect size to be
#' detected (under H_1) in terms of odds ratio
#' @param pro_ctr distribution of clinical categories for the
#' control group
#' @param U the desirability of each outcome level
#' @param ntrial the number of simulated trials
#' @param method whether the statistical test for interim/final analysis is Bayesian or
#' Frequentist. method = "Frequentist" for Frequentist approach; method = "Bayesian"
#' for Bayesian approach
#' @param n_range the additional sample size for each arm each stage after n_po, n_npo.
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
#'   Specifically, the numerical utilities U reflect the desirability of each outcome
#'   level. To do this, in our example, we first set U[1] = 100 and U[5] = 0, and then
#'   asked physicians to specify numerical values for the intermediate levels, that
#'   reflect their desirability relative to the best and worst levels.
#'
#'   Arguments n_po and n_npo are the estimated sample size for the treatment and
#'   control groups at each stage based on PO model and NPO model respectively. Users can
#'   obtained them through function ss_po and ss_npo.
#'
#'
#' @examples
#' \donttest{
#' ss_switch(alpha = 0.05, power=0.8, n_po = 475, n_npo = 75, n_range = 10,
#'           or_alt = c(1.5,1.5,1.5,1.5,1.5), pro_ctr = c(0.58,0.05,0.17,0.03,0.04,0.13),
#'           U = c(100,80,65,25,10,0), ntrial = 5, method = "Frequentist")
#'          }
#'



## sample size calculator
ss_switch = function(alpha, power, n_po, n_npo, or_alt, pro_ctr, U, ntrial,
                         method, n_range){

  # under null, calculate thresholds
  or = rep(1,length(pro_ctr)-1)
  cf_grid        = 0.2#seq(0.7, 0.7, by=0.05)
  threshold_grid = seq(0.99, 0.99, by=0.01)
  output = c()

  for (cf in cf_grid){
    for (threshold in threshold_grid){
      # adding noise step integrates in the function
      out = multiple_trial_switch(or, sim_runs = ntrial, sd=0.2, pro_ctr,
                                  n_po = n_po, n_npo = n_npo, U, cf,
                                  threshold, method = method)
      rr = c(cf, threshold, out)
      output = rbind(output, rr)
      colnames(output)[1:5] = c("cf", "threshold", "PET(%)","alpha", "Avg SS")
      results = as.data.frame(output)
    }
  }
  index = min(which(abs(results$alpha-alpha)==min(abs(results$alpha-alpha))))
  vec = c(results[index,c(1,2,4)])
  thrsh = c(vec$cf, vec$threshold)
  names(thrsh) = c("futility", "superiority")

  # calculate power
  #po = seq(n_po, n_po+n_range, by = 10)
  #npo = seq(n_npo, n_npo+n_range, by = 10)

  po = seq(n_po, n_po+n_range, by = 10)
  npo = seq(n_npo, n_npo+n_range, by = 10)

  ngrid = cbind(po, npo)


  output = c()
  or_alt = c(1.6,1.5,1.5,1.4,1.4)
  for (i in 1:dim(ngrid)[1]){

    out = multiple_trial_switch(or_alt, sim_runs = ntrial, sd = 0.2,
                                pro_ctr = pro_ctr, n_po = ngrid[i,1], n_npo = ngrid[i,2],
                                U, cf = vec$cf, threshold = vec$threshold,
                                method = method)

    output = as.data.frame(rbind(output, out))
    colnames(output) = c("PET(%)", "Power(%)", "Avg SS","PO(%)", "NPO(%)")
  }


  results = list()
  index = min(which(output[,2] > power))
  results$total_sample_size_for_each_group = output[index,3]
  results$power = output[index,2]*100
  results$threshold = thrsh
  results$typeIerror = round(vec$alpha, digits = 3)
  model_sele = output[index,c(4,5)]
  names(model_sele) = c("PO(%)", "NPO(%)")
  results$model_selection = model_sele
  return(results)

}

