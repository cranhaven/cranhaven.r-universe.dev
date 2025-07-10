#' Generate operating characteristics for Bayesian two-stage trial design
#' of ordinal endpoints without proportional odds assumption.
#'
#' @description
#' Obtain operating characteristics (OC) of the Bayesian two-stage trial
#' design with ordinal endpoints while the proportional odds assumption are
#' violated.
#'
#' @param alpha the desired type I error to be controlled.
#' @param pro_ctr distribution of clinical categories for the
#' control group.
#' @param U the desirability of each outcome level.
#' @param ors a user-defined matrix, each row denotes the various scenarios,
#' the number of columns depend on the number of outcome scales.
#' @param n_range the additional sample size for each arm each stage after n_po, n_npo.
#' @param fixed_es fixed effect size when simulate the OC for various sample
#' size.
#' @param n_po sample size for the treatment and control groups, at each stage
#' based on PO model.
#' @param n_npo sample size for the treatment and control groups, at each stage
#' based on NPO model.
#' @param ntrial the number of simulated trials.
#' @param method whether the statistical test for interim/final analysis is Bayesian or
#' Frequentist. method = "Frequentist" for Frequentist approach; method = "Bayesian"
#' for Bayesian approach.
#'
#' @details
#'   Grid search of sample size is used for guarantee a desirable type I error rate.
#'   The upper limitation is 200, and lower limitation default is sample size 50
#'   for the control and treatment groups at each stage. Default increment of the
#'   sequence is 10.
#'
#'   For the parameter estimation section, we have two options, and can be selected using
#'   the method argument. Two following options are available: (i) method = "Frequentist",
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
#'   in terms of effect size. If user specifies the value of n_range and fixed_es, function
#'   will calculate the design's power in terms of sample size, and n_range is the upper
#'   limitation of sample size for the treatment and control groups at each stage, the lower
#'   limitation is 50, the default increment of the sequence is 10.
#'
#'   Arguments n_po and n_npo are the estimated sample size for the treatment and
#'   control groups at each stage based on PO model and NPO model respectively. Users can
#'   obtained them through function ss_po and ss_npo.
#'
#'
#' @return get_oc_NPO() returns the operating characteristics of design as a
#' table, including (1) user-defined value, either sample size or effect size
#' (2) corresponding power (3) average sample size
#' @export
#'
#' @examples
#'
#' \donttest{
#'
#'get_oc_Switch(alpha = 0.05, pro_ctr = c(0.58,0.05,0.17,0.03,0.04,0.13),
#'              U = c(100,80,65,25,10,0), n_range = 10, fixed_es = c(1.5,1.5,1,1,1),
#'              n_po = 475,n_npo = 75, ntrial = 5, method = "Frequentist")
#'
#'
#' or2 = matrix(rep(seq(1,1.3, by=0.1), times=1, each=3),ncol = 3,byrow = TRUE)
#' or1 = matrix(rep(1.5, dim(or2)[1]*2), ncol = 2, byrow = TRUE)
#' ors = cbind(or1, or2)
#'
#' get_oc_Switch(alpha = 0.05, pro_ctr = c(0.58,0.05,0.17,0.03,0.04,0.13),
#'               U = c(100,80,65,25,10,0), ors, n_po = 475, n_npo = 75,
#'               ntrial = 5, method = "Frequentist")
#'               }


get_oc_Switch = function(alpha, pro_ctr, U, ors, n_range, fixed_es,
                          n_po, n_npo, ntrial, method){



  if (missing(n_range))
    n_range = NULL

  if (missing(ors))
    ors = NULL

  if (missing(fixed_es))
    fixed_es = NULL

  if (is.numeric(n_range) & is.numeric(ors)) {
    stop("n_range and ors can not be specified at the same time.")
  }

  #if (is.numeric(fixed_es) & is.numeric(fixed_ss)) {
  #  stop("fixed_es and fixed_ss can not be specified at the same time.")
  #}

  # grid search
  or = rep(1,length(pro_ctr)-1)
  cf_grid = seq(0.7, 0.7, by = 0.1)
  threshold_grid = seq(0.9, 0.9, by = 0.1)
  output = c()

  for (cf in cf_grid){
    for (threshold in threshold_grid){
      out = multiple_trial_switch(or, sim_runs = ntrial, sd=0.2, pro_ctr,
                                  n_po=n_po, n_npo=n_npo,
                                  U, cf, threshold, method = method)
      rr = c(cf, threshold, out)
      output = rbind(output, rr)
      colnames(output) = c("cf", "threshold", "PET(%)", "alpha", "Avg SS", "PO(%)", "NPO(%)")
      results = as.data.frame(output)
    }
  }
  index = min(which(abs(results$alpha-alpha)==min(abs(results$alpha-alpha))))
  vec = c(results[index,c(1,2,4)])
  thrsh = c(vec$cf, vec$threshold)
  names(thrsh) = c("futility", "superiority")


  output = c()

  if(is.numeric(fixed_es)&is.numeric(n_range)){

    po = seq(n_po, n_po+n_range, by = 10)
    npo = seq(n_npo, n_npo+n_range, by = 10)

    ngrid = cbind(po, npo)

    for (i in 1:dim(ngrid)[1]){
      out = multiple_trial_switch(or = fixed_es, sim_runs = ntrial, sd=0.2, pro_ctr,
                                  n_po = ngrid[i,1], n_npo = ngrid[i,2], U,
                                  #cf =0.6, threshold = 0.9,
                                  cf = vec$cf, threshold = vec$threshold,
                                  method = method)
      output = rbind(output, out)
      colnames(output) = c("PET(%)", "Power(%)", "Avg SS", "PO(%)", "NPO(%)")

    }
    output[,2] = output[,2]*100
  }else if(is.numeric(ors)){

    for (i in 1:dim(ors)[1]){
      or = ors[i,]

      p2 = pro_trt_cal(or, pro_ctr)
      dif_utility = round(mean_u(U,pro_ctr,p2)[3],digits = 2)

      out = multiple_trial_switch(or, sim_runs = ntrial, sd=0.2, pro_ctr, n_po, n_npo,
                                  U, cf = vec$cf, threshold = vec$threshold,
                                  method = method)
      rr = c(dif_utility, out)
      output = rbind(output, rr)
      colnames(output) = c("Effect Size",  "PET(%)", "Power(%)", "Avg SS", "PO(%)", "NPO(%)")
    }

    output[,3] = output[,3]*100
  }

  output = as.data.frame(output)
  rownames(output) = paste0("Scenario ", 1:dim(output)[1])
  results = list()
  results$design = round(output,digits = 2)
  results$threshold = thrsh
  results$typeIerror = round(vec$alpha,digits = 2)
  #results$model_selection = c(output$`PO(%)`, output$`NPO(%)`)
  return(results)

}
