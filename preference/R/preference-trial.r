
check_ss <- function(pref_ss, selection_ss, treatment_ss) {
  if (!is.numeric(pref_ss) || pref_ss < 1) {
    stop("The pref_ss parameter should be a numeric value of at least 1.")
  }
  if (!is.numeric(selection_ss) || selection_ss < 1) {
    stop("The selection_ss parameter should be a numeric value of at least 1.")
  }
  if (!is.numeric(treatment_ss) || treatment_ss < 1) {
    stop("The treatment_ss parameter should be a numeric value of at least 1.")
  }
  invisible(TRUE)
}

check_effect <- function(pref_effect, selection_effect, treatment_effect) {
  if (!is.numeric(pref_effect) || length(pref_effect) != 1) {
    stop("The pref_effect parameter should be a single numeric value.")
  }
  if (!is.numeric(selection_effect) || length(selection_effect) != 1) {
    stop("The selection_effect parameter should be a single numeric value.")
  }
  if (!is.numeric(treatment_effect) || length(treatment_effect) != 1 ) {
    stop("The treatment_effect parameter should be a single numeric value.")
  }
  invisible(TRUE)
}

# Multi-strata parameters.
check_props_and_sigma2 <- function(stratum_prop, choice_prop, pref_prop, 
  sigma2) {
  if (length(stratum_prop[[1]]) != length(pref_prop[[1]])) {
    stop(paste("The stratum_prop and pref_pop parameters",
               "must have the same length."))
  }
  if (sum(stratum_prop[[1]]) != 1) {
    stop(paste("Elements of the stratum_prop parameter must sum to 1.",
               "Got a value of", sum(stratum_prop[[1]])))
  }
  if (any(!is.numeric(stratum_prop[[1]])) || any(stratum_prop[[1]] < 0) ||
      any(stratum_prop[[1]] > 1)) {
    stop("The stratum_prop parameter must be a list of numeric values in [0, 1]")
  }
  if (any(!is.numeric(choice_prop[[1]])) || any(choice_prop[[1]] < 0) ||
      any(choice_prop[[1]] > 1)) {
    stop("The choice_prop parameter must be a numeric values in [0, 1]")
  }
  if (any(!is.numeric(pref_prop[[1]])) || any(pref_prop[[1]] < 0) ||
      any(pref_prop[[1]] > 1)) {
    stop("The pref_prop parameter must be a list of numeric values in [0, 1]")
  }
  if (!is.numeric(sigma2[[1]]) || 
      length(sigma2[[1]]) != length(stratum_prop[[1]]) ||
      any(sigma2[[1]] < 0)) {
    stop(paste("The sigma2 parameter must be a list of numeric values",
               "greater than zero and the length is equal to",
               "the stratum_prop and pref_prop parameters."))
  }
  invisible(TRUE)
}

check_alpha <- function(alpha) {
  if (!is.numeric(alpha) || length(alpha) != 1 || any(alpha < 0 | alpha > 1)) {
    stop("The alpha parameter must be a single numeric value in [0, 1].")
  }
}

power_preference_trial_internal <- function(x) {
  ret <- data.frame(treatment_power=numeric(), selection_power=numeric(),
    pref_power=numeric())
  for (i in seq_len(nrow(x))) {
    pows <- overall_power(
      #sum(as.vector(x[,c("pref_ss", "selection_ss", "treatment_ss")])),
      x[,"pref_ss"],
      unlist(x$pref_prop[[i]]),
      x$sigma2[[i]],
      x$pref_effect[[i]],
      x$selection_effect[[i]],
      x$treatment_effect[[i]],
      x$alpha,
      x$choice_prop[[i]],
      x$stratum_prop[[i]],
      length(x$stratum_prop[[i]]))
    ret <- rbind(ret, pows)
  }
  class(ret) <- setdiff(class(ret), "preference.trial")
  ret
}

# Internal function for creating a single preference trial object.
preference.trial.single <- function(pref_ss, pref_effect, selection_ss, 
  selection_effect, treatment_ss, treatment_effect, sigma2, 
  pref_prop, choice_prop=0.05, stratum_prop=1, alpha=0.05, k=1) {

  check_ss(pref_ss, selection_ss, treatment_ss)
  check_effect(pref_effect, selection_effect, treatment_effect)
  check_alpha(alpha)

  if (!is.list(pref_prop)) pref_prop <- list(pref_prop)
  if (!is.list(stratum_prop)) stratum_prop <- list(stratum_prop)
  if (!is.list(sigma2)) sigma2 <- list(sigma2)

  check_props_and_sigma2(stratum_prop, choice_prop, pref_prop, sigma2)
   
  ret <- data.frame(pref_ss=pref_ss, pref_effect=pref_effect, 
    selection_ss=selection_ss, selection_effect=selection_effect, 
    treatment_ss=treatment_ss, treatment_effect=treatment_effect, 
    alpha=alpha, k=k)
  ret$pref_prop <- pref_prop
  ret$choice_prop <- choice_prop
  ret$stratum_prop <- stratum_prop
  ret$sigma2 <- sigma2
  pows <- power_preference_trial_internal(ret)
  ret$treatment_power <- pows$treatment
  ret$selection_power <- pows$selection
  ret$pref_power <- pows$preference
  class(ret) <- c("preference.trial", class(ret))
  ret
}

# Circular indexing to recycle values.
cind <- function(i, vec_len) {
  (i-1) %% vec_len + 1
}

#' Create a Preference Trial
#' 
#' @param pref_ss the sample size of the preference arm.
#' @param pref_effect the effect size of the preference arm (delta_pi). 
#' @param selection_ss the sample size of the selection arm.
#' @param selection_effect the effect size of selection arm (delta_nu).
#' @param treatment_ss the sample size of the treatment arm .
#' @param treatment_effect the sample size of the treatment arm (delta_tau)
#' @param sigma2 the variance estimate of the outcome of interest. This 
#' value should be positive numeric values. If study is stratified, should 
#' be vector of within-stratum variances with length equal to the number of 
#' strata in the study.
#' @param pref_prop the proportion of patients preferring treatment 1. This
#' value should be between 0 and 1 (phi).
#' @param choice_prop the proportion of patients assigned to choice arm in 
#' the initial randomization. Should be numeric value between
#' 0 and 1 (default=0.5) (theta).
#' @param stratum_prop xi a numeric vector of the proportion of patients in 
#' each stratum. Length of vector should equal the number of strata in the 
#' study and sum of vector should be 1. All vector elements should be numeric
#' values between 0 and 1. Default is 1 (i.e. unstratified design) (xi).
#' @param alpha the desired type I error rate (default 0.05).
#' @param k the ratio of treatment A to treatment B in the random arm
#' (default 1)..
#' @examples
#'
#' # Unstratified single trial.
#' preference.trial(pref_ss=100, pref_effect=1, selection_ss=100, 
#'   selection_effect=1, treatment_ss=100, treatment_effect=1,
#'   sigma2=1, pref_prop=0.6)
#' 
#' # Stratified single trial.
#' preference.trial(pref_ss=100, pref_effect=1, selection_ss=100,
#'   selection_effect=1, treatment_ss=100, treatment_effect=1,
#'   sigma2=list(c(1, 0.8)), pref_prop=list(c(0.6, 0.3)),
#'   choice_prop=0.5, stratum_prop=list(c(0.3, 0.7)))
#' 
#' # Multiple trials unstratified.
#' preference.trial(pref_ss=100, pref_effect=seq(0.1, 2, by=0.5), 
#'   selection_ss=100, selection_effect=1, treatment_ss=100, 
#'   treatment_effect=1, sigma2=1, pref_prop=0.6)
#' 
#' # Multiple, stratified trials.
#' preference.trial(pref_ss=100, pref_effect=seq(0.1, 2, by=0.5), 
#'   selection_ss=100, selection_effect=1, treatment_ss=100, 
#'   treatment_effect=1, sigma2=list(c(1, 0.8)), pref_prop=list(c(0.6, 0.3)), 
#'   choice_prop=0.5, stratum_prop=list(c(0.3, 0.7)))
#' 
#' @references Turner RM, et al. (2014). "Sample Size and Power When Designing
#'  a Randomized Trial for the Estimation of Treatment, Selection, and
#'  Preference Effects." \emph{Medical Decision Making}, \strong{34}:711-719.
#' (\href{https://pubmed.ncbi.nlm.nih.gov/24695962/}{PubMed})
#' @references Cameron B, Esserman D (2016). "Sample Size and Power for a
#' Stratified Doubly Randomized Preference Design." \emph{Stat Methods Med Res}.
#' (\href{https://pubmed.ncbi.nlm.nih.gov/27872194}{PubMed})
#' @export
preference.trial <- function(pref_ss, pref_effect, selection_ss, 
  selection_effect, treatment_ss, treatment_effect, sigma2, 
  pref_prop, choice_prop=0.5, stratum_prop=1, alpha=0.05, k=1) {

  # Evaluate the arguments once from the match.call return.
  args <- as.list(match.call())[-1]

  args$pref_ss <- pref_ss
  args$pref_effect <- pref_effect
  args$selection_ss <- selection_ss
  args$selection_effect  <- selection_effect 
  args$treatment_ss <- treatment_ss
  args$treatment_effect <- treatment_effect
  args$sigma2 <- sigma2
  args$pref_prop <- pref_prop

  # Default arguments are not included in match.call. Fill them in
  # manually.
  args$alpha <- alpha
  args$stratum_prop <- stratum_prop
  args$choice_prop <- choice_prop
  args$k <- k

  # Get the lengths of the arguments.
  arg_lens <- vapply(args, length, 0L)
  if (all(arg_lens == 1)) {
    preference.trial.single(pref_ss, pref_effect, selection_ss,
      selection_effect, treatment_ss, treatment_effect, sigma2,
      pref_prop, choice_prop, stratum_prop, alpha, k)
  } else {
    # Use circular to create multiple trials.
    max_arg_len <- max(arg_lens)
    if (any(max_arg_len %% arg_lens != 0)) {
      warning(paste("One argument is not a sub-multiple or multiple",
                    "of the longest argument."))
    }

    # Create a set of preference trials.
    exp_arg_list <- Map(function(x) x[cind(1:max_arg_len, length(x))], args)
    x <- as.data.frame(exp_arg_list[setdiff(names(args), 
      c("pref_prop", "choice_prop", "stratum_prop", "sigma2"))])
    x$pref_prop <- exp_arg_list$pref_prop
    x$choice_prop <- exp_arg_list$choice_prop
    x$stratum_prop <- exp_arg_list$stratum_prop
    x$sigma2 <- exp_arg_list$sigma2
    ret <- NULL
    for (i in seq_len(nrow(x))) {
      ret <- rbind(ret,
        preference.trial.single(x$pref_ss[i], x$pref_effect[i], 
                                x$selection_ss[i], x$selection_effect[i], 
                                x$treatment_ss[i], x$treatment_effect[i], 
                                x$sigma2[i], x$pref_prop[i], x$choice_prop[i], 
                                x$stratum_prop[i], x$alpha[i], x$k[i]))
    }
    ret
  }
}

#' @title Design Preference Trials with Power Constraint(s)
#'
#' @description Create a set of preference trials with specified power.
#' The power parameter guarantees that the power will be at least what is
#' specified for each of the three arms.
#'
#' @param power the desired power(s) for the trial(s)
#' @param pref_effect the effect size of the preference arm (delta_pi). 
#' @param selection_effect the effect size of selection arm (delta_nu).
#' @param treatment_effect the sample size of the treatment arm (delta_tau)
#' @param sigma2 the variance estimate of the outcome of interest. This 
#' value should be positive numeric values. If study is stratified, should 
#' be vector of within-stratum variances with length equal to the number of 
#' strata in the study.
#' @param pref_prop the proportion of patients preferring treatment 1. This
#' value should be between 0 and 1 (phi).
#' @param choice_prop the proportion of patients assigned to choice arm in 
#' the initial randomization. Should be numeric value between
#' 0 and 1 (default=0.5) (theta).
#' @param stratum_prop xi a numeric vector of the proportion of patients in 
#' each stratum. Length of vector should equal the number of strata in the 
#' study and sum of vector should be 1. All vector elements should be numeric
#' values between 0 and 1. Default is 1 (i.e. unstratified design) (xi).
#' @param alpha the desired type I error rate (default 0.05).
#' @param k the ratio of treatment A to treatment B in the random arm
#' (default 1)..
#' @examples
#' 
#' # Unstratified trials with power constraints.
#' pt_from_power(power=seq(.1, 0.8, by=0.1), pref_effect=1, selection_effect=1, 
#'   treatment_effect=1, sigma2=1, pref_prop=0.6)
#'
#' # Stratified trials with power constraints. Note that the proportion
#' # of patients in the choice arm (choice prop) is fixed for all strata.
#' pt_from_power(power=seq(0.1, 0.8, by=0.1), pref_effect=1, 
#'   selection_effect=1, treatment_effect=1,
#'   sigma2=list(c(1, 0.8)), pref_prop=list(c(0.6, 0.3)),
#'   choice_prop=0.5, stratum_prop=list(c(0.3, 0.7)))
#' 
#' # or...
#' 
#' pt_from_power(power=seq(0.1, 0.8, by=0.1), pref_effect=1, 
#'   selection_effect=1, treatment_effect=1,
#'   sigma2=c(1, 0.8), pref_prop=c(0.6, 0.3),
#'   choice_prop=0.5, stratum_prop=c(0.3, 0.7))
#' 
#' @export
pt_from_power <- function(power, pref_effect, selection_effect, 
  treatment_effect, sigma2, pref_prop, choice_prop=0.5, stratum_prop=1,
  alpha=0.05, k=1) {

  # Check the power parameter. Other parameters will be checked later.
  if(!is.numeric(power) || any(power <= 0) || any(power >= 1)) {
    stop('Power must be numeric in [0,1]')
  }

  # Use preference.trial to create the data frame. Use power as a
  # place holder. Fill it in after the object is created.
  args <- as.list(match.call())[-1]
  args$power <- power
  args$pref_effect <- pref_effect
  args$selection_effect <- selection_effect
  args$treatment_effect <- treatment_effect
  args$sigma2 <- sigma2
  args$pref_prop <- pref_prop
  args$choice_prop <- choice_prop
  args$stratum_prop <- stratum_prop
  args$alpha <- alpha
  args$k <- k
  args$pref_ss <- args$selection_ss <- args$treatment_ss <- rep(1,length(power))
  
  # Make the strata vectors lists.
  if (!is.list(args$sigma2)) {
    args$sigma2 <- list(args$sigma2)
  }
  if (!is.list(args$pref_prop)) {
    args$pref_prop <- list(args$pref_prop)
  }
  if (!is.list(args$stratum_prop)) {
    args$stratum_prop <- list(args$stratum_prop)
  }
  args <- args[names(args) != "power"]
  ret <- do.call(preference.trial, args)
  for (i in seq_len(nrow(ret))) {
    sss <- overall_sample_size_norm(
      power[cind(i, length(power))], 
      ret$pref_prop[[cind(i, length(ret$pref_prop))]],
      ret$sigma2[[cind(i, length(ret$sigma2))]], 
      ret$pref_effect[cind(i, length(ret$pref_effect))], 
      ret$selection_effect[cind(i, length(ret$selection_effect))],
      ret$treatment_effect[cind(i, length(ret$treatment_effect))],
      ret$alpha[cind(i, length(ret$alpha))], 
      ret$choice_prop[cind(i, length(ret$choice_prop))],
      ret$stratum_prop[[cind(i, length(ret$stratum_prop))]],
      length(ret$stratum_prop[[cind(i, length(ret$stratum))]]),
      ret$k[cind(i, length(ret$k))])
    ret$treatment_ss[i] <- sss$treatment[1]
    ret$pref_ss[i] <- sss$preference[1]
    ret$selection_ss[i] <- sss$selection[1]
    ret$treatment_power[i] <- power[cind(i, length(power))] 
    ret$pref_power[i] <- power[cind(i, length(power))]
    ret$selection_power[i] <- power[cind(i, length(power))]
  }
  ret
}

#' @title Design Preference Trials with Sample Size Constraint(s)
#'
#' @description Create a set of preference trials where the maximum 
#' sample size for an arm is specified.
#'
#' @param ss the maximum size of any of the three arms.
#' @param pref_effect the effect size of the preference arm (delta_pi). 
#' @param selection_effect the effect size of selection arm (delta_nu).
#' @param treatment_effect the sample size of the treatment arm (delta_tau)
#' @param sigma2 the variance estimate of the outcome of interest. This 
#' value should be positive numeric values. If study is stratified, should 
#' be vector of within-stratum variances with length equal to the number of 
#' strata in the study.
#' @param pref_prop the proportion of patients preferring treatment 1. This
#' value should be between 0 and 1 (phi).
#' @param choice_prop the proportion of patients assigned to choice arm in 
#' the initial randomization. Should be numeric value between
#' 0 and 1 (default=0.5) (theta).
#' @param stratum_prop xi a numeric vector of the proportion of patients in 
#' each stratum. Length of vector should equal the number of strata in the 
#' study and sum of vector should be 1. All vector elements should be numeric
#' values between 0 and 1. Default is 1 (i.e. unstratified design) (xi).
#' @param alpha the desired type I error rate (default 0.05)..
#' @param k the ratio of treatment A to treatment B in the random arm
#' (default 1).
#' @examples
#' 
#' # Unstratified trials with power constraints.
#' pt_from_ss(ss=seq(100, 1000, by=100), pref_effect=1, 
#'   selection_effect=1, treatment_effect=1, sigma2=1, pref_prop=0.6)
#'
#' # Stratified trials with power constraints. Note that the proportion
#' # of patients in the choice arm (choice prop) is fixed for all strata.
#' pt_from_ss(ss=seq(100, 1000, by=100), pref_effect=1, 
#'   selection_effect=1, treatment_effect=1,
#'   sigma2=list(c(1, 0.8)), pref_prop=list(c(0.6, 0.3)),
#'   choice_prop=0.5, stratum_prop=list(c(0.3, 0.7)))
#' 
#' # or...
#' 
#' pt_from_ss(ss=seq(100, 1000, by=100), pref_effect=1, 
#'   selection_effect=1, treatment_effect=1,
#'   sigma2=c(1, 0.8), pref_prop=c(0.6, 0.3),
#'   choice_prop=0.5, stratum_prop=c(0.3, 0.7))
#' 
#' @export
pt_from_ss <- function(ss, pref_effect, selection_effect, 
  treatment_effect, sigma2, pref_prop, choice_prop=0.5, stratum_prop=1,
  alpha=0.05, k=1) {

  if (!is.list(sigma2)) {
    sigma2 <- list(sigma2)
  } 
  if (!is.list(pref_prop)) {
    pref_prop <- list(pref_prop)
  }
  if (!is.list(stratum_prop)) {
    stratum_prop <- list(stratum_prop)
  }

  preference.trial(
    pref_ss=ss,
    pref_effect=force(pref_effect), 
    selection_ss=ss,
    selection_effect=selection_effect, 
    treatment_ss=ss, 
    treatment_effect=treatment_effect,
    sigma2=sigma2, 
    pref_prop=pref_prop,
    choice_prop=choice_prop,
    stratum_prop=stratum_prop,
    alpha=alpha,
    k=k)
}

