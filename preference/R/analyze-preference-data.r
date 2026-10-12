
##########################
### ANALYSIS FUNCTIONS ###
##########################

#' Fit the Preference Data Collected from a Two-stage Clinical Trial
#'
#' Computes the test statistics and p-values for the preference, 
#' selection, and treatment effects for the two-stage randomized trial using 
#' collected outcome, random, treatment, and strata values for specified
#' significance level.
#'
#' @param outcome (numeric) individual trial outcomes.
#' @param arm (character or factor) a vector of "choice" and "random" character
#' or factor values indicating the arm of the sample?
#' @param treatment (character, factor, or integer) which treatment an 
#' individual received
#' @param strata (optional integer) which strata the individual belongs to.
#' @param alpha (optional numeric) Level of significance (default=0.05)
#' @examples
#' # Unstratified
#' outcome <- c(10, 8, 6, 10, 5, 8, 7, 6, 10, 12, 11, 6, 8, 10, 5, 7, 9, 
#'              12, 6, 8, 9, 10, 7, 8,11)
#' arm <- c(rep("choice", 13), rep("random", 12))
#' treatment <- c(rep(1, 5), rep(2, 8), rep(1, 6), rep(2, 6))
#' fit_preference(outcome, arm, treatment)
#' 
#' # Stratified
#' # Same data plus strata information.
#' strata <- c(1,1,2,2,2,1,1,1,1,2,2,2,2,1,1,1,2,2,2,1,1,1,2,2,2)
#' fit_preference(outcome, arm, treatment, strata, alpha=0.1)
#' @importFrom stats var
#' @export
fit_preference <- function(outcome, arm, treatment, strata, alpha=0.05) {
  if (!isTRUE(all(as.character(unique(arm)) %in% c("choice", "random")))) {
    stop("arm parameter values must be either \"choice\" or \"random\".")
  }
  if (missing(strata)) {
    strata <- rep(1, length(outcome))
  }
  
  if (length(unique(treatment)) != 2) {
    stop("You may only have two treatments.")
  }
  
  pd <- data.frame(outcome=outcome, arm=arm, treatment=treatment,
                   strata=strata)
  # Compute unstratified test statistics
  strat_split <- split(seq_len(length(strata)), strata)
  treatments <- sort(unique(treatment)) 
  #initialize the vectors to create for the fit_preference_summary function
  x1mean <- NULL
  x1var <- NULL
  m1 <- NULL
  x2mean <- NULL
  x2var <- NULL
  m2 <- NULL
  y1mean <- NULL
  y1var <- NULL
  n1 <- NULL
  y2mean <- NULL
  y2var <- NULL
  n2 <- NULL
  
  for (ss in strat_split) { 
    pds <- pd[ss , ]
    x1mean <- c(x1mean, 
      mean(pds$outcome[pds$arm == "choice" & pds$treatment == treatments[1]]))
    x1var <- c(x1var, 
      var(pds$outcome[pds$arm == "choice" &pds$treatment == treatments[1]])) 
    m1 <- c(m1, 
      length(pds$outcome[pds$arm == "choice" & pds$treatment == treatments[1]]))
    x2mean <- c(x2mean,  
      mean(pds$outcome[pds$arm == "choice" & pds$treatment == treatments[2]]))
    x2var <- c(x2var, 
      var(pds$outcome[pds$arm == "choice" & pds$treatment == treatments[2]]))
    m2 <- c(m2, 
      length(pds$outcome[pds$arm == "choice" & pds$treatment == treatments[2]]))
    y1mean <- c(y1mean, 
      mean(pds$outcome[pds$arm == "random" & pds$treatment == treatments[1]]))
    y1var <- c(y1var, 
      var(pds$outcome[pds$arm == "random" & pds$treatment == treatments[1]]))
    n1 <- c(n1, 
      length(pds$outcome[pds$arm == "random" & pds$treatment == treatments[1]]))
    y2mean <- c(y2mean, 
      mean(pds$outcome[pds$arm == "random" & pds$treatment == treatments[2]]))
    y2var <- c(y2var, 
      var(pds$outcome[pds$arm == "random" & pds$treatment == treatments[2]]))
    n2 <- c(n2, 
      length(pds$outcome[pds$arm == "random" & pds$treatment == treatments[2]]))
  }
  
  #calculate xi
  xi <- table(strata) / length(outcome)
  nstrata <- length(unique(strata))

  results <- fit_preference_summary(x1mean = x1mean, x1var = x1var, m1 = m1, 
    x2mean = x2mean, x2var = x2var, m2 = m2, y1mean = y1mean, y1var = y1var, 
    n1 = n1, y2mean = y2mean, y2var = y2var, n2 = n2, xi = xi, 
    nstrata = nstrata, alpha = alpha)
  return(results)
}

### T-test from summary data (null hypothesis of no difference, no assumption 
# of equal variances)
# m1,m1: sample means
# s1,s2: sample variances
# n1, n2: sample sizes

#' @importFrom stats pt
t.test2 <- function(m1, m2, s1, s2, n1, n2) {
  se <- sqrt( (s1/n1) + (s2/n2) )
  
  # Welch-satterthwaite df
  df <- ( (s1/n1 + s2/n2)^2 )/( (s1/n1)^2/(n1-1) + (s2/n2)^2/(n2-1) )
  
  t <- (m1 - m2)/se 
  
  dat <- data.frame(m1-m2, se, t, 2*pt(-abs(t),df))    
  
  names(dat) <- c("Mean.Diff", "Std.Err", "t", "p.value")
  
  dat
}

### Analysis Function (Summary Data)
#' @importFrom stats qnorm
unstrat_analyze_summary_data <- function(x1mean, x1var, m1, x2mean, x2var, m2, 
                                         y1mean, y1var, n1, y2mean, y2var, n2, 
                                         alpha) {
  # Define sample sizes
  m <- m1 + m2
  n <- n1 + n2
  N <- m + n
  
  # Calculate z values as defined by Rucker
  z1 <- m1*x1mean - m1*y1mean
  z2 <- m2*x2mean - m2*y2mean

  # Calculate variance components (formulas from Rucker paper)
  var1 <- m1 * x1var + 
    (1 + ((m - 1)/m)*m1)*m1*(y1var/n1) + (m1*m2/m)*(x1mean - y1mean)^2
  
  var2 <- m2 * x2var + 
    (1 + ((m - 1)/m)*m2)*m2*(y2var/n2) + (m1*m2/m)*(x2mean - y2mean)^2
  
  cov <- -(m1*m2/m)*(x1mean - y1mean)*(x2mean - y2mean)
  
  #Call the test2 function
  treat_out <- t.test2(y1mean, y2mean, y1var, y2var, n1, n2)
  
  # Compute effect estimates 
  pref_effect <- 0.5*(m/(m1*m2))*(z1+z2)
  sel_effect <- 0.5*(m/(m1*m2))*(z1-z2)
  treat_effect <- treat_out$Mean.Diff
  
  #Compute SE estimates 
  pref_SE <- sqrt(var1 + var2 + 2*cov)*0.5*(m/(m1*m2))
  sel_SE <- sqrt(var1 + var2 - 2*cov)*0.5*(m/(m1*m2))
  treat_SE <- treat_out$Std.Err
  
  # Compute test statistics 
  pref_test <- pref_effect/pref_SE 
  sel_test <- sel_effect/sel_SE 
  treat_test <- treat_out$t
  
  # Compute p-values (Assume test stats approximately normally distributed)
  pref_pval <- pnorm(abs(pref_test), lower.tail = FALSE)*2 # Preference effect
  sel_pval <- pnorm(abs(sel_test), lower.tail = FALSE)*2 # Selection effect
  treat_pval <- treat_out$p.value
  
  #Compute approximate (1-alpha)% confidence intervals
  zalpha <- qnorm(1 - (alpha/2))
  pref_LB <- pref_effect - zalpha*pref_SE
  pref_UB <- pref_effect + zalpha*pref_SE
  sel_LB <- sel_effect - zalpha*sel_SE
  sel_UB <- sel_effect + zalpha*sel_SE
  treat_LB <- treat_effect - zalpha*treat_SE
  treat_UB <- treat_effect + zalpha*treat_SE
  data.frame(pref_effect, pref_SE, pref_test, pref_pval,pref_LB, pref_UB,
             sel_effect, sel_SE, sel_test, sel_pval, sel_LB, sel_UB,
             treat_effect, treat_SE,  treat_test, treat_pval, treat_LB, 
             treat_UB)
}


#' Fit Preference Model from Summary Data
#' 
#' Computes the test statistics and p-values for the preference, 
#' selection, and treatment effects in a two-stage randomized trial using 
#' summary data.
#' 
#' @param x1mean mean of responses for patients choosing treatment 1. If study
#'   is stratified, should be vector with length equal to the
#'   number of strata.
#' @param x1var variance of responses for patients choosing treatment 1. If 
#'   study is stratified, should be vector with length equal to the
#'   number of strata.
#' @param m1 number of patients choosing treatment 1. If study
#'   is stratified, should be vector with length equal to the
#'   number of strata.
#' @param x2mean mean of responses for patients choosing treatment 2. If study
#'   is stratified, should be vector with length equal to the
#'   number of strata.
#' @param x2var variance of responses for patients choosing treatment 2. If 
#'   study is stratified, should be vector with length equal to the
#'   number of strata.
#' @param m2 number of patients choosing treatment 2. If study
#'   is stratified, should be vector with length equal to the
#'   number of strata.
#' @param y1mean mean of responses for patients randomized to treatment 1. If 
#'   study is stratified, should be vector with length equal to the
#'   number of strata.
#' @param y1var variance of responses for patients randomized to treatment 1. 
#'   If study is stratified, should be vector with length equal to the
#'   number of strata.
#' @param n1 number of patients randomized to treatment 1. If study is 
#'   stratified, should be vector with length equal to the number of 
#'   strata.
#' @param y2mean mean of responses for patients randomized to treatment 2. If 
#'   study is stratified, should be vector with length equal to the
#'   number of strata.
#' @param y2var variance of responses for patients randomized to treatment 2. 
#'   If study is stratified, should be vector with length equal to the
#'   number of strata.
#' @param n2 number of patients randomized to treatment 2. If study is 
#'   stratified, should be vector with length equal to the number of 
#'   strata.
#' @param xi a numeric vector of the proportion of patients in each stratum. 
#'   Length of vector should equal the number of strata in the study and 
#'   sum of vector should be 1. All vector elements should be numeric 
#'   values between 0 and 1. Default is 1 (i.e. unstratified design).
#' @param nstrata number of strata. Default is 1 (i.e. unstratified design).
#' @param alpha Type I error rate, used to determine confidence interval level 
#'   for the effect estimates. Default is 0.05 (i.e. 95\% confidence interval)
#' @examples
#' # Unstratified
#' 
#' x1mean <- 5
#' x1var <- 1
#' m1 <- 15
#' x2mean <- 7
#' x2var <- 1.1
#' m2 <- 35
#' y1mean <- 6
#' y1var <- 1
#' n1 <- 25
#' y2mean <- 8
#' y2var <- 1.2
#' n2 <- 25
#' fit_preference_summary(x1mean, x2var, m1, x2mean, x2var, m2, y1mean, y1var,
#'                n1, y2mean, y2var, n2)
#' 
#' # Stratified
#'
#' x1mean <- c(5, 3)
#' x1var <- c(1, 1)
#' m1 <- c(15, 30)
#' x2mean <- c(7, 7)
#' x2var <- c(1.1, 3.1)
#' m2 <- c(35, 40)
#' y1mean <- c(6, 4)
#' y1var <- c(1, 2)
#' n1 <- c(25, 35)
#' y2mean <- c(8, 12)
#' y2var <- c(1.2, 1)
#' n2 <- c(25, 20)
#' fit_preference_summary(x1mean, x2var, m1, x2mean, x2var, m2, y1mean, y1var,
#'                        n1, y2mean, y2var, n2, alpha=0.1)
#' @references Rucker G (1989). "A two-stage trial design for testing treatment,
#' self-selection and treatment preference effects." \emph{Stat Med}, 
#' \strong{8}(4):477-485. 
#' (\href{https://pubmed.ncbi.nlm.nih.gov/2727471}{PubMed})
#' @references Cameron B, Esserman D (2016). "Sample Size and Power for a 
#' Stratified Doubly Randomized Preference Design." 
#' \emph{Stat Methods Med Res}. 
#' (\href{https://pubmed.ncbi.nlm.nih.gov/27872194}{PubMed})
#' @importFrom stats pnorm
#' @export
fit_preference_summary <- function(x1mean, x1var, m1, x2mean, x2var, m2, y1mean,
                                   y1var, n1, y2mean, y2var, n2, xi=1, 
                                   nstrata=1, alpha=0.05) {
  
  # Compute unstratified test statistics
  unstrat_stats <- Reduce(
    cbind,
    Map(
      function(i) {
        t(unstrat_analyze_summary_data(x1mean[i], x1var[i], m1[i], x2mean[i], 
                                       x2var[i], m2[i], y1mean[i], y1var[i], 
                                       n1[i], y2mean[i], y2var[i], n2[i], 
                                       alpha))
      }, seq_len(nstrata)))
    
  #Calculate the overall effect estimate
  overall_pref_effect <- sum(
    vapply(seq_len(nstrata), 
           function(i) xi[i] * unlist(unstrat_stats[1, i]), 
           0.0))
  
  overall_sel_effect <- sum(
    vapply(seq_len(nstrata), 
           function(i) xi[i] * unlist(unstrat_stats[7, i]), 
           0.0))
  
  overall_treat_effect <- sum(
    vapply(seq_len(nstrata),
           function(i) xi[i] * unlist(unstrat_stats[13, i]), 
           0.0))
  
  #Calculate the overall SE
  overall_pref_SE <- sqrt(sum(
    vapply(seq_len(nstrata), 
           function(i) xi[i]^2 * unlist(unstrat_stats[2, i])^2, 
           0.0)))
  
  overall_sel_SE <- sqrt(sum(
    vapply(seq_len(nstrata), 
           function(i) xi[i]^2 * unlist(unstrat_stats[8, i])^2, 
           0.0)))
  
  overall_treat_SE <- sqrt(sum(
    vapply(seq_len(nstrata),
           function(i) xi[i]^2 * unlist(unstrat_stats[14, i])^2, 
           0.0)))
  
  #Calculate overall test statistic
  overall_pref_test <- overall_pref_effect / overall_pref_SE
  
  overall_sel_test <- overall_sel_effect / overall_sel_SE
  
  overall_treat_test <- overall_treat_effect / overall_treat_SE
  
  # Compute p-values (Assume test stats approximately normally distributed)
  
  # preference effect
  overall_pref_pval <- 2 * pnorm(abs(overall_pref_test), lower.tail = FALSE)
  
  # selection effect
  overall_sel_pval <- 2 * pnorm(abs(overall_sel_test), lower.tail = FALSE) 
  
  # treatment effect
  overall_treat_pval <- 2 * pnorm(abs(overall_treat_test), lower.tail = FALSE)
  
  #Compute the upper and lower bounds of the confidence interval
  
  zalpha <- qnorm(1-(alpha/2))
  
  overall_pref_LB <- overall_pref_effect - zalpha * overall_pref_SE
  
  overall_pref_UB <- overall_pref_effect + zalpha * overall_pref_SE
  
  overall_sel_LB <- overall_sel_effect - zalpha * overall_sel_SE
  
  overall_sel_UB <- overall_sel_effect + zalpha * overall_sel_SE
  
  overall_treat_LB <- overall_treat_effect - zalpha * overall_treat_SE
  
  overall_treat_UB <- overall_treat_effect + zalpha * overall_treat_SE
  
  overall_stats<-data.frame(
    overall_pref_effect = overall_pref_effect, 
    overall_pref_SE = overall_pref_SE, 
    overall_pref_test = overall_pref_test,
    overall_pref_pval = overall_pref_pval, 
    overall_pref_LB = overall_pref_LB, 
    overall_pref_UB = overall_pref_UB,
    overall_sel_effect = overall_sel_effect, 
    overall_sel_SE = overall_sel_SE, 
    overall_sel_test = overall_sel_test,
    overall_sel_pval = overall_sel_pval,  
    overall_sel_LB = overall_sel_LB, 
    overall_sel_UB = overall_sel_UB,
    overall_treat_effect = overall_treat_effect, 
    overall_treat_SE = overall_treat_SE, 
    overall_treat_test = overall_treat_test,
    overall_treat_pval = overall_treat_pval, 
    overall_treat_LB = overall_treat_LB, 
    overall_treat_UB = overall_treat_UB)
  
  ret <- list(alpha = alpha, unstratified_statistics = unstrat_stats, 
              overall_statistics = overall_stats) 
  class(ret) <- c("preference.fit")
  ret
}

#' @export
summary.preference.fit <- function(object, ...) {
  ret <- list(alpha=pf$alpha)
  pf <- pf$overall_statistics
  retm <- matrix(c(
    pf$overall_treat_effect, pf$overall_pref_effect, pf$overall_sel_effect,
    pf$overall_treat_SE, pf$overall_pref_SE, pf$overall_sel_SE,
    pf$overall_treat_test, pf$overall_pref_test, pf$overall_sel_test,
    pf$overall_treat_pval, pf$overall_pref_pval, pf$overall_sel_pval),
    nrow=3, ncol=4, 
    dimnames=list(c("Treatment", "Preference", "Selection"),
                  c("Overall Effect", "Std. Error", "z value", "Pr(>|z|)")))
  ret$overall_effects <- retm
  class(ret) <- "preference.fit.summary"
  ret
}

#' @export
print.preference.fit.summary <- function(x, ...) {
  cat("\nSignificance\n\n")
  cat("alpha: ", x$alpha, "\n\n")
  cat("Overall Effects:\n\n")
  print(x$overall_effects)
}

#' @title Fit Preference Data Collected from a Two-stage Clinical Trial
#'
#' @description The variables in the formula should reference columns in the
#' data parameter and should have the following characteristics.
#' \itemize{
#'   \item{outcome: }{Numeric values giving the outcome of interest.}
#'   \item{treatment: }{Character, categorical, or integer values denoting the 
#'                      treatment received by an individual.}
#'   \item{random: }{Logical value indicating whether the sample was from the
#'                   random arm (TRUE) or choice (FALSE).}
#'   \item{strata: }{An optional integer value denoting which strata 
#'         individuals belong to.}
#' }
#'
#' @param form a formula of the form outcome ~ treatment:arm \{| strata\}.
#' @param data a data.frame containing variables specified in the formula. It 
#' should be noted that the arm values must be either "choice" or "random".
#' @param alpha (optional numeric) Level of significance (default 0.05)
#' @importFrom stats terms
#' @examples
#' 
#' # Unstratified
#' 
#' outcome <- c(10, 8, 6, 10, 5, 8, 7, 6, 10, 12, 11, 6, 8, 10, 5, 7, 9, 
#'              12, 6, 8, 9, 10, 7, 8, 11)
#' arm <- c(rep("choice", 13), rep("random", 12))
#' treatment <- c(rep(1, 5), rep(2, 8), rep(1, 6), rep(2, 6))
#' d <- data.frame(outcome=outcome, treatment=treatment, arm=arm)
#' preference(outcome ~ treatment:arm, d)
#' 
#' # Stratified
#' random <- c(rep(FALSE, 13), rep(TRUE, 12))
#' treatment <- c(rep(1, 5), rep(2, 8), rep(1, 6), rep(2, 6))
#' strata <- c(1,1,2,2,2,1,1,1,1,2,2,2,2,1,1,1,2,2,2,1,1,1,2,2,2)
#' d <- data.frame(outcome=outcome, treatment=treatment, arm=arm, 
#'                 strata=strata)
#' preference(outcome ~ treatment:arm|strata, d, alpha=0.1)
#' 
#' @export
preference <- function(form, data, alpha=0.05) {

  outcome_var <- as.character(terms(form)[[2]])
  rhs <- as.character(terms(form)[[3]])
  if (rhs[1] == "|") {
    # It's stratified.
    strat_var <- rhs[3]
    interact <- unlist(strsplit(rhs[2], ":"))
    treatment_var <- interact[1]
    arm_var <- interact[2]
  } else if (rhs[1] == ":") {
    treatment_var <- rhs[2]
    arm_var <- rhs[3]
    strat_var <- NULL
  } else {
    stop(paste0("Formula argument should be of the form outcome ~ treatment:",
                "arm {|strata}."))
  }
  if (!is.null(strat_var)) {
    fit_preference(data[, outcome_var], 
                   data[, arm_var],
                   data[, treatment_var],
                   data[, strat_var], 
                   alpha=alpha)
  } else {
     fit_preference(data[, outcome_var], 
                    data[, arm_var],
                    data[, treatment_var],
                    alpha=alpha)
  }
}

