#' Overall Sample Size
#'
#' Calculates the sample size required to detect a given set of effects 
#' in a two-stage randomized clinical trial. Returns the largest
#' of the required sample sizes for a given set of treatment, selection, and 
#' preference effects.
#'
#' @param power desired study power. Should be numeric value between 0 and 1.
#' @param phi the proportion of patients preferring treatment 1. Should be
#'            numeric value between 0 and 1. If study is stratified, should be
#'            vector with length equal to the number of strata in the study.
#' @param sigma2 variance estimate. Should be positive numeric values. If study
#'               is stratified, should be vector of within-stratum variances 
#'               with length equal to the number of strata in the study.
#' @param delta_pi overall study preference effect.
#' @param delta_nu overall study selection effect.
#' @param delta_tau overall study treatment effect.
#' @param alpha desired type I error rate.
#' @param theta proportion of patients assigned to choice arm in the initial
#'        randomization. Should be numeric value between
#'        0 and 1 (default=0.5).
#' @param xi a numeric vector of the proportion of patients in each stratum. 
#'        Length of vector should equal the number of strata in the study and 
#'        sum of vector should be 1. All vector elements should be numeric 
#'        values between 0 and 1. Default is 1 (i.e. unstratified design).
#' @param nstrata number of strata. Default is 1 (i.e. unstratified design).
#' @param k the ratio of treatment A to treatment B in the random arm. 
#'        (default 1, i.e. equal distribution to the two treatments in the 
#'        random arm)
#' @keywords internal
#' @references Turner RM, et al. (2014). "Sample Size and Power When Designing
#'  a Randomized Trial for the Estimation of Treatment, Selection, and 
#'  Preference Effects." \emph{Medical Decision Making}, \strong{34}:711-719.
#' (\href{https://pubmed.ncbi.nlm.nih.gov/24695962}{PubMed})
#' @references Cameron B, Esserman D (2016). "Sample Size and Power for a 
#' Stratified Doubly Randomized Preference Design." 
#' \emph{Stat Methods Med Res}. 
#' (\href{https://pubmed.ncbi.nlm.nih.gov/27872194}{PubMed})
overall_sample_size <- function(power, phi, delta_pi, delta_nu, 
  delta_tau, sigma2, alpha=0.05, theta=0.5, xi=1, nstrata=1, k=1, dist="norm") {
  ## Write this
  if (dist == "norm") {
    overall_sample_size_norm(power, phi, sigma2, delta_pi, delta_nu,
                             delta_tau, alpha, theta, xi, nstrata, k)
  } else {
    stop('Unsupported "dist" argument.')
  }
}

overall_sample_size_norm <- function(power, phi, sigma2, delta_pi, delta_nu, 
  delta_tau, alpha=0.05, theta=0.5, xi=1, nstrata=1, k=1) {
  
  zbeta <- qnorm(power)
  zalpha <- qnorm(1-(alpha/2))
  
  # selection sample size  
  sel_terms <- vapply(seq_len(nstrata), 
    function(x) {
      (xi[x]/(phi[x]^2*(1-phi[x])^2)) *
      (sigma2[x]+phi[x]*(1-phi[x])*((2*phi[x]-1)*delta_nu+delta_pi)^2 +
      2*(theta/(1-theta))*sigma2[x]*(phi[x]^2+(1-phi[x])^2))
    }, 0.0)
  sel_sum_total <- sum(sel_terms)
  sel_N <- ceiling((zalpha+zbeta)^2/(4*theta*delta_nu^2)*sel_sum_total)
  
  #preference sample size
  pref_terms <- vapply(seq_len(nstrata), 
    function(x) {
      (xi[x] / (phi[x]^2 * (1-phi[x])^2)) *
        (sigma2[x] + phi[x] * (1-phi[x]) * ((2*phi[x]-1)*delta_pi+delta_nu)^2 +
        2*(theta/(1-theta)) * sigma2[x]*(phi[x]^2+(1-phi[x])^2))
    }, 0.0)
  pref_sum_total <- sum(pref_terms)
  pref_N <- ceiling((zalpha+zbeta)^2 / (4*theta*delta_pi^2) * pref_sum_total)

  #Treatment sample size
  
  treat_terms <- vapply(seq_len(nstrata), function(x) xi[x]*sigma2[x], 0.0)
  treat_sum_total <- sum(treat_terms)
  treat_N <- ceiling(((k+1)^2 / (4*k)) * 4 * (zalpha+zbeta)^2 / 
    ((1-theta)*delta_tau^2) * treat_sum_total)
  
  data.frame(treatment=treat_N, selection=sel_N, preference=pref_N) 
}

#' Power Calculation from Sample Size
#'
#' Calculates the study power to detect a set of effects given a particular 
#' sample size in a two-stage randomized clinical trial
#'
#' @param N overall study sample size.
#' @param phi the proportion of patients preferring treatment 1. Should be
#'            numeric value between 0 and 1. If study is stratified, should be
#'            vector with length equal to the number of strata in the study.
#' @param sigma2 variance estimate. Should be positive numeric values. If study
#'               is stratified, should be vector of within-stratum variances 
#'               with length equal to the number of strata in the study.
#' @param delta_pi overall study preference effect.
#' @param delta_nu overall study selection effect.
#' @param delta_tau overall study treatment effect.
#' @param alpha desired type I error rate.
#' @param theta proportion of patients assigned to choice arm in the initial
#'              randomization. Should be numeric value between
#'              0 and 1 (default=0.5).
#' @param xi a numeric vector of the proportion of patients in each stratum. 
#'          Length of vector should equal the number of strata in the study and 
#'          sum of vector should be 1. All vector elements should be numeric 
#'          values between 0 and 1. Default is 1 (i.e. unstratified design).
#' @param nstrata number of strata. Default is 1 (i.e. unstratified design).
#' @param k the ratio of treatment A to treatment B in the random arm
#' (default 1).
#' @keywords internal
#' @importFrom stats pnorm
#' @references Turner RM, et al. (2014). "Sample Size and Power When Designing
#'  a Randomized Trial for the Estimation of Treatment, Selection, and 
#'  Preference Effects." \emph{Medical Decision Making}, \strong{34}:711-719.
#' (\href{https://pubmed.ncbi.nlm.nih.gov/24695962}{PubMed})
#' @references Cameron B, Esserman D (2016). "Sample Size and Power for a 
#' Stratified Doubly Randomized Preference Design." \emph{Stat Methods Med Res}.
#' (\href{https://pubmed.ncbi.nlm.nih.gov/27872194}{PubMed})
overall_power <- function(N, phi, sigma2, delta_pi, delta_nu, delta_tau, 
                          alpha=0.05, theta=0.5, xi=1, nstrata=1, k=1) {
  
  zalpha <- qnorm(1-(alpha/2))
  # Calculate study power for treatment effect
  
  treat_strata_terms <- vapply(seq_len(nstrata),
                         function(i) xi[i] * sigma2[i],
                         0.0)
  N_k <- (N*4*k)/(k+1)^2

  trt_pwr <- pnorm( sqrt( ((1-theta)*delta_tau^2*N_k) / 
    (4*sum(treat_strata_terms)) ) - zalpha )
  
  #Calculate study power for preference effect
  pref_strata_terms <- vapply(seq_len(nstrata), 
    function(x) {
      ( xi[x] / (phi[x]^2*(1-phi[x])^2) ) * 
        ( sigma2[x] + phi[x]*(1-phi[x])*
        ( (2*phi[x]-1)*delta_pi+delta_nu )^2 +
        2* (theta / (1-theta)) *sigma2[x] * ( phi[x]^2+(1-phi[x])^2 ) )
    }, 0.0)

  pref_sum_total <- sum(pref_strata_terms)

  pref_pwr <- pnorm( sqrt( (4*theta*delta_pi^2*N) / pref_sum_total ) - zalpha )
  
  #Calcualte study power for preference effect
  sel_strata_terms <- vapply(seq_len(nstrata), 
    function(x) {
      ( xi[x] / (phi[x]^2 *(1 - phi[x])^2) ) *
        ( sigma2[x] + phi[x] *(1 - phi[x]) * 
        ( (2 * phi[x] - 1) * delta_nu + delta_pi )^2 +
        2 * (theta / (1-theta) ) * sigma2[x] * (phi[x]^2 + (1-phi[x])^2) )
    }, 0.0)
  sel_sum_total <- sum(sel_strata_terms)
  sel_pwr <- pnorm(sqrt((4*theta*delta_nu^2*N)/(sel_sum_total))-zalpha)
  
  data.frame(treatment = trt_pwr, selection = sel_pwr, preference = pref_pwr)
}



######################
### MISC FUNCTIONS ###
######################

#' Treatment Effect Back Calculation
#' 
#' Calculates the treatment effect that can be detected given a desired study 
#' power and overall study sample size for the two-stage randomized design
#' 
#' @param N overall study sample size.
#' @param power desired study power. Should be numeric value between 0 and 1.
#' @param sigma2 variance estimate. Should be positive numeric values. If study
#'               is stratified, should be vector of within-stratum variances 
#'               with length equal to the number of strata in the study.
#' @param alpha desired type I error rate.
#' @param theta proportion of patients assigned to choice arm in the initial
#'              randomization. Should be numeric value between
#'              0 and 1 (default=0.5).
#' @param xi a numeric vector of the proportion of patients in each stratum. 
#'          Length of vector should equal the number of strata in the study and 
#'          sum of vector should be 1. All vector elements should be numeric 
#'          values between 0 and 1. Default is 1 (i.e. unstratified design).
#' @param nstrata number of strata. Default is 1 (i.e. unstratified design).
#' @examples
#' treatment_effect_size(N=300, power=0.9, sigma2=c(1,0.8), xi=c(0.3,0.7), 
#'                       nstrata=2)
#' @importFrom stats qnorm
#' @export
treatment_effect_size <- function(N, power, sigma2, alpha=0.05, theta=0.5, xi=1,
                                  nstrata=1) {
  # Error messages
  if( N < 0 | !is.numeric(N) | length(N) != 1) {
    stop('N must be a single positive numeric value')
  }

  if( power < 0 | power > 1 | !is.numeric(power) || length(power) != 1) {
    stop('Power must be single numeric value in [0,1]')
  }

  if(length(sigma2) != nstrata) {
    stop('Length of variance vector does not match number of strata')
  }

  if(any(sigma2<=0) | any(!is.numeric(sigma2))) {
    stop('Variance estimate must be numeric value greater than 0')
  }

  if( alpha < 0 | alpha > 1 | !is.numeric(alpha) || length(alpha) != 1 ) {
    stop('Type I error rate must be alpha numeric in [0,1]')
  }

  if(length(theta) != 1 || theta < 0 | theta > 1 | !is.numeric(theta)) {
    stop('Theta must be single numeric in [0,1]')
  }

  if(any(!is.numeric(xi)) | any(xi < 0) | any(xi > 1)) {
    stop('Proportion of patients in strata must be numeric value in [0,1]')
  }

  if (length(xi) != nstrata) {
    stop('Length of vector does not match number of strata')
  }

  if (sum(xi) != 1) {
    stop('Stratum proportions do not sum to 1')
  }

  if(nstrata <= 0 | !is.numeric(nstrata) || length(nstrata) != 1) {
    stop('Number of strata must be numeric greater than 0')
  }

  # Calculate effect size
  zbeta <- qnorm(power)
  zalpha <- qnorm(1-(alpha/2))
  strata_terms <- vapply(seq_len(nstrata), function(i) xi[i] * sigma2[i], 0.0)
  sqrt( ( (4 * (zbeta+zalpha)^2) / ((1-theta) * N) ) * sum(strata_terms) )
}

#' Unstratified Optimized Theta
#'
#' Calculates the optimal proportion of patients assigned to the choice arm
#' in an unstratified two-stage randomized trial
#'
#' @param w_sel weight assigned to the estimation of the selection effect. Each 
#'              weight should be a numeric value between 0 and 1 and sum of 
#'              three weights should be 1.
#' @param w_pref weight assigned to the estimation of the preference effect. 
#'               Each weight should be a numeric value between 0 and 1 and 
#'               sum of three weights should be 1.
#' @param w_treat weight assigned to estimation of the treatment effect. Each 
#'              weight should be a numeric value between 0 and 1 and sum of 
#'              three weights should be 1.
#' @param sigma2 variance estimate. Should be a positive numeric value.
#' @param phi proportion of patients preferring treatment 1. Should be numeric
#'            value between 0 and 1.
#' @param delta_pi overall study preference effect.
#' @param delta_nu overall study selection effect. 
#' @examples
#' optimal_proportion(w_sel=0.2, w_pref=0.4, w_treat=0.4, sigma2=1, phi=0.5,
#'                    delta_pi=1, delta_nu=0.5)
#' @references Walter et. al. (2011). "Optimal allocation of participants for
#' the estimation of selection, preference and treatment effects in the 
#' two-stage randomised trial design." \emph{Stat Med}, 
#' \strong{31}(13):1307-1322.
#' (\href{https://pubmed.ncbi.nlm.nih.gov/22362374}{PubMed})
#' @importFrom stats uniroot
#' @export
optimal_proportion <- function(w_sel, w_pref, w_treat, sigma2, phi, delta_pi,
                             delta_nu) {
  if (w_sel<0 | w_sel>1 | w_pref<0 | w_pref>1 | w_treat<0 | w_treat>1 | 
      any(!is.numeric(c(w_sel,w_pref,w_treat))) | length(w_sel)!=1 | 
      length(w_pref)!=1 | length(w_treat)!=1) {

    stop('Weights must be single numeric value in [0,1]')
  }
  if (w_sel + w_pref + w_treat != 1) {
    stop('weights do not sum to 1')
  }
  if(sigma2<=0 | any(!is.numeric(sigma2))) {
    stop('Variance estimate must be numeric value greater than 0')
  }
  if(phi<0 | phi>1 | !is.numeric(phi)) {
    stop('Preference rate must be numeric value in [0,1]')
  }
  if(!is.numeric(delta_pi) | !is.numeric(delta_nu) ||
     length(delta_pi)!=1 || length(delta_nu)!=1) {
    stop('Effect size must be single numeric value')
  }
  # Based on Equation 16 in Walter paper
  num <- w_sel + w_pref + 
    phi * (1-phi) * 
    ( (w_sel*( (2*phi-1) * delta_nu + delta_pi )^2 + 
       w_pref*( (2*phi-1) * delta_pi + delta_nu)^2) / sigma2 )
  denom <- 16 * w_treat * phi^2 * (1-phi)^2 +
    2 * (w_sel+w_pref) * ( phi^2+(1-phi)^2 )
  
  uniroot(f,c(0,1),value=(num/denom))$root
}

# Function used in theta optimization function
f <- function(theta,value) {
  ( theta/(1-theta) )^2-value
}  

#' Calculate Effect Sizes from Means
#'
#' Calculates the preference, selection and treatment effects given the means
#' of each treatment group in the choice and random arms for the 2-stage 
#' randomized study.
#'
#' @param mu1 mean response of the patients receiving treatment 1 in the 
#'            random arm. For unstratified design, should be numeric value.
#'            For the stratified design, should be vector of length equal to
#'            number of strata with each entry corresponding to stratum-
#'            specific mean.
#' @param mu2 mean response of the patients receiving treatment 2 in the 
#'            random arm. For unstratified design, should be numeric value.
#'            For the stratified design, should be vector of length equal to
#'            number of strata with each entry corresponding to stratum-
#'            specific mean.
#' @param mu11 mean response of the patients choosing treatment 1 in the choice
#'             arm. For unstratified design, should be numeric value.
#'            For the stratified design, should be vector of length equal to
#'            number of strata with each entry corresponding to stratum-
#'            specific mean.
#' @param mu22 mean response of the patients choosing treatment 2 in the choice
#'             arm. For unstratified design, should be numeric value.
#'            For the stratified design, should be vector of length equal to
#'            number of strata with each entry corresponding to stratum-
#'            specific mean.
#' @param phi proportion of patients preferring treatment 1. For unstratified 
#'            design, should be numeric value. For the stratified design, 
#'            should be vector of length equal to number of strata with each 
#'            entry corresponding to stratum-specific preference rate. All 
#'            elements should be numeric values between 0 and 1.
#' @param nstrata number of strata. Default is 1 (unstratified design).
#' @param xi a numeric vector of the proportion of patients in each stratum. 
#'          Length of vector should equal the number of strata in the study and 
#'          sum of vector should be 1. Should only be specified for stratified
#'          design.
#' @examples
#' effects_from_means(mu1=1, mu2=2, mu11=1.5, mu22=2.5, phi=0.5)
#' @references Rucker G (1989). "A two-stage trial design for testing treatment, 
#' self-selection and treatment preference effects." \emph{Stat Med}, 
#' \strong{8}(4):477-485. 
#' (\href{https://pubmed.ncbi.nlm.nih.gov/2727471}{PubMed})
#' @export
effects_from_means <- function(mu1,mu2,mu11,mu22,phi,nstrata=1,xi=NULL) {

  # Error messages
  if(nstrata <= 0 | !is.numeric(nstrata) || length(nstrata) != 1) {
    stop('Number of strata must be numeric greater than 0')
  }
  if (nstrata > 1 & is.null(xi)) {
    stop('Must define xi for stratified design')
  }

  if (length(phi) != nstrata || length(mu1) != nstrata ||
      length(mu2) != nstrata || length(mu11) != nstrata ||
      length(mu22) != nstrata) {
    stop('Length vector does not match number of strata')
  }
  if(any(phi < 0) || any(phi > 1) || any(!is.numeric(phi))) {
    stop('Preference rate must be numeric value in [0,1]')
  }
  if(!is.numeric(mu1) || !is.numeric(mu2) || !is.numeric(mu11) ||
     !is.numeric(mu22)) {
    stop('Mean must be numeric value')
  }

  if((any(xi < 0) || any(xi > 1) || any(!is.numeric(xi))) & !is.null(xi)) {
    stop('Proportion of patients in strata must be numeric value in [0,1]')
  }
  if (length(xi) != nstrata && nstrata != 1) {
    stop('Length of vector does not match number of strata')
  }
  if (sum(xi) != 1 && !any(is.null(xi))) {
    stop('Stratum proportions do not sum to 1.')
  }

  # Calculate unobserved means
  mu12 <- (mu1 - phi * mu11) / (1-phi)
  mu21 <- (mu2 - (1-phi) * mu22) / phi

  # Calculate effect sizes
  delta_tau <- mu1 - mu2
  delta_nu <- (mu11 + mu21 -mu12 - mu22) / 2
  delta_pi <- (mu11 - mu21 -mu12 + mu22) / 2

  if (nstrata == 1) {
    # Unstratified case
    effects <- list(treatment = delta_tau, selection = delta_nu,
                    preference = delta_pi)
  } else {
    # Stratified case
    effects <- list(
      treatment = sum(vapply(seq_len(nstrata),
                             function(x) xi[x]*delta_tau[x], 0.0)),
      selection = sum(vapply(seq_len(nstrata),
                             function(x) xi[x]*delta_nu[x], 0.0)),
      preference = sum(vapply(seq_len(nstrata),
                              function(x) xi[x]*delta_pi[x], 0.0)))
  }
  effects
}

