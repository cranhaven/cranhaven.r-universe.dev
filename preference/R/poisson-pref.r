

############################
### SAMPLE SIZE FORMULAS ###
############################


#' Overall Sample Size Poisson Distribution
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
#' @param lambda11 response mean of patients choosing to receive treatment 1 
#'            in the choice arm. Should be numeric value larger than 0. If
#'            study is stratified, should be vector with length equal to the 
#'            number of strata in the study.
#' @param lambda22 response mean of patients choosing to receive treatment 2 
#'            in the choice arm. Should be numeric value larger than 0. If
#'            study is stratified, should be vector with length equal to the 
#'            number of strata in the study.
#' @param lambda1 response mean of patients randomized to receive treatment 1 
#'            in the random arm. Should be numeric value larger than 0. If
#'            study is stratified, should be vector with length equal to the 
#'            number of strata in the study.
#' @param lambda2 response mean of patients randomized to receive treatment 2 
#'            in the random arm. Should be numeric value larger than 0. If
#'            study is stratified, should be vector with length equal to the 
#'            number of strata in the study.
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
#' # Unstratified
#' overall_sample_size_pois(power = 0.8, phi = 0.6, lambda11 = 0.6, 
#'                          lambda22 = 0.7, lambda1 = 0.4, lambda2 = 0.6)
#' # Stratified
#' overall_sample_size_pois(power = 0.8, phi = c(0.6, 0.5), 
#'                          lambda11 = c(0.6, 0.7), lambda22 = c(0.7,0.7), 
#'                          lambda1 = c(0.4, 0.6), lambda2 = c(0.6, 0.6), 
#'                          xi = c(0.5, 0.5), nstrata = 2)
#' @export
overall_sample_size_pois <- function(power, phi, lambda11, lambda22, lambda1, 
  lambda2, alpha=0.05, theta=0.5, xi=1, nstrata=1) {

  # Error messages
  if(!is.numeric(power) || power <= 0 || power >= 1) {
    stop('Power must be numeric in [0,1]')
  }
  if(any(!is.numeric(lambda11)) || any(lambda11 <= 0)) {
    stop('Response mean lambda11 must be positive')
  }
  if(any(!is.numeric(lambda22)) || any(lambda22 <= 0) ) {
    stop('Response mean lambda22 must be positive')
  }
  if(any(!is.numeric(lambda1)) || any(lambda1 <= 0) ) {
    stop('Response mean lambda1 must be positive')
  }
  if(any(!is.numeric(lambda2)) || any(lambda2 <= 0) ) {
    stop('Response mean lambda2 must be positive')
  }
  if (length(phi) != nstrata)  {
    stop('Length of vector does not match number of strata')
  }
  if(any(!is.numeric(phi)) || any(phi <= 0) || any(phi >= 1)) {
    stop('Preference rate must be numeric value in [0,1]')
  }
  if(length(lambda11) != nstrata || length(lambda22) != nstrata || 
            length(lambda1) != nstrata || length(lambda2) != nstrata) {
    stop('Length of response vector does not match number of strata')
  }
  if(!is.numeric(alpha) || alpha <= 0 || alpha >= 1) {
    stop('Type I error rate must be numeric in [0,1]')
  }
  if(!is.numeric(theta) || theta <= 0 || theta >= 1) {
    stop('Theta must be numeric in [0,1]')
  }
  if(any(!is.numeric(xi) || any(xi <= 0) || any(xi > 1))) {
    stop('Proportion of patients in strata must be numeric value in [0,1]')
  }
  if (length(xi) != nstrata) {
    stop('Length of vector does not match number of strata')
  }
  if (sum(xi) != 1) {
    stop('Stratum proportions do not sum to 1')
  }
  if(!is.numeric(nstrata) || nstrata <=0) {
    stop('Number of strata must be numeric greater than 0')
  }
  
  zalpha <- qnorm(1 - alpha/2)
  zbeta <- qnorm(power)
 
  d1 <- lambda11 - lambda1

  d2 <- lambda22 - lambda2
 
  #Selection Sample Size
 
  delta_nu <-
    unlist(sapply(
      seq_len(nstrata), 
      function(i) 
      calc_delta_nu_pois(phi[i],lambda11[i], lambda1[i], lambda22[i],
                         lambda2[i]))
    )
  
  #delta_nu <- calc_delta_nu_pois(phi,lambda11, lambda1, lambda22, lambda2)
 
  
  sel_terms <- unlist(sapply(
    seq_len(nstrata), 
    function (i) {
        phi[i] * lambda11[i] + (1 - phi[i]) * lambda22[i] +
          (phi[i]^2 * d1[i] + (1-phi[i])^2 * d2[i])^2 / (phi[i]*(1 - phi[i])) +
          2*(theta / (1-theta)) * 
          (phi[i]^2*lambda1[i] + (1-phi[i])^2*lambda2[i])
    }))

#  sel_terms <- phi * lambda11 + (1 - phi) * lambda22 +
#    (phi^2 * d1 + (1-phi)^2 * d2)^2 / (phi*(1 - phi)) +
#    2*(theta / (1-theta)) * (phi^2*lambda1 + (1-phi)^2*lambda2)
  
  sel_terms_tot <- sapply(
    seq_len(nstrata), 
    function(i) {
      ( xi[i] / (phi[i]^2 * (1 -phi[i])^2) ) * sel_terms[i]
    })
      
#  sel_terms_tot <- ( xi / (phi^2 * (1 -phi)^2) ) * sel_terms

  sel_avg <- sum(unlist(sapply(
    seq_len(nstrata), 
    function(i) {
      xi[i]*delta_nu[i]
    })))

#  sel_avg <- sum(xi * delta_nu)
  
  sel_N <- ceiling( (zalpha + zbeta)^2 / (4 * theta * sel_avg^2) * 
    sum(sel_terms_tot) )
  
  #Preference Sample size
  
  delta_pi <- unlist(sapply(seq_len(nstrata), 
    function(i) calc_delta_pi_pois(phi[i],lambda11[i], lambda1[i],
                                   lambda22[i],lambda2[i])))

#  delta_pi <- calc_delta_pi_pois(phi, lambda11, lambda1, lambda22, lambda2)
  
  pref_terms <- unlist(sapply(1:nstrata, function (i) 
    phi[i]*lambda11[i]+(1-phi[i])*lambda22[i]+
      (phi[i]^2*d1[i]+(1-phi[i])^2*d2[i])^2/(phi[i]*(1-phi[i]))+
      2*(theta/(1-theta))*(phi[i]^2*lambda1[i]+
                             (1-phi[i])^2*lambda2[i])))

#  pref_terms <- phi * lambda11 + (1-phi) * lambda22 +
#      (phi^2 * d1 + (1-phi)^2 * d2)^2 / (phi * ( 1-phi )) +
#      2 * (theta / (1-theta)) * (phi^2 * lambda1 + (1-phi)^2 * lambda2)

  
  pref_terms_tot=sapply(1:nstrata, function(i) (xi[i]/(phi[i]^2*(1-phi[i])^2))*
                     pref_terms[i])

#  pref_terms_tot <- (xi / (phi^2 * (1-phi)^2)) * pref_terms

  pref_avg<-sum(unlist(sapply(1:nstrata, function(i) xi[i]*delta_pi[i])))

#  pref_avg <- sum(xi * delta_pi)
  
  pref_N <- ceiling( (zalpha + zbeta)^2 / (4 * theta * pref_avg^2) *
                    sum(pref_terms_tot))
  
  #Treatment Sample size
  
  delta_tau<-unlist(sapply(1:nstrata, function(i) lambda1[i]-lambda2[i]))

#  delta_tau <- lambda1 - lambda2
  
  treat_terms<-unlist(sapply(1:nstrata, function(i) lambda1[i]+lambda2[i]))

#  treat_terms <- lambda1 + lambda2
  
  treat_avg<-sum(unlist(sapply(1:nstrata, function(i) xi[i]*delta_tau[i])))

#  treat_avg <- sum(xi * delta_tau)
  
  treat_terms_tot=sapply(1:nstrata, function(i) (xi[i])*treat_terms[i])

#  treat_terms_tot <- xi * treat_terms

  delta_tau_avg<-sum(unlist(sapply(1:nstrata, function(i) xi[i]*delta_tau[i])))

#  delta_tau_avg <- sum(xi * delta_tau)
  
  treat_N <- ceiling( (zalpha+zbeta)^2 / ( (1-theta) * treat_avg^2 ) *
                     sum(treat_terms_tot))
  
  data.frame(treatment = treat_N, selection = sel_N, preference = pref_N) 
  
}


###################################
### POWER CALCULATION FUNCTIONS ###
###################################

#'Power Calculation from Sample Size
#'
#' Calculates the study power to detect a set of effects given a particular 
#' sample size in a two-stage randomized clinical trial
#'
#' @param N overall study sample size.
#' @param phi the proportion of patients preferring treatment 1. Should be
#'            numeric value between 0 and 1. If study is stratified, should be
#'            vector with length equal to the number of strata in the study.
#' @param lambda11 response mean of patients choosing to receive treatment 1 
#'            in the choice arm. Should be numeric value larger than 0. If
#'            study is stratified, should be vector with length equal to the 
#'            number of strata in the study.
#' @param lambda22 response mean of patients choosing to receive treatment 2 
#'            in the choice arm. Should be numeric value larger than 0. If
#'            study is stratified, should be vector with length equal to the 
#'            number of strata in the study.
#' @param lambda1 response mean of patients randomized to receive treatment 1 
#'            in the random arm. Should be numeric value larger than 0. If
#'            study is stratified, should be vector with length equal to the 
#'            number of strata in the study.
#' @param lambda2 response mean of patients randomized to receive treatment 2 
#'            in the random arm. Should be numeric value larger than 0. If
#'            study is stratified, should be vector with length equal to the 
#'            number of strata in the study.
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
#' # Unstratified
#' pwr_overall_pois(N=400, phi=0.5, lambda11=0.8, lambda22=0.5, lambda1=0.5, lambda2=0.4)
#' # Stratified
#' pwr_overall_pois(N=400, phi=c(0.5,0.5), lambda11=c(0.7,0.8), lambda22=c(0.4,0.4), 
#' lambda1=c(0.5,0.4), lambda2=c(0.2,0.3), xi=c(0.3,0.7), nstrata=2)
#' @export
pwr_overall_pois<-function(N, phi, lambda11, lambda22, lambda1, lambda2, 
                           alpha = 0.05, theta = 0.5, xi = 1, nstrata = 1) {

  # Error messages
  if(N<0 | !is.numeric(N)) {
    stop('N must be a positive numeric value')
  }
  if(any(!is.numeric(lambda11)) || any(lambda11 <= 0)) {
    stop('Response mean lambda11 must be positive')
  }
  if(any(!is.numeric(lambda22)) || any(lambda22 <= 0) ) {
    stop('Response mean lambda22 must be positive')
  }
  if(any(!is.numeric(lambda1)) || any(lambda1 <= 0) ) {
    stop('Response mean lambda1 must be positive')
  }
  if(any(!is.numeric(lambda2)) || any(lambda2 <= 0) ) {
    stop('Response mean lambda2 must be positive')
  }
  if (length(phi) != nstrata) {
    stop('Length of vector does not match number of strata')
  }
  if(any(!is.numeric(phi)) || any(phi <= 0) || any(phi >= 1)) {
    stop('Preference rate must be numeric value in [0,1]')
  }
  if(length(lambda11) != nstrata || length(lambda22) != nstrata || 
     length(lambda1) != nstrata || length(lambda2) != nstrata) {
    stop('Length of response vector does not match number of strata')
  }
  if(!is.numeric(alpha) || alpha <= 0 || alpha >= 1) {
    stop('Type I error rate must be numeric in [0,1]')
  }
  if(!is.numeric(theta) || theta <= 0 || theta >= 1) {
    stop('Theta must be numeric in [0,1]')
  }
  if(any(!is.numeric(xi) || any(xi <= 0) || any(xi > 1))) {
    stop('Proportion of patients in strata must be numeric value in [0,1]')
  }
  if (length(xi) != nstrata) {
    stop('Length of vector does not match number of strata')
  }
  if (sum(xi) != 1) {
    stop('Stratum proportions do not sum to 1')
  }
  if(!is.numeric(nstrata) || nstrata <=0) {
    stop('Number of strata must be numeric greater than 0')
  }
  
  zalpha <- qnorm(1-(alpha/2))
  d1<-unlist(sapply(1:nstrata, function(i) lambda11[i]-lambda1[i]))
  d2<-unlist(sapply(1:nstrata, function(i) lambda22[i]-lambda2[i]))

#  d1 <- lambda11 - lambda1

#  d2 <- lambda22 - lambda2
  
  #Preference
  
  delta_pi<-unlist(sapply(1:nstrata, function(i) calc_delta_pi_pois(phi[i],lambda11[i],
                                                                    lambda1[i],lambda22[i],lambda2[i])))

  
  pref_terms<-unlist(sapply(1:nstrata, function (i) 
    phi[i]*lambda11[i]+(1-phi[i])*lambda22[i]+
      (phi[i]^2*d1[i]+(1-phi[i])^2*d2[i])^2/(phi[i]*(1-phi[i]))+
      2*(theta/(1-theta))*(phi[i]^2*lambda1[i]+
                             (1-phi[i])^2*lambda2[i])))
  
  pref_terms_tot<-sum(sapply(1:nstrata, function(i) (xi[i]/(phi[i]^2*(1-phi[i])^2))*
                         pref_terms[i]))
  pref_avg<-sum(unlist(sapply(1:nstrata, function(i) xi[i]*delta_pi[i])))
  
  pref_power<-round(pnorm(sqrt((4*theta*pref_avg^2*N)/(pref_terms_tot))-zalpha), digits=3)
  
  #Selection

  delta_nu<-unlist(sapply(1:nstrata, function(i) calc_delta_nu_pois(phi[i],lambda11[i],
                                                                    lambda1[i],lambda22[i],lambda2[i])))
  sel_terms<-unlist(sapply(1:nstrata, function (i) 
    phi[i]*lambda11[i]+(1-phi[i])*lambda22[i]+
      (phi[i]^2*d1[i]+(1-phi[i])^2*d2[i])^2/(phi[i]*(1-phi[i]))+
      2*(theta/(1-theta))*(phi[i]^2*lambda1[i]+(1-phi[i])^2*lambda2[i])))
  
  sel_terms_tot<-sum(sapply(1:nstrata, function(i) (xi[i]/(phi[i]^2*(1-phi[i])^2))*
                         sel_terms[i]))
  sel_avg<-sum(unlist(sapply(1:nstrata, function(i) xi[i]*delta_nu[i])))
  
  sel_power<-round(pnorm(sqrt((4*theta*sel_avg^2*N)/(sel_terms_tot))-zalpha), digits=3)
  
  #Treatment
  
  delta_tau<-unlist(sapply(1:nstrata, function(i) lambda1[i]-lambda2[i]))
  treat_avg<-sum(unlist(sapply(1:nstrata, function(i) xi[i]*delta_tau[i])))
  
  treat_terms<-unlist(sapply(1:nstrata, function(i) lambda1[i]+lambda2[i]))
  treat_terms_tot<-sum(sapply(1:nstrata, function(i) (xi[i])*treat_terms[i]))
  
  treat_power<-round(pnorm(sqrt((((1-theta)*treat_avg^2*N)/(treat_terms_tot)))-zalpha), digits=3)
  
 
  
  data.frame(treat_power=treat_power,pref_power=pref_power,sel_power=sel_power)
}


######################################
### Extra (non-exported) functions ###
######################################

# Calculate preference effect from response proportions
calc_delta_pi_pois <- function(phi, lambda11, lambda1, lambda22, lambda2){
  (phi*(lambda11 - lambda1) + (1-phi)*(lambda22 - lambda2)) / 
    (2*( phi*(1-phi) ))
}

# Calculate selection effect from response proportions
calc_delta_nu_pois<-function(phi,lambda11,lambda1,lambda22,lambda2){
  (phi*(lambda11 - lambda1) - (1-phi)*(lambda22 - lambda2)) /
    (2*( phi*(1-phi) ))
}
