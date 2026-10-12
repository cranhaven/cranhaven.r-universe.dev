#'Overall Sample Size Binomial
#'
#'Calculates the sample size required to detect a given set of effects
#'in a two-stage randomize clinical trial with a binary outcome. 
#'Returns the sample size for each of the three effects: preference,
#'selection and treatment

#' @param power desired study power. Should be numeric value between 0 and 1.
#' @param phi the proportion of patients preferring treatment 1. Should be
#'            numeric value between 0 and 1. If study is stratified, should be
#'            vector with length equal to the number of strata in the study.
#' @param p11 response proportion of patients choosing to receive treatment 1 
#'            in the choice arm. Should be numeric value between 0 and 1. If
#'            study is stratified, should be vector with length equal to the 
#'            number of strata in the study.
#' @param p22 response proportion of patients choosing to receive treatment 2 
#'            in the choice arm. Should be numeric value between 0 and 1. If
#'            study is stratified, should be vector with length equal to the 
#'            number of strata in the study.
#' @param p1 response proportion of patients randomized to receive treatment 1 
#'            in the random arm. Should be numeric value between 0 and 1. If
#'            study is stratified, should be vector with length equal to the 
#'            number of strata in the study.
#' @param p2 response proportion of patients randomized to receive treatment 2 
#'            in the random arm. Should be numeric value between 0 and 1. If
#'            study is stratified, should be vector with length equal to the 
#'            number of strata in the study.
#' @param alpha desired type I error rate.
#' @param theta proportion of patients assigned to choice arm in the initial
#'              randomization. Should be numeric value between
#'              0 and 1 (default=0.5).
#' @param xi a numeric vector of the proportion of patients in each stratum. 
#'          Length of vector should equal the number of strata in the study 
#'          and sum of vector should be 1. All vector elements should be
#'          numeric values between 0 and 1. Default is 1 (i.e. unstratified 
#'          design).
#' @param nstrata number of strata. Default is 1 (i.e. unstratified design).
#' @param k the ratio of treatment A to treatment B in the random arm. 
#'        (default 1, i.e. equal distribution to the two treatments in the 
#'        random arm)
#' @export
overall_sample_size_bin <- function(power, phi, p11, p22, p1, p2, 
                                    alpha=0.05, theta=0.5, xi=1, nstrata=1, 
                                    k=1) {

  # Error messages
  if(!is.numeric(power) || power <= 0 || power >= 1)
    stop('Power must be numeric in [0,1]')
  if(any(!is.numeric(p11)) || any(p11 <= 0) || any(p11 >= 1))
    stop('Response proportion p11 must be numeric in [0,1]')
  if(any(!is.numeric(p22)) || any(p22 <= 0) || any(p22 >= 1))
    stop('Response proportion p22 must be numeric in [0,1]')
  if(any(!is.numeric(p1)) || any(p1 <= 0) || any(p1 >= 1))
    stop('Response proportion p1 must be numeric in [0,1]')
  if(any(!is.numeric(p2)) || any(p2 <= 0) || any(p2 >= 1))
    stop('Response proportion p2 must be numeric in [0,1]')
  if (length(phi) != nstrata) 
    stop('Length vector does not match number of strata')
  if(any(!is.numeric(phi)) || any(phi <= 0) || any(phi >= 1))
    stop('Preference rate must be numeric value in [0,1]')
  if(length(p11) != nstrata || length(p22) != nstrata || length(p1) != nstrata
     || length(p2) != nstrata)
    stop('Length of response vector does not match number of strata')
  if(!is.numeric(alpha) || alpha <= 0 || alpha >= 1)
    stop('Type I error rate must be numeric in [0,1]')
  if(!is.numeric(theta) || theta <= 0 || theta >= 1)
    stop('Theta must be numeric in [0,1]')
  if(any(!is.numeric(xi) || any(xi <= 0) || any(xi > 1)))
    stop('Proportion of patients in strata must be numeric value in [0,1]')
  if (length(xi) != nstrata) 
    stop('Length of vector does not match number of strata')
  if (sum(xi) != 1) 
    stop('Stratum proportions do not sum to 1')
  if(!is.numeric(nstrata) || nstrata <=0)
    stop('Number of strata must be numeric greater than 0') 
  
  zalpha <-qnorm((1-alpha/2))
  zbeta <-qnorm(power)
  
  d1<-unlist(sapply(1:nstrata, function(i) p11[i]-p1[i]))
  d2<-unlist(sapply(1:nstrata, function(i) p22[i]-p2[i]))
  
  #Selection Sample size
  
  delta_nu <- unlist(sapply(1:nstrata, function(i) calc_delta_nu_bin(phi[i],p11[i],
                                                               p1[i],p22[i],p2[i])))
  
  sel_terms <- unlist(sapply(1:nstrata, function (i) 
    phi[i]*p11[i]*(1-p11[i])+(1-phi[i])*p22[i]*(1-p22[i])+
      (phi[i]^2*d1[i]+(1-phi[i])^2*d2[i])^2/(phi[i]*(1-phi[i]))+
      2*(theta/(1-theta))*(phi[i]^2*p1[i]*(1-p1[i])+(1-phi[i])^2*p2[i]*(1-p2[i]))))
  
  sel_terms_tot <- sapply(1:nstrata, function(i) (xi[i]/(phi[i]^2*(1-phi[i])^2))*
                     sel_terms[i])
  sel_avg <- sum(unlist(sapply(1:nstrata, function(i) xi[i]*delta_nu[i])))
  
  sel_N <- ceiling((zalpha+zbeta)^2/(4*theta*sel_avg^2)*sum(sel_terms_tot))
  
  #Preference Sample size
  
  delta_pi <- unlist(sapply(1:nstrata, function(i) calc_delta_pi(phi[i],p11[i],
                                                               p1[i],p22[i],p2[i])))
  pref_terms <- unlist(sapply(1:nstrata, function (i) 
    phi[i]*p11[i]*(1-p11[i])+(1-phi[i])*p22[i]*(1-p22[i])+
      (phi[i]^2*d1[i]+(1-phi[i])^2*d2[i])^2/(phi[i]*(1-phi[i]))+
      2*(theta/(1-theta))*(phi[i]^2*p1[i]*(1-p1[i])+
                             (1-phi[i])^2*p2[i]*(1-p2[i]))))
  
  pref_terms_tot <- sapply(1:nstrata, function(i) (xi[i]/(phi[i]^2*(1-phi[i])^2))*
                     pref_terms[i])
  pref_avg <- sum(unlist(sapply(1:nstrata, function(i) xi[i]*delta_pi[i])))
  
  pref_N <- ceiling((zalpha+zbeta)^2/(4*theta*pref_avg^2)*sum(pref_terms_tot))
  
  #Treatment Sample size
  
  delta_tau <- unlist(sapply(1:nstrata, function(i) p1[i]-p2[i]))
  
  treat_terms <- unlist(sapply(1:nstrata, function(i) p1[i]*(1-p1[i])+p2[i]*(1-p2[i])))
  
  treat_terms_tot <- sapply(1:nstrata, function(i) (xi[i])*treat_terms[i])
  
  treat_avg<-sum(unlist(sapply(1:nstrata, function(i) xi[i]*delta_tau[i])))
  
  treat_N <- ceiling(((k+1)^2/(4*k))*((zalpha+zbeta)^2) /((1-theta)*treat_avg^2)*sum(treat_terms_tot))
  
  
  data.frame(treatment=treat_N, selection=sel_N, preference=pref_N) 

}


#' Power Calculation from Sample Size
#' 
#' Calculates the study power to detect the preference effect given a particular 
#' sample size in a two-stage randomized clinical trial with a binary outcome measure
#' 
#' @param N overall study sample size.
#' @param phi the proportion of patients preferring treatment 1. Should be
#'            numeric value between 0 and 1. If study is stratified, should be
#'            vector with length equal to the number of strata in the study.
#' @param p11 response proportion of patients choosing to receive treatment 1 
#'            in the choice arm. Should be numeric value between 0 and 1. If
#'            study is stratified, should be vector with length equal to the 
#'            number of strata in the study.
#' @param p22 response proportion of patients choosing to receive treatment 2 
#'            in the choice arm. Should be numeric value between 0 and 1. If
#'            study is stratified, should be vector with length equal to the 
#'            number of strata in the study.
#' @param p1 response proportion of patients randomized to receive treatment 1 
#'            in the random arm. Should be numeric value between 0 and 1. If
#'            study is stratified, should be vector with length equal to the 
#'            number of strata in the study.
#' @param p2 response proportion of patients randomized to receive treatment 2 
#'            in the random arm. Should be numeric value between 0 and 1. If
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
#' @export
overall_power_binom<-function(N, phi, p11, p22, p1, p2, alpha=0.05, theta=0.5, 
                              xi=1, nstrata=1) {
  # Error messages
  if(N<0 | !is.numeric(N)) 
    stop('N must be a positive numeric value')
  if(any(!is.numeric(p11)) || any(p11 <= 0) || any(p11 >= 1))
    stop('Response proportion p11 must be numeric in [0,1]')
  if(any(!is.numeric(p22)) || any(p22 <= 0) || any(p22 >= 1))
    stop('Response proportion p22 must be numeric in [0,1]')
  if(any(!is.numeric(p1)) || any(p1 <= 0) || any(p1 >= 1))
    stop('Response proportion p1 must be numeric in [0,1]')
  if(any(!is.numeric(p2)) || any(p2 <= 0) || any(p2 >= 1))
    stop('Response proportion p2 must be numeric in [0,1]')
  if (length(phi) != nstrata) 
    stop('Length vector does not match number of strata')
  if(any(!is.numeric(phi)) || any(phi <= 0) || any(phi >= 1))
    stop('Preference rate must be numeric value in [0,1]')
  if(length(p11) != nstrata || length(p22) != nstrata || length(p1) != nstrata
     || length(p2) != nstrata)
    stop('Length of response vector does not match number of strata')
  if(!is.numeric(alpha) || alpha <= 0 || alpha >= 1)
    stop('Type I error rate must be numeric in [0,1]')
  if(!is.numeric(theta) || theta <= 0 || theta >= 1)
    stop('Theta must be numeric in [0,1]')
  if(any(!is.numeric(xi) || any(xi <= 0) || any(xi > 1)))
    stop('Proportion of patients in strata must be numeric value in [0,1]')
  if (length(xi) != nstrata) 
    stop('Length of vector does not match number of strata')
  if (sum(xi) != 1) 
    stop('Stratum proportions do not sum to 1')
  if(!is.numeric(nstrata) || nstrata <=0)
    stop('Number of strata must be numeric greater than 0')
  
  
  # Calculate study power
  #trt_pwr<-trt_pwr_binom(N=N,p11=p11, p22=p22, p1=p1, p2=p2,
  #                theta=theta,xi=xi,nstrata=nstrata, alpha=alpha)
  zalpha<-qnorm(1-(alpha/2))
  d1<-unlist(sapply(1:nstrata, function(i) p11[i]-p1[i]))
  d2<-unlist(sapply(1:nstrata, function(i) p22[i]-p2[i]))
  
  #Preference power
  delta_pi<-unlist(sapply(1:nstrata, function(i) calc_delta_pi(phi[i],p11[i],
                                                               p1[i],p22[i],p2[i])))

  pref_terms=unlist(sapply(1:nstrata, function (i) 
    phi[i]*p11[i]*(1-p11[i])+(1-phi[i])*p22[i]*(1-p22[i])+
      (phi[i]^2*d1[i]+(1-phi[i])^2*d2[i])^2/(phi[i]*(1-phi[i]))+
      2*(theta/(1-theta))*(phi[i]^2*p1[i]*(1-p1[i])+
                             (1-phi[i])^2*p2[i]*(1-p2[i]))))
  
  pref_terms_tot=sum(sapply(1:nstrata, function(i) (xi[i]/(phi[i]^2*(1-phi[i])^2))*
                         pref_terms[i]))
  delta_pi_avg<-sum(unlist(sapply(1:nstrata, function(i) xi[i]*delta_pi[i])))
  
  pref_power=pnorm(sqrt((4*theta*delta_pi_avg^2*N)/(pref_terms_tot))-zalpha)
  
  #Selection Power
  delta_nu<-unlist(sapply(1:nstrata, function(i) calc_delta_nu_bin(phi[i],p11[i],
                                                               p1[i],p22[i],p2[i])))
  
  sel_terms=unlist(sapply(1:nstrata, function (i) 
    phi[i]*p11[i]*(1-p11[i])+(1-phi[i])*p22[i]*(1-p22[i])+
      (phi[i]^2*d1[i]+(1-phi[i])^2*d2[i])^2/(phi[i]*(1-phi[i]))+
      2*(theta/(1-theta))*(phi[i]^2*p1[i]*(1-p1[i])+(1-phi[i])^2*p2[i]*(1-p2[i]))))
  
  sel_terms_tot=sum(sapply(1:nstrata, function(i) (xi[i]/(phi[i]^2*(1-phi[i])^2))*
                         sel_terms[i]))
  delta_nu_avg<-sum(unlist(sapply(1:nstrata, function(i) xi[i]*delta_nu[i])))
  
  sel_power=pnorm(sqrt((4*theta*delta_nu_avg^2*N)/(sel_terms_tot))-zalpha)
  
  #Treatment power
  delta_tau<-unlist(sapply(1:nstrata, function(i) p1[i]-p2[i]))
  delta_tau_avg<-sum(unlist(sapply(1:nstrata, function(i) xi[i]*delta_tau[i])))
  
  trt_terms=unlist(sapply(1:nstrata, function(i) p1[i]*(1-p1[i])+p2[i]*(1-p2[i])))
  trt_terms_tot=sum(sapply(1:nstrata, function(i) (xi[i])*trt_terms[i]))
  
  trt_power=pnorm(sqrt((((1-theta)*delta_tau_avg^2*N)/(trt_terms_tot)))-zalpha)
  
  
  return(data.frame(trt_power=trt_power,pref_power=pref_power,sel_power=sel_power))  
}



######################################
### Extra (non-exported) functions ###
######################################

# Calculate preference effect from response proportions
calc_delta_pi <- function(phi,p11,p1,p22,p2){
  pref_effect=(phi*(p11-p1)+(1-phi)*(p22-p2))/(2*(phi*(1-phi)))
  return(pref_effect)
}

# Calculate selection effect from response proportions
calc_delta_nu_bin<-function(phi,p11,p1,p22,p2){
  sel_effect=(phi*(p11-p1)-(1-phi)*(p22-p2))/(2*(phi*(1-phi)))
  return(sel_effect)
}
