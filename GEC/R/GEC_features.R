#' mle_eigp
#' @title Analytical solution of theta given eta in EIGP model.
#' @description  This function provides the analytical solution of theta for given eta EIGP model.
#' @param s a numeric value the sum of log(1/x_i^eta), where i is from 1 to m.
#' @param m m is the number of data items less than the density change point.
#' @param n n is the sample size, n has to be greater than m.
#' @return This function returns the Maximum Likelihood Estimate of theta for a given eta.
#' @examples
#' mle_eigp(5,2,5)
#' @export
mle_eigp = function(s,m,n){
  k = 0.144351
  alpha = 0.308298
  a = alpha - k
  theta = (m*alpha+(alpha-k)*(n-m))/(k*s)
  return(theta)
}

#' mle_iter_eigp
#' @title Iteration function to find the analytical solution of theta given eta and data in EIGP model.
#' @description This function finds the analytical solution of theta given eta and data in EIGP model.
#' @param data Observations.
#' @param eta The exponent parameter. This value is greater than 0.
#' @return This function returns the Maximum Likelihood Estimate of theta for a given eta with data.
#' @examples
#' mle_iter_eigp(seq(1:100),2)
#' @export
mle_iter_eigp = function(data,eta){ #MLE of theta
  sort_x = sort(data)
  len = length(sort_x)
  m_iter = 1
  s = sum(1/sort_x[1:m_iter]^eta)
  mle = mle_eigp(s = s,m = m_iter, n = len)
  while( (mle^(1/eta)>sort_x[m_iter+1] || mle^(1/eta)<sort_x[m_iter])){
    m_iter = m_iter+1
    s = sum(1/sort_x[1:m_iter]^eta)
    mle = mle_eigp(s = s, m = m_iter, n = len)
  }
  return(mle)
}

#' inv_gamma_eigp
#' @title The negative log density of a sample item if it follows inverse gamma in a EIGP model
#' @description  This function return the negative log density of a sample item if if it follows inverse gamma in a EIGP model.
#' @param x The value of a sample item.
#' @param theta The location parameter for the base distribution (eta = 1). The value needs to be positive.
#' @param eta The exponent parameter. The value provided needs to be positive.
#' @return This function return the negative log density of a sample item if if it follows inverse gamma in a EIGP model.
#' @examples
#' inv_gamma_eigp(1,5,2)
#' @export
inv_gamma_eigp = function(x,theta,eta){
  c = 0.711384
  k = 0.144351
  alpha = 0.308298
  value = log(c)+alpha*log(k*theta)+(-alpha-1)*eta*log(x)-(k*theta)/(x^eta)-log(gamma(alpha))+log(eta)+(eta-1)*log(x)
  value = -value #negative log likelihood
  return(value)
}

#' pareto_eigp
#' @title The negative log density of a sample item if it follows Pareto in a EIGP model
#' @description  This function return the negative log density of a sample item if if it follows Pareto in a EIGP model.
#' @param x The value of a sample item.
#' @param theta The location parameter for the base distribution (eta = 1). The value needs to be positive.
#' @param eta The exponent parameter. The value provided needs to be positive.
#' @return This function return the negative log density of a sample item if if it follows Pareto in a EIGP model.
#' @examples
#' pareto_eigp(10,5,2)
#' @export
pareto_eigp = function(x,theta,eta){
  c = 0.711384
  k = 0.144351
  alpha = 0.308298
  value = log(c)+log(alpha-k)+(alpha-k)*log(theta)+log(eta)+(eta-1)*log(x)-eta*(alpha-k+1)*log(x)
  value = -value
  return(value)
}

#' neg_log_eigp
#' @title The negative log likelihood function for EIGP distribution.
#' @description  This function computes the negative log-likelihood for EIGP distribution.
#' @param y n by 1 vector with all positive entries.
#' @param theta The location parameter for the base distribution (eta = 1). The value needs to be positive.
#' @param eta The exponent parameter. The value provided needs to be positive.
#' @return This function return the negative log density of a sample item if if it follows Pareto in a EIGP model.
#' @examples
#' neg_log_eigp(seq(1:100),2,2)
#' @export
neg_log_eigp = function(y,theta,eta){
  len = length(y)
  sum = 0
  for(i in 1:len){
    if (y[i]^eta<theta){
      sum = sum+inv_gamma_eigp(y[i],theta,eta)
    }
    if(y[i]^eta>theta){
      sum = sum+pareto_eigp(y[i],theta,eta)
    }
  }
  return(sum)
}

#' mle_search_eigp
#' @title The grid search procedure for parameter estimation of EIGP.
#' @description  This function find the parameter estimates of EIGP through a grid search procedure.
#' @param eta_seq A predefined range for eta values. The default is c(0.5,10,by = 0.01)
#' @param data n by 1 vector with all positive entries.
#' @return This function returns data frame as the parameter estimates for EIGP from grid search methods.
#' @examples
#' sample1 = eigp_sampling(200,eta = 2,theta = 3)
#' mle_search_eigp(data = sample1)
#' @export
mle_search_eigp = function(eta_seq = seq(0.5,10,by=0.01),data){
  negloglik = rep(0,length(eta_seq))
  for (i in 1:length(eta_seq)){
    negloglik[i] = neg_log_eigp(data,mle_iter_eigp(data,eta_seq[i]),eta_seq[i]);
  }
  min_index = which(negloglik == min(negloglik), arr.ind = TRUE)
  eta = eta_seq[min_index]
  theta = mle_iter_eigp(data,eta)
  estimate = data.frame(eta,theta)
  colnames(estimate) = c('eta','theta')
  return(estimate)
}

#' mle_eep
#' @title Analytical solution of theta given eta in EEP model.
#' @description  This function provides the analytical solution of theta for given eta EEP model.
#' @param s A numeric value the sum of log(1/x_i^eta), where i is from 1 to m.
#' @param m m is the number of data items less than the density change point.
#' @param n n is the sample size, n has to be greater than m.
#' @return This function returns the Maximum Likelihood Estimate of theta for a given eta
#' @examples
#' mle_eep(5,2,5)
#' @export
mle_eep = function(s,m,n){
  theta = 1.349976*s/(1.349976*m-0.349976*n)
  return(theta)
}

#' mle_iter_eep
#' @title Iteration function to find the analytical solution of theta given eta and data in EEP model.
#' @description  This function finds the analytical solution of theta given eta and data in EEP model.
#' @param data Observations.
#' @param eta The exponent parameter. This value is greater than 0.
#' @return This function returns the Maximum Likelihood Estimate of theta for a given eta with data.
#' @examples
#' mle_iter_eep(seq(1:100),2)
#' @export
mle_iter_eep = function(data,eta){ #MLE of theta
  sort_x = sort(data)
  len = length(sort_x)
  m_iter = 1
  s = sum(sort_x[1:m_iter]^eta)
  mle = mle_eep(s = s,m = m_iter, n = len)
  while( (mle>sort_x[m_iter+1]^eta) || (mle<sort_x[m_iter]^eta)){
    m_iter = m_iter+1
    s = sum(sort_x[1:m_iter]^eta)
    mle = mle_eep(s = s, m = m_iter, n = len)
  }
  return(mle)
}


#' exp_exp
#' @title The negative log density of a sample item if it follows exponential in a EEP model
#' @description  This function return the negative log density of a sample item if if it follows exponential in a EEP model.
#' @param x The value of a sample item.
#' @param theta The location parameter for the base distribution (eta = 1). The value needs to be positive.
#' @param eta The exponent parameter. The value provided needs to be positive.
#' @return This function return the negative log density of a sample item if if it follows exponential in a EEP model.
#' @examples
#' exp_eep(1,5,2)
#' @export
exp_eep = function(x,theta,eta){
  c = 0.574
  alpha = 0.349976
  value = log(c)+ log(alpha+1) - log(theta)-(alpha+1)/theta*(x^eta) +log(eta)+(eta-1)*log(x)
  value = -value #negative log likelihood
  return(value)
}


#' pareto_eep
#' @title The negative log density of a sample item if it follows Pareto in a EEP model
#' @description  This function return the negative log density of a sample item if if it follows Pareto in a EEP model.
#' @param x The value of a sample item.
#' @param theta The location parameter for the base distribution (eta = 1). The value needs to be positive.
#' @param eta The exponent parameter. The value provided needs to be positive.
#' @return This function return the negative log density of a sample item if if it follows Pareto in a EEP model.
#' @examples
#' pareto_eep(10,5,2)
#' @export
pareto_eep = function(x,theta,eta){
  c = 0.574
  alpha = 0.349976
  value = log(c)+log(alpha)+(alpha)*log(theta)+log(eta)+(eta-1)*log(x)-eta*(alpha+1)*log(x)
  value = -value
  return(value)
}

#' neg_log_eigp
#' @title The negative log likelihood function for EEP distribution.
#' @description  This function computes the negative log-likelihood for EEP distribution.
#' @param y n by 1 vector with all positive entries.
#' @param theta The location parameter for the base distribution (eta = 1). The value needs to be positive.
#' @param eta The exponent parameter. The value provided needs to be positive.
#' @return This function return the negative log density of a sample item if if it follows Pareto in a EEP model.
#' @examples
#' neg_log_eep(seq(1:100),2,2)
#' @export
neg_log_eep = function(y,theta,eta){
  len = length(y)
  sum = 0
  for(i in 1:len){
    if (y[i]^eta<theta){
      sum = sum+exp_eep(y[i],theta,eta)
    }
    if(y[i]^eta>theta){
      sum = sum+pareto_eep(y[i],theta,eta)
    }
  }
  return(sum)
}

#' mle_search_eep
#' @title The grid search procedure for parameter estimation of EEP.
#' @description  This function find the parameter estimates of EEP throgh a grid search procedure.
#' @param eta_seq A predefined range for eta values. The default is c(0.5,10,by = 0.01)
#' @param data Observations.
#' @return This function returns a data frame as the parameter estimates for EEP from grid search methods.
#' @examples
#' sample1 = eep_sampling(200,eta = 2,theta = 3)
#' mle_search_eep(data = sample1)
#' @export
mle_search_eep = function(eta_seq = seq(0.5,10,by=0.01),data){
  negloglik = rep(0,length(eta_seq))
  for (i in 1:length(eta_seq)){
    negloglik[i] = neg_log_eep(data,mle_iter_eep(data,eta_seq[i]),eta_seq[i]);
  }
  #plot(negloglik)
  min_index = which(negloglik == min(negloglik), arr.ind = TRUE)
  eta = eta_seq[min_index]
  theta = mle_iter_eep(data,eta)
  estimate = data.frame(eta,theta)
  colnames(estimate) = c('eta','theta')
  return(estimate)
}

#' se_eep
#' @title The function for calculating the standard errors of the parameters of EEP model.
#' @description  This function find the parameter estimates of EEP through a grid search procedure.
#' @param data Observations.
#' @param theta The MLE of theta
#' @param eta The MLE of eta
#' @return The estimate of SE for theta and eta
#' @examples
#' sample1 = eep_sampling(200,eta = 2,theta = 3)
#' theta = mle_search_eep(data = sample1)$theta
#' eta = mle_search_eep(data = sample1)$eta
#' se_eep(sample1,theta,eta)
#' @export
se_eep = function(data, theta, eta){
  c = 0.574
  alpha = 0.349976 #These denote the constants for EEP model
  data = sort(data) #arrange the data in ascending order
  n = length(data) #sample size
  y_m = data[data<= theta^(1/eta)]  #Fisrt m objects
  m = length(y_m) #no of items less than theta^(1/eta)
  fish = matrix(0,nrow = 2,ncol = 2) #initialization
  fish[1,1] =  -(alpha*n - (alpha+1)*m)/(theta)^2 -2*(alpha+1)/(theta)^3 *sum(y_m^eta)#second derivative wrt theta
  fish[1,2] = (alpha+1)/(theta^2)*sum(y_m^eta*log(y_m))
  fish[2,1] = fish[1,2]
  fish[2,2] = -n/(eta^2)-(alpha+1)/theta*sum(y_m^eta*log(y_m)*log(y_m))
  var = solve(-fish) #This is the inverse of a symmetric Fisher Information Matrix
  estimate = data.frame(sqrt(var[1,1]),sqrt(var[2,2]))
  colnames(estimate) = c('theta.se','eta.se')
  return(estimate)
}

#' se_eigp
#' @title The function for calculating the standard errors of the parameters of EIGP model.
#' @description  This function find the parameter estimates of EIGP through a grid search procedure.
#' @param data Observations.
#' @param theta The MLE of theta
#' @param eta The MLE of eta
#' @return The estimate of SE for theta and eta
#' @examples
#' sample1 = eigp_sampling(200,eta = 2,theta = 3)
#' theta = mle_search_eigp(data = sample1)$theta
#' eta = mle_search_eigp(data = sample1)$eta
#' se_eigp(sample1,theta,eta)
#' @export
se_eigp = function(data, theta, eta){
  c = 0.711384
  k = 0.144351
  alpha = 0.308298 #These denote the constants for eep model
  data = sort(data) #arrange the data in ascending order
  n = length(data) #sample size
  y_m = data[data<= theta^(1/eta)]  #Fisrt m objects
  m = length(y_m) #no of items less than theta^(1/eta)
  fish = matrix(0,nrow = 2,ncol = 2) #initialization
  fish[1,1] =  -(alpha*m+(alpha-k)*(n-m))/theta^2 #second derivative wrt theta
  fish[1,2] = k*sum(y_m^(-eta)*log(y_m))
  fish[2,1] = fish[1,2]
  fish[2,2] = -n/(eta^2)-k*theta*sum(y_m^(-eta)*log(y_m)*log(y_m))
  var = solve(-fish) #This is the inverse of a symmetric Fisher Information Matrix
  estimate = data.frame(sqrt(var[1,1]),sqrt(var[2,2]))
  colnames(estimate) = c('theta.se','eta.se')
  return(estimate)
}

#' LRT_eigp
#' @title Likelihood Ratio Test (LRT) for the exponent parameter in EIGP model.
#' @description  This function computes the test statistic and the p-value for LRT for the exponent parameter in EIGP model.
#' @param data Observations.
#' @param theta0 The MLE of theta when eta = 1.
#' @param theta1 The unrestricted MLE of theta.
#' @param eta1 The unrestricted MLE of eta.
#' @return This function returns the test statistic and the p-value from the LRT test
#' @examples
#' sample1 = eigp_sampling(200,eta = 1.1,theta = 3)
#' eta1 = mle_search_eigp(data = sample1)$eta
#' theta1 = mle_search_eigp(data = sample1)$theta
#' theta0 = mle_iter_eigp(data = sample1,eta = 1)
#' LRT_eigp(sample1,theta0,theta1,eta1)
#' @export
LRT_eigp = function(data,theta0,theta1,eta1){
  test_stat = 2*(neg_log_eigp(data,theta = theta0, eta = 1)-neg_log_eigp(data,theta=theta1, eta = eta1))
  pval = 1-stats::pchisq(test_stat,df=1)
  result = data.frame(test_stat,pval)
  colnames(result) = c('Test Statistic','p-value')
  return(result)
}

#' LRT_eep
#' @title Likelihood Ratio Test (LRT) for the exponent parameter in EEP model.
#' @description  This function computes the test statistic and the p-value of LRT for the exponent parameter in EEP model.
#' @param data Observations.
#' @param theta0 The MLE of theta when eta = 1.
#' @param theta1 The unrestricted MLE of theta.
#' @param eta1 The unrestricted MLE of eta.
#' @return This function returns the test statistic and the p-value of the LRT test
#' @examples
#' sample1 = eep_sampling(200,eta = 1.1,theta = 6)
#' eta1 = mle_search_eep(data = sample1)$eta
#' theta1 = mle_search_eep(data = sample1)$theta
#' theta0 = mle_iter_eep(data = sample1,eta = 1)
#' LRT_eep(sample1,theta0,theta1,eta1)
#' @export
LRT_eep = function(data,theta0,theta1,eta1){
  test_stat = 2*(neg_log_eep(data,theta = theta0, eta = 1)-neg_log_eep(data,theta = theta1, eta = eta1))
  pval = 1-stats::pchisq(test_stat,df=1)
  result = data.frame(test_stat,pval)
  colnames(result) = c('Test Statistic','p-value')
  return(result)
}

#' asymptotic_eigp
#' @title Asymptotic Wald's test for testing the exponent in a EIGP model.
#' @description This function computes the test statistic and the p-value of Wald's test for the exponent parameter in EIGP model.
#' @param data Observations.
#' @param eta0 To test if the exponent equals 1, the default for eta0 is et to be 1.
#' @param theta1 The unrestricted MLE of theta.
#' @param eta1 The unrestricted MLE of eta.
#' @return This function returns the test statistic and the p-value of the Wald's test.
#' @examples
#' sample1 = eigp_sampling(200,eta = 1.1,theta = 3)
#' theta1 = mle_search_eigp(data = sample1)$theta
#' eta1 = mle_search_eigp(data = sample1)$eta
#' asymptotic_eigp(sample1,eta0 = 1,theta1,eta1)
#' @export
asymptotic_eigp = function(data,eta0=1,theta1,eta1){
  se_eta = se_eigp(data,theta1,eta1)$eta.se
  test_stat = (eta1 - eta0)^2/((se_eta)^2)
  pval = 1-stats::pchisq(test_stat,df=1)
  result = data.frame(test_stat,pval)
  colnames(result) = c('Test Statistic','p-value')
  return(result)
}

#' asymptotic_eep
#' @title Asymptotic Wald's test for testing the exponent in a EEP model.
#' @description This function computes the test statistic and the p-value of Wald's test for the exponent parameter in EEP model.
#' @param data Observations.
#' @param eta0 To test if the exponent equals 1, the default for eta0 is et to be 1.
#' @param theta1 The unrestricted MLE of theta.
#' @param eta1 The unrestricted MLE of eta.
#' @return This function returns the test statistic and the p-value of the Wald's test.
#' @examples
#' sample1 = eep_sampling(200,eta = 1.1,theta = 3)
#' theta1 = mle_search_eep(data = sample1)$theta
#' eta1 = mle_search_eep(data = sample1)$eta
#' asymptotic_eep(sample1,eta0 = 1,theta1,eta1)
#' @export
asymptotic_eep = function(data,eta0,theta1,eta1){
  se_eta = se_eep(data,theta1,eta1)$eta.se
  test_stat = (eta1 - eta0)^2/((se_eta)^2)
  pval = 1-stats::pchisq(test_stat,df=1)
  result = data.frame(test_stat,pval)
  colnames(result) = c('Test Statistic','p-value')
  return(result)
}

#' q_eigp
#' @title The quantile function of EIGP.
#' @param theta The location parameter for the base distribution (eta = 1). The value needs to be positive.
#' @param eta The exponent parameter. The value provided needs to be positive.
#' @param p This indicates the p-th percentile. p is greater than 0 and less than 100.
#' @import mistr
#' @return Return the p-th percentile of EIGP.
#' @examples
#' q_eigp(1,2,5)
#' @export
q_eigp = function(theta,eta,p){
  c = 0.711384
  k = 0.144351
  alpha = 0.308298
  a = alpha - k
  C=mistr::compdist((1/mistr::gammadist(shape = alpha,rate = (k*theta))), mistr::paretodist(scale = theta,shape = a), weights =     c(1-c,c), breakpoints = theta)^(1/eta) #pareto(theta:scale;a:shape); gamma(beta = k*theta:rate; alpha:shape)
  return(mistr::mistr_q(C,p/100))
}

#' pdf_eigp
#' @title The probability density function of EIGP.
#' @param theta The location parameter for the base distribution (eta = 1). The value needs to be positive.
#' @param eta The exponent parameter. The value provided needs to be positive.
#' @param data Observations.
#' @import mistr
#' @return Return the density of EIGP
#' @examples
#' pdf_eigp(1,2,5)
#' @export
pdf_eigp = function(theta,eta,data){
  c = 0.711384
  k = 0.144351
  alpha = 0.308298
  a = alpha - k
  C=mistr::compdist((1/mistr::gammadist(shape = alpha,rate = (k*theta))), mistr::paretodist(scale = theta,shape = a), weights =c(1-c,c), breakpoints = theta)^(1/eta) #pareto(theta:scale;a:shape); gamma(beta = k*theta:rate; alpha:shape)
  if (eta != 1){
    pdf_est = mistr::mistr_d(C,x = sort(data),log = FALSE)}
  if (eta == 1){
    pdf_est = 0.711384*(0.144351*theta)^(0.308298)*(data^(eta))^(-0.308298-1)*exp(-0.144351*theta/(data^eta))/gamma(0.308298)*eta*data^(eta-1)*(data<=theta^(1/eta))+
      0.711384*(0.308298-0.144351)*(theta)^(0.308298-0.144351)/((data^(eta))^(0.308298-0.144351+1))*eta*(data)^(eta-1)*(data>theta^(1/eta))
  }
  return(pdf_est)
}

#' cdf_eigp
#' @title The cumulative distribution function of EIGP.
#' @param theta The location parameter for the base distribution (eta = 1). The value needs to be positive.
#' @param eta The exponent parameter. The value provided needs to be positive.
#' @param data Observations.
#' @import mistr
#' @return Return the cumulative probability of EIGP at the specific location.
#' @examples
#' cdf_eigp(1,2,5)
#' @export
cdf_eigp = function(theta,eta,data){
  c = 0.711384
  k = 0.144351
  alpha = 0.308298
  a = alpha - k
  C=mistr::compdist((1/mistr::gammadist(shape = alpha,rate = (k*theta))), mistr::paretodist(scale = theta,shape = a), weights =c(1-c,c), breakpoints = theta)^(1/eta) #pareto(theta:scale;a:shape); gamma(beta = k*theta:rate; alpha:shape)
  cdf_est = mistr::mistr_p(C,q = sort(data))
  return(cdf_est)
}


#' q_eep
#' @title The quantile function of EEP.
#' @param theta The location parameter for the base distribution (eta = 1). The value needs to be positive.
#' @param eta The exponent parameter. The value provided needs to be positive.
#' @param p This indicates the p-th percentile. p is greater than 0 and less than 100.
#' @import mistr
#' @return Return the p-th percentile of EEP.
#' @examples
#' q_eigp(1,2,5)
#' @export
q_eep = function(theta,eta,p){
  c = 0.574
  alpha = 0.349976
  exp_pareto=mistr::compdist(mistr::expdist(rate = (alpha+1)/theta), mistr::paretodist(scale = theta,shape = alpha), weights = c(1-c,c), breakpoints = theta)^(1/eta) #pareto(theta:scale;a:shape); exp: rate = (alpha+1)/theta
  return(mistr::mistr_q(exp_pareto,p/100))
}

#' pdf_eep
#' @title The probability function of EEP.
#' @param theta The location parameter for the base distribution (eta = 1). The value needs to be positive.
#' @param eta The exponent parameter. The value provided needs to be positive.
#' @param data Observations.
#' @import mistr
#' @return Return the density of EEP
#' @examples
#' pdf_eep(1,2,5)
#' @export
pdf_eep = function(theta,eta,data){
  c = 0.574
  alpha = 0.349976
  exp_pareto=mistr::compdist(mistr::expdist(rate = (alpha+1)/theta), mistr::paretodist(scale = theta,shape = alpha), weights = c(1-c,c), breakpoints = theta)^(1/eta) #pareto(theta:scale;a:shape); exp: rate = (alpha+1)/theta
  if (eta != 1){
    pdf_est = mistr::mistr_d(exp_pareto,x = sort(data),log = FALSE)
  }
  if (eta == 1){
    pdf_est = exp(log(c*(alpha+1)/theta*exp(-(alpha+1)/theta*(data^(eta)))*eta*data^(eta-1)*(data<=theta^(1/eta)) + c*alpha*theta^(alpha)/data^(eta*(alpha+1))*eta*data^(eta-1)*(data>theta^(1/eta))))
  }
  return(pdf_est)
}

#' cdf_eep
#' @title The cumulative distribution function of EEP.
#' @param theta The location parameter for the base distribution (eta = 1). The value needs to be positive.
#' @param eta The exponent parameter. The value provided needs to be positive.
#' @param data Observations.
#' @import mistr
#' @return Return the cumulative probability of EEP at the specific location.
#' @examples
#' cdf_eep(1,2,5)
#' @export
cdf_eep = function(theta,eta,data){
  c = 0.574
  alpha = 0.349976
  exp_pareto=mistr::compdist(mistr::expdist(rate = (alpha+1)/theta), mistr::paretodist(scale = theta,shape = alpha), weights = c(1-c,c), breakpoints = theta)^(1/eta) #pareto(theta:scale;a:shape); exp: rate = (alpha+1)/theta
  cdf_est = mistr::mistr_p(exp_pareto,q = sort(data))
  return(cdf_est)
}


#' hazard_eep
#' @title The hazard function of EEP.
#' @param theta The location parameter for the base distribution (eta = 1). The value needs to be positive.
#' @param eta The exponent parameter. The value provided needs to be positive.
#' @param data Observations.
#' @import mistr
#' @return Return the hazard of EEP at the specific location.
#' @examples
#' hazard_eep(2,1,5)
#' plot(hazard_eep(2,1,seq(0.01,100,by=0.01)))
#' @export
hazard_eep = function(theta,eta,data){
  return(pdf_eep(theta,eta,data)/(1-cdf_eep(theta,eta,data)))
}

#' hazard_eigp
#' @title The hazard function of EIGP.
#' @param theta The location parameter for the base distribution (eta = 1). The value needs to be positive.
#' @param eta The exponent parameter. The value provided needs to be positive.
#' @param data Observations.
#' @import mistr
#' @return Return the hazard of EIGP at the specific location.
#' @examples
#' hazard_eigp(1,2,5)
#' plot(hazard_eep(2,1,seq(0.01,100,by=0.01)))
#' @export
hazard_eigp = function(theta,eta,data){
  return(pdf_eigp(theta,eta,data)/(1-cdf_eigp(theta,eta,data)))
}
