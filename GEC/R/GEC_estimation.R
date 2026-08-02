#' eigp_nll
#' @title The EIGP Negative Log-likelihood Function.
#' @description  This function serves as the objective function for the Maximum Likelihood Estimation procedure for EIGP.
#' @param x Vector of parameters.
#' @param m The number of data items less than the density change point.
#' @param data Observations.
#' @return A scalar that represents the negative loglikelihood of a EIGP sample given the model parameter and the data.
#' @examples
#' eigp_nll(c(2,2),50,seq(1:100))
#' @export
eigp_nll = function(x,m,data){
  c = 0.711384
  k = 0.144351
  alpha = 0.308298
  theta = x[1]
  eta = x[2]
  n = length(data)
  logsum1 = sum(log(data[1:m]))
  logsum2 = sum(log(data[(m+1):n]))
  invsum1 = sum(1/data[1:m]^eta)
  -((alpha*m+(alpha-k)*(n-m))*log(theta)+n*log(eta)-k*theta*invsum1-(alpha*eta-1)*logsum1-((alpha-k)*eta+1)*logsum2)
}

#' raw_est_eigp
#' @title The Optimization Function for EIGP Maximum Likelihood Estimation.
#' @description  This function serves as the optimization function for EIGP at different locations of density change points.
#' @param data Observations.
#' @param init The vector of initial values of the model parameters. The default is c(1,1).
#' @param lower_bound The vector of the lower bound for the parameters. The default is c(0.01,0.01).
#' @return The matrix with estimates of theta and eta for n-1 different locations of density change points (1st column for theta, 2nd column for eta).
#' @importFrom stats optim
#' @examples
#' raw_est_eigp(seq(1:100))
#' @export
raw_est_eigp = function(data,init = c(1,1),lower_bound = c(0.01,0.01)){
  estimate = matrix(0,nrow = (length(data)-1),ncol = 2)#For theta and eta
  for(i in 1:(length(data)-1)){
    estimate[i,] = stats::optim(init,lower = lower_bound,eigp_nll,method="L-BFGS-B",m=i,data = sort(data))$par
  }
  return(estimate)
}

#' validation
#' @title The validation Function for Model Parameters.
#' @description  This function checks if the estimates from raw_est_eigp or raw_est_eep satisfy the pre-defined conditions for the parameters.
#' @param data Observations.
#' @param estimate The data frame with 2 columns named 'theta' and 'eta'.
#' @return A Boolean vector.
#' @examples
#' estimate = raw_est_eigp(seq(1:100),init = c(1,1),lower_bound = c(0.01,0.01))
#' estimate = data.frame(estimate)
#' colnames(estimate) = c('theta','eta')
#' validation(seq(1:100),estimate)
#' @export
validation = function(data,estimate){
  data = sort(data)
  eta = estimate$eta
  theta = estimate$theta
  judge = rep(0,length(data)-1)
  for(i in 1:(length(data)-1)){
    if (data[i]^(eta[i])<= theta[i] & data[i+1]^(eta[i])>= theta[i]){
      judge[i] = 1
    }
  }
  return(judge)
}

#' eigp_optim
#' @title The Wrapper Function that Returns the Final Estimates from Maximum Likelihood Estimation for EIGP.
#' @description  This function serves as a wrapper that returns the final estimates of theta, eta, and the corresponding density change point
#' @param data Observations.
#' @param init The vector of initial values of the model parameters. The default is c(1,1).
#' @param lower_bound The vector of the lower bound for the parameters. The default is c(0.01,0.01).
#' @return A data frame with 1 row and 3 columns that contains the MLE of theta, eta, and the predicted density change point.
#' @examples
#' eigp_optim(seq(1:100))
#' @export
eigp_optim = function(data,init = c(1,1),lower_bound = c(0.01,0.01)){
  estimate = raw_est_eigp(data,init,lower_bound)
  estimate = data.frame(estimate)
  colnames(estimate) = c('theta','eta')
  estimate$thres = (estimate$theta)^(1/estimate$eta)
  thres = estimate$thres
  mle = estimate[which(validation(data,estimate)==1),]
  return(mle)
}


#' eep_nll
#' @title The EEP Negative Log-likelihood Function.
#' @description  This function serves as the objective function for the Maximum Likelihood Estimation procedure for EEP.
#' @param x Vector of parameters.
#' @param m The number of data items less than the density change point.
#' @param data Observations.
#' @return A scalar that represents the negative loglikelihood of a EEP sample given the model parameter and the data.
#' @examples
#' eep_nll(c(2,2),50,seq(1:100))
#' @export
eep_nll = function(x,m,data){
  c = 0.574
  alpha = 0.349976
  theta = x[1]
  eta = x[2]
  n = length(data)
  logsum1 = sum(log(data[1:m]))
  logsum2 = sum(log(data[(m+1):n]))
  expsum = sum(data[1:m]^eta)
  -(n*log(eta)+(eta-1)*logsum1-(alpha*eta+1)*logsum2+(alpha*n-(alpha+1)*m)*log(theta)-(alpha+1)/theta*expsum)
}

#' raw_est_eep
#' @title The Optimization Function for EEP Maximum Likelihood Estimation.
#' @description  This function serves as the optimization function for EEP at different locations of density change points.
#' @param data Observations.
#' @param init The vector of initial values of the model parameters. The default is c(1,1).
#' @param lower_bound The vector of the lower bound for the parameters. The default is c(0.01,0.01).
#' @return The matrix with estimates of theta and eta for n-1 different locations of density change points (1st column for theta, 2nd column for eta).
#' @importFrom stats optim
#' @examples
#' raw_est_eep(seq(1:100))
#' @export
raw_est_eep = function(data,init = c(1,1),lower_bound = c(0.01,0.01)){
  n = length(data)
  estimate = matrix(0,nrow = n-1,ncol = 2)#For theta and eta
  for(i in 1:(n-1)){
    estimate[i,] = stats::optim(init,lower = lower_bound,eep_nll,method="L-BFGS-B",m=i,data = sort(data))$par
  }
  return(estimate)
}

#' eep_optim
#' @title The Wrapper Function that Returns the Final Estimates from Maximum Likelihood Estimation for EEP.
#' @description  This function serves as a wrapper that returns the final estimates of theta, eta, and the corresponding density change point
#' @param data Observations.
#' @param init The vector of initial values of the model parameters. The default is c(1,1).
#' @param lower_bound The vector of the lower bound for the parameters. The default is c(0.01,0.01).
#' @return A data frame with 1 row and 3 columns that contains the MLE of theta, eta, and the predicted density change point.
#' @examples
#' eep_optim(seq(1:100))
#' @export
eep_optim = function(data,init = c(1,1),lower_bound = c(0.01,0.01)){
  estimate = raw_est_eep(data,init,lower_bound)
  estimate = data.frame(estimate)
  colnames(estimate) = c('theta','eta')
  estimate$thres = (estimate$theta)^(1/estimate$eta)
  thres = estimate$thres
  mle = estimate[which(validation(data,estimate)==1),]
  return(mle)
}

