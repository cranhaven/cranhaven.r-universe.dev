

##########################################################################################
### Local Polynomial Whittle with Noise Estimator of Frederiksen et al. (2013, JoE)
##########################################################################################


#'Polynomial function used for approximation of short memory terms and noise dynamics.
#'@keywords internal
poly_LPWN<-function(lambda,theta_p){
  R_p<-length(theta_p)
  power<-1:R_p
  p_j<-sum(theta_p*lambda^(2*power))
}


#'Augmented spectral density function used in LPWN estimator.
#'@keywords internal
h_LPWN<-function(d,theta,lambda,R_short,R_noise){
  if(R_short>0 && R_noise>0){
    theta_y<-theta[1:R_short]
    theta_rho<-theta[(R_short+1)]
    theta_w<-theta[(R_short+2):(R_short+R_noise+1)]
    #print(theta_y)
    return(exp(poly_LPWN(lambda,theta_p=theta_y))+theta_rho*lambda^(2*d)*exp(poly_LPWN(lambda,theta_p=theta_w)))
  }
  if(R_short>0 && R_noise==0){
    theta_y<-theta[1:R_short]
    theta_rho<-theta[(R_short+1)]
    theta_w<-NULL
    #print(theta_y)
    return(exp(poly_LPWN(lambda,theta_y))+theta_rho*lambda^(2*d))
  }
  if(R_noise>0 && R_short==0){
    theta_y<-NULL
    theta_rho<-theta[1]
    theta_w<-theta[2:(R_noise+1)]
    return(1+theta_rho*lambda^(2*d)*exp(poly_LPWN(lambda,theta_w)))
  }
  if(R_noise==0 && R_short==0){
    theta_y<-NULL
    theta_rho<-theta[1]
    theta_w<-theta[2:(R_noise+1)]
    return(1+theta_rho*lambda^(2*d))
  }
}

#'Estimator of G using polynomial approximation.
#'@keywords internal
G_LPWN<-function(data, d, m, lambda_j, h_pol, R_short=R_short, R_noise=R_noise){
  peri<-per(data)[-1][1:m]
  mean(lambda_j^(2*d)*peri/h_pol)
}


#'Objective function of LPWN estimator.
#'@keywords internal
R.LPWN<-function(param.vec, data, m, R_short=R_short, R_noise=R_noise){
  d<-param.vec[1]
  theta<-param.vec[-1]
  T.S<-length(data)
  lambda_j<-2*pi/T.S*(1:m)
  h_pol<-apply(as.matrix(lambda_j,m,1), 1, h_LPWN, theta=theta, d=d, R_short=R_short, R_noise=R_noise)
  #for(i in 1:m){h_pol[i]<-h_LPWN(d,theta,lambda=lambda_j[i],R_short,R_noise)}
  #ts.plot(h_pol)
  Q<-log(G_LPWN(data=data,d=d,m=m,lambda_j=lambda_j,h_pol=h_pol, R_short=R_short, R_noise=R_noise))-2*d*mean(log(lambda_j))+mean(log(h_pol))
  Q
}


#'B matrix in asymptotic distribution. 
#'@keywords internal
B<-function(d,m,T,R_short,R_noise){
  B_mat<-diag(R_short+R_noise+2)
  lambda_m<-2*pi/T*m
  if(R_short>0){diag(B_mat)[2:(R_short+1)]<-lambda_m^(2*(1:R_short))}
  diag(B_mat)[(R_short+2):(R_short+R_noise+2)]<-lambda_m^(2*d+2*(0:R_noise))
  B_mat<-sqrt(m)*B_mat
  B_mat
}


#' @title Local polynomial Whittle plus noise estimator
#' @description \code{LPWN} calculates the local polynomial Whittle plus noise estimator of Frederiksen et al. (2012).
#' @details add details here.
#' @param data data vector
#' @param m bandwith parameter specifying the number of Fourier frequencies.
#' @param R_short number of (even) polynomial terms used for approximation of spectral density at the origin.
#' @param R_noise number of (even) polynomial terms used for approximation of dependence in perturbation term.
#' @references Frederiksen, P., Nielsen, F. S., and Nielsen, M. O. (2012): 
#'            Local polynomial Whittle estimation of perturbed fractional processes.
#'            Journal of Econometrics, Vol. 167, No.2, pp. 426-447.
#' @examples
#' library(fracdiff)
#' T<-2000
#' d<-0.2
#' series<-fracdiff.sim(n=T, d=d, ar=0.6)$series+rnorm(T)
#' LPWN(series, m=floor(1+T^0.8), R_short=1, R_noise=0)
#' @export

LPWN<-function(data, m, R_short=0, R_noise=0){
  
  # starting values and box constraints are set as in the Monte Carlo simulations of Frederiksen et al.
  
  # set starting values
  LWN<-optim(par=c(0.25,1), fn=R.LPWN, m=m, data=data, R_short=0, R_noise=0, method="L-BFGS-B", lower=c(0.01,10^(-6)), upper=c(0.99,10^6))$par
  if(0.01<LWN[1] && LWN[1]<0.99 && LWN[2]>10^(-6) && LWN[2]<10^6){param.vec<-c(LWN[1],rep(0,R_short), LWN[2], rep(0,R_noise))}else{param.vec<-c(0.25,rep(0,R_short),1,rep(0,R_noise))}
  
  # estimate parameters
  est<-optim(par=param.vec, fn=R.LPWN, m=m, data=data, R_short=R_short, R_noise=R_noise, method="L-BFGS-B", lower=c(0.01,rep(-Inf,R_short),10^(-6),rep(-Inf,R_noise)), upper=c(0.99,rep(Inf,R_short),10^6,rep(Inf,R_noise)))$par
  # extract estimated parameters
  T<-length(data)
  d<-est[1]
  theta_rho<-est[2+R_short]
  
  # Calculate all components of variance-covariance matrix
  B_mat<-B(d,m,T,R_short,R_noise) # variance stabilizing transformation
  if(R_short>0){mu_short<--4*(1:R_short)/(1+2*(1:R_short))^2}else{mu_short<-NULL} # covariance of short memory polynomial with d
  nu_noise<--4*(d+(0:R_noise))*theta_rho^(c(0,rep(1,R_noise)))/(1+2*d+2*(0:R_noise))^2
  if(R_short>0){
  Gamma_short<-matrix(NA,R_short,R_short)
  for(i in 1:R_short){for(k in 1:R_short){Gamma_short[i,k]<-4*i*k/((1+2*i+2*k)*(1+2*i)*(1+2*k))}}}else{Gamma_short<-NULL}
  Psi_noise<-matrix(NA,(R_noise+1),(R_noise+1))
  for(i in 1:(R_noise+1)){for(k in 1:(R_noise+1)){Psi_noise[i,k]<-(4*(d+i-1)*(d+k-1)*theta_rho^(as.numeric((k-1)>=1)+as.numeric((i-1)>=1)))/((1+2*(i-1)+2*(k-1)+4*d)*(1+2*(i-1)+2*d)*(1+2*(k-1)+2*d))}}
  if(R_short>0){
    psi_noise_short<-matrix(NA,(R_noise+1),R_short)
    for(i in 0:R_noise){for(k in 1:R_short){psi_noise_short[(i+1),k]<-(4*k*(d+i)*theta_rho^(as.numeric(i>=1)))/((1+2*d+2*k+2*i)*(1+2*d+2*i)*(1+2*k))}}
  }else{psi_noise_short<-NULL}
  if(R_short==0){Omega<-rbind(c(4,t(nu_noise)),cbind(nu_noise,Psi_noise))}else{Omega<-rbind(c(4,t(mu_short),t(nu_noise)),cbind(mu_short,Gamma_short,t(psi_noise_short)),cbind(nu_noise,psi_noise_short,Psi_noise))}
  
  # calculate standard errors
  se<-tryCatch(sqrt(diag(solve(B_mat)%*%solve(Omega)%*%solve(B_mat))), error = function(e){warning("Standard errors could not be calculated. System is computationally singular.","\n");return(rep(NA,(R_short+R_noise+2)))})
  # add names to parameters
  if(R_short>0){names_short<-paste("theta_y",1:R_short,sep="")}else{names_short<-NULL}
  if(R_noise>0){names_noise<-paste("theta_w",1:R_noise,sep="")}else{names_noise<-NULL}
  names(est)<-c("d",names_short,"theta_rho",names_noise)
  names(se)<-names(est)
  return(list("parameters"=est,"standard_errors"=se)) 
}







