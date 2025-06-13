#' MLE Parameters
#'
#' @author Jesus Prada, \email{jesus.prada@estudiante.uam.es}
#' 
#'
#' @references 
#' Link to the scientific paper
#' 
#'Prada, Jesus, and Jose Ramon Dorronsoro. "SVRs and Uncertainty Estimates in Wind 
#'Energy Prediction." Advances in Computational Intelligence. Springer International 
#'Publishing, 2015. 564-577,
#'
#'with theoretical background for this package is provided below.
#' 
#' \url{http://link.springer.com/chapter/10.1007/978-3-319-19222-2_47}
#'
#' @description \code{mle_parameters} computes the optimal parameters via MLE of
#' a given distribution.
#'
#' @param phi a vector with residual values used to estimate the parameters.
#' @param dist assumed distribution for the noise in the data. Possible values to take:
#'    \itemize{
#'      \item{l: }{Zero-mean Laplace distribution.}
#'      \item{lm: }{General Laplace distribution.}
#'      \item{n: }{Zero-mean Gaussian distribution.}
#'      \item{nm: }{General Gaussian distribution.}
#'      \item{b: }{Beta distribution.}
#'      \item{w: }{Weibull distribution.}
#'      \item{moge: }{MOGE distribution.}
#'    }
#' @param ... additional arguments to be passed to the low level
#' functions (see below).
#'
#' @return \code{mle_parameters} returns a list with the estimated parameters. Depending on the distribution
#' these parameters will be one or more of the following ones:
#' 
#' \describe{
#'  \item{sigma}{scale parameter of the Laplace distribution.}
#'  \item{mu}{location or mean parameter of the Laplace or Gaussian distribution,
#' respectively.}
#'  \item{sigma_cuad}{variance parameter of the Gaussian distribution.}
#'  \item{alpha}{shape1 parameter of the Beta distribution or second parameter of the MOGE distribution.}
#'  \item{beta}{shape2 parameter of the Beta distribution.}
#'  \item{k}{shape parameter of the Weibull distribution.}
#'  \item{lambda}{lambda scale parameter of the Weibull distribution or first parameter of the MOGE distribution.}
#'  \item{theta}{third parameter of the MOGE distribution.}
#' }
#' 
#' @export
#'
#' @examples
#' # Estimate optimal parameters using default distribution ("nm").
#' mle_parameters(rnorm(100))
#'
#' # Estimate optimal parameters using "lm" distribution.
#' mle_parameters(rnorm(100),dist="lm")

mle_parameters <- function(phi,dist="nm",...){
  res<- if (dist=="l") {zero_laplace_mle(phi,...);}
  else if (dist=="lm") {general_laplace_mle(phi,...);}
  else if (dist=="n") {zero_gaussian_mle(phi,...);}
  else if (dist=="nm") {general_gaussian_mle(phi,...);}
  else if (dist=="b") {beta_mle(phi,...);}
  else if (dist=="w") {weibull_mle(phi,...);}
  else if (dist=="moge") {moge_mle(phi,...);}
  return(res);
}


#' MLE Parameters
#'
#' @rdname mle_parameters
#'
#' @description \code{zero_laplace_mle} computes the optimal parameters via MLE
#' assuming a zero-mean Laplace as noise distribution.
#'
#' @details For the zero-\eqn{\mu} Laplace distribution the optimal MLE parameters are
#' \deqn{\sigma=mean(|\phi_i|)}, where \eqn{{\phi_i}} are the residuals passed as argument.
#'
#' @export
#'
#' @examples
#' 
#' # Equivalent to mle_parameters(rnorm(100),dist="l")
#' zero_laplace_mle(rnorm(100))
zero_laplace_mle <- function(phi){
  sigma<-mean(abs(phi),na.rm=T);
  return(list(sigma=sigma));
}

#' MLE Parameters
#'
#' @rdname mle_parameters
#'
#' @description \code{general_laplace_mle} computes the optimal parameters via MLE 
#' assuming a general Laplace as noise distribution.
#'
#' @details For the general Laplace distribution the optimal MLE parameters are
#' \deqn{\mu=median(\phi_i)}\deqn{\sigma=mean(|\phi_i - \mu|)}, where \eqn{{\phi_i}} are the residuals passed as argument.
#'
#' @export
#'
#' @examples
#' 
#' # Equivalent to mle_parameters(rnorm(100),dist="lm")
#' general_laplace_mle(rnorm(100))
general_laplace_mle <- function(phi){
  mu<-stats::median(phi,na.rm=T)
  sigma<-mean(abs(phi-mu),na.rm=T);
  return(list(sigma=sigma,mu=mu));
}

#' MLE Parameters
#'
#' @rdname mle_parameters
#'
#' @description \code{zero_gaussian_mle} computes the optimal parameters via MLE 
#' assuming a zero-mean Gaussian as noise distribution.
#'
#' @details For the zero-\eqn{\mu} Gaussian distribution the optimal MLE parameters are
#' \deqn{\sigma^2=mean(\phi_i^2)}, where \eqn{{\phi_i}} are the residuals passed as argument.
#'
#' @export
#'
#' @examples
#' 
#' # Equivalent to mle_parameters(rnorm(100),dist="n")
#' zero_gaussian_mle(rnorm(100))
zero_gaussian_mle <- function(phi){
  sigma_cuad<-mean(phi^2,na.rm=T);
  return(list(sigma_cuad=sigma_cuad));
}

#' MLE Parameters
#'
#' @rdname mle_parameters
#'
#' @description \code{general_gaussian_mle} computes the optimal parameters via MLE 
#' assuming a general Gaussian as noise distribution.
#'
#' @details For the general Gaussian distribution the optimal MLE parameters are
#' \deqn{\mu=mean(\phi_i)}\deqn{\sigma^2=mean((\phi_i-\mu)^2)}, where \eqn{{\phi_i}} are the residuals passed as argument.
#'
#' @export
#'
#' @examples
#' 
#' # Equivalent to mle_parameters(rnorm(100),dist="nm")
#' general_gaussian_mle(rnorm(100))
general_gaussian_mle <- function(phi){
  mu<-mean(phi,na.rm=T)
  sigma_cuad<-mean((phi-mu)^2,na.rm=T);
  return(list(sigma_cuad=sigma_cuad,mu=mu));
}


#' MLE Parameters
#'
#' @rdname mle_parameters
#'
#' @description \code{beta_mle} computes the optimal parameters via MLE 
#' assuming a Beta as noise distribution.
#' 
#' @param m1 first moment of the residuals. Used to compute \code{alpha_0}.
#' @param m2 second moment of the residuals. Used to compute \code{beta_0}.
#' @param alpha_0 initial value for Newton-Raphson method for the parameter \eqn{\alpha}.
#' @param beta_0 initial value for Newton-Raphson method for the parameter \eqn{\beta}.
#'
#' @details For the Beta distribution values of parameters \eqn{\alpha} and
#' \eqn{\beta} are estimated using Newton-Raphson method.
#' 
#' @import rootSolve
#' 
#' @export
#'
#' @examples
#' 
#' # Equivalent to mle_parameters(rnorm(100),dist="b")
#' beta_mle(rnorm(100))
beta_mle <- function(phi,m1=mean(phi,na.rm=T),m2=mean(phi^2,na.rm=T),
                     alpha_0=(m1*(m1-m2))/(m2 - m1^2),beta_0=(alpha_0*(1-m1)/m1)){
  phi<-abs(phi)/(max(abs(phi))+10^-6);
  c1<-mean(log(phi),na.rm=T);
  c2<-mean(log(1-phi),na.rm=T);
  
  f <- function(x){
    return(c(digamma(x[1])-digamma(x[1]+x[2])-c1,digamma(x[2])-digamma(x[1]+x[2])-c2));
  }
  
  
  roots<-rootSolve::multiroot(f,c(alpha_0,beta_0))$root;
  if (exists("roots")){
    alpha<-roots[1];
    beta<-roots[2];
  } else {
    alpha<-alpha_0;
    beta<-beta_0;
  }
  
  
  
  if (is.na(alpha)){
    alpha<-alpha_0
  }
  
  if (alpha <=0){
    alpha<-alpha_0
  }
  
  if (is.na(beta)){
    beta<-beta_0;
  }
  
  if (beta <=0){
    beta<-beta_0;
  }
  return(list(alpha=alpha,beta=beta));
}


#' MLE Parameters
#'
#' @rdname mle_parameters
#'
#' @description \code{weibull_mle} computes the optimal parameters via MLE 
#' assuming a Weibull as noise distribution.
#' 
#' @param k_0 initial value for Newton-Raphson method for the parameter \eqn{\kappa}.
#'
#' @details For the Weibull distribution value of parameter \eqn{\kappa} is estimated using Newton-Raphson method
#' and then estimated value of \eqn{\lambda} is computed using the following closed form that depends on \eqn{\kappa}:
#' \deqn{\lambda=mean(\phi_i^kappa)^(1/\kappa)}
#'
#' @import rootSolve
#' 
#' @export
#'
#' @examples
#' 
#' # Equivalent to mle_parameters(rnorm(100),dist="w")
#' weibull_mle(rnorm(100))
weibull_mle <- function(phi,k_0=1){
  phi<-abs(phi);
  
  c1<-mean(log(phi),na.rm=T);
  
  f <- function(x){
    return(sum((phi^x)*log(phi))/sum(phi^x)-(1/x)-c1);
  }
  
  
  
  roots<-rootSolve::multiroot(f,k_0)$root;
  if (exists("roots")){
    k<-roots[1];
  } else {
    k<-k_0;
  }
  
  
  
  if (is.na(k)){
    k<-k_0;
  }
  
  if (k <=0){
    k<-k_0;
  }
  
  
  lambda<-(mean(phi^k,na.rm=T))^(1/k);
  
  if (is.na(lambda)){
    lambda<-1;
  }
  
  if (lambda <=0){
    lambda<-1;
  }
  
  if (lambda == Inf){
    lambda<-1;
    k<-k_0;
  }
  
  return(list(lambda=lambda,kappa=kappa));
}

#' MLE Parameters
#'
#' @rdname mle_parameters
#'
#' @description \code{moge_mle} computes the optimal parameters via MLE 
#' assuming a MOGE as noise distribution.
#'
#' @param lambda_0 initial value for Newton-Raphson method for the parameter \eqn{\lambda}.
#' @param theta_0 initial value for Newton-Raphson method for the parameter \eqn{\theta}.
#' 
#' See also 'Details' and \link{multiroot}.
#' 
#' @details For the MOGE distribution values of parameters \eqn{\lambda}, \eqn{\alpha} and
#' \eqn{\theta} are estimated using Newton-Raphson method.
#' 
#' See also 'References'.
#'
#' @import rootSolve
#' 
#' @export
#'
#' @examples
#' 
#' # Equivalent to mle_parameters(rnorm(100),dist="moge")
#' moge_mle(rnorm(100))
moge_mle <- function(phi,lambda_0=1,alpha_0=1,theta_0=1){
  phi<-abs(phi);
  
  c1<- sum(phi,na.rm=T);
  c2<-0;
  c3<-0;
  n<-length(phi);
  
  f <- function(x){
    return(c(n/x[1] + (x[2]-1)*sum((phi*exp(-x[1]*phi))/(1-exp(-x[1]*phi))) - 2*x[2]*(1-x[3])*sum((phi*exp(-x[1]*phi)*(1-exp(-x[1]*phi))^(x[2]-1))/(x[3]+(1-x[3])*(1-exp(-x[1]*phi))^x[2]))-c1,
             n/x[2]-sum(log(1-exp(-x[1]*phi)))+2*x[3]*sum((log(1-exp(-x[1]*phi)))/(x[3]+(1-x[3])*(1-exp(-x[1]*phi))^x[2]))-c2,
             n/x[3]-2*sum((1-(1-exp(-x[1]*phi))^x[2])/(x[3]+(1-x[3])*(1-exp(-x[1]*phi))^x[2]))-c3))
  }
  
  
  
  try(roots<-rootSolve::multiroot(f,c(lambda_0,alpha_0,theta_0))$root);
  if (exists("roots")){
    lambda<-roots[1];
    alpha<-roots[2];
    theta<-roots[3];
  } else {
    lambda<-lambda_0;
    alpha<-alpha_0;
    theta<-theta_0;
  }
  
  
  if (is.na(lambda)){
    lambda<-lambda_0;
  }
  
  if (lambda <=0){
    lambda<-lambda_0;
  }
  
  if (is.na(alpha)){
    alpha<-alpha_0;
  }
  
  if (alpha <=0){
    alpha<-alpha_0;
  }
  
  if (is.na(theta)){
    theta<-theta_0;
  }
  
  if (theta <=0){
    theta<-theta_0;
  }
  
  return(list(lambda=lambda,alpha=alpha,theta=theta));
}


#' Cost Functions Derivatives
#'
#' @author Jesus Prada, \email{jesus.prada@estudiante.uam.es}
#' 
#'
#' @references 
#' Link to the scientific paper
#' 
#'Prada, Jesus, and Jose Ramon Dorronsoro. "SVRs and Uncertainty Estimates in Wind 
#'Energy Prediction." Advances in Computational Intelligence. Springer International 
#'Publishing, 2015. 564-577,
#'
#'with theoretical background for this package is provided below.
#' 
#' \url{http://link.springer.com/chapter/10.1007/978-3-319-19222-2_47}
#'
#' @description \code{ILF_cost_der} computes the ILF derivative value at a given point.
#'
#' @param phi point to use as argument of the loss function derivative.
#' @param epsilon width of the insensitive band.
#' @param nu parameter to control value of \code{epsilon}.
#'
#' @return Returns a \code{numeric} representing the derivative value at a given point.
#'  
#' @export
#'
#' @examples
#' # ILF derivative value at point phi=1 with default epsilon.
#' ILF_cost_der(1)
#'
#' # ILF derivative value at point phi=1 with epsilon=2.
#' ILF_cost_der(1,2)
ILF_cost_der <- function(phi,epsilon=0.1,nu=0){
  if (abs(phi) > epsilon){
    return(sign(phi));      
  } else {
    return(0);
  }
}

#' Cost Functions Derivatives
#'
#' @rdname ILF_cost_der
#'
#' @description \code{zero_laplace_cost_der} computes the value at a given point of the loss function derivative
#' corresponding to a zero-mean Laplace distribution.
#'
#' @param sigma scale parameter of the Laplace distribution.
#'  
#' @export
#'
#' @examples
#' 
#' # Zero-mean Laplace loss function derivative value at point phi=1 with sigma=1.
#' zero_laplace_cost_der(1,1)
zero_laplace_cost_der <- function(phi,sigma){
  if (phi > 0){
    return(1/sigma);
  } else if (phi==0){
    return(0);
  } else {
    return(-1/sigma);
  }
}

#' Cost Functions Derivatives
#'
#' @rdname ILF_cost_der
#'
#' @description \code{general_laplace_cost_der} computes the value at a given point of the loss function derivative
#' corresponding to a general Laplace distribution.
#'
#' @param mu location or mean parameter of the Laplace or Gaussian distribution, respectively.
#'  
#' @export
#'
#' @examples
#' 
#' # General Laplace loss function derivative value at point phi=1 with mu=0 and sigma=1.
#' general_laplace_cost_der(1,1,0)
general_laplace_cost_der <- function(phi,sigma,mu){
  if ((phi-mu) > 0){
    return(1/sigma);
  } else if ((phi-mu)==0){
    return(0);
  } else {
    return(-1/sigma);
  }
}

#' Cost Functions Derivatives
#'
#' @rdname ILF_cost_der
#'
#' @description \code{zero_gaussian_cost_der} computes the value at a given point of the loss function derivative
#' corresponding to a zero-mean Gaussian distribution.
#'
#' @param sigma_cuad variance parameter of the Gaussian distribution.
#'  
#' @export
#'
#' @examples
#' 
#' # Zero-mean Gaussian loss function derivative value at point phi=1 with sigma_cuad=1.
#' zero_gaussian_cost_der(1,1)
zero_gaussian_cost_der <- function(phi,sigma_cuad){
  return(phi/sigma_cuad);
}

#' Cost Functions Derivatives
#'
#' @rdname ILF_cost_der
#'
#' @description \code{general_gaussian_cost_der} computes the value at a given point of the loss function derivative
#' corresponding to a general Gaussian distribution.
#'  
#' @export
#'
#' @examples
#' 
#' # General Gaussian loss function derivative value at point phi=1 with mu=0 and sigma_cuad=1.
#' general_gaussian_cost_der(1,1,0)
general_gaussian_cost_der <- function(phi,sigma_cuad,mu){
  return((phi-mu)/sigma_cuad);
}

#' Cost Functions Derivatives
#'
#' @rdname ILF_cost_der
#'
#' @description \code{beta_cost_der} computes the value at a given point of the loss function derivative
#' corresponding to a Beta distribution.
#'  
#' @param alpha shape1 parameter of the Beta distribution or second parameter of the MOGE distribution.
#' @param beta shape2 parameter of the Beta distribution.
#'   
#' @export
#'
#' @examples
#' 
#' # Beta loss function derivative value at point phi=1 with alpha=2 and beta=3.
#' beta_cost_der(1,2,3)
beta_cost_der <- function(phi,alpha,beta){
  phi<-abs(phi);
  if (phi==1){
    phi<-0.99;
  } else if (phi==0){
    phi<-0.01;
  }
  return((1-alpha)/phi - (1-beta)/(1-phi)); 
}

#' Cost Functions Derivatives
#'
#' @rdname ILF_cost_der
#'
#' @description \code{weibull_cost_der} computes the value at a given point of the loss function derivative
#' corresponding to a Weibull distribution.
#'  
#' @param lambda lambda scale parameter of the Weibull distribution or first parameter of the MOGE distribution.
#' @param kappa shape parameter of the Weibull distribution.
#'   
#' @export
#'
#' @examples
#' 
#' # Weibull loss function derivative value at point phi=1 with lambda=2 and kappa=3.
#' weibull_cost_der(1,2,3)
weibull_cost_der <- function(phi,lambda,kappa){
  phi<-abs(phi);
  if (phi <= 0){
    return(0);
  } else {
    return((1-kappa)/phi + (kappa/lambda)*(phi/lambda)^(kappa-1)); 
  }
}

#' Cost Functions Derivatives
#'
#' @rdname ILF_cost_der
#'
#' @description \code{moge_cost_der} computes the value at a given point of the loss function derivative
#' corresponding to a MOGE distribution.
#'  
#' @param theta third parameter of the MOGE distribution.
#'  
#' @details See also 'References'.
#'  
#' @export
#'
#' @examples
#' 
#' # MOGE loss function derivative value at point phi=1 with lambda=2 ,alpha=3 and theta=4.
#' moge_cost_der(1,2,3,4)
moge_cost_der <- function(phi,lambda,alpha,theta){
  phi<-abs(phi);
  if (phi <= 0){
    return(0);
  } else {
    return(lambda*(1+exp(-lambda*phi)*(2*alpha*(1-theta)*(1-exp(-lambda*phi))^
           (alpha-1) + (1-alpha)/(1-exp(-lambda*phi)))));
  }
}

#' Kernels
#'
#' @author Jesus Prada, \email{jesus.prada@estudiante.uam.es}
#' 
#' 
#' @description \code{linear_kernel} computes the linear kernel between two given vector, \eqn{x} and \eqn{y}.
#'
#' @param x \code{numeric} vector indicating value of \eqn{x}.
#' @param y \code{numeric} vector indicating value of \eqn{y}.
#'
#' @return Returns a \code{numeric} representing the kernel value.
#'  
#' @details Linear kernel: \deqn{k(x,y) = x*y}   
#' @export
#'
#' @examples
#' # Linear kernel value between point x=c(1,2,3) and point y=c(2,3,4).
#' linear_kernel(c(1,2,3),c(2,3,4))
linear_kernel<-function(x,y,gamma=0){x%*%y};

#' Kernels
#'
#' @rdname linear_kernel 
#' 
#' @description \code{gaussian_kernel} computes the gaussian kernel between two given vectors, \eqn{x} and \eqn{y}.
#'
#' @param gamma gaussian kernel parameter \eqn{\gamma}.
#' 
#' @details Gaussian kernel: \deqn{k(x,y) = exp(-\gamma||x-y||^2)} 
#'  
#' @export
#'
#' @examples
#' 
#' # Gaussian kernel value between point x=c(1,2,3) and point y=c(2,3,4) with gamma=0.1.
#' gaussian_kernel(c(1,2,3),c(2,3,4),0.1)
gaussian_kernel<-function(x,y,gamma){exp(-gamma*(norm(x-y,type="2")^2))};

#' Predictor Function
#'
#' @author Jesus Prada, \email{jesus.prada@estudiante.uam.es}
#' 
#' @references 
#' Link to the scientific paper
#' 
#' Kivinen J., Smola A. J., Williamson R.C.: Online learning with kernels. In: IEEE
#' transactions on signal processing, vol. 52, pp. 2165-2176, IEEE (2004).
#'
#'with theoretical background for NORMA optimization is provided below.
#' 
#' \url{http://realm.sics.se/papers/KivSmoWil04(1).pdf}
#' 
#' @description Computes the predictor function of a general noise SVR based on NORMA optimization.
#'
#' @param point \code{numeric} with the value of the point where we want to evaluate the predictor function.
#' @param t time parameter value indicating the iteration we want to consider.
#' @param x \code{matrix} containing training points. Each row must be
#' a point.
#' @param alpha \code{matrix} representing \eqn{\alpha} parameters of NORMA optimization in each iteration, one per row.
#' @param beta \code{numeric} representing \eqn{\beta} parameter of NORMA optimization in each iteration.
#' @param f_0 initial hypothesis.
#' @param kernel kernel function to use.
#' @param gamma gaussian kernel parameter \eqn{\gamma}.
#' @param no_beta \code{boolean} indicating if an offset \eqn{b} is used (FALSE) or not (TRUE).  
#'
#' @return Returns a \code{numeric} representing the prediction value.
#' 
#' @export
#'
#' @examples
#' f(c(1,2,3),2,matrix(c(1,2,3,4,5,6),nrow=2,ncol=3,byrow=TRUE),
#' matrix(c(1,2,3,4,5,6),nrow=2,ncol=3,byrow=TRUE),
#' c(1,2),0,function(x,y,gamma=0){x%*%y},0.1,FALSE)
f<-function(point,t,x,alpha,beta,f_0,kernel=function(x,y,gamma){exp(-gamma*(norm(x-y,type="2")^2))},gamma,no_beta){
  
  if (t==1 & ! is.na(f_0)){
    return(f_0);
  } else {
    res <- c();
    for (i in 1:(t-1)){
      res<-c(res,alpha[t,i]*kernel(x[i,],point,gamma))
    }
    res<-sum(res);
    if (no_beta){
      return(res);
    } else {
      return(res+beta[t]);
    }
  }
}



#' NORMA Optimization
#'
#' @author Jesus Prada, \email{jesus.prada@estudiante.uam.es}
#' 
#' @references 
#' Link to the scientific paper
#' 
#' Kivinen J., Smola A. J., Williamson R.C.: Online learning with kernels. In: IEEE
#' transactions on signal processing, vol. 52, pp. 2165-2176, IEEE (2004).
#'
#'with theoretical background for NORMA optimization is provided below.
#' 
#' \url{http://realm.sics.se/papers/KivSmoWil04(1).pdf}
#' 
#' @description Computes general noise SVR based on NORMA optimization.
#'
#' @param x \code{matrix} containing training points. Each row must be a point.
#' @param y \code{numeric} containing target for training points \eqn{x}.
#' @param f_0 initial hypothesis.
#' @param beta_0 initial value for offset \eqn{b}.
#' @param lambda NORMA optimization parameter \eqn{lambda}
#' @param rate learning rate for NORMA optimization. Must be a function with one argument.
#' @param kernel kernel function to use. Must be a function with three arguments such as \code{gaussian_kernel}.
#' See also \link{linear_kernel}
#' @param cost_der Loss function derivative to use. See also \link{ILF_cost_der}. Must be "ILF_cost_der" when
#' ILF derivative is used.
#' @param cost_name \code{character} indicating the symbolic name of \code{cost_der}.
#' @param gamma gaussian kernel parameter \eqn{\gamma}.
#' @param max_iterations maximum number of NORMA iterations computed.
#' @param stopping_threshold value indicating when to stop NORMA optimization. See also 'Details'.
#' @param trace \code{boolean} indicating if information messages should be printed (TRUE) or not (FALSE).
#' @param no_beta \code{boolean} indicating if an offset \eqn{b} is used (FALSE) or not (TRUE). 
#' @param fixed_epsilon \code{boolean} indicating if \code{epsilon} should be updated (FALSE) or not (TRUE).
#' @param ... additional arguments to be passed to the low level functions. 
#' 
#' @details Optimization will stop when the sum of the differences between all training predicted values of present
#' iteration versus values from previous iteration does not exceeds \code{stopping_threshold}.
#' 
#' @return Returns a \code{list} containing:
#' 
#' \describe{
#'  \item{alpha}{\code{matrix} representing \eqn{\alpha} parameters of NORMA optimization in each iteration, one per row.}
#'  \item{beta}{\code{numeric} representing \eqn{\beta} parameter of NORMA optimization in each iteration.}
#'  \item{n_iterations}{Number of NORMA iterations performed.}
#' }
#' 
#' @export
#'
#' @examples
#' NORMA(x=matrix(rnorm(10),nrow=10,ncol=1,byrow=TRUE),y=rnorm(10),kernel=function(x,y,gamma=0){x%*%y},
#' cost_der=function(phi,sigma_cuad,mu){return((phi-mu)/sigma_cuad)},cost_name="example",
#' sigma_cuad=1,mu=0)

NORMA <- function(x,y,f_0=0,beta_0=0,lambda=0,rate=function(t){1},kernel=linear_kernel,cost_der=ILF_cost_der,cost_name="ILF_cost_der",
                  gamma=1,max_iterations=nrow(x),stopping_threshold=0,trace=TRUE,no_beta=TRUE,fixed_epsilon=TRUE,...){

  # Initialize alpha values
  alpha<- t(matrix(rep(-rate(1)*cost_der(f(x[1,],1,x,NA,NA,f_0,kernel,gamma,no_beta)-y[1],...),max_iterations)));
  
  # Initialize alpha values
  beta<-beta_0;
  
  if ((cost_name=="ILF_cost_der") & !fixed_epsilon){
    # Initialize cost parameters
    epsilon<-list(...)$epsilon;
    nu<-list(...)$nu;
  }
  
  # NORMA iteration
  for (t in 2:max_iterations){
    # Show trace
    if (trace){
      print(sprintf("Starting iteration %s",t));
    }
    
    # Update distribution parameters for ILF cost
    if ((cost_name=="ILF_cost_der") & !fixed_epsilon){
      if (abs(f(x[t-1,],t-1,x,alpha,beta,f_0,kernel,gamma,no_beta)-y[t-1]) > epsilon){
        epsilon<-epsilon + (1-nu)*rate(t-1);
      } else {
        epsilon<-epsilon - nu*rate(t-1);
      }
      
      # New beta value
      beta[t]<-beta[t-1]-rate(t-1)*cost_der(f(x[t-1,],t-1,x,alpha,beta,f_0,kernel,gamma,no_beta)-y[t-1],epsilon,nu);
      
      # Initialize new alpha to last alpha
      alpha<-rbind(alpha,alpha[t-1,]);
      
      # New alpha value
      alpha[t,t]<- -rate(t)*cost_der(f(x[t,],t,x,alpha,beta,f_0,kernel,gamma,no_beta)-y[t],epsilon,nu);
    } else {
      # New beta value
      beta[t]<-beta[t-1]-rate(t-1)*cost_der(f(x[t-1,],t-1,x,alpha,beta,f_0,kernel,gamma,no_beta)-y[t-1],...);
      
      # Initialize new alpha to last alpha
      alpha<-rbind(alpha,alpha[t-1,]);
      
      # New alpha value
      alpha[t,t]<- -rate(t)*cost_der(f(x[t,],t,x,alpha,beta,f_0,kernel,gamma,no_beta)-y[t],...);
    }
    
    
    
    # Update last iteration alpha values
    for (i in 1:(t-1)){
      alpha[t,i]<-(1-rate(t)*lambda)*alpha[t,i];
    }
    
    # Remove infinite values
    alpha[alpha == Inf]<-.Machine$double.xmax;
    alpha[alpha == -Inf]<--.Machine$double.xmax;
    
    
    # Test if update step is greater than stopping threshold (if stopping_threshold different from 0)
    if (stopping_threshold != 0){
      diff<-0;
      for (i in 1:length(x)){
        diff<-diff+abs(f(x[i,],t,x,alpha,beta,f_0,kernel,gamma,no_beta) - f(x[i,],t-1,x,alpha,beta,f_0,kernel,gamma,no_beta));
      }
      diff<-as.numeric(diff/length(x));
      if (diff < stopping_threshold){
        break;
      }
    }
  }
  
  # Return final predictor function
  return(list(alpha=alpha[t,],beta=beta[t],n_iterations=t));
}