#' @title The L-Logistic Bayesian Regression
#' @name llbayesireg
#' @description Function to estimate a L-Logistic regression model with median and  precision regression structures.
#' @param y Object of class vector, with the response.
#' @param X Object of class matrix, with the variables for modelling the meadian. The default is NULL.
#' @param W Object of class matrix, with the variables for modelling the presision. The default is NULL.
#' @param niter A positive integer specifying the number of iterations for each chain. The default is 1000.
#' @param chains A positive integer specifying the number of Markov chains. The default is 1.
#' @param burn A positive integer specifying the period sampling (known as the burn-in). The default is niter/2.
#' @param jump A positive integer specifying the period for saving samples. The default is 1.
#'
#' @return llbayesireg(y, X, W, niter = 1000, chains = 1, burn = floor(niter/2), jump = 1) give the estimate a L-Logistic regression model with median and  precision regression structures..
#'
#' @source The L-Losgistic distribution was introduced by Tadikamalla and Johnson (1982), which refer to this distribution as Logit-Logistic
#' distribution. Here, we have a new parameterization of the Logit-Logistic with the median as a parameter.

#'
#' @references Paz, R.F., Balakrishnan, N and Bazán, J.L. (2018). L-Logistic Distribution: Properties, Inference and an Application to Study Poverty and Inequality in Brazil.
#'
#' @details See https://cran.r-project.org/web/packages/llogistic/llogistic.pdf.
#'
#' @examples
#' # Modelation the coeficient with generated data
#'
#' library(llbayesireg)
#' library(llogistic)
#'
#' # Number of elements to be generated
#'
#' n=50
#'
#' # Generated response
#'
#' bin=2005
#' set.seed(bin)
#' y=rllogistic(n,0.5, 2)
#'
#' fitll = llbayesireg(y, niter=100, jump=10)
#'
#' m.hat=mean(fitll$sample.m); m.hat
#' phi.hat=mean(fitll$sample.phi); phi.hat
#'
#' \donttest{
#' # Modelation the coeficient with real data
#'
#' library(llbayesireg)
#'
#' data("Votes","MHDI")
#'
#' y = Votes[,4]
#' X = MHDI
#'
#' fitll = llbayesireg(y,X)
#'
#' summary(fitll$object, pars = c("beta","delta"), probs = c(0.025,0.975))
#'
#' plot(fitll$betas[,1,1], type = "l")
#' }
#'
#' @importFrom rstan stan
#'
#' @import llogistic ggplot2 StanHeaders Rcpp stats
#'
#' @export llbayesireg
llbayesireg = function (y, X=NULL, W=NULL, niter=1000, chains=1, burn=floor(niter/2), jump=1){

  if(is.null(y)){
    stop("There is no data")
  }

  if(length(y)==1){
    stop("There is no ideal data size, length(y) must be greater than 1")
  }

  if(burn < 0){
    stop("Burn must be a positive number")
  }

  if(niter <= 0){
    stop("The number of simulations must be greater than 0")
  }

  if(jump < 0|jump > niter){
    stop("Jumper must be a positive number lesser than niter")
  }


  y=as.vector(y)

  if(is.null(X) && is.null(W)){

    Loglink = function(y,betas){
      med=c(exp(betas[1])/(1+exp(betas[1])))
      phi=exp(betas[2])
      som=sum(dllogistic(y,med,phi, log= TRUE))
      return(som)
    }

    post.betas=function(y,betas){
      Loglink(y,betas)+sum(dnorm(betas,0,100,log=TRUE))
    }

    update.betas=function(y,betas){
      btan=betas
      btan[1]=rnorm(1, mean = betas[1],0.2)
      fnew=  post.betas(y,btan)
      fold=  post.betas(y,betas)
      wp=min(1,exp(fnew - fold));
      u=runif(1)
      if(u< wp){betas[1]=btan[1]}

      btan=betas
      btan[2]=rnorm(1, mean = betas[2],0.2)
      fnew=  post.betas(y,btan)
      fold=  post.betas(y,betas)
      wp=min(1,exp(fnew - fold));
      u=runif(1)
      if(u< wp){betas[2]=btan[2]}

      return(betas)
    }

    btas=rnorm(2,0,0.5)

    inter=0
    while(inter< burn){
      btas=update.betas(y,btas)
      inter=inter+1
    }

    betas=numeric()
    niter=niter-burn
    inter=0
    while(inter< niter){
      for(i in 1:jump){
        btas=update.betas(y,btas)
      }
      betas=rbind(betas, btas)
      inter=inter+1
    }

    fitll=list()
    fitll$object = matrix(betas)
    fitll$betas = matrix(betas[,1])
    fitll$deltas = matrix(betas[,2])

    fitll$sample.m= exp(fitll$betas)/(1+exp(fitll$betas))
    fitll$sample.phi=exp(fitll$deltas)

  }else{
    if(is.null(X)){
      X = rep(1, length(y))
    }else{
      X = cbind(rep(1, length(y)), X)
    }

    if(is.null(W)){
      W = rep(1, length(y))
    }else{
      W = cbind(rep(1, length(y)), W)
    }

    X=as.matrix(X)
    W=as.matrix(W)



    ####inicio do codigo STAN

    stanmodelcode <- "
    // density fuction
    functions{
    real llogistic_rng(real m, real phi){
    real y_ppc;
    real x;
    x = uniform_rng(0, 1);
    y_ppc=(m*x^(1/phi))/((1-x)^(1/phi)*(1-m)+x^(1/phi)*m);
    return y_ppc;
    }
    // likelihoo fuction
    real llogistic_log(vector y, vector m, vector phi){
    vector[num_elements(y)] prob;
    real lprob;
    for(i in 1:num_elements(y)){
    prob[i]=log(phi[i])+phi[i]*log(1-m[i])+phi[i]*log(m[i])+(phi[i]-1)*log(1-y[i])+
    (phi[i]-1)*log(y[i])-2*log((pow(1-m[i], phi[i])*pow(y[i], phi[i])+pow(m[i], phi[i])*pow(1-y[i], phi[i])));
    }
    lprob = sum(prob);
    return lprob;
    }
    }
    // fitting  model
    data {
    int<lower=0> n; //the number of observations
    int q;         //the number of columns in the model matrix for beta
    int d;         //the number of columns in the model matrix for delta
    vector[n] y;    //the response
    matrix[n,q] X; //the model matrix for beta
    matrix[n,d] W; //the model matrix for delta
    }

    parameters{
    vector[q] beta;    //the regression parameters for beta
    vector[d] delta;    //the regression parameters for delta
    }
    transformed parameters {
    vector[n] m;
    vector[n] phi;
    m = inv_logit(X*beta);
    phi=exp(W*delta);
    }
    model{
    to_vector(delta) ~ normal(0,10);
    to_vector(beta) ~ normal(0,10);
    y ~ llogistic(m,phi);
    }
    //sample from predictive
    generated quantities{
    vector[n] y_pred;
    {
    for(i in 1:n){
    y_pred[i] =  llogistic_rng(m[i],phi[i]);
    }
    }
    }
    "
    ###### fim do c?digo STAN

    dat = list(q=ncol(X),d=ncol(W),n= length(y), X=X, W=W,y=y);

    fit = stan(model_code = stanmodelcode, data = dat, iter = niter, chains = chains, warmup = burn, thin = jump)

    fitll=list()
    fitll$object = fit
    fitll$betas=extract(fit, pars = "beta", permuted = FALSE)
    fitll$deltas=extract(fit, pars = "delta", permuted = FALSE)
    fitll$sample.m=extract(fit, pars = "m", permuted = FALSE)
    fitll$sample.phi=extract(fit, pars = "phi", permuted = FALSE)
    fitll$pred=extract(fit, pars = "y_pred", permuted = FALSE)
    fitll$q=ncol(X)
    fitll$d=ncol(W)

  }

  return(fitll)

}
