\name{dminML.LA.ridgeGLM}
\alias{dminML.LA.ridgeGLM}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Partial derivatives of -log(ML) of ridge penalised GLMs
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Returns the partial derivatives (w.r.t. 'loglambdas') of the minus log Laplace approximation (LA) of the marginal likelihood of ridge penalised generalised linear models. Note: currently only implemented for linear and logistic regression.
}
\usage{
dminML.LA.ridgeGLM(loglambdas, XXblocks, Y, sigmasq = 1, 
                  Xunpen = NULL, intrcpt = TRUE, model, minlam = 0, 
                  opt.sigma = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{loglambdas}{
%%     ~~Describe \code{loglambdas} here~~
Logarithm of the ridge penalties as returned by ecpc or squeezy; Gx1 vector.
}
  \item{XXblocks}{
%%     ~~Describe \code{XXblocks} here~~
List of sample covariance matrices X_g \%*\% t(X_g) for groups g = 1,..., G.
}
  \item{Y}{
%%     ~~Describe \code{Y} here~~
Response data; n-dimensional vector (n: number of samples) for linear and logistic outcomes.
}
  \item{sigmasq}{
%%     ~~Describe \code{sigmasq} here~~
(linear model only) Noise level (Y~N(X*beta,sd=sqrt(sigmasq))).
}
  \item{Xunpen}{
%%     ~~Describe \code{Xunpen} here~~
Unpenalised variables; nxp_1-dimensional matrix for p_1 unpenalised variables.
}
  \item{intrcpt}{
%%     ~~Describe \code{intrcpt} here~~
Should an intercept be included? Set to TRUE by default.
}
  \item{model}{
%%     ~~Describe \code{model} here~~
Type of model for the response; linear or logistic. 

}
  \item{minlam}{
%%     ~~Describe \code{minlam} here~~
Minimum value of lambda that is added to exp(loglambdas); set to 0 as default.
}
  \item{opt.sigma}{
%%     ~~Describe \code{opt.sigma} here~~
(linear model only) TRUE/FALSE if log(sigmasq) is given as first argument of 
  loglambdas for optimisation purposes
}
}

\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
Partial derivatives of the Laplace approximation of the minus log marginal likelihood to the model parameters 'loglambdas';

For opt.sigma=FALSE: Gx1-dimensional vector for the G log(group ridge penalties). 

For opt.sigma=TRUE (linear model only): (G+1)x1-dimensional vector for the partial derivative to log(sigmasq) (first element) and for the G log(group ridge penalties). 

}
\examples{
#Simulate toy data
n<-100
p<-300
X <- matrix(rnorm(n*p),n,p)
Y <- rnorm(n)
groupset <- list(1:(p/2),(p/2+1):p)
sigmahat <- 2
alpha <- 0.5
tauMR <- c(0.01,0.005)

XXblocks <- lapply(groupset, function(x)X[,x]\%*\%t(X[,x]))

#compute partial derivatives of the minus log marginal likelihood to the penalties only
dminML.LA.ridgeGLM(loglambdas = log(sigmahat/tauMR), 
                  XXblocks, Y, sigmasq = sigmahat, 
                  model="linear",opt.sigma=FALSE)

#additionally, compute the partial derivative to the linear regression noise parameter sigma^2
dminML.LA.ridgeGLM(loglambdas = log(c(sigmahat,sigmahat/tauMR)), 
                   XXblocks, Y, sigmasq = sigmahat, 
                   model="linear",opt.sigma=TRUE)
}
