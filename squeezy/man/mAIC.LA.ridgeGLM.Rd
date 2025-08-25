\name{mAIC.LA.ridgeGLM}
\alias{mAIC.LA.ridgeGLM}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Marginal AIC of a multi-group, ridge penalised GLM
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Compute the marginal AIC for the marginal likelihood (ML) of multi-group, ridge penalised generalised linear models. Note: currently only implemented for linear and logistic regression.
}
\usage{
mAIC.LA.ridgeGLM(loglambdas, XXblocks, Y, sigmasq = 1, 
                Xunpen = NULL, intrcpt = TRUE, model, minlam = 0)
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
}

\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
  \item{mAIC }{mAIC of the model}
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

#compute the mAIC of a co-data model with multiple groups
mAIC.LA.ridgeGLM(loglambdas=log(sigmahat/tauMR), XXblocks=XXblocks, 
                 Y = Y, sigmasq = sigmahat, model="linear")

#compute the mAIC of a co-data agnostic model, i.e. only one group of covariates
mAIC.LA.ridgeGLM(loglambdas=log(sigmahat/median(tauMR)), 
                 XXblocks=list(X\%*\%t(X)), 
                 Y = Y, sigmasq = sigmahat, model="linear")
}


