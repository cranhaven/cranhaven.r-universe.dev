\name{squeezy}
\alias{squeezy}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Fit a group-adaptive elastic net penalised linear or logistic model
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Estimate group-specific elastic net penalties and fit a linear or logistic regression model.
}
\usage{
squeezy(Y, X, groupset, alpha = 1, model = NULL, X2 = NULL, 
        Y2 = NULL, unpen = NULL, intrcpt = TRUE, 
        method = c("ecpcEN", "MML", "MML.noDeriv", "CV"), 
        fold = 10, compareMR = TRUE, selectAIC = FALSE, fit.ecpc = NULL, 
        lambdas = NULL, lambdaglobal = NULL, lambdasinit = NULL, 
        sigmasq = NULL, ecpcinit = TRUE, SANN = FALSE, minlam = 10^-3, 
        standardise_Y = NULL, reCV = NULL, opt.sigma = NULL, 
        resultsAICboth = FALSE, silent=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Y}{
%%     ~~Describe \code{Y} here~~
Response data; n-dimensional vector (n: number of samples) for linear and logistic outcomes.
}
  \item{X}{
%%     ~~Describe \code{X} here~~
Observed data; (nxp)-dimensional matrix (p: number of covariates) with each row the observed high-dimensional feature vector of a sample.
}
  \item{groupset}{
%%     ~~Describe \code{groupset} here~~
Co-data group set; list with G groups.  Each group is a vector containing the indices of the covariates in that group.
}
  \item{alpha}{
%%     ~~Describe \code{alpha} here~~
Elastic net penalty mixing parameter.
}
  \item{model}{
%%     ~~Describe \code{model} here~~
Type of model for the response; linear or logistic.
}
  \item{X2}{
%%     ~~Describe \code{X2} here~~
(optional) Independent observed data for which response is predicted.
}
  \item{Y2}{
%%     ~~Describe \code{Y2} here~~
(optional) Independent response data to compare with predicted response.
}
  \item{unpen}{
%%     ~~Describe \code{unpen} here~~
Unpenalised covariates; vector with indices of covariates that should not be penalised.
}
  \item{intrcpt}{
%%     ~~Describe \code{intrcpt} here~~
Should an intercept be included? Included by default for linear and logistic, excluded for Cox for which the baseline hazard is estimated.
}
  \item{method}{
%%     ~~Describe \code{method} here~~
Which method should be used to estimate the group-specific penalties? Default MML.
}
  \item{fold}{
%%     ~~Describe \code{fold} here~~
Number of folds used in inner cross-validation to estimate (initial) global ridge penalty lambda (if not given).
}
  \item{compareMR}{
%%     ~~Describe \code{compareMR} here~~
TRUE/FALSE to fit the multi-ridge model and return results for comparison.
}
  \item{selectAIC}{
%%     ~~Describe \code{selectAIC} here~~y
TRUE/FALSE to select the single-group model or multi-group model.
}
  \item{fit.ecpc}{
%%     ~~Describe \code{fit.ecpc} here~~
(optional) Model fit obtained by the function ecpc (from the ecpc R-package)
}
  \item{lambdas}{
%%     ~~Describe \code{lambdas} here~~
(optional) Group-specific ridge penalty parameters. If given, these are transformed to elastic net penalties.
}
  \item{lambdaglobal}{
%%     ~~Describe \code{lambdaglobal} here~~
(optional) Global ridge penalty parameter used for initialising the optimisation.
}
  \item{lambdasinit}{
%%     ~~Describe \code{lambdasinit} here~~
(optional) Group-specific ridge penalty parameters used for initialising the optimisation.
}
  \item{sigmasq}{
%%     ~~Describe \code{sigmasq} here~~
(linear model only) If given, noise level is fixed (Y~N(X*beta,sd=sqrt(sigmasq))).
}
  \item{ecpcinit}{
%%     ~~Describe \code{ecpcinit} here~~
TRUE/FALSE for using group-specific ridge penalties as given in `fit.ecpc' for initialising the optimisation.
}
  \item{SANN}{
%%     ~~Describe \code{SANN} here~~
('method'=MML.noDeriv only) TRUE/FALSE to use simulated annealing in optimisation of the ridge penalties.
}
  \item{minlam}{
%%     ~~Describe \code{minlam} here~~
Minimal value of group-specific ridge penalty used in the optimisation.
}
  \item{standardise_Y}{
%%     ~~Describe \code{standardise_Y} here~~
TRUE/FALSE should Y be standardised?
}
  \item{reCV}{
%%     ~~Describe \code{reCV} here~~
TRUE/FALSE should the elastic net penalties be recalibrated by cross-validation of a global rescaling penalty?
}
  \item{opt.sigma}{
%%     ~~Describe \code{opt.sigma} here~~
(linear model only) TRUE/FALSE to optimise sigmasq jointly with the ridge penalties.
}
  \item{resultsAICboth}{
%%     ~~Describe \code{resultsAICboth} here~~
(selectAIC=TRUE only) TRUE/FALSE to return results of both the single-group and multi-group model.
}
  \item{silent}{
%%     ~~Describe \code{normalise} here~~
Should output messages be suppressed (default FALSE)?
}
}

\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
\item{betaApprox}{
Estimated regression coefficients of the group-adaptive elastic net model; p-dimensional vector.
}
\item{a0Approx}{
Estimated intercept of the group-adaptive elastic net model; scalar.
}
\item{lambdaApprox}{
Estimated group penalty parameters of the group-adaptive elastic net model; G-dimensional vector.
}
\item{lambdapApprox}{
Estimated elastic net penalty parameter of the group-adaptive elastic net model for all covariates; p-dimensional vector.
}
\item{tauMR}{
Estimated group variances of the multi-ridge model; G-dimensional vector.
}
\item{lambdaMR}{
Estimated group penalties of the multi-ridge model; G-dimensional vector.
}
\item{lambdaglobal}{
Estimated global ridge penalty; scalar. Note: only optimised if selectAIC=TRUE or compareMR=TRUE, else the returned crude estimate is sufficient for initialisation of squeezy.
}
\item{sigmahat}{
(linear model) Estimated sigma^2; scalar.
}
\item{MLinit}{
Min log marginal likelihood value at initial group penalties; scalar.
}
\item{MLfinal}{
Min log marginal likelihood value at estimated group penalties; scalar.
}
\item{alpha}{
Value used for the elastic net mixing parameter alpha; scalar.
}
\item{glmnet.fit}{
Fit of the `glmnet' function to obtain the regression coefficients.
}

If `compareMR'=TRUE, multi-ridge model is returned as well:
\item{betaMR}{
Estimated regression coefficients of the multi-ridge model; p-dimensional vector.
}
\item{a0MR}{
Estimated intercept of the multi-ridge model; scalar.
}

If independent test set `X2' is given, predictions and MSE are returned:
\item{YpredApprox}{
Predictions for the test set of the estimated group-adaptive elastic net model.
}
\item{MSEApprox}{
Mean squared error on the test set of the estimated group-adaptive elastic net model.
}
\item{YpredMR}{
Predictions for the test set of the estimated group-adaptive multi-ridge model.
}
\item{MSEMR}{
Mean squared error on the test set of the estimated group-adaptive multi-ridge model.
}

If `selectAIC'=TRUE, the multi-group or single-group model with best AIC is selected.
Results in `betaApprox', `a0Approx', `lambdaApprox' contain those results of the best model.
Summary results of both models are included as well:
\item{AICmodels}{
List with elements ``multigroup" and ``onegroup".- Each element is a list with results of the multi-group or single-group model, containing the group penalties (`lambdas'), sigma^2 (`sigmahat', linear model only), and AIC (`AIC').

If besides `selectAIC'=TRUE, also `resultsAICboth'=TRUE, the fit of both the single-group model and multi-group model as obtained with \link{squeezy} are returned (`fit').
}
\item{modelbestAIC}{
Either ``onegroup" or ``multigroup" for the selected model.
}




}
\references{
%% ~put references to the literature/web site here ~
Mirrelijn M. van Nee, Tim van de Brug, Mark A. van de Wiel, "Fast marginal likelihood estimation of penalties for group-adaptive elastic net", arXiv preprint, arXiv:2101.03875 (2021).
}
\author{
%%  ~~who you are~~
Mirrelijn M. van Nee, Tim van de Brug, Mark A. van de Wiel
}
\examples{

#####################
# Simulate toy data #
#####################
p<-100 #number of covariates
n<-50 #sample size training data set
n2<-100 #sample size test data set
G<- 5 #number of groups

taugrp <- rep(c(0.05,0.1,0.2,0.5,1),each=p/G) #ridge prior variance
groupIndex <- rep(1:G,each=p/G) #groups for co-data
groupset <- lapply(1:G,function(x){which(groupIndex==x)}) #group set with each element one group
sigmasq <- 2 #linear regression noise
lambda1 <- sqrt(taugrp/2) #corresponding lasso penalty
#A Laplace(0,b) variate can also be generated as the difference of two i.i.d.
#Exponential(1/b) random variables
betas <-   rexp(p, 1/lambda1) -  rexp(p, 1/lambda1) #regression coefficients
X <- matrix(rnorm(n*p),n,p) #simulate training data
Y <- rnorm(n,X\%*\%betas,sd=sqrt(sigmasq))
X2 <- matrix(rnorm(n*p),n,p) #simulate test data
Y2 <- rnorm(n,X2\%*\%betas,sd=sqrt(sigmasq))

###############
# Fit squeezy #
###############
#may be fit directly..
res.squeezy <- squeezy(Y,X,groupset=groupset,Y2=Y2,X2=X2,
                       model="linear",alpha=0.5)

\donttest{
  #..or with ecpc-fit as initialisation
  if(requireNamespace("ecpc")){
    res.ecpc <- ecpc::ecpc(Y,X, #observed data and response to train model
                     groupsets=list(groupset), #informative co-data group set
                     Y2=Y2,X2=X2, #test data
                     model="linear",
                     hypershrinkage="none",postselection = FALSE)
    res.squeezy <- squeezy(Y,X, #observed data and response to train model
                           groupset=groupset, #informative co-data group set
                           Y2=Y2,X2=X2, #test data
                           fit.ecpc = res.ecpc, #ecpc-fit for initial values
                           model="linear", #type of model for the response
                           alpha=0.5) #elastic net mixing parameter
  }
}


summary(res.squeezy$betaApprox) #estimated elastic net regression coefficients
summary(res.squeezy$betaMR) #estimated multi-ridge regression coefficients
res.squeezy$lambdaApprox #estimated group elastic net penalties
res.squeezy$tauMR #multi-ridge group variances
res.squeezy$MSEApprox #MSE group-elastic net model
res.squeezy$MSEMR #MSE group-ridge model

#once fit, quickly find model fit for different values of alpha:
res.squeezy2 <- squeezy(Y,X, #observed data and response to train model
                        groupset=groupset, #informative co-data groupset
                        Y2=Y2,X2=X2, #test data
                        lambdas = res.squeezy$lambdaMR, #fix lambdas at multi-ridge estimate
                        model="linear", #type of model for the response
                        alpha=0.9) #elastic net mixing parameter

\donttest{
  #Select single-group model or multi-group model based on best mAIC
  res.squeezy <- squeezy(Y,X, #observed data and response to train model
                         groupset=groupset, #informative co-data group set
                         Y2=Y2,X2=X2, #test data
                         fit.ecpc = res.ecpc, #ecpc-fit for initial values
                         model="linear", #type of model for the response
                         alpha=0.5, #elastic net mixing parameter
                         selectAIC = TRUE,resultsAICboth = TRUE)
  
  res.squeezy$modelbestAIC #selected model
  res.squeezy$AICmodels$multigroup$fit$MSEApprox #MSE on test set of multi-group model
  res.squeezy$AICmodels$onegroup$fit$MSEApprox #MSE on test set of single-group model
}

}
