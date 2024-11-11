\name{cv.EBglmnet}
\alias{cv.EBglmnet}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Cross Validation (CV) Function to Determine Hyperparameters of the EBglmnet Algorithms}
\description{The degree of shrinkage, or equivalently, the number of non-zero effects selected by EBglmnet are 
				controlled by the hyperparameters in the prior distribution, which can be obtained 
				via Cross Validation (CV). This function performs k-fold CV for hyperparameter selection, and 
				outputs the model fit results using the optimal parameters. Therefore, this function runs
				\code{EBglmnet} for (\code{k x n_parameters + 1}) times. By default, EBlasso-NE tests 20
				\eqn{\lambda}s , EBEN tests an additional 10 \eqn{\alpha}s (thus a total of 200 pair of 
				hyperparameters), and EBlasso-NEG tests up to 25 pairs of (a,b).
			}
\usage{
cv.EBglmnet(x, y, family=c("gaussian","binomial"),
		prior= c("lassoNEG","lasso","elastic net"), nfolds=5, 
		foldId, verbose = 0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{input matrix of dimension \code{n} x \code{p}; each row is an
  observation vector, and each column is a candidate variable. When epistasis is considered, users do not need
  to create a giant matrix including both main and interaction terms. Instead, \code{x} should always be
  the matrix corresponding to the \code{p} main effects, and \code{cv.EBglmnet} will generate the interaction terms
  dynamically during running time.}
  \item{y}{response variable. Continuous for \code{family="gaussian"}, and binary for 
  \code{family="binomial"}. For binary response variable, y can be a Boolean or numeric vector, or factor type
  array.}
  \item{family}{model type taking values of "gaussian" (default) or "binomial". } 
  \item{prior}{prior distribution to be used. Taking values of "lassoNEG"(default), "lasso", and "elastic net". 
  All priors will produce a sparse outcome of the regression coefficients; see Details for choosing priors. }  
  \item{nfolds}{number of n-fold CV. \code{nfolds} typically >=3. Although \code{nfolds}
    can be as large as the sample size (leave-one-out CV), it will be computationally intensive for large datasets. Default value is \code{nfolds=5}.}
  \item{foldId}{an optional vector of values between 1 and \code{nfolds}
    identifying which fold each observation is assigned to. If not supplied, each of the \code{n} samples will be 
	assigned to the \code{nfolds} randomly.} 
  \item{verbose}{parameter that controls the level of message output from EBglment. It takes values from 0 to 5; larger verbose displays more messages. 0 is recommended for CV to avoid excessive outputs. Default value for \code{verbose} is minimum message output.}
}
\details{	The three priors in EBglmnet all contain hyperparameters that control how heavy the tail probabilities are. Different values of the hyperparameters will yield different number of non-zero effects retained in the model. 
	Appropriate selection of their values is required to obtain optimal results, and CV is the most 
	oftenly used method. For Gaussian model, CV determines the optimal hyperparameter values that yield 
	the minimum square error. In Binomial model, CV calculates the mean logLikelihood in each of 
	the left out fold, and chooses the values that yield the maximum mean logLikelihood value of the k-folds.
	See \code{EBglmnet} for the details of hyperparameters in each prior distribution. \cr \cr

}		
		
		
\value{
	\item{CrossValidation}{matrix of CV result with columns of: \cr
	column 1: hyperparameter1 \cr
	column 2: hyperparameter2 \cr
	column 3: prediction metrics/Criteria\cr
	column 4: standard error in the k-fold CV. \cr
	
	Prediction metrics is the mean square error (MSE) for Gaussian model and mean log likelihood (logL) for the binomial model.	}
	\item{optimal hyperparameter}{the hyperparameters that yield the smallest MSE or the largest logL.}
	\item{fit}{model fit using the optimal parameters computed by CV. See \code{EBglmnet} for values in this item. }
	\item{WaldScore}{the Wald Score for the posterior distribution.	See (Huang A., Martin E., et al., 2014b) for using Wald Score to identify significant  effect set.}
	\item{Intercept}{model intercept. This parameter is not shrunk (assumes uniform prior).}
	\item{residual variance}{the residual variance if the Gaussian family is assumed in the GLM}
	\item{logLikelihood}{the log Likelihood if the Binomial family is assumed in the GLM}
	\item{hyperparameters}{the hyperparameter(s) used to fit the model}
	\item{family}{the GLM family specified in this function call}
	\item{prior}{the prior used in this function call}
	\item{call}{the call that produced this object}	
	\item{nobs}{number of observations}	
	\item{nfolds}{number of folds in CV}	
}

\references{	Cai, X., Huang, A., and Xu, S. (2011). Fast empirical Bayesian LASSO for multiple quantitative trait locus mapping. BMC Bioinformatics 12, 211.\cr\cr
	Huang A, Xu S, Cai X. (2013). Empirical Bayesian LASSO-logistic regression for multiple binary trait locus mapping. BMC genetics  14(1):5. \cr\cr
	Huang, A., Xu, S., and Cai, X. (2014a). Empirical Bayesian elastic net for multiple quantitative trait locus mapping. Heredity 10.1038/hdy.2014.79 \cr\cr 
uang, A., E. Martin, et al. (2014b). Detecting genetic interactions in pathway-based genome-wide association studies. Genet Epidemiol 38(4): 300-309.	
	\cr	}
\author{Anhui Huang and Dianting Liu \cr Dept of Electrical and Computer Engineering, Univ of Miami, Coral Gables, FL}



\examples{

rm(list = ls())
library(EBglmnet)
#Use R built-in data set state.x77
y= state.x77[,"Life Exp"]
xNames = c("Population","Income","Illiteracy", "Murder","HS Grad","Frost","Area")
x = state.x77[,xNames]
#
#Gaussian Model
#lassoNEG prior as default
out = cv.EBglmnet(x,y)
out$fit
#lasso prior
out = cv.EBglmnet(x,y,prior= "lasso")
out$fit
#elastic net prior
out = cv.EBglmnet(x,y,prior= "elastic net")
out$fit
#
#Binomial Model
#create a binary response variable
yy = y>mean(y);
out = cv.EBglmnet(x,yy,family="binomial")
out$fit

}

