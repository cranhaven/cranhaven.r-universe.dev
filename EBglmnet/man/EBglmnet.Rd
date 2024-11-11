\name{EBglmnet}
\alias{EBglmnet}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Main Function for the EBglmnet Algorithms}
\description{
    EBglmnet is the main function to fit a generalized linear model via the empirical Bayesian methods with lasso and elastic net hierarchical priors.
	It features with \code{p>>n} capability, produces a sparse outcome for the 
	regression coefficients, and performs significance test for nonzero effects 
	in both  linear and logistic regression models.
			}
\usage{
EBglmnet(x, y, family=c("gaussian","binomial"),prior= c("lassoNEG","lasso","elastic net"),
	hyperparameters, verbose = 0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{input matrix of dimension \code{n} x \code{p}; each row is an
  observation vector, and each column is a variable. 
  }
  
  \item{y}{response variable. Continuous for \code{family="gaussian"}, and binary for 
  \code{family="binomial"}. For binary response variable, y can be a Boolean or numeric vector, or factor type
  array.}
  
  \item{family}{model type taking values of "gaussian" (default) or "binomial". } 
  
  \item{prior}{prior distribution to be used. It takes values of "lassoNEG"(default), "lasso", and "elastic net". 
  All priors will produce a sparse outcome of the regression coefficients; see Details for choosing priors. }  
  
  \item{hyperparameters}{the optimal hyperparameters in the prior distribution. Similar as \eqn{\lambda} in lasso
  method, the hyperparameters control the number of nonzero elements in the regression coefficients. Hyperparameters 
  are most oftenly determined by CV. See \code{cv.EBglmnet} for the method in determining their values. 
  While \code{cv.EBglmnet} already provides the model fitting results using the hyperparameters determined in CV,
  users can use this function to obtain the results under other parameter selection criteria such as Akaike information criterion
  (AIC) or Bayesian information criterion (BIC). } 
  
  \item{verbose}{parameter that controls the level of message output from EBglment. It takes values from 0 to 5; larger verbose displays more messages. small values are recommended to avoid excessive outputs. Default value for \code{verbose} is minimum message output.}
}
\details{EBglmnet implements three set of hierarchical prior distributions for the regression parameters \eqn{\beta}: \cr\cr
\bold{lasso prior}:
\deqn{\beta_j  \sim N(0,\sigma_j^2),}
\deqn{\sigma_j^2  \sim exp(\lambda), j = 1, \dots, p.}

\bold{lasso-NEG prior}:
\deqn{\beta_j  \sim N(0,\sigma_j^2),}
\deqn{\sigma_j^2  \sim exp(\lambda),}
\deqn{\lambda  \sim gamma(a,b), j = 1, \dots, p.}

\bold{elastic net prior}:
\deqn{\beta_j  \sim N[0,(\lambda_1 + \tilde{\sigma_j}^{-2})^{-2}],}
\deqn{\tilde{\sigma_j}^{2}  \sim generalized-gamma(\lambda_1, \lambda_2), j = 1, \dots,p.}

The prior distributions are peak zero and flat tail probability distributions that assign a high prior 
probability mass to zero and still allow heavy probability on the two tails, which reflect the prior 
belief that a sparse solution exists: most of the variables will have no effects on the response variable,
and only some of the variables will have non-zero effects in contributing the outcome in \code{y}. \cr

The three priors all contains hyperparameters that control how heavy the tail probability is, 
and different values of them will yield different number of non-zero effects retained in the model. 
Appropriate selection of their values is required for obtaining optimal results, 
and CV is the most oftenly used method. See \code{cv.EBglmnet} for details for determining the 
optimal hyperparameters in each priors under different GLM families. \cr \cr
\emph{lassoNEG prior}\cr
\code{"lassoNEG"} prior has two hyperparameters (a,b), with \eqn{a \ge -1} and \code{b>0}. Although
\code{a} is allowed to be greater than -1.5, it is not encouraged to choose values in (-1.5, -1) unless the signal-to-noise 
ratio in the explanatory variables are very small.

\emph{lasso prior}\cr
\code{"lasso"} prior has one hyperparameter  \eqn{\lambda}, with \eqn{\lambda \ge 0}.  \eqn{\lambda} is similar as 
the shrinkage parameter in \code{lasso} except that even for \code{p>>n}, \eqn{\lambda} is allowed to be zero, and \code{EBlasso}
can still provide a sparse solution thanks to the implicit constraint that \eqn{\sigma^2 \ge 0}.

\emph{elastic net prior}\cr
Similar as the elastic net in package \pkg{glmnet}, EBglmnet transforms the two hyperparameters \eqn{\lambda_1} 
and \eqn{\lambda_2} in the \code{"elastic net"} prior in terms of other two parameters \eqn{\alpha (0\le \alpha \le 1)}
and \eqn{\lambda (\lambda >0)}.  Therefore, users are asked to specify \code{hyperparameters=c}(\eqn{\alpha, \lambda}). 
}
		
		
		
\value{
	\item{fit}{the model fit using the hyperparameters provided. EBglmnet selects the variables having nonzero regression 
	coefficients and estimates their posterior distributions. With the posterior mean and variance, a \code{t-test} 
	is performed and the \code{p-value} is calculated. Result in fit is a matrix with rows corresponding to the 
	variables having nonzero effects, and columns having the following values: \cr\cr
	column1: (predictor index in X) denoting the column number in the input matrix \code{x}. \cr\cr
	column2: beta. It is the posterior mean of the nonzero regression coefficients. \cr\cr
	column3: posterior variance.  It is the diagonal element of the posterior covariance matrix among the nonzero regression coefficients. \cr\cr
	column4: t-value calculated using column 3-4. \cr\cr
	column5: p-value from t-test.	
	}
	
	\item{WaldScore}{the Wald Score for the posterior distribution. It is computed as \eqn{\beta^T\Sigma^{-1}\beta}. 
	See (Huang A, 2014b) for using Wald Score to identify significant effect set.}
	\item{Intercept}{the intercept in the linear regression model. This parameter is not shrunk.}
	\item{residual variance}{the residual variance if the Gaussian family is assumed in the GLM}
	\item{logLikelihood}{the log Likelihood if the Binomial family is assumed in the GLM}
	\item{hyperparameters}{the hyperparameter used to fit the model}
	\item{family}{the GLM family specified in this function call}
	\item{prior}{the prior used in this function call}
	\item{call}{the call that produced this object}	
	\item{nobs}{number of observations}	
}

\references{	Cai, X., Huang, A., and Xu, S. (2011). Fast empirical Bayesian LASSO for multiple quantitative trait locus mapping. BMC Bioinformatics 12, 211.\cr\cr
	Huang A, Xu S, Cai X. (2013). Empirical Bayesian LASSO-logistic regression for multiple binary trait locus mapping. BMC genetics  14(1):5. \cr\cr
	Huang, A., Xu, S., and Cai, X. (2014a). Empirical Bayesian elastic net for multiple quantitative trait locus mapping. Heredity 10.1038/hdy.2014.79 \cr 
}
\author{Anhui Huang and Dianting Liu \cr }



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
out = EBglmnet(x,y,hyperparameters=c(0.5,0.5))
out$fit
#lasso prior
out = EBglmnet(x,y,prior= "lasso",hyperparameters=0.5)
out$fit
#elastic net prior
out = EBglmnet(x,y,prior= "elastic net",hyperparameters=c(0.5,0.5))
out$fit
#residual variance
out$res
#intercept
out$Intercept
#
#Binomial Model
#create a binary response variable
yy = y>mean(y);
out = EBglmnet(x,yy,family="binomial",hyperparameters=c(0.5,0.5))
out$fit

}

