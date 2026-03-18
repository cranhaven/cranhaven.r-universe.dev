\name{llbayesireg}
\alias{llbayesireg}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{The L-Logistic Bayesian Regression
}
\source{
The L-Losgistic distribution was introduced by Tadikamalla and Johnson (1982), which refer to this distribution as Logit-Logistic
distribution. Here, we have a new parameterization of the Logit-Logistic with the median as a parameter.
}
\description{Function to estimate a L-Logistic regression model with median and  precision regression structures.
}
\usage{
llbayesireg(y,X,W,niter=1000,chains=1,burn=floor(niter/2),jump=1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{y}{Object of class vector, with the response.}
  \item{X}{Object of class matrix, with the variables for modelling the meadian. The default is NULL.}
  \item{W}{Object of class matrix, with the variables for modelling the presision. The default is NULL.}
  \item{niter}{A positive integer specifying the number of iterations for each chain. The default is 1000.}
  \item{chains}{A positive integer specifying the number of Markov chains. The default is 1.}
  \item{burn}{A positive integer specifying the period sampling (known as the burn-in). The default is niter/2.}
  \item{jump}{A positive integer specifying the period for saving samples. The default is 1.}
}
\details{
See https://cran.r-project.org/web/packages/llogistic/llogistic.pdf.
}
\value{
Object of the class matrix, if the user does not provide arguments X and W, with:
   \item{object}{Object of "fitll".}
  \item{betas}{Object of class matrix with the samples of regression coeficient related to median.}
 \item{deltas}{Object of class matrix with the samples of regression coeficient related to precision parameter.}
 \item{sample.m}{Object of class matrix with the samples of  median.}
 \item{sample.phi}{Object of class matrix with the samples of  precision parameter.}

Object of the class matrix, if the user provide arguments X and W, with:
  \item{object}{Object of "fitll".}
 \item{betas}{Object of class matrix with the samples of regression coeficient related to median.}
 \item{deltas}{Object of class matrix with the samples of regression coeficient related to precision parameter.}
 \item{sample.m}{Object of class matrix with the samples of  median.}
 \item{sample.phi}{Object of class matrix with the samples of  precision parameter.}
 \item{pred}{Object of class matrix with predicte vaules.}
 \item{q}{The number of columns of X.}
 \item{d}{The number of columns of W.}
}
\references{Paz, R.F., Balakrishnan, N and Bazán, J.L. (2018). L-Logistic Distribution: Properties, Inference and an Application to Study Poverty and Inequality in Brazil.
}
\author{
%%  ~~who you are~~
  Sara Alexandre Fonsêca \email{saralexandre@alu.ufc.br},
  Rosineide Fernando da Paz \email{rfpaz2@gmail.com},
  Jorge Luís Bazán
}

\examples{
# Modelation the coeficient with generated data

library(llbayesireg)
library(llogistic)

# Number of elements to be generated

n=50

# Generated response

bin=2005
set.seed(bin)
y=rllogistic(n,0.5, 2)

fitll = llbayesireg(y, niter=100, jump=10)

m.hat=mean(fitll$sample.m); m.hat
phi.hat=mean(fitll$sample.phi); phi.hat

 \donttest{
# Modelation the coeficient with real data
library(llbayesireg)

data("Votes","MHDI")

y = Votes[,4]
X = MHDI

fitll = llbayesireg(y,X)

summary(fitll$object, pars = c("beta","delta"), probs = c(0.025,0.975))

plot(fitll$betas[,1,1], type = "l")
 }

}
