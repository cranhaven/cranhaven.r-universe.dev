\name{llHPD}
\alias{llHPD}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Highest Posterior Density for the L-Logistic Bayesian Regression}
\source{
The L-Losgistic distribution was introduced by Tadikamalla and Johnson (1982), which refer to this distribution as Logit-Logistic
distribution. Here, we have a new parameterization of the Logit-Logistic with the median as a parameter.
}
\description{Compute the highest posterior density for the L-Logistic Bayesian Regression intervals of betas and deltas.
}
\usage{
llHPD(fitll, prob = 0.95, chain = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{fitll}{Object of class matrix with the llbayesireg function result.}
  \item{prob}{A number of quantiles of interest. The default is 0.95.}
  \item{chain}{Chain chosen for construction. The default is 1.}
}
\details{This function compute the highest posterior density intervals for a Bayesian posterior distribution.
}
\value{
Object of class matrix with:
 \item{betas}{The highest posterior density intervals of betas.}
 \item{deltas}{The highest posterior density intervals of deltas.}
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

llHPD(fitll)

 \donttest{
# Modelation the coeficient with real data
library(llbayesireg)

data("Votes","MHDI")

y = Votes[,4]
X = MHDI

fitll = llbayesireg(y,X)

llHPD(fitll)
 }

}

