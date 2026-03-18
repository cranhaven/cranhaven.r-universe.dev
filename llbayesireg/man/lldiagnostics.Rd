\name{lldiagnostics}
\alias{lldiagnostics}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Diagnostics from a fitll object}
\source{
The L-Losgistic distribution was introduced by Tadikamalla and Johnson (1982), which refer to this distribution as Logit-Logistic
distribution. Here, we have a new parameterization of the Logit-Logistic with the median as a parameter.
}
\description{Prints diagnostics or extract those diagnostics from a fitll object.
}
\usage{
lldiagnostics(object)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{Object of "fitll".}
}
\details{The function calls the check_* functions and the get_* functions are for access to the diagnostics. If the matrix X and W are missing, the coda package is used by test the convergence
of the chains by  Cramer-von-Mises statistic and an image of the correlation is show for both
of generated chains.}
\value{lldiagnostics(object) prints diagnostics or extract those diagnostics from a fitll object.
}
\references{Paz, R.F., Balakrishnan, N and Bazán, J.L. (2018). L-Logistic Distribution: Properties, Inference and an Application to Study Poverty and Inequality in Brazil.
The Stan Development Team Stan Modeling Language User's Guide and Reference Manual. http://mc-stan.org/.
Plummer, M., Best, N., Cowles, K., and Vines, K. (2006). Coda: Convergence diagnosis and output analysis for mcmc. R News, 6(1):7-11.
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

lldiagnostics(fitll$object)
 \donttest{
# Modelation the coeficient with real data
library(llbayesireg)

data("Votes","MHDI")

y = Votes[,4]
X = MHDI

fitll = llbayesireg(y,X)

lldiagnostics(fitll$object)
 }

}

