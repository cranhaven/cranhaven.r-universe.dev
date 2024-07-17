\name{ocME}
\alias{ocME}
\alias{print.ocME}
\title{Marginal Effect for Ordered Choice Model}
\description{This function calculates marginal effects for an ordered chioce model and their standard errors. Either an ordered probit or logit model can be accommodated.}

\usage{ocME(w, rev.dum = TRUE, digits = 3)}
  
\arguments{
  \item{w}{ an ordered probit or logit model object estimated by \code{polr} from the MASS library.}
  \item{rev.dum}{ a logical value (default of TRUE) of whether to revise the estimates and standard erros for binary independant variables. If FALSE, derivatives are taken on binary independant variables as continuous variables.}
  \item{digits}{ number of digits for output.}
}
\details{Marginal effects from an ordered probit or logit model is calculated. Marginal effects are calculated at the mean of the independent variables. \code{rev.dum = TRUE} allows marginal effects for dummy variables are calculated differently, instead of treating them as continuous variables. The standard errors are computed by delta method. The software of LIMDEP is used to compare and benchmark the results.
}

\value{Return a list object of class "ocME" with the following components:

  \item{w}{ input of an ordered choice model}
  \item{out}{a list object of marginal effects, standard errors, t-values, and p-values by factor level.}
}

\section{Methods}{
  One method is defined as follows:
  \describe{
    \item{\code{print}:}{print the key output of marginal effects.}
  }
}

\references{Greene, W.H. 2003. Econometric Analysis (5th ed.). Prentice Hall, New York. 1026 P.}

\author{Changyou Sun (\email{cs258@msstate.edu})}

\seealso{\code{\link{ocProb}}}

\examples{
# Loading data from the MASS library
library(MASS)
data(housing) 
str(housing); head(housing)

# Fit an ordered choice model with polr from the MASS library
# This is the original specification used in MASS.
fm <- Sat ~ Infl + Type + Cont
ra <- polr(fm, data = housing, weights = Freq, Hess = TRUE, method = "probit")
rb <- polr(fm, data = housing, weights = Freq, Hess = TRUE, method = "logistic")
summary(ra); summary(rb)

# Compute the marginal effect
mea <- ocME(w = ra); mea
meb <- ocME(w = rb); meb
meb$out
}

\keyword{regression}