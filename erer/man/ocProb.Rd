\name{ocProb}
\alias{ocProb}
\alias{print.ocProb}
\alias{plot.ocProb}
\title{Probability trend for a continuous variable in an ordered choice model }
\description{This function computes the probability values for a continuous variable, based on an ordered choice model. Either an ordered probit or logit model can be used.}

\usage{ocProb(w, nam.c, n = 100, digits = 3)}

\arguments{
  \item{w }{ a object of class of "polr" estimated from \code{polr} from the MASS library.}
  \item{n }{ number of points for calculating probability; the large the number, the smoother the curve.}
  \item{nam.c }{ a name of a continuous indepedent variable; this must be given for the function to work.}
  \item{digits }{ number of digitis for output formatting.}
}

\details{This function computes the probability values associated with a continous variable in an ordered probit or logit model. The standard errors of each probability value by factor level is computed using delta method. The software of LIMDEP is used to compare and benchmark the results.}

\value{Return a list object of class "ocProb" with the following components:

w, nam.c, method=w$method, mean.x=colMeans(x), out, lev)

  \item{w }{ an object of class "polr"}
  \item{nam.c }{ the name of a continous variable}
  \item{method }{ type of the ordered choice model of \code{w}, either "probit" or "logistic"}    
  \item{mean.x }{ mean values of the independent variables used in \code{w}}
  \item{out }{ This is a list object with the estimated probability values, standard errors, t values, and p values for each of the factor level in \code{lev}.}  
  \item{lev }{ a character string representing the factor level used in \code{w}}
}
              
\section{Methods}{
  Two methods are defined as follows:
  \describe{
    \item{\code{print}:}{print the probability output.}
    \item{\code{plot}:}{Plot the probability values for a continuous variable.}
  }
}

\references{Greene, W.H. 2003. Econometric Analysis (5th ed.). Prentice Hall, New York. 1026 P.}

\author{Changyou Sun (\email{cs258@msstate.edu})}

\seealso{\code{\link{ocME}}}

\examples{
# Loading data from the MASS library
library(MASS)
data(housing) 
str(housing); head(housing)

# Fit an ordered choice model with polr from the MASS library
# Note this is a fake specification
# The variable of "Freq" is included to have a 
#   continuous variable for demonstration.
fm2 <- Sat ~ Infl + Type + Cont + Freq
rc <- polr(fm2, data = housing, Hess = TRUE, method = "probit")
rd <- polr(fm2, data = housing, Hess = TRUE, method = "logistic")
summary(rc); summary(rd)

# Compute predicated probabilities with one continous variable
(fa <- ocProb(w = rc, nam.c = 'Freq', n = 300))
(fb <- ocProb(w = rd, nam.c = 'Freq', n = 300))
plot(fa)
plot(fb) 
}

\keyword{regression}