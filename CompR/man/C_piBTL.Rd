\name{C_piBTL}
\alias{C_piBTL}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Estimation of Bradley's scores
}
\description{
Returns the Bradley's scores of the different items and the value of the LogLikelihood
}
\usage{
C_piBTL(Matpair, Constraint=0, eps1=1e-04, Pi=NULL, TestPi=FALSE, Zht=NULL)
}

\arguments{
  \item{Matpair}{
Matrix of the cumulative sum of the results of paired comparisons or object of class\cr
\code{DataPairComp}
}
  \item{Constraint}{
Kind of constraint on Bradley's scores. If \code{Constraint=0}, the sum of Bradley's scores should be \cr equal to 1. For other values for \code{Constraint}, the product of Bradley's scores should be equal to 1.\cr (default is \code{Constraint=0})
}
  \item{eps1}{
value to take into account for the convergence criteria of the algorithm of\cr Bradley's scores estimation.(default is \code{eps1=1e-04})
}
  \item{Pi}{
Initial values for Bradley's scores. If \code{Pi=NULL} the initialisation is based on a mean score for each \cr 
item obtained from the data \code{Matpair}. Else,initial values for Bradley's scores are \code{Pi} given by the \cr
user.(default is \code{Pi=NULL})
}
  \item{TestPi}{
Indicate if the user wants to perform a multiple comparison tests on the Bradley's scores. \cr
(default \code{TestPi=FALSE})
}
\item{Zht}{
Indicate the individuals probabilities to belong to the different classes. \code{Zht} has not to be provided for \cr 
external use of this function. It is used in the main function \code{EstimBradley} (default \code{Zht=NULL})
}
}
\details{
The algorithm is based on a maximum likelihood approach using Dykstra method.
}
\value{
List of following components:
    \item{Pi }{Bradley's scores}
    \item{lnL }{value of the log-likelihood}
    \item{lvrHO}{value of the log-likelihood under the hypothesis of equal values for the Bradley's scores}
    \item{lvrH1}{value of the log-likelihood at the end of the Bradley's scores estimation algorithm}
    \item{lRatio}{value of the likelihood ration statistic}
    \item{Pvalue}{Pvalue of the test}
    \item{H1}{logical value, FALSE if Bradley's scores should be considered as equal, TRUE otherwise}
    \item{VarcovPi}{Matrix of covariances of Bradley's scores}
    \item{restestij}{Matrix of the following elements\cr
        - products i and j compared \cr       
        - value of the test statistic\cr
        - Pvalue of the test\cr        
        - decision at a 0.05 level
        }
}
\examples{
  data(Cocktail_Cum)
  res<-C_piBTL(Cocktail_Cum,Constraint=0,eps1=1e-04,Pi=NULL,TestPi=TRUE)
  res
}