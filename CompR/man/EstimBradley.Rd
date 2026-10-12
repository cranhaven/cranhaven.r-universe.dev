\name{EstimBradley}
\alias{EstimBradley}
\title{
Estimation of Bradley's scores in the different classes of subjects
}
\description{
Estimates Bradley's scores according the desired number of classes.
}
\usage{
EstimBradley(Data, Constraint=0, Tcla=1, eps=1e-04, eps1=1e-04, TestPi=TRUE)
}
\arguments{
  \item{Data}{
    Object of class \code{DataPairComp}
}
  \item{Constraint}{
Kind of constraint on Bradley's scores. If \code{Constraint=0}, the sum of Bradley's scores should be equal to 1.
    For other values for \code{Constraint}, the product of Bradley's scores should be equal to 1.(default \code{constraint=0})}
  \item{Tcla}{
    Number of classes, default=1, no segmentation.
}
  \item{eps}{
    value of the convergence criteria for the EM algorithm (default \code{eps=1e-04}).
}
  \item{eps1}{
    value of the criteria convergence for Dykstra algorithm (default \code{eps1=1e-04}).
}
  \item{TestPi}{
    if \code{TestPi=TRUE} multiple comparison tests for Bradley's scores are performed. Else no multiple comparison test. (default is \code{TestPi=TRUE} )
}
}
\details{
The estimation is based on maximum likelihood for mixture distributions with E.M. algorithm.
}
\value{
Object of class \code{BradleyEstim} with the following components:
    \item{Lvriter}{\code{matrix} describing the evolution of log likelihood at the different steps of the maximization procedure.
    }
    \item{Lvr}{Final value of the log likelihood
    }
    \item{Lambda}{\code{numeric} Final estimates of classes' weight
    }
    \item{Pi}{\code{list} of \code{Tcla} elements containing Bradley'scores for the different criteria
    }
    \item{Zh}{\code{matrix} of the belongings probabilities of the individuals to the different classes and the belonging class according to these probabilities
    }
    \item{IC}{value of Information Criterion (AIC,BIC,CAIC)
    }
    \item{Restestglob}{ (given if \code{TestPi=TRUE})
    \code{list} of five elements:
    
              \code{lvrH0} { \code{matrix} of size (\code{Tcla} * number of criteria), giving the value of the log likelihood under the hypothesis of equality of Bradley's scores
              }
              
              \code{lvrH1} {\code{matrix} of size (\code{Tcla} * number of criteria), giving the value of the log likelihood under the hypothesis of non equality of Bradley's scores
              }
              
              \code{lRatio} {\code{matrix} of size (\code{Tcla} * number of criteria), giving the value of the log likelihood Ratio statistic
              }
              
              \code{Pvalue} {\code{matrix} of size (\code{Tcla} * number of criteria), giving the P value of the log likelihood Ratio test
              }
              
              \code{H1} {\code{matrix} of size (\code{Tcla} * number of criteria) giving the result of rejection of equality of Bradley's scores
              }
    }
    \item{Restestprod}{(given if \code{TestPi=TRUE} and if Bradley's scores are not equal)
    \code{list} of \code{Tcla} elements of type \code{matrix} of size (number of paired comparison * 7), each column corresponding to:
    
              class identification,
              
              criterion identification,
              
              product identification i,
              
              product identification j,
              
              value for the statistic corresponding to H0: equality of the Bradley's scores of products i and j,
              
              P value of this test,
              
              Rejection or acceptation of H0 for a level of 5\%.
              
    }
    \item{Varcov}{(given if \code{TestPi=TRUE})
    
    \code{list} of \code{Tcla} elements containing Bradley'scores covariance matrices for the different criteria.
    }
}
\examples{
data(Cocktail)
show(Cocktail)
ResCock1<-EstimBradley(Cocktail,Constraint=0,Tcla=1,eps=0.001,eps1=0.001,TestPi=TRUE)
show(ResCock1)
}