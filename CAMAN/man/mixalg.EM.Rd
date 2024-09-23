\name{mixalg.EM}
\alias{mixalg.EM}

\title{EM algorithm}

\description{
In the fixed support size case the number of components \code{k} is assumed to
be known. Here the unknown parameters are the mixing weights \code{p[j]} and the
parameters \code{lambda[j]} of the subpopulation.\cr

Estimation of these models' parameters is usually achieved by application
of the EM algorithm.
}


\usage{
mixalg.EM (mix = NULL, p, t, obs = NULL, weights = NULL, family = "gaussian", 
    data = NULL, pop.at.risk = NULL, var.lnOR = NULL, limit = 0.01, 
    acc = 10^(-7), numiter = 5000)    
}


\arguments{  
\item{mix}{
A CAMAN-object which quantifies a finite mixture model and the input data.
}
\item{p}{ vector containing the starting values for p}
\item{t}{ vector containing the starting values for the distribution specific parameters (poisson->\eqn{\lambda}, gaussian-> \eqn{\mu})}
    \item{obs}{
     observed / dependent variable. Vector or colname of \code{data}. Must be specified!
	}
	\item{weights}{
	weights of the data. Vector or colname of \code{data}. Default is \code{NULL}.
	}
  \item{family}{
        the underlying type density function as a character ("gaussian", "poisson" or "binomial")!
        }
  \item{data}{
        an optional data frame. \code{obs}, \code{weights}, \code{pop.at.risk} and \code{var.lnOR} can be specified as column name of the data frame. 
        }      
  \item{pop.at.risk}{
  	population at risk: These data could be used to determine a mixture model for Poisson data. Vector or colname of \code{data}. Default is\code{NULL}. 
  }
  \item{var.lnOR}{
    variances of the data: These variances might be given when working with meta analyses! Vector or colname of \code{data}. Default is \code{NULL}.
   }
  \item{limit}{
    parameter to control the limit of union several components. Default is 0.01.
}
 \item{acc}{
	convergence criterion. VEM and EM loops stop when deltaLL<acc. Default is 10^(-7). 
}
 \item{numiter}{
	parameter to control the maximal number of iterations in the VEM and EM loops. Default is 5000. 
 }
}


\value{
    The function returns a \code{CAMAN.object} object, thus the same as mixalg!
}



\author{
Peter Schlattmann and Johannes Hoehne   
}


\examples{
data(vitA)
m1<-mixalg.EM(obs="logrr",var.lnOR="var" ,family="gaussian",
              p=c(1),t=c(0),data=vitA)
m2<-mixalg.EM(obs="logrr",var.lnOR="var" ,family="gaussian",
              p=c(0.5,0.5),t=c(-0.3,0.2),data=vitA)

# apply EM-algorithm on an existing CAMAN.object: 
data(thai_cohort)
mix0 <- mixalg(obs="counts", weights="frequency", family="poisson", 
               data=thai_cohort, numiter=18000, acc=0.00001,startk=25)
em0<-mixalg.EM(mix0,p=c(1),t=c(1))
em1<-mixalg.EM(mix0,p=c(0.7,0.3),t=c(2,9))

}

\concept{meta-analysis}
\keyword{models}
\concept{EM-algorithm}
\concept{mixture model}