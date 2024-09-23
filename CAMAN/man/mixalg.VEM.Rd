\name{mixalg.VEM}
\alias{mixalg.VEM}

\title{VEM algorithm}

\description{
When fitting finite mixture models two cases must be distinguished. The
flexible support size case, where no assumption about the
number of components \code{k} is made in advance and the fixed support size
case. For the flexible support size case the VEM-algorithm can be used.
\cr \cr
The algorithm proceeds as follows:\cr
Step 1: Define an approximating grid lambda[1], \dots, lambda[L] \cr
Step 2: Use the VEM algorithm to maximize \code{L(P)} in the simplex 
\eqn{\Omega} and identify grid points with positive support. \cr
}


\usage{
mixalg.VEM(mix = NULL, obs=NULL, weights=NULL, data=NULL, pop.at.risk=NULL, 
           var.lnOR=NULL, family="gaussian", limit=0.01, acc=10^(-7), 
           numiter=5000, startk=50)    
}


\arguments{  
\item{mix}{
A CAMAN-object which quantifies a finite mixture model and the input data.
}
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
  \item{startk}{
   starting/maximal number of components. This number will be used to compute the grid in the VEM. Default is 50.
  }
  
}


\value{
    The function returns a \code{CAMAN.VEM.object} object.
}



\author{
Peter Schlattmann and Johannes Hoehne   
}


\examples{
data(vitA)
m0<-mixalg.VEM(obs="logrr",var.lnOR="var",family="gaussian", data=vitA,startk=20)
plot(m0@totalgrid[,2],m0@totalgrid[,3],type="l",xlab="parameter",ylab="gradient")
m1<-mixalg.EM(obs="logrr",var.lnOR="var" ,family="gaussian",p=c(1),t=c(0),data=vitA)
}

\keyword{models}
\concept{meta-analysis}
\concept{mixture model}
\concept{EM algorithm}