\name{mixalg}
\alias{mixalg}

\title{Fitting Finite Mixture Models}


\description{
This hybrid mixture algorithm  combines the VEM algorithm  for flexible
support size and the EM algorithm for a fixed number of components.
The solution of the VEM algorithm provides starting values for the EM
algorithm. By the NPMLE theorem the EM algorithm
thus starts very close to the global maximum and proper convergence of the
EM algorithm to a global maximum is ensured.

The algorithm proceeds as follows


Step 1: Define an approximating grid \code{lambda[1], \dots, lambda[L]}

Step 2: Use the VEM algorithm to maximize L(P) in the simplex
\eqn{\Omega} and identify grid points with positive support. 
Here positive support is defined as \code{p[j] >= epsilon} (often \code{epsilon = 10^-2}).\cr
This gives an initial estimate of \code{k}.\cr

Step 3: Use these \code{k} points and corresponding mixing weights
p[j] as starting values for the EM algorithm

Step 4: Collapse identical components if \code{| lambda[j]- lambda[i] | < delta } (often delta=0.05) for i != j

Step 5: Obtain the final number of components \code{k}

This sequential algorithm leads to an initial estimate of the  NPMLE
and a proper solution for the subsequent EM algorithm. Crucial points are
the definitions of \eqn{\delta} and \eqn{\epsilon}. Depending on these settings
different solutions could result from this algorithm.
}


\usage{
mixalg(obs, weights=NULL, family="gaussian", data=NULL, pop.at.risk=NULL, 
       var.lnOR=NULL, limit=0.01, acc=10^(-7), numiter=5000, startk=50)
}


\arguments{   
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
    The function returns a CAMAN.object, describing a finite mixture model.
	The main information about the mixture model is printed by just typing the <object>. Additional information is given in \code{summary(object)} (summary.CAMAN.object). 
	Single attributes can be accessed using the \code{@}, e.g. mix@LL.
    \cr 
    
    \item{dat}{(input) data }\cr
    \item{family}{underlying type density function  }
    \item{LL}{Likelihood of the final (best) iteration }
    \item{BIC}{Likelihood of the final (best) iteration }
    \item{num.k}{number of components obtained }
    \item{p}{probability of each component}
    \item{t}{parameter of distribution (normal distr. -> mean, poisson distr. -> lambda, binomial distr. -> prob) }
    \item{component.var}{variance of each component (ONLY if \code{family} == "gaussian") }
    \item{prob}{probabilies, belonging to each component }
	\item{classification}{classification labels for each observation (\code{which.max} of \code{@prob}). }
	\item{steps}{number of steps performed (EM, VEM). }
	\item{VEM_result}{result of VEM algorithm. }
	\item{cl}{the matched call. }
	\item{is_metaAnalysis}{parameter specifying, whether a meta analysis was performed. } 
	\item{VEM_result}{Outcome of the VEM-algorithm, which was run before the EM.}
    \item{finalacc}{deltaLL of the final iteration (for VEM and EM)}

}

\details{
The documentation of \code{\link{leukDat}} contains a disease mapping example using \code{mixalg} and the documentation of \code{\link{golubMerge}} contains a microarray analysis example.
}

\seealso{
  \code{\link{mixalg.EM}}, \code{\link{mixalg.VEM}}, \code{\link{anova.CAMAN.object}}, \code{\link{mixcov}}, \code{\link{mixalg.boot}} 
}

\author{
Peter Schlattmann and Johannes Hoehne  
}

\references{
D. B{\"o}hning, P. Schlattmann, B.G. Lindsay: C.A.MAN - Computer Assisted
Analysis of Mixtures: Statistical Algorithms.\emph{Biometrics}, 1992, 48, 283-303

P. Schlattmann: On bootstrapping the unknown number of components in
finite mixtures of Poisson distributions. \emph{Statistics and Computing}, 2005, 15, 179-188

Schlattmann, P. (2009). \emph{Medical Applications of Finite Mixture Models.} Berlin: Springer.
}

\examples{
### POISSON data with weights: thai_cohort
data(thai_cohort)
mix <- mixalg(obs="counts", weights="frequency", family="poisson", 
              data=thai_cohort, numiter=18000, acc=0.00001, startk=25)


# meta analysis
data(aspirin)
mix <- mixalg(obs="logrr", var.lnOR="var", data=aspirin)


## See the documentation of golub.Merge for a
## microarray analysis example using mixalg

## See the documentation of leukDat for a disease
## mapping example using mixalg

}

\concept{meta-analysis}
\concept{mixture model}
\keyword{models}