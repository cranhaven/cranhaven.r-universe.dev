\name{anova.CAMAN.object}
\alias{anova.CAMAN.object}

\title{ANOVA for finite mixture models}

\description{
A common problem in the estimation of mixture models is to determine the
number of components. This may be done using a parametric bootstrap.
This function simulates from a mixture model under the null hypothesis
with \code{k0} components. A mixture model with k0 and usually k0 +1 components 
is fit to the data and then the likelihood ratio statistic (LRS) is computed. \cr

Based on the bootstrap the distribution of the LRS is obtained which allows to obtain
an approximation to the achieved level of significance corresponding to
the value of \eqn{-2 \log \xi} obtained from the original sample.
}


\usage{
\method{anova}{CAMAN.object}(object, object1, nboot=2500, limit=0.01, acc=10^(-7), 
     numiter=5000, giveBootstrapData=FALSE, giveLikelihood=FALSE, ...)
}

\arguments{
\item{object}{
A CAMAN-object which quantifies a finite mixture model under null hypothesis.
}
\item{object1}{
A CAMAN-object which quantifies another finite mixture model under the alterative hypothesis.
}
\item{nboot}{
Number of bootstrap samples.
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
  \item{giveBootstrapData}{
  A Boolean that indicates whether the bootstrapped data should be returned or not
  }
  \item{giveLikelihood}{
  Return the likelihood-values of both models for each generated dataset.
  }
  \item{...}{
  Arguments to be passed on to other methods; currently none.
  }
}

\details{The parameters \code{limit, acc} and \code{numiter} are used for the VEM algorithm in each bootstrap sample.
}

\value{
    The function returns a list with components
    \itemize{
    \item \code{overview}: comparison of the models, including BIC, LL and LL-ratio \cr
    \item \code{`LL ratios in bootstrap-data`}: 90, 95, 97.5 and 99 percentiles of LL-ratios \cr
    \item \code{`simulated p-value`}: p-value, quantifying the null model  \cr
}
}

\references{
McLachlan, G. and Peel, D. (2000). \emph{Finite Mixture Models}, Chichester: Wiley. \cr

Schlattmann, P. (2009). \emph{Medical Applications of Finite Mixture Models}. Berlin: Springer.
}


\author{
Peter Schlattmann and Johannes Hoehne   
}


\examples{
data(thai_cohort)
mix0 <- mixalg(obs="counts", weights="frequency", family="poisson", data=thai_cohort,
               numiter=18000, acc=0.00001,startk=25)
em0<-mixalg.EM(mix0,p=c(1),t=c(1))
em1<-mixalg.EM(mix0,p=c(0.7,0.3),t=c(2,9))
\dontrun{ll<-anova(em0,em1,nboot=250)} #might take some minutes
}

\concept{meta-analysis}
\keyword{htest}
