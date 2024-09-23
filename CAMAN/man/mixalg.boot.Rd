\name{mixalg.boot}
\alias{mixalg.boot}
\alias{mixalg.Boot}
\alias{mixboot}


\title{bootstrap replication / validation of finite mixture models}

\description{
    
This function may be used to estimate the number of components based on a
nonparametric bootstrap approach.A bootstrap sample is obtained from the
original sample  with replacement. Corresponding to the bootstrap data set we
obtain an estimate of the number of components \code{k} applying a combination
of the VEM- and EM algorithm.
The bootstrap algorithm involves drawing \code{B} independent bootstrap samples
and estimating \code{k} using the hybrid mixture algorithm. The result is
the bootstrap distribution of the number of components \code{k}. 
The mode of this distribution is taken as an estimate of the number of components.   
}


\usage{
mixalg.boot(mix, nboot=500, limit=0.01, acc=10^(-5), numiter=5000, 
            startk=50, returnBootstrapRep=FALSE)    
}


\arguments{
the parameters \code{limit, acc, numiter} and \code{startk} are used for the VEM algorithm in each bootstrap sample.
\item{mix}{
A CAMAN-object which quantifies a finite mixture model.
}
\item{nboot}{
number of bootstrap replications}
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
   starting/maximal number of components for the VEM algorithm in each bootstrap sample. This number will be used to compute the grid in the VEM. Default is 50.
  }
  \item{returnBootstrapRep}{
  A Boolean that indicates whether the bootstrapped data should be returned or not
  }
}



\value{
    The function returns a list, describing the bootstrap replications: \cr 
    \item{$dat.bootstrap}{ (used) sampled data }
    \item{$LL}{ Likelihood of the final solutions of each bootstrap replication }
    \item{$LL_k1}{ vector with LL for each bootstrap replication using a homogeneous model (k=1) }
    \item{$numk.boot}{ number of components obtained in replications.}
}

\note{
\code{mixalg.Boot} and \code{mixboot}  are deprecated names for the \code{mixalg.boot} function.
}

\author{
Peter Schlattmann and Johannes Hoehne   
}


\examples{
### POISSON data with weights: thai_cohort
data(thai_cohort)
m.thai <- mixalg(obs="counts", weights="frequency",
                 family="poisson", data=thai_cohort,
                 acc = 0.00003)
\dontrun{boot <- mixalg.boot(m.thai, nboot=1000)} #may take a few minutes

### POISSON data with observed and expected data: hepab
data(hepab)
mix <- mixalg(obs="observations",pop.at.risk="expected",family= "poisson",data=hepab)
\dontrun{boot <- mixalg.boot(mix, nboot=250) #may take some time 
table(boot$numk.boot)}
}

\concept{meta-analysis}
\concept{bootstrap}
\concept{mixture model}