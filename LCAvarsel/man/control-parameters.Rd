\name{control-parameters}
\alias{controlLCA}
\alias{controlReg}
\alias{controlGA}

\title{Set control parameters for various purposes}

\description{
Set control parameters for the EM algorithm for latent class model estimation, multinomial logistic regression estimation in the regression step, and genetic algorithm for variable selection procedure.
}

\usage{
controlLCA(maxiter = 1e05, tol = 1e-04, nrep = 5)

controlReg(maxiter = 5000, tol = 1e-05)

controlGA(popSize = 20, maxiter = 100, run = maxiter/2,
          pcrossover = 0.8, pmutation = 0.2,
          elitism = base::max(1, round(popSize*0.05)))
}
\arguments{
\item{maxiter}{Maximum number of iterations in the EM algorithm, the multinomial logistic regression and the genetic algorithm.}

\item{tol}{Tolerance value for judging when convergence has been reached. Used in the EM algorithm and the multinomial logistic regression.}

\item{nrep}{Number of times to estimate the latent class analysis model, using different starting values for the matrix \code{z} of a posteriori probabilities. Each time, a different random initialization is used in order to search for the global maximum of the log-likelihood.}

\item{popSize}{Population size. This number corresponds to the number of different models to be considered at each iteration of the genetic algorithm.}

\item{run}{Number of consecutive generations without any improvement in the best fitness value of the variable selection procedure before the genetic algorithm is stopped.}

\item{pcrossover}{Probability of crossover between pairs of models.}

\item{pmutation}{Probability of mutation in a parent model.}

\item{elitism}{Number of best fitness models to survive at each iteration of the genetic algorithm in the variable selection procedure.}
}

\details{
Function \code{controlLCA} is used to set control parameters of the EM algorithm employed to estimate the latent class analysis model.

Function \code{controlReg} controls tolerance and maximum number of iterations in the estimation of the multinomial logistic regression. This regression is used to model the conditional distribution of a proposed variable given the current set of clustering variables in the variable selection procedure.

Function \code{controlGA} sets parameters of the genetic algorithm used for variable selection.
}

\value{
A list of parameters values.
}

\examples{
data(carcinoma, package = "poLCA")
# increase number of replicates and decrease tolerance value
fit <- fitLCA(carcinoma, ctrlLCA = controlLCA(nrep = 10, tol = 1e-07))
}

\seealso{
\code{\link[poLCA]{poLCA}}, \code{\link[GA]{ga}}
}
