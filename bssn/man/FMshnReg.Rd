\name{FMshnReg}
\alias{FMshnReg}
\title{
Linear regression models using finite mixture of Sinh-normal distribution
}
\description{
Performs the EM-type algorithm with conditonal maximation to perform maximum likelihood inference of the parameters of the proposed model based on the assumption that the error term follows a finite mixture of Sinh-normal distributions.
}
\usage{
FMshnReg(y, x1, alpha = NULL, Abetas = NULL, medj=NULL,
pii = NULL, g = NULL, get.init = TRUE,algorithm = "K-means",
accuracy = 10^-6, show.envelope="FALSE", iter.max = 100)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{y}{
the response matrix (dimension nx1).
}
  \item{x1}{
Matrix or vector of covariates.
}
  \item{alpha}{
Value of the shape parameter for the EM algorithm. Each of them must be a vector of length g. (the algorithm considers the number of components to be adjusted based on the size of these vectors).
}
  \item{Abetas}{
Parameters of vector regression dimension \eqn{(p + 1)} include intercept.
}
  \item{medj}{
a list of \code{g} arguments of vectors of values (dimension p) for the location parameters.
}
  \item{pii}{
Value for the EM algorithm. Each of them must be a vector of length g. (the algorithm considers the number of components to be adjusted based on the size of these vectors).
}
  \item{g}{
The number of cluster to be considered in fitting.
}
  \item{get.init}{
if TRUE, the initial values are generated via k-means.
}
  \item{algorithm}{
clustering procedure of a series of vectors according to a criterion. The clustering algorithms may classified in 4 main categories: exclusive, overlapping, hierarchical and probabilistic.
}
  \item{accuracy}{
The convergence maximum error.
}

\item{show.envelope}{Logical; if TRUE, show the simulated envelope for the fitted model.}

\item{iter.max}{
The maximum number of iterations of the EM algorithm}
}

%\details{
%}
\value{
The function returns a list with 10 elements detailed as

\item{iter}{Number of iterations.}
\item{criteria}{Attained criteria value.}
\item{convergence}{Convergence reached or not.}
\item{SE}{Standard Error estimates, if the output shows \code{NA} the function does not provide the standard error for this parameter.}
\item{table}{Table containing the inference for the estimated parameters.}
\item{LK}{log-likelihood.}
\item{AIC}{Akaike information criterion.}
\item{BIC}{Bayesian information criterion.}
\item{EDC}{Efficient Determination criterion.}
\item{time}{Processing time.}
}
\references{
Maehara, R. and Benites, L. (2020). Linear regression models using finite mixture of Sinh-normal distribution. In Progress.

Bartolucci, F. and Scaccia, L. (2005). The use of mixtures for dealing with non-normal regression errors, Computational Statistics & Data Analysis 48(4): 821-834.
}
\author{Rocio Maehara \email{rmaeharaa@gmail.com} and Luis Benites \email{lbenitesanchez@gmail.com}
}
%\note{
%}



\examples{
\dontrun{
#Using the AIS data

library(FMsmsnReg)
data(ais)

#################################
#The model
x1    <- cbind(1,ais$SSF,ais$Ht)
y     <- ais$Bfat

library(ClusterR) #This library is useful for using the k-medoids algorithm.

FMshnReg(y, x1, get.init = TRUE, g=2, algorithm="k-medoids",
accuracy = 10^-6, show.envelope="FALSE", iter.max = 1000)

#########################################
#A simple output example

------------------------------------------------------------
Finite Mixture of Sinh Normal Regression Model
------------------------------------------------------------

Observations = 202

-----------
Estimates
-----------

       Estimate      SE
alpha1  0.81346 0.10013
alpha2  3.04894 0.32140
beta0  15.08998 1.70024
beta1   0.17708 0.00242
beta2  -0.07687 0.00934
mu1    -0.25422 0.18069
mu2     0.37944 0.38802
pii1    0.59881 0.41006

------------------------
Model selection criteria
------------------------

        Loglik    AIC     BIC     EDC
Value -355.625 721.25 737.791 725.463

-------
Details
-------

Convergence reached? = TRUE
EM iterations = 39 / 1000
Criteria = 6.58e-07
Processing time = 0.725559 secs
}
}

\keyword{EM}
\keyword{FMshn}
