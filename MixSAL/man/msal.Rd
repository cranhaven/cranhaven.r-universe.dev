\name{msal}
\alias{msal}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Model-Based Clustering using a Mixture of SAL Distributions
}
\description{
Performs model-based clustering using a mixture of SAL distributions. The expectation-maximization (EM) algorithm is used for parameter estimation, the Aitken's acceleration criterion is used to determine convergence, both the BIC and ICL values are given for the considered mixtures.
}
\usage{
msal(x, G, start = 1, max.it = 10000, eps = 0.01, print.it = F, print.warn = F, 
print.prmtrs = F)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
A n by p matrix where each row corresponds a p-dimensional observation.
}
  \item{G}{
The desired number of mixture components.
}
  \item{start}{
Specifies how to intialize the zig matrix. If start equals 1, k-means clustering is used. If start equals 2, a random start is used. If start is a vector of length n, then the zig matrix is constructed based from this vector.
}
  \item{max.it}{
The desired number of iterations for the EM algorithm.
}
  \item{eps}{
The desired difference between the asymptotic estimate of the log-likelihood and the current log-likelihood value. 
}
  \item{print.it}{
If True, the iteration number of the EM algorithm is printed.
}
  \item{print.warn}{
If True, the observation number that the mean vector is closet too is given.
}
  \item{print.prmtrs}{
If True, the parameter set is printed on each iteration of the EM algorithm.
}
}
\details{
The mixture of SAL distributions are fitted using an EM algorithm with a ``Set-Back'' procedure to deal with the issue of Infinite Log-Likelihood Values that arise when updating the mean vector (see Section 3.4.2 of Franczak et.al (2014) for details).
}
\value{
The {msal} function outputs a list with the following components:
\item{loglik}{A vector giving the log-likelihood values from each iteration of the considered EM algorithm.}
\item{alpha}{A matrix where each row specifies the direction of skewness in each variable for each mixture component.}
\item{sig}{An array where each matrix specifies the covariance matrix for each mixture component.}
\item{mu}{A matrix where each row gives the mean vector for each mixture component.}
\item{pi.g}{A vector specifying the mixing components.}
\item{bic}{An integer giving the Bayesian Information Criterion (BIC) for the fitted model.}
\item{icl}{An integer giving the Integrated Completed Likelihood (ICL) for the fitted model.}
\item{cluster}{A vector of length n giving the group label for each observation in the considered data set.}
}
\references{
Franczak et. al (2014). Mixtures of Shifted Asymmetric Laplace Distributions. IEEE Transactions on Pattern Analysis and Machine Intelligence, 38(6), 1149-1157.
}
\author{
Brian C. Franczak [aut, cre], Ryan P. Browne [aut, ctb], Paul D. McNicholas [aut, ctb]

Maintainer: Brian C. Franczak <franczakb@macewan.ca>
}
\examples{
## Clustering Simulated Data
alpha <- matrix(c(2,2,1,2),2,2)
sig <- array(NA,dim=c(2,2,2))
sig[,,1] <- diag(2)
sig[,,2] <- matrix(c(1,0.5,0.5,1),2,2)
mu <- matrix(c(0,0,-2,5),2,2)
pi.g <- rep(1/2,2)
x <- rmsal(n=500,p=2,alpha=alpha,sig=sig,mu=mu,pi.g=pi.g)

msal.ex1 <- msal(x=x[,-1],G=2)
table(x[,1],msal.ex1$cluster)

## Clustering the Old Faithful Geyser Data
data(faithful)
msal.ex2 <- msal(x=faithful,G=2)
plot(x=faithful,col=msal.ex2$cluster)

## Clustering the Yeast Data
data(yeast)
msal.ex3 <- msal(x=yeast[,-1],G=2)
table(yeast[,1],msal.ex3$cluster)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ Mixtures of SAL Distributions }% use one of  RShowDoc("KEYWORDS")
\keyword{ EM Algorithm }% __ONLY ONE__ keyword per line
\keyword{ Model-Based Clustering }