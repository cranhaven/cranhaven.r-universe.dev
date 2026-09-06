\name{mixedMem-package}
\alias{mixedMem-package}
\alias{mixedMem}
\docType{package}

\title{
Tools for fitting discrete multivariate mixed membership models
}

\description{
The \code{mixedMem} package contains tools for fitting and interpreting discrete multivariate mixed membership models following the general framework outlined in Erosheva et al 2004. In a mixed membership models, individuals can belong to multiple groups instead of only a single group (Airoldi et al 2014). This extension allows for a richer description of heterogeneous populations and has been applied in a wide variety of contexts including: text data (Blei et al 2003), genotype sequences (Pritchard  et al 2000), ranked data (Gormley and Murphy 2009), and survey data (Erosheva et al 2007, Gross and Manrique-Vallier 2014). 
}

\examples{
library(mixedMem)
data(ANES)
# Dimensions of the data set: 279 individuals with 19 responses each
dim(ANES)
# The 19 variables and their categories
# The specific statements for each variable can be found using help(ANES)
# Variables titled EQ are about Equality
# Variables titled IND are about Econonic Individualism
# Variables titled ENT are about Free Enterprise
colnames(ANES)
# Distribution of responses
table(unlist(ANES))

# Sample Size
Total <- 279
# Number of variables
J <- 19 
# we only have one replicate for each of the variables
Rj <- rep(1, J)
# Nijr indicates the number of ranking levels for each variable.
# Since all our data is multinomial it should be an array of all 1s
Nijr <- array(1, dim = c(Total, J, max(Rj)))
# Number of sub-populations
K <- 3
# There are 3 choices for each of the variables ranging from 0 to 2.
Vj <- rep(3, J)
# we initialize alpha to .2
alpha <- rep(.2, K)
# All variables are multinomial
dist <- rep("multinomial", J)
# obs are the observed responses. it is a 4-d array indexed by i,j,r,n
# note that obs ranges from 0 to 2 for each response
obs <- array(0, dim = c(Total, J, max(Rj), max(Nijr)))
obs[ , ,1,1] <- as.matrix(ANES)

# Initialize theta randomly with Dirichlet distributions
set.seed(123)
theta <- array(0, dim = c(J,K,max(Vj)))
for(j in 1:J)
{
    theta[j, , ] <- gtools::rdirichlet(K, rep(.8, Vj[j]))
}

# Create the mixedMemModel
# This object encodes the initialization points for the variational EM algorithim
# and also encodes the observed parameters and responses
initial <- mixedMemModel(Total = Total, J = J, Rj = Rj,
                         Nijr = Nijr, K = K, Vj = Vj, alpha = alpha,
                         theta = theta, dist = dist, obs = obs)
\dontrun{
# Fit the model
out <- mmVarFit(initial)
summary(out)
}
}

\details{
Mixed membership model objects can be created using the \code{mixedMemModel}
constructor function. This function checks the internal consistency of the data/parameters
and returns an object suitable for use by the \code{mmVarFit} function. The 
\code{mmVarFit} function is the main function in the package. It utilizes a variational EM algorithim to fit an approximate posterior distribution for the latent variables and select pseudo-MLE estimates for the global parameters. A step-by-step guide to using the package is detailed in the package vignette "Fitting Mixed Membership Models using \code{mixedMem}". 

The package supports multivariate models (with or without repeated measurements) where each variable can be of a different type. Currently supported data types include: Bernoulli, rank (Plackett-Luce) and multinomial. Given a fixed number of sub-populations K, we assume the following generative model for each mixed membership model:

\itemize{
\item{For each individual i = 1,\ldots Total:
    \itemize{
      \item{Draw \eqn{\lambda_i} from a Dirichlet(\eqn{\alpha}). \eqn{\lambda_i} is a vector of length K whose components indicates the degree of membership for individual i in each of the K sub-populations.}
      \item{For each variable j = 1 \ldots, J:}
      \item {For each of replicate r = 1, \dots, \eqn{R_j}:}
      \item{For each ranking level n = 1\ldots, \eqn{N_{i,j,r}}:
                                                      \itemize{
                                                      \item Draw \eqn{Z_{i,j,r,n}} from a multinomial(1, \eqn{\lambda_i}). The latent sub-population indicator \eqn{Z_{i,j,r,n}} determines the sub-population which governs the response for observation \eqn{X_{i,j,r,n}}. This is sometimes referred to as the context vector because it determines the context from which the individual responds.
\item  Draw \eqn{X_{i,j,r,n}} from the latent sub-population distribution parameterized by \eqn{\theta_{j,Z_{i,j,r,n}}}. The parameter \eqn{\theta} governs the observations for each sub-population. For example, if variable j is a multinomial or rank distribution with \eqn{V_j} categories/candidates, then \eqn{\theta_{j,k}} is a vector of length \eqn{V_j} which parameterizes the responses to variable j for sub-population k. Likewise, if variable j is a Bernoulli random variable, then \eqn{\theta_{j,k}} is a value which determines the probability of success. 
}}}}}

}

\author{
Y. Samuel Wang <ysamuelwang@gmail.com>, Elena Erosheva <erosheva@uw.edu>
}

\references{
Airoldi, E. M., Blei, D., Erosheva, E. A., & Fienberg, S. E.. 2014. Handbook of Mixed Membership Models and Their Applications. CRC Press. Chicago  

Blei, David; Ng, Andrew Y.; Jordan, Michael I.. 2003. Latent Dirichlet Allocation. Journal of Machine Learning Research, 3, 993-1022. 

Erosheva, Elena A.; Fienberg, Stephen E.; Joutard, Cyrille. 2007. Describing Disability Through Individual-level Mixture Models for Multivariate Binary Data. The Annals of Applied Statistics 1 (2007), no. 2, 502--537. doi:10.1214/07-AOAS126.

Erosheva, Elena A.; Fienberg, Stephen E.; Lafferty, John. 2004. Mixed-membership Models of Scientific Publications".
PNAS, 101 (suppl 1), 5220-5227. doi:10.1073/pnas.0307760101.

Gormley, Isobel C.; Murphy, Thomas B.. 2009. A Grade of Membership Model for Rank Data. Bayesian Analysis, 4, 265 - 296. DOI:10.1214/09-BA410.

National Election Studies, 1983 Pilot Election Study. Ann Arbor, MI: University of Michigan, Center for Political Studies, 1999

Pritchard, Jonathan K.; Stephens, Matthew; Donnelly, Peter. 2000. Inference of Population Structure using Multilocus Genotype Data. Genetics 155.2: 945-959.

Gross, Justin; Manrique-Vallier, Daniel. 2014.  A Mixed-membership Approach to the Assessment of Political Ideology from Survey Responses. In Airoldi, Edoardo M.; Blei, David; Erosheva, Elena A.; & Fienberg, Stephen E.. Handbook of Mixed Membership Models and Their Applications. CRC Press. Chicago

}
% Optionally other standard keywords, one per line,
% from the file KEYWORDS in the R documentation.
\concept{mixed membership}
\concept{grade of membership}
\concept{variational inference}