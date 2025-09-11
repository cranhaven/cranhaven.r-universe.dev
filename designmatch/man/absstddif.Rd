\name{absstddif}

\alias{absstddif}

\title{
	Absolute standardized differences in means.  
}	

\description{
	Function for calculating absolute differences in means between the covariates in the treatment and control groups in terms of the original units of the covariates.  Here, the absolute differences in means are normalized by the simple average of the treated and control standard deviations before matching (see Rosenbaum and Rubin 1985 for details).
}	
\usage{
	absstddif(X_mat, t_ind, std_dif)
}

\arguments{
	\item{X_mat}{matrix of covariates: a matrix of covariates used to build the rank-based Mahalanobis distance matrix.}

	\item{t_ind}{treatment indicator: a vector of zeros and ones indicating treatment (1 = treated; 0 = control).}

	\item{std_dif}{standardized differences: a scalar determining the number of absolute standardized differences.}
}

\value{
	A vector that can be used with the \code{mom}, \code{near} and \code{far} options of \code{bmatch} and \code{nmatch}.
}

\references{
	Rosenbaum, P. R., and Rubin, D. B. (1985), "Constructing a Control Group by Multivariate Matched Sampling Methods that Incorporate the Propensity Score," \emph{The American Statistician}, 39, 33-38.
}

\author{
	Jose R. Zubizarreta <zubizarreta@hcp.med.harvard.edu>, Cinar Kilcioglu <ckilcioglu16@gsb.columbia.edu>.
}

\examples{
# Load and attach data
data(lalonde)
attach(lalonde)

# Treatment indicator
t_ind = treatment

# Constrain differences in means to be at most .05 standard deviations apart
mom_covs = cbind(age, education, black, hispanic, married, nodegree, re74, re75)
mom_tols = absstddif(mom_covs, t_ind, .05)
}
