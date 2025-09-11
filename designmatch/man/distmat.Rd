\name{distmat}

\alias{distmat}

\title{Build a rank-based Mahalanobis distance matrix}

\description{
	Function for building a normalized rank-based Mahalanobis distance matrix with a penalty for caliper violation.  
}	

\usage{
	distmat(t_ind, X_mat, calip_cov = NULL, calip_size = NULL, calip_penalty = NULL,
	        near_exact_covs = NULL, near_exact_penalties = NULL, digits = 1)
}

\arguments{
	\item{t_ind}{treatment indicator: a vector of zeros and ones indicating treatment (1 = treated; 0 = control).}

	\item{X_mat}{matrix of covariates: a matrix of covariates used to build the based Mahalanobis distance matrix.}

	\item{calip_cov}{caliper covariate: a covariate vector used to define the caliper.  In most applications this is the propensity score, but a covariate can be used as well.}

	\item{calip_size}{caliper size: a scalar that determines the size of the caliper for which there will be no penalty.  Most applications use \code{0.2*sd(calip_cov)}.}
	
	\item{calip_penalty}{a scalar used to multiply the magnitude of the violation of the caliper.}
	
	\item{near_exact_covs}{a matrix of covariates used for near-exact matching.}
	
	\item{near_exact_penalties}{a vector of scalars used for near-exact matching.  The length of \code{near_exact_penalties} has to be equal to the number of columns of \code{near_exact_covs}.}
	
	\item{digits}{a scalar indicating the number of digits used to produce each entry of the distance matrix.  The default is 1 digit.}
}

\details{
	\code{distmat} is a function for building a normalized rank-based Mahalanobis distance matrix with a penalty for caliper violations on a covariate (say, the propensity score) and penalties for near-exact matching. 
	
	As explained in Rosenbaum (2010), the use of a rank-based Mahalanobis distance prevents an outlier from inflating the variance for a variable, and it thus decreases its importance in the matching.  
	In the calculation of the matrix the variances are constrained to not decrease as ties become more common, so that it is not more important to match on a rare binary variable than on a common binary one.  
	The penalty for caliper violations ensures good balance on the propensity score or the covariate used.
	In this way the rank-based Mahalanobis distance with a penalty for caliper violations in the propensity score constitutes a robust distance for matching.
	
	As explained in Zubizarreta et al. (2011), the distance matrix can also be modified for near-exact matching. 
	Penalties are added to the distance matrix every time that a treated and a control unit have a different value for the corresponding near-exact matching covariate.  
}

\value{
	A matrix that can be used for optimal matching with the \code{bmatch} functions in the \code{designmatch} package.
}

\references{	
	Rosenbaum, P. R. (2010), \emph{Design of Observational Studies}, Springer.

	Zubizarreta, J. R., Reinke, C. E., Kelz, R. R., Silber, J. H., and Rosenbaum, P. R. (2011), "Matching for Several Sparse Nominal Variables in a Case-Control Study of Readmission Following Surgery," \emph{The American Statistician}, 65, 229-238.
}

\examples{
	# Load data
	data(germancities)
	attach(germancities)

	# Treatment indicator
	t_ind = treat

	# Matrix of covariates
	X_mat = cbind(log2pop, popgrowth1939, popgrowth3339, emprate, indrate, rubble, 
	rubblemiss, flats, flatsmiss, refugees)

	# Distance matrix
	dist_mat = distmat(t_ind, X_mat)
}