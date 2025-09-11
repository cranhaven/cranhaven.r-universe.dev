\name{meantab}

\alias{meantab}

\title{Tabulate means of covariates after matching}

\description{
	Function for tabulating the means and other basic statistics useful to assess covariate balance after matching.  
}

\usage{
	meantab(X_mat, t_ind, t_id, c_id, exact = NULL, digits = 2)
}

\arguments{
	\item{X_mat}{matrix of covariates: a matrix of covariates used to assess balance.}

	\item{t_ind}{treatment indicator: a vector of zeros and ones indicating treatment (1 = treated; 0 = control).}
	
	\item{t_id}{a vector of indexes of the treated units.}

	\item{c_id}{a vector of indexes of the matched controls.}
	
	\item{exact}{a vector of characters equal to "f" or "w" indicating whether Fisher's exact test or Wilcoxon rank-sum test should be used for binary (or categorical) and continous covariates, respectively.  Otherwise, if exact \code{exact = NULL}, simple t-tests are used.  The default is \code{exact = NULL}.  If \code{exact != NULL}, the length of \code{exact} has to be equal to the number of columns of \code{X_mat}.}
	
	\item{digits}{a scalar indicating the number of digits to display in the columns of the table.}
}

\details{
	\code{meantab} is a function for tabulating the means and other basic statistics useful to assess covariate balance after matching. 		
}

\value{
	A table with the following columns:
	
	\item{Mis}{proportion of missing values for each covariate;}
	
	\item{Min}{minimum value for each covariate;}
	
	\item{Max}{maximum value for each covariate;}
	
	\item{Mean T}{mean of the treated units for each covariate;}
	
	\item{Mean C}{mean of the matched controls for each covariate;}
	
	\item{Std Dif}{standardized differences in means after matching for each covariate;}
	
	\item{P-val}{P-values for t-tests for differences in means between treated units and matched controls for each covariate.}
}	

\examples{
	# Load data
	data(germancities)

	# Sort and attach data
	germancities = germancities[order(germancities$treat, decreasing = TRUE), ]
	attach(germancities)

	# Treatment indicator
	t_ind = treat
	
	# Indexes of the treated units
	t_id = which(t_ind == 1)

	# Matrix of covariates
	X_mat = cbind(log2pop, popgrowth1939, popgrowth3339, emprate, indrate, rubble, 
	rubblemiss, flats, flatsmiss, refugees)

	# Indices of the matched controls (obtained using bmatch in designmatch)
	c_id = c(67, 75, 39, 104, 38, 93, 79, 59, 64, 55, 106, 99, 97, 61, 82, 57, 
	76, 47, 46, 49)
 
 	# meantab
	meantab(X_mat, t_ind, t_id, c_id)
	
	# meantab
	meantab(X_mat, t_ind, t_id, c_id, exact = c(rep("w", 6), "f", "w", "f", "w"), digits = 3)
}
