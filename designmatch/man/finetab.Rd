\name{finetab}

\alias{finetab}

\title{Tabulate the marginal distribution of a nominal covariate after matching}

\description{
	Function for tabulating the marginal distributions of a nominal covariate for the treated units and matched controls.
}

\usage{
	finetab(nom_cov, t_id, c_id)
}

\arguments{
	\item{nom_cov}{a nominal covariate vector used to assess balance.}

	\item{t_id}{a vector of indexes of the treated units.}

	\item{c_id}{a vector of indexes of the matched controls.}
}

\details{
	\code{finetab} is a function for tabulating the marginal distributions of a nominal covariate for the treated units and matched controls.	
	\code{finetab} is useful for assessing covariate balance after matching with after exact, near-exact matching, fine and near-balance with the \code{bmatch} or \code{nmatch} functions in the \code{designmatch} package. 	
}

\value{
	A table with the counts for the treated units and matched controls for each category of a nominal covariate.
}	

\author{
	Jose R. Zubizarreta <zubizarreta@hcp.med.harvard.edu>, Cinar Kilcioglu <ckilcioglu16@gsb.columbia.edu>.
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

	# Indixes of the matched controls (obtained using bmatch in designmatch)
	c_id = c(80, 82, 35, 59, 69, 68, 34, 62, 104, 61, 106, 120, 56, 119, 28, 
	113, 76, 118, 75, 71)
 
 	# finetab
	finetab(publicat, t_id, c_id)
	finetab(busiservcat, t_id, c_id)
}
