\name{ecdfplot}

\alias{ecdfplot}

\title{Empirical cumulative distribution function plot for assessing covariate balance}

\description{
	Function that plots the empirical cumulative distribution function of a given covariate for treated units and matched controls.
	\code{ecdfplot} can be used to visually inspect the balance of the entire empirical distribution function of the covariate in question.  
}

\usage{
	ecdfplot(x, t_id, c_id, main_title = "", legend_position = "right")
}

\arguments{
	\item{x}{a covariate vector to be used to assess balance.}

	\item{t_id}{a vector of indexes of the treated units.}

	\item{c_id}{a vector of indexes of the matched controls.}
	
	\item{main_title}{a string defining the main title of the plot.}
	
	\item{legend_position}{a string specifying the position of the legend.  
		  The default is \code{right}.  
		  Other options are: \code{topright}, \code{bottomright}, \code{bottom}, \code{bottomleft}, \code{left}, \code{topleft}, \code{top} and \code{center}}
}

\details{
	Function that plots the empirical cumulative distribution function of a given covariate for treated units and matched controls.
	\code{ecdfplot} can be used to visually inspect the balance of the entire empirical distribution function of the covariate in question.  
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
		
	# Indexes of the controls before matching
	c_id_before = which(t_ind == 0)
	
	# Indixes of the matched controls (obtained using bmatch in designmatch)
	c_id_after = c(80, 82, 35, 59, 69, 68, 34, 62, 104, 61, 106, 120, 56, 119, 28, 
	113, 76, 118, 75, 71)
	
	# ecdfplot
	par(mfrow = c(2, 1))
	ecdfplot(rubble, t_id, c_id_before, "Before matching")
	ecdfplot(rubble, t_id, c_id_after, "After matching")
}
