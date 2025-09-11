\name{loveplot}

\alias{loveplot}

\title{Love plot for assessing covariate balance}

\description{
	Function that creates a Love plot for assessing covariate balance after matching.  
}

\usage{
	loveplot(X_mat, t_id, c_id, v_line, legend_position = "topright")
}

\arguments{
	\item{X_mat}{matrix of covariates: a matrix of covariates used to assess balance.}

	\item{t_id}{a vector of indexes of the treated units.}

	\item{c_id}{a vector of indexes of the matched controls.}
	
	\item{v_line}{a scalar defining the location of the vertical line that denotes a satisfactory balance.}
	
	\item{legend_position}{a string specifying the position of the legend.  
		  The default is \code{topright}.  
		  Other options are: \code{bottomright}, \code{bottom}, \code{bottomleft}, \code{left}, \code{topleft}, \code{top}, \code{topright}, \code{right} and \code{center}}
}

\details{
	In the spirit of Love (2004), \code{loveplot} draws a love plot for assessing covariate balance after matching.
	Specifically, \code{loveplot} plots the absolute standardized differences in means before and after matching for all the covariates specified in \code{X_mat}.
}

\references{
	Love, T. (2004), "Graphical Display of Covariate Balance," http://chrp.org/love/JSM2004RoundTableHandout.pdf.
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

	# Matrix of covariates
	X_mat = cbind(log2pop, popgrowth1939, popgrowth3339, emprate, indrate, 
	rubble, rubblemiss, flats, flatsmiss, refugees)

	# Indices of the matched controls (obtained using bmatch in designmatch)
	c_id = c(67, 75, 39, 104, 38, 93, 79, 59, 64, 55, 106, 99, 97, 61, 82, 57, 76, 47, 46, 49)
 
	# Vertical line for satisfactory balance
	vline = 0.15
	
 	# loveplot
	loveplot(X_mat, t_id, c_id, vline) 
}
