\name{pairsplot}

\alias{pairsplot}

\title{Pairs plot for visualizing matched pairs}

\description{
	Function for visualizing matched pairs in two dimensions.  
}

\usage{
	pairsplot(cov1, cov2, t_id, c_id, xlab, ylab, main)
}

\arguments{
	\item{cov1}{a vector for the covariate to be plotted on the x axis.}
	
	\item{cov2}{a vector for the covariate to be plotted on the y axis.}

	\item{t_id}{a vector of indexes of the treated units.}

	\item{c_id}{a vector of indexes of the matched controls.}
	
	\item{xlab}{a string specifying the label of the x axis.}
	
	\item{ylab}{a string specifying the label of the y axis.}
	
	\item{main}{a string specifying the main title of the plot.}
}

\details{
	\code{pairsplot} is a function for visualizing matched pairs in two dimensions, usually defined by two of the matching covariates.
	Matched pairs are connected by line segments.
	Horizontal and vertical lines show the means of treated units and matched controls for each of the covariates.
	Among others, \code{pairsplot} can be useful for visualizing near/far matches, e.g. when building a stronger instrumental variable (Baiocchi et al., 2010).
}

\references{
	Baiocchi, M., Small, D., Lorch, S. and Rosenbaum, P. R. (2010), "Building a Stronger Instrument in an Observational Study of Perinatal Care for Premature Infants," \emph{Journal of the American Statistical Association}, 105, 1285-1296.
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
	
	# Indices of the matched controls (obtained using bmatch in designmatch)
	c_id = c(67, 75, 39, 104, 38, 93, 79, 59, 64, 55, 106, 99, 97, 61, 82, 
	57, 76, 47, 46, 49)
 
	# pairsplot	
	pairsplot(rubble, flats, t_id, c_id, "Rubble", "Flats", "")
}