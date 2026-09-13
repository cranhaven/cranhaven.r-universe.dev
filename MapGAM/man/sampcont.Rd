\name{sampcont}
\alias{sampcont}
\title{
Unmatched Control Sampling
}
\description{
Take all cases and a random sample of controls from a data frame.  Simple random sampling and spatially stratified random sampling are available.  For spatially statified random sampling, strata can be defined by region, or by region and additional stratification variables (see Tang et al., 2023 for examples and simulation comparisons).  If no specific regions are specified with stratified sampling, the function will create a regular grid for spactially stratified sampling.     
}
\usage{
sampcont(rdata, type = "stratified", casecol=1, Xcol=2, Ycol=3, regions = NULL, 
		addstrat = NULL, times = NULL, n = 1, nrow = 100, ncol = 100)
}
\arguments{
  \item{rdata}{
a data frame with case status in the \code{casecol} column (by default, the 1st column), and the geocoordinates (e.g., X and Y) in the \code{Xcol} and \code{Ycol} columns (by default, the 2nd and 3rd columns).  Additional columns are not used in the sampling scheme but are retained in the sampled data frame.  
}
  \item{casecol}{
the column number in \code{rdata} for the case status (coded with 0 for controls, 1 for cases).  	
}
  \item{Xcol}{
the column number in \code{rdata} for the X geocoordinate.	
}
  \item{Ycol}{
the column number in \code{rdata} for the Y geocoordinate.  	
}
  \item{type}{
\code{"stratified"} (default) or \code{"simple"}.  If \code{"simple"} then a simple random sample of \code{n} controls (rows of \code{rdata} with \code{outcome=0}) is obtained.  If \code{"stratified"} then a stratified random sample of controls is obtained, with up to \code{n} controls per stratum.  Sampling strata are defined by the \code{regions} and \code{times} arguments.  All cases (rows with outcome=1) are taken for the sample regardless of the value supplied for \code{type}.  
}
  \item{regions}{
a vector of length equal to the number of rows in \code{rdata}, used to construct sampling strata.  Only used if \code{type = "stratified"}.  If \code{regions = NULL} then the function will define \code{regions} as a vector of specific grid cells on a regular grid with \code{nrow} rows and \code{ncol} columns.  If \code{times = NULL} then the nonempty regions are used as the sampling strata.  If \code{times} is a vector, then the sampling strata are all nonempty combinations of \code{regions} and \code{times}.      
}
  \item{addstrat}{
a vector of length equal to the number of rows in \code{rdata}, used along with \code{regions} to construct sampling strata.  If \code{addstrat = NULL} then the sampling strata are defined only by the \code{regions} argument.  If \code{addstrat} is a vector, then the sampling strata are all nonempty combinations of \code{regions} and \code{addstrat}.  Continuous variables should generally be binned before being passed through this argument, as there are no efficiency gains if each value in \code{addstrat} is unique.     
}
  \item{times}{
included for backward compatibility; now replaced by the \code{addstrat} argument which serves the same purpose.   	
}
  \item{n}{
the number of controls to sample from the eligible controls in each stratum.  All available controls will be taken for strata with fewer than \code{n} eligible controls.  
}
  \item{nrow}{
the number of rows used to create a regular grid for sampling regions.  Only used when \code{regions = NULL}.
}
  \item{ncol}{
the number of columns used to create a regular grid for sampling regions.  Only used when \code{regions = NULL}.
}
}
\value{
  \item{rdata}{
a data frame with all cases and a random sample of controls.  
}
  \item{w}{
the inverse probability weights for the rows in \code{rdata}.  Important to include as weights in subsequent analyses.    	
}
  \item{ncont}{
the total number of controls in the sample. 
}
  \item{type}{
statified or simple sampling, as specified by the same argument described above.
}
  \item{gridsize}{
a vector with the numbers of rows and columns for the stratified sampling grid.
}
  \item{grid}{
the stratified sampling grid in PolySet format.
}
}
  \author{
Scott M. Bartell and Ian W. Tang
\email{sbartell@uci.edu}.
}

  \references{
Tang IW, Bartell SM, Vieira VM. Unmatched Spatially Stratified Controls: A simulation study examining efficiency and precision using spatially-diverse controls and generalized additive models. Spatial and Spatio-temporal Epidemiology 2023, 45:100584. \doi{10.1016/j.sste.2023.100584}
}

\seealso{
\code{\link{modgam}}
}

\examples{
#### load beertweets data, which has 719 cases and 9281 controls
data(beertweets)
# take a simple random sample of 1000 controls
samp1 <- sampcont(beertweets, type="simple", n=1000)

# take a stratified random sample of controls on a 80x50 grid
samp2 <- NULL
\donttest{
samp2 <- sampcont(beertweets, nrow=80, ncol=50)

# Compare locations for the two sampling designs (cases in red)
par(mfrow=c(2,1), mar=c(0,3,4,3))
plot(samp1$rdata$longitude, samp1$rdata$latitude, col=3-samp1$rdata$beer,
	cex=0.5, type="p", axes=FALSE, ann=FALSE)
# Show US base map if maps package is available
mapUS <- require(maps)
if (mapUS) map("state", add=TRUE)
title("Simple Random Sample, 1000 Controls")

if (!is.null(samp2)) {
	plot(samp2$rdata$longitude, samp2$rdata$latitude, 
		col=3-samp2$rdata$beer, cex=0.5, type="p", axes=FALSE, 
		ann=FALSE)
	if (mapUS) map("state", add=TRUE)
	title(paste("Spatially Stratified Sample,",samp2$ncont,"Controls"))
	}
}
par(mfrow=c(1,1))

## Note that weights are needed in statistical analyses
# Prevalence of cases in sample--not in source data
mean(samp1$rdata$beer)		 
# Estimated prevalence of cases in source data 
weighted.mean(samp1$rdata$beer, w=samp1$w)	
## Do beer tweet odds differ below the 36.5 degree parallel?
# Using full data
glm(beer~I(latitude<36.5), family=binomial, data=beertweets) 
# Stratified sample requires sampling weights 
if (!is.null(samp2)) glm(beer~I(latitude<36.5), family=binomial, 
	data=samp2$rdata, weights=samp2$w)
}

\keyword{misc}
