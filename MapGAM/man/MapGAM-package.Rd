\name{MapGAM-package}
\alias{MapGAM-package}
\alias{MapGAM}
\docType{package}
\title{
Mapping Smoothed Effect Estimates from Individual-Level Spatial Data
}
\description{
Contains functions for estimating and mapping hazard ratios, odds ratios, rate ratios, and continuous outcomes from individual-level data such as case-control study data, using generalized additive models (GAMs) or extended Cox models.  This allows for smoothing with a two-dimensional predictor such as geolocation or exposure to chemical mixtures, while adjusting for confounding variables using methods described by Kelsall and Diggle (1998), Webster at al. (2006), Bai et al. (2020), and Tang et al. (2023).  Includes convenient functions for mapping, efficient control sampling, confidence intervals, and permutation tests for the null hypothesis that the two-dimensional predictor is not associated with the outcome variable, adjusting for confounders.     
}
\details{
\tabular{ll}{
Package: \tab MapGAM\cr
Type: \tab Package\cr
Version: \tab 1.3\cr
Date: \tab 2023-07-12\cr
License: \tab GPL-3\cr
}
Typical spatial applications will start with the predgrid function to create a regular grid of points within the study area, with optional map boundaries (e.g., a country, state, or regional map available in the maps package, or uploaded from a shapefile using the st_read function in the sf package).  Crude or adjusted odds ratios, hazard ratios, or continuous outcomes are then estimated at each grid point using the modgam function to smooth by geolocation.  Finally, the predicted values (and optionally, "clusters"--areas of signficantly increased or decreased values determined via permutation tests or confidence intervals) are plotted using the plot or colormap functions.  Most users will use the plot function to create maps from modgam objects, but colormap is provided for backwards compatibility as well as for creating maps from models fit using functions other than modgam.  The trimdata and sampcont functions can be used to restrict data to those within map boundaries and to conduct simple or spatiotemporal stratified sampling from eligible controls.  The optspan function can be used to find an optimal span size for the LOESS smoother used by the modgam function; it is automatically used within the modgam function when the span size is not provided by the user.  These functions can also be applied to non-spatial data when two-dimensional smoothing is of interest, such as investigation of the effects of a mixture of two chemicals.}

\author{
Lu Bai, Scott Bartell, Robin Bliss, and Veronica Vieira

Send bug reports to \email{sbartell@uci.edu}.
}

\references{
Bai L, Gillen DL, Bartell SM, Vieira VM. Mapping smoothed spatial effect estimates from individual-level data:  MapGAM. The R Journal 2020, 12:32-48. \doi{10.32614/RJ-2020-001} 

Hastie TJ, Tibshirani RJ. Generalized Additive Models.  (Chapman & Hall/CRC Monographs on Statistics & Applied Probability, Boca Raton, Florida, 1990).  

Kelsall J, Diggle P. Spatial variation in risk of disease: a nonparametric binary regression approach. J Roy Stat Soc C-App 1998, 47:559-573. \doi{10.1111/1467-9876.00128} 	

Tang IW, Bartell SM, Vieira VM. Unmatched Spatially Stratified Controls: A simulation study examining efficiency and precision using spatially-diverse controls and generalized additive models. Spatial and Spatio-temporal Epidemiology 2023, 45:100584. \doi{10.1016/j.sste.2023.100584}
	
Vieira V, Webster T, Weinberg J, Aschengrau A, Ozonoff D. Spatial analysis of lung, colorectal, and breast cancer on Cape Cod: An application of generalized additive models to case-control data. Environmental Health 2005, 4:11. \doi{10.1186/1476-069X-4-11} 

Webster T, Vieira V, Weinberg J, Aschengrau A. Method for mapping population-based case-control studies using Generalized Additive Models. International Journal of Health Geographics 2006, 5:26. \doi{10.1186/1476-072X-5-26}

Young RL, Weinberg J, Vieira V, Ozonoff A, Webster TF. A power comparison of generalized additive models and the spatial scan statistic in a case-control setting. International Journal of Health Geographics 2010, 9:37. \doi{10.1186/1476-072X-9-37}

}

\keyword{ package }
\keyword{ smooth }
\keyword{ datasets }
\keyword{ hplot }
\keyword{ misc }

\seealso{
\code{\link{trimdata}}, 
\code{\link{sampcont}}, 
\code{\link{predgrid}}, 
\code{\link{optspan}}, 
\code{\link{modgam}},
\code{\link{colormap}}.
\code{\link{plot.modgam}}.
}

\examples{
# Load synthetic data and a preformatted base map
data(MAmap)		
data(MAdata)
\donttest{
# Create a grid on the base map 
gamgrid <- predgrid(MAdata, map=MAmap) 
# Fit a GAM with a smooth term for spatial location
fit1 <- modgam(data=MAdata, rgrid=gamgrid, m="crude", sp=0.5)   
# Display odds ratio estimates on the base map
plot(fit1, MAmap, exp=TRUE)								
}
#### See colormap and modgam help files for more examples 
}
