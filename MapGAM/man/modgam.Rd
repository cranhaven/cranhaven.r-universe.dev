\name{modgam}
\alias{modgam}
\title{
Fit a Generalized Additive Model (GAM) with a Two-Dimensional Smooth and Make Predictions}
\description{
Fits a crude or adjusted regression on a user-supplied grid for spatial analysis using a generalized additive model with a two-dimensional LOESS smooth for geolocation (or any other two-dimensional predictor).  Includes optional pointwise confidence intervals and permutation tests for global and local tests of the null hypothesis that the two-dimensional predictor (e.g., geolocation) is not associated with the outcome.  Most applications will pass the output of this function to \code{plot.modgam} to map the resulting odds ratios or other effect estimates.}
\usage{
modgam(formula, rgrid, data, subset, offset, family = binomial(), permute = 0, 
       conditional = TRUE, pointwise = FALSE, m = "adjusted", sp = seq(0.05,0.95,0.05), 
       degree=1, keep=FALSE, type=c("spatial","all"), reference = "median", 
       se.fit = FALSE, alpha = 0.05, verbose = TRUE, \dots)
}
\arguments{
  \item{formula}{
a formula expression, with the response on the left of a ~ oprator, and the predictors on the right. If fitting a Cox additive model, the response must be a survival object as returned by the Surv function. A built-in nonparametric smoothing term is indicated by \code{lo} for loess smooth terms. The two-dimensional predictor (e.g.,geolocation) should be specified as auguments in \code{lo()}. \code{formula} can be missing if \code{data} and \code{family} are specified.
}
  \item{rgrid}{
a data frame containing the values of X and Y coordinates at which to generate predictions. If missing, the predictions will be generated for the two-dimentional predictor in \code{data}. The \code{predgrid} function can be used to supply an appropriate rgrid for rdata, with the grid clipped according to any specified base map (see examples below).   
}
  \item{data}{
a data frame containing the variables in the model. If \code{formula} is specified, \code{data} is optional. In this way, if not found in \code{data}, the variables are taken from \code{environment(formula)}. If \code{formula} is missing, the data must be structured so that the outcome is in the 1st column (1st and 2nd colums for survival data) and the X and Y coordinates for two-dimensional predictor (e.g., geolocation) are in the 2nd and 3rd columns (3rd and 4th columns for survival data), respectively.  If more than 3 (4 for survival data) columns are provided and \code{m = "adjusted"}, the additional columns will be entered as linear predictors in the GAM model.    
}
  \item{family}{
a description of the error distribution and link function to be used in the model. This can be a character string naming a family function, a family function or the result of a call to a family function. (See \code{\link{family}} for details of family functions.  Note that unlike other packages, MapGAM defaults to the binomial family and logit link, i.e., a logistic model.). If \code{formula} is missing, \code{family = "survival"} must be specified to fit a Cox proportional hazard additive model.
}
  \item{subset}{
an optional vector specifying a subset of observations to be used in the fitting process.
}
  \item{offset}{
name in data and rgrid that will be used as offset in the model. It will be ignored if offset is specified in \code{formula}. \code{offset} will be ignored if \code{family="survival"}
}
  \item{permute}{
the number of permutations of the data set for testing the significance of the two-dimensional predictor. \code{permute = 0} (default) produces no permutation tests.  If \code{permute > 0}, the paired coordinates for the two-dimensional predictor are randomly permuted in order to simulate the distribution of results under the null hypothesis that location is not associated with the outcome.  1000 permutations are recommended for reasonable accuracy of p-values.  WARNING: Because each permutation requires refitting the GAM, permutation tests can be quite slow.    
}
  \item{conditional}{
a logical value indicating whether to run a conditional or unconditional permutation test; this argument is used only when \code{permute > 0}.  The default is a conditional permutation test: the span size is held fixed throughout all permutations even if \code{sp = NULL} to optimize the span size for the original data (see Young et al., 2012).  The unconditional permutation test repeats the span size optimization for each permutation, which is more conservative but takes about 20 times longer to compute.  If \code{conditional = FALSE} the sp argument is ignored when fitting both the original and permuted data sets.               
}
  \item{pointwise}{
a logical value indicated whether to include pointwise permutation tests for every grid point.  This argument is only used
when \code{permute > 0}.  Using \code{se.fit = TRUE} to determine grid points with significantly different outcomes is usually preferable to using pointwise permutation tests.     	
}
  \item{m}{
model type for the GAM.  Options are "crude" or "adjusted" (default).  If "crude", only the smooth for the two-dimensional predictor is included in the model (i.e., "crude" is a synonym for "unadjusted").  If "adjusted", all covariates in the data set (columns >= 4) are included in the model.     
}
  \item{sp}{
the span size for the LOESS smooth of the two-dimensional predictor.  If \code{sp} is a vector an optimal span size will be determined using the \code{optspan} function, using the vector as candidate values.  By default the optimal span is chosen from among a sequence of values ranging from 0.05 to 0.95, in increments of 0.05. See the help file for \code{\link{optspan}} for more details. It will be ignored if span is indicated in \code{formula}.  
}
  \item{degree}{
the degree of the polynomials to be used for LOESS smooth, normally 1 or 2. It will be ignored if the degree is indicated in \code{formula}.
}
  \item{keep}{
a logical value indicating whether to store and return the pointwise odds ratios, and if conditional = FALSE the span size, for every permuted data set.  These values aren't necessary for mapping or cluster identification, and storing them slows the permutation tests, so the default is \code{keep = FALSE}.
}
  \item{type}{
use \code{type="spatial"}(default for censored survival data) to estimate the spatial effect, or \code{type="all"}(default for univariate outcome) to estimate the effect of all covariates included in the model.  
}
  \item{reference}{
referent for the predicted values or effects. If \code{reference="none"}, the output will be predicted values at each grid point for either the spatial effect (\code{type="spatial"}) or the linear predictor including all covariates (\code{type="all"}).  If \code{reference = "median"}, the output will be the difference between the estimated effect/predictor at each grid point and the median effect/predictor (e.g., the log odds ratio for a binomial model with logit link). If \code{reference = "mean"}, the output will be the difference between the estimated effect/predictor at each grid point and the mean effect/predictor.  If \code{reference} is a data frame indicating a specific geolocation, the output will be the difference between the estimated effect/predictor at each grid point and the effect/predictor at the geolocation specified by \code{reference}.
}
  \item{se.fit}{
if \code{TRUE}, pointwise standard errors and confidence intervals are computed along with the predictions.  Results can be passed to \code{plot.modgam} with \code{contours = "interval"} to identify  clusters of the outcome (e.g., spatial regions with statistically significant outcomes--e.g., unusually high or low risks).   
}
  \item{alpha}{
the significace level used for confidence intervals when \code{se.fit = TRUE}  
}
  \item{verbose}{
a logical value indicating whether to print the GAM model statement, the percentile rank for the global deviance statistic in the permutation test, and the progress of the permutation test (report completion of every 10 permutations).  The default is \code{verbose = TRUE}.      
}
  \item{\dots}{
further arguments to be passed to the \code{\link[gam:gam]{gam}} or \code{\link{gamcox}} function (e.g., weights).  	
}
}

\details{
The model used to fit the data is a generalized additive model with a LOESS smooth for a two-dimensional predictor such as geolocation (Hastie and Tibshirani, 1990; Kelsall and Diggle, 1998; Webster et al., 2006).  Although any family and link function supported by the \code{gam} function is supported, the default binomial family with logit link yields the following model:
\deqn{\ln \left(\frac{\pi_{i}}{1-\pi_{i}}\right) = S(x_{i},y_{i}) + \mathbf{Z_{i}} \boldsymbol{\beta}}{
	ln(\pi / (1-\pi)) = S(x,y) + Z \beta}	
where \eqn{\pi_{i}}{\pi} is the probability that the outcome is 1 for participant \emph{i}, \eqn{x_{i}}{x} and \eqn{y_{i}}{y} are predictor coordinates for participant \emph{i} (i.e., projected distance east and north, respectively, from an arbitrarily defined origin location), \eqn{S(.,.)} is a 2-dimensional smoothing function (currently LOESS), \eqn{\mathbf{Z_{i}}}{Z} is a row vector of covariate values for participant \emph{i}, and \eqn{\boldsymbol{\beta}}{\beta} is a vector of unknown regression parameters (including an intercept).  
When a permutation test is requested, for each permutation the paired X and Y coordinates in the data set are randomly reassigned to participants, consistent with the null hypothesis that the geolocation (or another two-dimensional predictor entered in place of X and Y) is not associated with the outcome.  Note that this procedure intentionally preserves associations between other covariates and the outcome variable so the permutation test reflects the significance of geolocation.  See the references for more details.              
}

\value{

\code{modgam} returns an object of class \code{modgam}. It can be examined by \code{print} and \code{plot}.

\item{grid}{a data frame with X and Y coordinates from rgrid.}
\item{span}{the span size used for the LOESS smooth.  If \code{keep = TRUE} and \code{conditional = FALSE}, a vector with optimized span sizes for the original data set and each of the permuted data sets.}
\item{gamobj}{the \code{\link[gam:gam]{gam}} or \code{\link{gamcox}} model object from the fit to the data.}
\item{predobj}{the \code{\link{mypredict.gam}} or \code{\link{predict.gamcox}} model object from the fit to the data.}
\item{family}{the family and link function used for the model.}
\item{fit}{the predicted values for each point on the grid.  For Gaussian family models, predicted values are for the outcome variable at each grid point.  For non-Gaussian families, predicted values are for the spatial effect alone on the scale of the linear predictor, compared to a referent specified through augument \code{reference} (e.g., for a binomial family logit link model with \code{reference = "median"}, the fit values are the difference in log odds of the outcome between the grid point and the median log odds, holding other predictors constant).  If predicted responses are desired for non-Gaussian families, they can be obtained from the fit values by setting \code{reference="none"} and applying the inverse link function.}
\item{exp.fit}{the exponential of \code{fit}.  Primarily useful for logit or log link functions (e.g., this provides odds ratios for a binomial family with logit link function.)}
\item{global.pvalue}{the p-value based on the likelihood ratio test comparing models with and without geolocation (or other two-dimensional predictor).
}
If \code{se.fit = TRUE} then the following values are provided:
\item{se}{an estimated standard error for each predicted value.}
\item{conf.low}{the lower bounds for pointwise (1-\code{alpha}) confidence intervals.}
\item{conf.high}{the higher bounds for pointwise (1-\code{alpha}) confidence intervals.}
If \code{permute > 0} then the following values are also provided:
\item{global.permt}{the p-value based on the deviance statistic comparing models with and without geolocation (or other two-dimensional predictor), using the distribution of deviance statistics from the permuted data sets to represent the null distribution.  For a test of H0: geolocation is unassociated with the outcome variable (adjusting for any covariates if \code{m = "adjusted"}), reject H0 if the percentile rank is below alpha.   WARNING: by default \code{modgam} uses a conditional permutation test which produces inflated type I error rates; Young et al. (2012) recommend using alpha=0.025 to limit the type I error rate to approximately 5\%.} 
\item{pointwise.permt}{Only provided when \code{pointwise = TRUE}.  For each point on the grid, the percentile rank of the local linear predictor for the model compared to the local linear predictor distribution from the permuted data sets.  This result can be used to define clusters of the outcome (e.g., spatial regions with statistically significant outcomes--e.g., unusually high or low risks), as in \code{plot.modgam} with \code{contours="permrank"}.}
If \code{permute} > 0 and \code{keep = T} then the following values are also provided:
\item{permutations}{a matrix containing null permuted values for each point on the grid, with the results for each permutation in a separate column.  For the binomial family and logit link these are provided as odds ratios, otherwise they are reported as linear predictors.}
\item{deviances.permt}{a vector containing deviances for each permutation.}
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

\author{
Lu Bai, Scott Bartell, Robin Bliss, and Veronica Vieira 

Send bug reports to \email{sbartell@uci.edu}.
}

\section{Warning }{Permutation tests are computationally intensive, often requiring several hours or more.} 

\seealso{
\code{\link{predgrid}}, 
\code{\link{optspan}}, 
\code{\link{plot.modgam}},
\code{\link{colormap}},
\code{\link[gam:gam]{gam}}.
}
\examples{
\donttest{
# Load base map in SpatialPolygonsDataFrame format
# This map was read from ESRI shapefiles using the readShapePoly function (soon to be deprecated)
data(MAmap)	
# Load data and create grid on base map
data(MAdata)							
gamgrid <- predgrid(MAdata, map=MAmap)    
# Fit crude logistic GAM to the MA data using span size of 50\%
# and predict odds ratios for every point on gamgrid
fit1 <- modgam(data=MAdata, rgrid=gamgrid, m="crude", sp=0.5)  
# Summary statistics for pointwise crude odds ratios 
summary(fit1$exp.fit)			
# Summary stats for pointwise crude log odds (linear predictor) 
summary(fit1$fit)			

# fit adjusted GAM using span size of 50\%, 
# including a (too small) conditional permutation test
fit2 <- modgam(data=MAdata, rgrid=gamgrid, permute=25, m="adjusted", sp=0.5)  
fit2

# fit adjusted GAM by specifiying formula
fit2.f <- modgam(Case~lo(Xcoord,Ycoord)+Smoking + Mercury + Selenium,data=MAdata,
          rgrid=gamgrid, sp=0.5)
fit2.f

# Detailed example with a continuous outcome variable, map projections, 
# and data trimming:  investigating tweet times by geolocation
# NOTE: this example requires the maps and mapproj packages
# Convert base map and beer tweet data locations to US Albers projection 
# for better distance estimates than using (lat,long) as (X,Y) coords 
if(require(maps) & require(mapproj)) {
	USmap <- map("state",projection="albers",parameters=c(29.5,45.5),
		plot=FALSE,fill=TRUE,col="transparent")
	data(beertweets)
	# Reuse last map projection to convert data coordinates	
	XY <- mapproject(beertweets$longitude,beertweets$latitude)[1:2]  
	jtime <- julian(beertweets$time)
	# Convert tweet dates and times to time of day (24-hour)
	tweettime <- as.numeric(jtime-trunc(jtime))*24
	beerproj <- data.frame(tweettime, XY[1], XY[2], beertweets$beer)			 
	# Generate grid on the US map, trimmed to range of beer data
	USgrid <- predgrid(beerproj, map=USmap)						
	# Fit adjusted model--adjusting for beer indicator variable 
	fit3 <- modgam(data=beerproj, rgrid=USgrid, family=gaussian, 
		reference="none", m="adjusted", sp=0.05)	
	# Get summary statistics for predicted tweet times across grid points 
	summary(fit3$fit)	
}

# Smoothing survival rates over geolocations with adjustement of Age and Insurance
# Including generating pointwise standard errors and confidence intervals
data(CAdata)
data(CAgrid)
data = CAdata[1:1000,]	# use a subset of the California data
# manually set the categorical variables as factors
data$INS = factor(data$INS)
# no formula needed if data are arranged in a specific order (see \code{data} argument)
fit4 <- modgam(data=data, rgrid=CAgrid, family="survival", sp=0.2)
fit4
# fit the same model using the formula argument, with data columns in any order
fit4.f <- modgam(Surv(time,event)~AGE+factor(INS)+lo(X,Y), data=data,
                rgrid = CAgrid, family="survival", sp=0.2)

# Smoothing for two-dimensional chemical exposure instead of geolocation
# case status in 1st column, mercury and selenium in 2nd and 3rd columns   
ma2 <- MAdata[,c(1,5:6)]  
expgrid <- predgrid(ma2)
fit5 <- modgam(data=ma2,rgrid=expgrid,sp=.5,m="crude") 
summary(fit5$exp.fit)
# plot the results, with mercury on the X axis and selenium on the Y axis
plot(fit5, contours="response", arrow=FALSE, axes=TRUE) 
}
}

\keyword{misc}
\keyword{smooth}
