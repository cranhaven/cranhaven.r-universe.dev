\name{LOCALDEP}
\alias{LOCALDEP}
\title{Local dependence}
\description{Provides the residual correlations after partialling latent trait scores 
	out of an inter-item correlation matrix, along with local dependence statistics.}
\usage{LOCALDEP(data, corkind, item_type, thetas, theta_type, verbose)}
\arguments{
  \item{data}{
  \code{}An all-numeric dataframe where the rows are cases & the columns are the variables.}

  \item{corkind}{
  \code{}The kind of correlation matrix to be used for the analyses. 
  The options are 'pearson', 'kendall', 'spearman', 'gamma', and 'polychoric'. Required only if 
  the entered data is not a correlation matrix.}
                  
 \item{item_type}{
  \code{}(optional) The type of items for the IRT analyses. If item_type is not specified, then
   it is assumed that the items follow a graded or 2PL model. The options for item_type are
   those that can be used in the mirt function from the mirt package, which include 
   'Rasch', '2PL', '3PL', '3PLu', '4PL', 'graded', 'grsm', 'grsmIRT', 'gpcm', 'gpcmIRT',
   'rsm', 'nominal', 'ideal', item_type 'ggum', among other possibilities.}

 \item{thetas}{
  \code{}(optional) A vector of the latent trait scores that will be partialled out of the item
  correlations and used in computing other local dependence statistics. If thetas are not
  supplied, then they will be estimated internally using the fscores function from the mirt package.}

 \item{theta_type}{
  \code{}(optional) If thetas are not supplied, then they will be estimated internally using the 
  fscores function from the mirt package with the following options: "EAP" (default), "MAP", "ML", 
  "WLE", "EAPsum", "plausible", and "classify".}

  \item{verbose}{
  \code{}Should detailed results be displayed in console? TRUE (default) or FALSE}
}
\details{Item response theory models are based on the assumption that the items 
	display local independence. The latent trait is presumed to be responsible for 
	the associations between the items. Once the latent trait is partialled out, 
	the residual correlations between pairs of items should be negligible. Local 
	dependence exists when there is additional systematic covariance among the items. 
	It can occur when pairs of items have highly similar content or between sequentially 
	presented items in a test. Local dependence distorts IRT parameter estimates, 
	it can artificially increase scale information, and it distorts the latent trait, 
	which becomes too heavily defined by the locally dependent items. Examining the 
	residual (partial) correlations is a preliminary, exploratory method of determining whether 
	local dependence exists. The function also displays the local dependence Q3 statistic  
	values described by Yen (1984), the X2 statistic values described by Chen and Thissen
	(1997), the G2 statistic values described by Chen and Thissen (1997), and the 
	jack-knife statistic values described by Edwards et al. (2018). The Q3, X2, G2,
	and jack-knife statistic values are obtained using the mirt function from the 
	mirt package (Chalmers, 2012).}
\value{ A list with the following elements:
 \item{correlations}{The correlation matrix}
 \item{residcor}{The residualized (partial) correlation matrix}
 \item{eigenvalues}{The eigenvalues}
 \item{resid_Q3}{A matrix with the Q3 statistic values described by Yen (1984)}
 \item{resid_LD}{A matrix with the X2 statistic values described by Chen and Thissen (1997)}
 \item{resid_LDG2}{A matrix with the G2 statistic values described by Chen and Thissen (1997)}
 \item{resid_JSI}{A matrix with the jack-knife statistic values described by Edwards et al. (2018)}
 \item{localdep_stats}{All of the above local dependence statistics in long format}
 }
\references{
	   {Chalmers, R., P. (2012). mirt: A multidimensional item response theory
	   	package for the R environment. \emph{Journal of Statistical Software, 48(6)}, 1-29.}
\cr\cr {Chen, W. H. & Thissen, D. (1997). Local dependence indices for item pairs using item
	response theory. \emph{Journal of Educational and Behavioral Statistics, 22}, 265-289.}
\cr\cr {Edwards, M. C., Houts, C. R. & Cai, L. (2018). A diagnostic procedure to detect departures
	from local independence in item response theory models. \emph{Psychological Methods, 23}, 138-149.}
\cr\cr {Yen, W. (1984). Effects of local item dependence on the fit and equating performance of the three
	parameter logistic model. \emph{Applied Psychological Measurement, 8}, 125-145.}
}
\author{Brian P. O'Connor}
\examples{
\donttest{
# Rosenberg Self-Esteem scale items
LOCALDEP(data_RSE)
}
}

