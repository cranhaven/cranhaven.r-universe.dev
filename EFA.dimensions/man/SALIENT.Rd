\name{SALIENT}
\alias{SALIENT}
\title{Salient loadings criterion for the number of factors}
\description{Salient loadings criterion for determining the number of factors,
	as recommended by Gorsuch. Factors are retained when they consist of a specified minimum 
	number (or more) variables that have a specified minimum (or higher) loading value.}
\usage{SALIENT(data, salvalue = .4, numsals = 3, max_cross = NULL, min_eigval = .7,
               corkind = 'pearson', extraction = 'paf', rotation = 'promax',
               loading_mat = 'structure', ppower = 3, iterpaf = 100, 
               Ncases = NULL, verbose = TRUE, factormodel, rotate)}
\arguments{
 \item{data}{
 \code{}An all-numeric dataframe where the rows are cases & the columns are the variables,
        or a correlation matrix with ones on the diagonal. The function internally determines 
        whether the data are a correlation matrix.}

 \item{salvalue}{
 \code{}(optional) The loading value that is considered salient. Default = .40. This
         can also be a vector of up to three values, e.g., salvalue = c(.4, .5, .6).}

 \item{numsals}{
 \code{}(optional) The required number of salient loadings for a factor. Default = 3.
         This can also be a vector of up to three values, e.g., numsals = c(3, 2, 1).}

 \item{max_cross}{
 \code{}(optional) The maximum value for cross-loadings.}

 \item{min_eigval}{
 \code{}(optional) The minimum eigenvalue for including a factor in the analyses. Default = .7}

 \item{corkind}{
 \code{}(optional) The kind of correlation matrix to be used if data is not a correlation matrix. 
        The options are 'pearson', 'kendall', 'spearman', 'gamma', and 'polychoric'. Required only if
        the entered data is not a correlation matrix.}

 \item{extraction}{
 \code{}(optional) The factor extraction method for the analysis. 
        The options are 'pca', 'paf' (the default), 'ml', 'image', 'minres', 'uls', 'ols', 'wls', 'gls', 
        'alpha', and 'fullinfo'. }

 \item{rotation}{
 \code{}(optional) The factor rotation method for the analysis. The orthogonal rotation options are: 
        'varimax' (the default), 'quartimax', 'bentlerT', 'equamax', 'geominT', 'bifactorT', 
        'entropy', and 'none'. The oblique rotation options are: 'promax' (the default), 'quartimin', 
        'oblimin', 'oblimax', 'simplimax', 'bentlerQ', 'geominQ', 'bifactorQ', and 'none'.}

 \item{iterpaf}{
 \code{}(optional) The maximum number of iterations for paf. Default value = 100}

 \item{loading_mat}{
 \code{}(optional) The kind of factor rotation matrix for an oblique rotation. 
        The options are 'structure' (the default) or 'pattern'.}

 \item{ppower}{ 
 \code{}(optional) The power value to be used in a promax rotation (required only if
        rotation = 'promax'). Default value = 3}

 \item{Ncases}{
 \code{}The number of cases. Required only if data is a correlation matrix.}
                                                          
  \item{verbose}{
  \code{}(optional) Should detailed results be displayed in console? TRUE (default) or FALSE}
  
  \item{factormodel}{ 
  \code{}(Deprecated.) Use 'extraction' instead.}
  
  \item{rotate}{ 
  \code{}(Deprecated.) Use 'rotation' instead.}
}
\details{
	In this procedure for determining the number of factors, factors are retained when 
	each factor has at least a specified minimum number variables (e.g., 3) that have loadings 
	that are greater than or equal to a specified minimum loading value (e.g., .40).
	Factor are considered trivial when they do not contain a sufficient number 
	of salient loadings (Gorsuch, 1997, 2015; Boyd, 2011).
	
	The procedure begins by extracting and rotating (if requested) an excessive
	number of factors. If the initial factor loadings do not meet the specified criteria,
	then the factor analysis is conducted again with one less factor and the
	loadings are again examined to determine whether the factor loadings meet the 
	specified criteria. The procedure stops when a loading matrix meets the criteria,
	in which case the number of columns in the loading matrix is the number
	of factors according to the salient loadings criteria.
	
	The initial, excessive number of factors for the procedure is determined using the
	\bold{min_eigval} argument (for minimum eigenvalue). The default is .70, which can be 
	adjusted (raised) when analyses produce an error caused by there being too few variables.
	
	Although there is no consensus on what constitutes a 'salient' loading, an
	absolute value of .40 is common in the literature.
	
	There are different versions of the salient loadings criterion method, which has not
	been extensively tested to date. The procedure is nevertheless considered promising
	by Gorsuch and others.
	
	Some versions involve the use of multiple salient loading values, each with its own 
	minimum number of variables. This can be done in the SALIENT function by providing a
	vector of values for the \bold{salvalue} argument and a corresponding vector of values for the
	\bold{numsals} argument. The maximum number of possible values is three, and there 
	should be a logical order in the values, i.e., increasing values for salvalue and 
	decreasing values for numsals.
	
	It is also possible to place a restriction of the maximum value of the cross-loadings
	for the salient variables, e.g., requiring that a salient loading is not
	accompanied by cross-loadings on other variables that are greater than .15.
	Use the \bold{max_cross} argument for this purpose, although it may be difficult to claim
	that cross-loadings should be small when the factors are correlated.
}
\value{The number of factors according to the salient loadings criterion.}
\references{
	  {Boyd, K. C. (2011). Factor analysis. In M. Stausberg & S. Engler (Eds.), 
   	       \emph{The Routledge Handbook of Research Methods in the Study of Religion} 
   	       (pp. 204-216). New York: Routledge.} 
\cr\cr{Gorsuch, R. L. (1997). Exploratory factor analysis: Its role in item analysis. 
	       \emph{Journal of Personality Assessment, 68,} 532-560.}
\cr\cr{Gorsuch, R. L. (2015). \emph{Factor analysis}. Routledge/Taylor & Francis Group.}
}
\author{Brian P. O'Connor }
\examples{
\donttest{
# the Harman (1967) correlation matrix
SALIENT(data_Harman, salvalue=.4, numsals=3, Ncases=305)

# Rosenberg Self-Esteem scale items, using Pearson correlations
SALIENT(data_RSE, salvalue=.4, numsals=3, corkind='pearson')

# NEO-PI-R scales
SALIENT(data_NEOPIR, salvalue = c(.4, .5, .6), numsals = c(3, 2, 1), extraction = 'paf', 
        rotation='promax', loading_mat = 'pattern')
}
}




