\name{PCA}
\alias{PCA}
\title{Principal components analysis}
\description{Principal components analysis}
\usage{PCA(data, Nfactors=NULL, rotation='promax', corkind='pearson', 
                 Ncases=NULL, ppower = 3, delta = .01, 
                 verbose=TRUE, rotate)}
\arguments{
  \item{data}{
  \code{}An all-numeric dataframe where the rows are cases & the columns are 
  the variables, or a correlation matrix with ones on the diagonal.The function 
  internally determines whether the data are a correlation matrix.}

  \item{Nfactors}{
  \code{}The number of components to extraction. If not specified, then the EMPKC procedure will be used
  to determine the number of components.}

 \item{rotation}{
  \code{}The factor rotation method for the analysis. The orthogonal rotation options are: 
  'varimax' (the default), 'quartimax', 'bentlerT', 'equamax', 'geominT',  
  'entropy', and 'none'. The oblique rotation options are: 'promax' (the default), 'quartimin', 
  'oblimin', 'oblimax', 'simplimax', 'bentlerQ', 'geominQ', and 'none'.}

  \item{corkind}{
  \code{}The kind of correlation matrix to be used if data is not a correlation matrix. 
  The options are 'pearson', 'kendall', 'spearman', 'gamma', and 'polychoric'. Required 
  only if the entered data is not a correlation matrix.}

  \item{Ncases}{
  \code{}The number of cases. Required only if data is a correlation matrix.}

  \item{ppower}{ 
  \code{}The power value to be used in a promax rotation (required only if
         rotation = 'promax'). Suggested value: 3}

  \item{delta}{ 
  \code{} When rotation = 'geominQ' or 'geominT', or when bifactor_kind = 
  'bigeominT' or 'bigeominQ', delta is a tuning parameter 
  for the Geomin criterion. It acts as a small constant that is added to prevent 
  mathematical problems when a factor loading is exactly zero. 
  Delta is sometimes referred to as 'epsilon'.}
                  
  \item{verbose}{
  \code{}Should detailed results be displayed in console? TRUE (default) or FALSE}
  
  \item{rotate}{ 
  \code{}(Deprecated.) Use 'rotation' instead.}
}
\details{The factor rotation computations for the following methods are conducted
	using the GPArotation package (Bernaards & Jennrich, 2005, 2026): 'bentlerQ', 
	'bentlerT', 'entropy', 'geominQ', 'geominT', 'oblimax', 'oblimin', 'quartimax', 
	'quartimin', and 'simplimax'.
	
  For factor rotation (see Jennrich, 2018, for a review):

	\itemize{
		
    \item\strong{bentlerQ} is an oblique rotation based on Bentler's invariant 
    pattern simplicity criterion. 
    
    \item\strong{bentlerT} is an orthogonal rotation based on Bentler's 
    invariant pattern simplicity criterion

    \item\strong{bifactorQ} is an oblique bifactor rotation designed for when 
    a strong, overarching global dimension is expected alongside separate 
    sub-domains that are allowed to correlate with each other.

    \item\strong{bifactorT} is an orthogonal bifactor rotation 
    designed for situations where a single, overarching global dimension is expected
    alongside separate sub-domains, and when all factors should be 
    uncorrelated.
        
    \item\strong{bigeominQ} is an oblique bifactor rotation designed for 
    where there is a broad, overarching factor 
    alongside sub-domains that are allowed to overlap. 
    
    \item\strong{bigeominT} an orthogonal bifactor rotation method designed for 
    exploratory bifactor analysis that tries to ensure that 
    once the general factor's variance is pulled out, each item loads 
    onto only one specific group factor. 
        
    \item\strong{entropy} entropy is an orthogonal factor rotation 
    that pushes factor loadings to be either 
    strongly dominant (close to 1.0) or cleanly absent (close to 0.0), 
    reducing the overall informational "noise" of the matrix.
        
    \item\strong{equamax} is an orthogonal rotation designed as a 
    mathematical compromise between varimax and quartimax. 
        
    \item\strong{geominQ} is an oblique factor rotation method that 
    find a clean "simple structure" 
    even when the data contains highly complex variables (variables that 
    naturally load on multiple factors).
    
    \item\strong{geominT} is an orthogonal factor rotation that 
    combines the strict geometric constraint of uncorrelated factors with 
    Brownes flexible, product-based geomin complexity criterion. 
        
    \item\strong{oblimax} is an oblique factor rotation method that maximizes 
    the number of very high and very low (near-zero) factor loadings. 
        
    \item\strong{oblimin} is an oblique factor rotation that allows 
    factors to correlate freely to achieve the simplest possible loading structure. 
        
    \item\strong{promax} is a two-stage oblique factor rotation method. 
        
    \item\strong{quartimax} is an orthogonal factor rotation 
    designed to spread the explained variance evenly across the factors, 
    actively preventing a single general factor from dominating. 
        
    \item\strong{simplimax} is an oblique factor rotation that attempts to 
    recover complex or messy "simple structures" 
    where traditional continuous methods like oblimin or geomin fail.
        
    \item\strong{quartimin} is an oblique factor rotation that focuses 
    on simplifying the rows of the loading matrix.
        
    \item\strong{varimax} is an orthogonal 
    factor rotation that maximizes the variance of the squared factor loadings within 
    each individual factor column. \cr
  	}

Run one of the following commands for more detailed descriptions of the 
above rotation methods:

\itemize{

\item{RShowDoc("EFA_BIFACTOR_vignettes", package = "EFA.dimensions")}

\item{vignette("EFA_BIFACTOR_vignettes")}
}
}
\value{A list with the following elements:
 \item{loadingsNOROT}{The unrotated factor loadings}
 \item{loadingsROT}{The rotated factor loadings}
 \item{pattern}{The pattern matrix}
 \item{structure}{The structure matrix}
 \item{phi}{The correlations between the factors}
 \item{varexplNOROT1}{The initial eigenvalues and total variance explained}
 \item{varexplROT}{The rotation sums of squared loadings and total variance explained for the rotated loadings}
 \item{cormat_reprod}{The reproduced correlation matrix, based on the rotated loadings}
 \item{fit_coeffs}{Model fit coefficients}
 \item{communalities}{The unrotated factor solution communalities}
 \item{uniquenesses}{The unrotated factor solution uniquenesses}
}
\author{Brian P. O'Connor }
\examples{
\donttest{
# the Harman (1967) correlation matrix
PCA(data_Harman, Nfactors=2, Ncases=305, rotation='oblimin')

# Rosenberg Self-Esteem scale items
PCA(data_RSE, corkind='polychoric', Nfactors=2)

# NEO-PI-R scales
PCA(data_NEOPIR, Nfactors=5, rotation='promax')
}
}
