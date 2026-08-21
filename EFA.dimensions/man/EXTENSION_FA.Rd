\name{EXTENSION_FA}
\alias{EXTENSION_FA}
\title{Extension factor analysis}
\description{Extension factor analysis, which provides correlations between nonfactored items and 
	         the factors that exist in a set of core items. The extension item correlations
	         are then used to decide which factor, if any, a prospective item belongs to.}
\usage{
EXTENSION_FA(data, Ncore, Next, higherorder, roottest,  
             corkind, 
             extraction, rotation, 
             Nfactors, NfactorsHO, 
             Ndatasets, percentile, 
             salvalue, numsals, 
             iterpaf, ppower, 
             verbose, factormodel, rotate)
}
\arguments{
  \item{data}{
  \code{}An all-numeric dataframe where the rows are cases & the columns are the variables.}

  \item{Ncore}{
  \code{}An integer indicating the number of core variables. The function will run the
         factor analysis on the data that appear in column #1 to column #Ncore of the data matrix.}

  \item{Next}{
  \code{}An integer indicting the number of extension variables, if any. The function will run
         extension factor analyses on the remaining columns in data, i.e., using column
         #Ncore+1 to the last column in data. Enter zero if there are no extension variables.}

  \item{higherorder}{
  \code{}Should a higher-order factor analysis be conducted? The options are TRUE or FALSE.}

  \item{roottest}{
  \code{}The method for determining the number of factors. The options are:
         'Nsalient' for number of salient loadings (see salvalue & numsals below);
         'parallel' for parallel analysis (see Ndatasets & percentile below);
         'MAP' for Velicer's minimum average partial test;
         'SEscree' for the standard error scree test;
         'nevals>1' for the number of eigenvalues > 1; and
         'user' for a user-specified number of factors (see Nfactors & NfactorsHO below).}

  \item{corkind}{
  \code{}The kind of correlation matrix to be used. The options are 'pearson', 'kendall',
         'spearman', 'gamma', and 'polychoric'.}

  \item{extraction}{ 
  \code{}The factor extraction method. The options are:
  	    'PAF' for principal axis / common factor analysis;
  	    'PCA' for principal components analysis;
  	    'ML' for maximum likelihood.}

  \item{rotation}{
  \code{}The factor rotation method. The options are: 'promax', 'varimax', and 'none'.}

  \item{Nfactors}{
  \code{}An integer indicating the user-determined number of factors
         (required only if roottest = 'user').}

  \item{NfactorsHO}{
  \code{}An integer indicating the user-determined number of higher order factors
         (required only if roottest = 'user' and higherorder = TRUE).}

  \item{Ndatasets}{
  \code{}An integer indicating the # of random data sets for parallel analyses (required only
         if roottest = 'parallel').}

  \item{percentile}{
  \code{}An integer indicating the percentile from the distribution of parallel analysis
         random eigenvalues to be used in determining the # of factors (required only
         if roottest = 'parallel'). Suggested value: 95}

  \item{salvalue}{
  \code{}The minimum value for a loading to be considered salient (required only if
  	     roottest = 'Nsalient'). Suggested value: .40}

  \item{numsals}{
  \code{}The number of salient loadings required for the existence of a factor
         i.e., the number of loadings > or = to salvalue (see above) for the function
         to identify a factor. Required only if roottest = 'Nsalient'. Gorsuch
         (1995a, p. 545) suggests: 3}

  \item{iterpaf}{
  \code{}The maximum # of iterations for a principal axis / common factor analysis
         (required only if extraction = 'PAF'). Suggested value: 100}

  \item{ppower}{ 
  \code{}The power value to be used in a promax rotation (required only if
         rotation = 'promax'). Suggested value: 3}
                  
  \item{verbose}{
  \code{}Should detailed results be displayed in console? TRUE (default) or FALSE}
  
  \item{factormodel}{ 
  \code{}(Deprecated.) Use 'extraction' instead.}
  
  \item{rotate}{ 
  \code{}(Deprecated.) Use 'rotation' instead.}
}
\details{
Traditional scale development statistics can produce results that are baffling or misunderstood by
many users, which can lead to inappropriate substantive interpretations and item selection decisions. 
High internal consistencies do not indicate unidimensionality; item-total correlations are inflated 
because each item is correlated with its own error as well as the common variance among items; and 
the default number-of-eigenvalues-greater-than-one rule, followed by principal components analysis 
and varimax rotation, produces inflated loadings and the possible appearance of numerous uncorrelated 
factors for items that measure the same construct (Gorsuch, 1997a, 1997b). Concerned investigators 
may then neglect the higher order general factor in their data as they use misleading statistical 
output to trim items and fashion unidimensional scales.

These problems can be circumvented in exploratory factor analysis by using more appropriate factor 
analytic procedures and by using extension analysis as the basis for adding items to scales. 
Extension analysis provides correlations between nonfactored items and the factors that exist in 
a set of core items. The extension item correlations are then used to decide which factor, if any, 
a prospective item belongs to. The decisions are unbiased because factors are defined without being 
influenced by the extension items. One can also examine correlations between extension items and 
any higher order factor(s) in the core items. The end result is a comprehensive, undisturbed, and 
informative picture of the correlational structure that exists in a set of core items and of the 
potential contribution and location of additional items to the structure.

Extension analysis is rarely used, at least partly because of limited software availability. 
Furthermore, when it is used, both traditional extension analysis and its variants (e.g., correlations 
between estimated factor scores and extension items) are prone to the same problems as the procedures 
mentioned above (Gorsuch, 1997a, 1997b). However, Gorusch (1997b) described how diagonal 
component analysis can be used to bypass the problems and uncover the noninflated and unbiased 
extension variable correlations -- all without computing factor scores.
}
\value{A list with the following elements:
\item{fits1}{eigenvalues & fit coefficients for the first set of core variables}
\item{rff}{factor intercorrelations}
\item{corelding}{core variable loadings on the factors}
\item{extcorrel}{extension variable correlations with the factors}
\item{fits2}{eigenvalues & fit coefficients for the higher order factor analysis}
\item{rfflding}{factor intercorrelations from the first factor analysis and
	  the loadings on the higher order factor(s)}
\item{ldingsef}{variable loadings on the lower order factors and
	  their correlations with the higher order factor(s)}
\item{extsef}{extension variable correlations with the lower order factor(s) and 
	  their correlations with the higher order factor(s)}
}
\references{
       {Dwyer, P. S. (1937) The determination of the factor loadings of a given test 
	from the known factor loadings of other tests. \emph{Psychometrika, 3,} 173-178.}
\cr\cr {Gorsuch, R. L. (1997a). Exploratory factor analysis: Its role in item analysis. 
       	\emph{Journal of Personality Assessment, 68,} 532-560.}
\cr\cr {Gorsuch, R. L. (1997b). New procedure for extension analysis in exploratory factor 
	analysis. \emph{Educational and Psychological Measurement, 57,} 725-740.}
\cr\cr {Horn, J. L. (1973) On extension analysis and its relation to correlations between 
	variables and factor scores. \emph{Multivariate Behavioral Research, 8(4),} 477-489.}
\cr\cr {O'Connor, B. P. (2001). EXTENSION: SAS, SPSS, and MATLAB programs for extension 
	analysis. \emph{Applied Psychological Measurement, 25,} p. 88.}
}
\author{Brian P. O'Connor }
\examples{
\donttest{
EXTENSION_FA(data_RSE, Ncore=7, Next=3, higherorder=TRUE, 
             roottest='MAP',  
             corkind='pearson', 
             extraction='PCA', rotation='promax', 
             Nfactors=2, NfactorsHO=1, 
             Ndatasets=100, percentile=95, 
             salvalue=.40, numsals=3, 
             iterpaf=200, 
             ppower=4, 
             verbose=TRUE)

EXTENSION_FA(data_NEOPIR, Ncore=12, Next=6, higherorder=TRUE, 
             roottest='MAP',  
             corkind='pearson', 
             extraction='PCA', rotation='promax', 
             Nfactors=4, NfactorsHO=1, 
             Ndatasets=100, percentile=95, 
             salvalue=.40, numsals=3, 
             iterpaf=200, 
             ppower=4, 
             verbose=TRUE)             
}
}
