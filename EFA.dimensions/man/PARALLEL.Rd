\name{PARALLEL}
\alias{PARALLEL}
\title{Parallel analysis of eigenvalues (random data only)}
\description{Generates eigenvalues and corresponding percentile values for 
	random data sets with specified numbers of variables and cases. 
}
\usage{
PARALLEL(Nvars=20, Ncases=300, Ndatasets=100, extraction='PCA', percentile='95',
         corkind='pearson', verbose=TRUE, factormodel)
}
\arguments{
  \item{Nvars}{
  \code{}The number of variables.}

  \item{Ncases}{
  \code{}The number of cases.}

  \item{Ndatasets}{
  \code{}An integer indicating the # of random data sets for parallel analyses.}
  	    
  \item{extraction}{ 
  \code{}The factor extraction method. The options are:
  	    'PAF' for principal axis / common factor analysis;
  	    'PCA' for principal components analysis.
  	    'image' for image analysis.}
  	    
  \item{percentile}{
  \code{}An integer indicating the percentile from the distribution of parallel analysis
         random eigenvalues. Suggested value: 95}
         
  \item{corkind}{
  \code{}The kind of correlation matrix to be used for the random data. The options are 'pearson', 'kendall',
         and 'spearman'.}
                  
  \item{verbose}{
  \code{}Should detailed results be displayed in console? TRUE (default) or FALSE}
  
  \item{factormodel}{ 
  \code{}(Deprecated.) Use 'extraction' instead.}
}
\details{
This procedure for determining the number of components or factors involves comparing 
the eigenvalues derived from an actual 
data set to the eigenvalues derived from the random data. In Horn's 
original description of this procedure, the mean eigenvalues from the random data 
served as the comparison baseline, whereas the more common current practice is to 
use the eigenvalues that correspond to the desired percentile (typically the 95th) 
of the distribution of random data eigenvalues.  Factors or components are retained 
as long as the ith eigenvalue from the actual data is greater than the ith eigenvalue 
from the random data. This function produces only random data eigenvalues and it does 
not take real data as input. See the RAWPAR function in this package for parallel 
analyses that also involve real data.
	
The PARALLEL function permits users to specify PCA or PAF or image as the 
factor extraction method. Principal components eigenvalues are often used to determine the number  
of common factors. This is the default in most statistical software packages, and it is the  
primary practice in the literature. It is also the method used by many factor analysis experts, including 
Cattell, who often examined principal components eigenvalues in his scree plots to determine 
the number of common factors. Principal components eigenvalues are based on all of the variance  
in correlation matrices, including both the variance that is shared among variables and the  
variances that are unique to the variables. In contrast, principal axis eigenvalues are based  
solely on the shared variance among the variables. The procedures are qualitatively different.
Some therefore claim that the eigenvalues from one extraction method should not be used to determine the 
number of factors for another extraction method. The PAF option in the extraction argument for the
PARALLEL function was included solely for research purposes. It is best to use PCA as the extraction 
method for regular data analyses.
}
\value{Random data eigenvalues}
\references{
 {Horn, J. L. (1965). A rationale and test for the number of factors in factor analysis. 
 	\emph{Psychometrika, 30,} 179-185.}
\cr\cr {O'Connor, B. P. (2000). SPSS and SAS programs for determining 
  the number of components using parallel analysis and Velicer's 
  MAP test. \emph{Behavior Research Methods, Instrumentation, and
  Computers, 32,} 396-402.}
\cr\cr {Zwick, W. R., & Velicer, W. F. (1986). Comparison of five rules for determining the number of 
	components to retain. \emph{Psychological Bulletin, 99,} 432-442.}
}
\author{Brian P. O'Connor }
\examples{
\donttest{
PARALLEL(Nvars=15, Ncases=250, Ndatasets=100, extraction='PCA', percentile=95,
         corkind='pearson', verbose=TRUE)
}
}
