\name{RAWPAR}
\alias{RAWPAR}
\title{Parallel analysis of eigenvalues (for raw data)}
\description{Parallel analysis of eigenvalues, with real data as input,
	for deciding on the number of components or factors.
}
\usage{
RAWPAR(data, randtype, extraction, Ndatasets, percentile, 
       corkind, corkindRAND, Ncases=NULL, eval_plot=TRUE, verbose, factormodel)
}
\arguments{
  \item{data}{
  \code{}An all-numeric dataframe where the rows are cases & the columns are the variables, 
  or a correlation matrix with ones on the diagonal. The function internally determines whether the data are a correlation matrix.}

  \item{randtype}{
  \code{}The kind of random data to be used in the parallel analysis:
   	    'generated' for random normal data generation;
  	    'permuted' for permutations of the raw data matrix.}
  	    
  \item{extraction}{ 
  \code{}The factor extraction method. The options are:
  	    'PAF' for principal axis / common factor analysis;
  	    'PCA' for principal components analysis.
  	    'image' for image analysis.}
  	    
  \item{Ndatasets}{
  \code{}An integer indicating the # of random data sets for parallel analyses.}

  \item{percentile}{
  \code{}An integer indicating the percentile from the distribution of parallel analysis
         random eigenvalues to be used in determining the # of factors. Suggested value: 95}
         
  \item{corkind}{
  \code{}The kind of correlation matrix to be used if data is not a correlation matrix. The options are 'pearson', 'kendall',
         'spearman', 'gamma', and 'polychoric'. Required only if the entered data is not a correlation matrix.}

  \item{corkindRAND}{
  \code{}The kind of correlation matrix to be used for the random data analyses. The options are 'pearson', 'kendall',
         'spearman', 'gamma', and 'polychoric'. The default is 'pearson'.}

  \item{Ncases}{
  \code{}The number of cases upon which a correlation matrix is based. Required only if data is a correlation matrix.}

  \item{eval_plot}{
  \code{}Should a plot of the real and parallel analysis eigenvalues be produced?
         The options are TRUE (the default) or FALSE.}

  \item{verbose}{
  \code{}Should detailed results be displayed in console? TRUE (default) or FALSE}
  
  \item{factormodel}{ 
  \code{}(Deprecated.) Use 'extraction' instead.}
}
\details{
	The parallel analysis procedure for deciding on the number of 
	components or factors involves extracting eigenvalues from random data 
	sets that parallel the actual data set with regard to the number of cases 
	and variables. For example, if the original data set consists of 305 
	observations for each of 8 variables, then a series of random data matrices 
	of this size (305 by 8) would be generated, and eigenvalues would be computed 
	for the correlation matrices for the original, real data and for each of the 
	random data sets. The eigenvalues derived from the actual data are then 
	compared to the eigenvalues derived from the random data. In Horn's original 
	description of this procedure, the mean eigenvalues from the random data served 
	as the comparison baseline, whereas the more common current practice is to use 
	the eigenvalues that correspond to the desired percentile (typically the 95th) 
	of the distribution of random data eigenvalues. Factors or components are 
	retained as long as the ith eigenvalue from the actual data is greater than 
	the ith eigenvalue from the random data.
	
	The RAWPAR function permits users to specify PCA or PAF or image as the  factor 
	extraction method. Principal components eigenvalues are often used to determine the number
	of common factors. This is the default in most statistical software packages, and it is the
	primary practice in the literature. It is also the method used by many factor analysis experts, 
	including  Cattell, who often examined principal components eigenvalues in his scree plots to 
	determine  the number of common factors. Principal components eigenvalues are based on all of 
	the variance   in correlation matrices, including both the variance that is shared among 
	variables and the   variances that are unique to the variables. In contrast, principal axis 
	eigenvalues are based  solely on the shared variance among the variables. The procedures are 
	qualitatively different. Some therefore claim that the eigenvalues from one extraction method 
	should not be used to determine the  number of factors for another extraction method. 
	The PAF option in the extraction argument for the  RAWPAR and PARALLEL function was included solely for 
	research purposes. It is best to use PCA as the extraction  method for regular data analyses. 
	
	Polychoric correlations are time-consuming to compute. While polychoric correlations should 
	probably be specified for the real data eigenvalues when data consists of  item-level responses, 
	polychoric correlations probably should not be specified for the random data computations, 
	even for item-level data. The procedure would take  much time and it is unnecessary. Polychoric 
	correlations are estimates of what the Pearson correlations would be had the real data been 
	continuous. For item-level data, specify polychoric correlations for the real data 
	eigenvalues (corkind='polychoric') and use the default for the random data 
	eigenvalues (corkindRAND='pearson'). The option for using polychoric correlations for the random 
	data computations (corkindRAND='polychoric') was provided solely for research purposes.	
}
\value{ A list with:
  \item{eigenvalues}{the eigenvalues for the real and random data}
  \item{NfactorsPA}{the number of factors based on the parallel analysis}
}
\references{
 {Horn, J. L. (1965). A rationale and test for the number of factors in factor 
 	analysis. \emph{Psychometrika, 30,} 179-185.}
\cr\cr {O'Connor, B. P. (2000). SPSS and SAS programs for determining 
  the number of components using parallel analysis and Velicer's 
  MAP test. \emph{Behavior Research Methods, Instrumentation, and
  Computers, 32,} 396-402.}
\cr\cr {Zwick, W. R., & Velicer, W. F. (1986). Comparison of five rules for determining  
	the number of components to retain. \emph{Psychological Bulletin, 99,} 432-442.}
}
\author{Brian P. O'Connor }
\examples{
\donttest{
# WISC data
RAWPAR(data_TabFid, randtype='generated', extraction='PCA', Ndatasets=100,
       percentile=95, corkind='pearson', verbose=TRUE)

# the Harman (1967) correlation matrix
RAWPAR(data_Harman, randtype='generated', extraction='PCA', Ndatasets=100, 
       percentile=95, corkind='pearson', Ncases=305, verbose=TRUE)

# Rosenberg Self-Esteem scale items, using Pearson correlations
RAWPAR(data_RSE, randtype='permuted', extraction='PCA', Ndatasets=100,
       percentile=95, corkind='pearson', corkindRAND='pearson', verbose=TRUE)

# Rosenberg Self-Esteem scale items, using polychoric correlations
RAWPAR(data_RSE, randtype='generated', extraction='PCA', Ndatasets=100,
       percentile=95, corkind='polychoric', verbose=TRUE)

# NEO-PI-R scales
RAWPAR(data_NEOPIR, randtype='generated', extraction='PCA', Ndatasets=100, 
       percentile=95, corkind='pearson', Ncases=305, verbose=TRUE)
}
}
