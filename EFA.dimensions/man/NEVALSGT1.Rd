\name{NEVALSGT1}
\alias{NEVALSGT1}
\title{The number of eigenvalues greater than 1}
\description{Returns the count of the number of eigenvalues greater than 1 in
	         a correlation matrix. This value is often referred to as the "Kaiser", 
	         "Kaiser-Guttman", or "Guttman-Kaiser" rule for determining
	         the number of components or factors in a correlation matrix.
}
\usage{NEVALSGT1(data, corkind, Ncases, verbose=TRUE)}
\arguments{
 \item{data}{
 \code{}An all-numeric dataframe where the rows are cases & the columns are the variables,
        or a correlation matrix with ones on the diagonal. The function internally determines 
        whether the data are a correlation matrix.}

 \item{corkind}{
 \code{}The kind of correlation matrix to be used if data is not a correlation matrix. 
        The options are 'pearson', 'kendall', 'spearman', 'gamma', and 'polychoric'. Required only if
        the entered data is not a correlation matrix.}
                  
 \item{Ncases}{
  \code{}The number of cases. Required only if data is a correlation matrix.}

  \item{verbose}{
  \code{}Should detailed results be displayed in console? TRUE (default) or FALSE}
}
\details{The rationale for this traditional procedure for determining the number of components 
	or factors is that a component with an eigenvalue of 1 accounts for as much
	variance as a single variable. Extracting components with eigenvalues of 1 
	or less than 1 would defeat the usual purpose of component and factor analyses. 
	Furthermore, the reliability of a component will always be nonnegative when its 
	eigenvalue is greater than 1. This rule is the default retention criteria in SPSS and SAS.

	There are a number of problems with this rule of thumb. Monte Carlo investigations have 
	found that its accuracy rate is not acceptably high (Zwick & Velicer, 1986)). The rule was  
	originally intended to be an upper bound for the number of components to be retained, but  
	it is most often used as the criterion to determine the exact number of components or factors. 
	Guttman's original proof applies only to the population correlation matrix and the 
	sampling error that occurs in specific samples results in the rule often overestimating 
	the number of components. The rule is also considered overly mechanical, e.g., a component 
	with an eigenvalue of 1.01 achieves factor status whereas a component with an 
	eigenvalue of .999 does not.
	         
	This function is included in this package for curiosity and research purposes.
}
\value{A list with the following elements:
 \item{NfactorsNEVALSGT1}{The number of eigenvalues greater than 1.}
 \item{totvarexplNOROT}{The eigenvalues and total variance explained}
}
\references{
       {Fabrigar, L. R., Wegener, D. T., MacCallum, R. C., & Strahan, E. J. (1999). Evaluating the use of 
	    exploratory factor analysis in psychological research. \emph{Psychological Methods, 4,} 272-299.}
\cr\cr {Guttman, L. (1954). Some necessary conditions for common factor analysis. \emph{Psychometrika, 19,} 149-161.}
\cr\cr {Hayton, J. C., Allen, D. G., Scarpello, V. (2004). Factor retention decisions in exploratory 
	    factor analysis: A tutorial on parallel analysis. \emph{Organizational Research Methods, 7,} 191-205.}	
\cr\cr {Kaiser, H. F. (1960). The application of electronic computer to factor analysis.
	    \emph{Educational and Psychological Measurement, 20,} 141-151.}
\cr\cr {Zwick, W. R., & Velicer, W. F. (1986). Comparison of five rules for determining the number of 
	    components to retain. \emph{Psychological Bulletin, 99,} 432-442.}
}
\author{Brian P. O'Connor}
\examples{
\donttest{
# the Harman (1967) correlation matrix
NEVALSGT1(data_Harman, corkind='pearson', Ncases = 305, verbose=TRUE)

# Rosenberg Self-Esteem scale items, using Pearson correlations
NEVALSGT1(data_RSE, corkind='pearson', verbose=TRUE)

# Rosenberg Self-Esteem scale items, using polychoric correlations
NEVALSGT1(data_RSE, corkind='polychoric', verbose=TRUE)

# NEO-PI-R scales
NEVALSGT1(data_NEOPIR, corkind='pearson', verbose=TRUE)
}
}
