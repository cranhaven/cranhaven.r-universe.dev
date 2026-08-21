\name{MAP}
\alias{MAP}
\title{Velicer's minimum average partial (MAP) test}
\description{Velicer's minimum average partial (MAP) test for determining the number 
	of components, which focuses on the common variance in a correlation matrix.
}
\usage{MAP(data, corkind, Ncases, verbose)}
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
\details{This method for determining the number 
	of components focuses on the common variance in a correlation matrix. It involves 
	a complete principal components analysis followed by the examination of a series 
	of matrices of partial correlations. Specifically, on the first step, the first 
	principal component is partialled out of the correlations between the variables 
	of interest, and the average squared coefficient in the off-diagonals of the 
	resulting partial correlation matrix is computed. On the second step, the first 
	two principal components are partialled out of the original correlation matrix 
	and the average squared partial correlation is again computed. These computations 
	are conducted for k (the number of variables) minus one steps. The average squared 
	partial correlations from these steps are then lined up, and the number of components 
	is determined by the step number in the analyses that resulted in the lowest average 
	squared partial correlation. The average squared coefficient in the original 
	correlation matrix is also computed, and if this coefficient happens to be lower 
	than the lowest average squared partial correlation, then no components should be 
	extractioned from the correlation matrix. Statistically, components are retained as 
	long as the variance in the correlation matrix represents systematic variance. 
	Components are no longer retained when there is proportionately more unsystematic 
	variance than systematic variance (see O'Connor, 2000, p. 397).
}
\value{A list with the following elements:
 \item{totvarexplNOROT}{The eigenvalues and total variance explained}
 \item{avgsqrs}{Velicers average squared correlations}
 \item{NfactorsMAP}{The number of components according to the original (1976) MAP test}
 \item{NfactorsMAP4}{The number of components according to the revised (2000) MAP test}
}
\references{
 {O'Connor, B. P. (2000). SPSS and SAS programs for determining 
 the number of components using parallel analysis and Velicer's 
 MAP test. \emph{Behavior Research Methods, Instrumentation, and
 Computers, 32,} 396-402.}
\cr\cr {Velicer, W. F. (1976). Determining the number of components from the matrix of partial 
 	correlations. \emph{Psychometrika, 41,} 321-327.}
\cr\cr {Velicer, W. F., Eaton, C. A., and Fava, J. L. (2000). Construct explication 
	through factor or component analysis: A review and 
 evaluation of alternative procedures for determining the number of factors or 
 components. In R. D. Goffin & E. Helmes, eds., \emph{Problems and solutions in human 
 assessment (p.p. 41-71).} Boston: Kluwer.}
}
\author{Brian P. O'Connor }
\examples{
\donttest{
# the Harman (1967) correlation matrix
MAP(data_Harman, corkind='pearson', Ncases = 305, verbose=TRUE)

# Rosenberg Self-Esteem scale items, using Pearson correlations
MAP(data_RSE, corkind='pearson', verbose=TRUE)

# Rosenberg Self-Esteem scale items, using polychoric correlations
MAP(data_RSE, corkind='polychoric', verbose=TRUE)

# NEO-PI-R scales
MAP(data_NEOPIR, verbose=TRUE)
}
}
