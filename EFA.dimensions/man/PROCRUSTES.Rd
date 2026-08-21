\name{PROCRUSTES}
\alias{PROCRUSTES}
\title{Procrustes factor rotation}
\description{Conducts Procrustes rotations of a factor loading matrix
	 to a target factor matrix, and it computes the factor solution congruence and 
	 the root mean square residual (based on comparisons of the entered factor loading
	 matrix with the Procrustes-rotated matrix).}
\usage{PROCRUSTES(loadings, target, type, verbose)}
\arguments{
  \item{loadings}{
  \code{}The loading matrix that will be aligned with the target.}

  \item{target}{
  \code{}The target loading matrix.}

  \item{type}{
  \code{}The options are 'orthogonal' or 'oblique' rotation.}
                  
  \item{verbose}{
  \code{}Should detailed results be displayed in console? TRUE (default) or FALSE}
}
\details{This function conducts Procrustes rotations of a factor loading matrix to 
	a target factor matrix, and it computes the factor solution congruence and the root 
	mean square residual (based on comparisons of the entered factor loading 
	matrix with the Procrustes-rotated matrix). The orthogonal Procrustes rotation is based on 
	Schonemann (1966; see also McCrae et al., 1996). The oblique Procrustes rotation is based 
	on Hurley and Cattell (1962). The factor solution congruence is the 
	Tucker-Wrigley-Neuhaus factor solution congruence coefficient 
	(see Guadagnoli & Velicer, 1991; and ten Berge, 1986, for reviews).
}
\value{ A list with the following elements:
 \item{loadingsPROC}{The Procrustes-rotated loadings}
 \item{congruence}{The factor solution congruence after factor Procrustes rotation}
 \item{rmsr}{The root mean square residual}
 \item{residmat}{The residual matrix after factor Procrustes rotation}
}
\references{ 
	  {Guadagnoli, E., & Velicer, W. (1991). A comparison of pattern matching indices. 
	   \emph{Multivariate Behavior Research, 26,} 323-343.}
\cr\cr{Hurley, J. R., & Cattell, R. B. (1962). The Procrustes program: Producing direct rotation 
	to test a hypothesized factor structure. \emph{Behavioral Science, 7,} 258-262.}
\cr\cr{McCrae, R. R., Zonderman, A. B., Costa, P. T. Jr., Bond, M. H., & Paunonen, S. V. (1996). 
	Evaluating replicability of factors in the revised NEO personality inventory: Confirmatory 
	factor analysis versus Procrustes rotation. 
	\emph{Journal of Personality and Social Psychology, 70,} 552-566.}
\cr\cr{Schonemann, P. H. (1966). A generalized solution of the orthogonal Procrustes problem. 
	\emph{Psychometrika, 31,} 1-10.}
\cr\cr{ten Berge, J. M. F. (1986). Some relationships between descriptive comparisons of 
	components from different studies. \emph{Multivariate Behavioral Research, 21,} 29-40.}
}
\author{Brian P. O'Connor}
\examples{
\donttest{
# RSE data
PCAoutput_1 <- PCA(data_RSE[1:150,],   Nfactors = 2, rotation='promax', verbose=FALSE)

PCAoutput_2 <- PCA(data_RSE[151:300,], Nfactors = 2, rotation='promax', verbose=FALSE)

PROCRUSTES(target=PCAoutput_1$pattern, loadings=PCAoutput_2$pattern, 
           type = 'orthogonal', verbose=TRUE)
}
}
