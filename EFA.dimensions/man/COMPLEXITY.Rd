\name{COMPLEXITY}
\alias{COMPLEXITY}
\title{Factor solution complexity}
\description{Provides Hoffman's (1978) complexity coefficient for each 
	item and (optionally) the percent complexity in the factor solution 
	using the procedure and code provided by Pettersson and Turkheimer (2014).
}
\usage{COMPLEXITY(loadings, percent=TRUE, degree.change=100, averaging.value=100, verbose=TRUE)}  
\arguments{
  \item{loadings}{
  \code{}The factor loading matrix.}

  \item{percent}{
  \code{}(logical) Should the percent complexity be computed? The default = TRUE.}

  \item{degree.change}{
  \code{}If percent=TRUE, the number of incremental changes toward simple structure. The default = 100.}
                  
  \item{averaging.value}{
  \code{}If percent=TRUE, the number of repeats per unit of degree change. The default = 100.}
                  
  \item{verbose}{
  \code{}(logical) Should detailed results be displayed in console? The default = TRUE.}
}
\details{This function provides Hoffman's (1978) complexity coefficient for each item 
	and (optionally) the percent complexity in the factor solution using the procedure 
	and code provided by Pettersson and Turkheimer (2014). For the percent complexity 
	coefficient, values closer to zero indicate greater consistency with simple structure.
}
\value{A list with the following elements:
 \item{comp_rows}{The complexity coefficient for each item}
 \item{percent}{The percent complexity in the factor solution}
}
\references{ 
	   {Hofmann, R. J. (1978). Complexity and simplicity as objective indices descriptive of 
	   factor solutions. \emph{Multivariate Behavioral Research, 13,} 247-250.}
\cr\cr{Pettersson E, Turkheimer E. (2010) Item selection, evaluation, and simple structure in 
	   personality data. \emph{Journal of research in personality, 44(4),} 407-420.}
\cr\cr{Pettersson, E., & Turkheimer, E. (2014). Self-reported personality pathology has complex 
	   structure and imposing simple structure degrades test information. 
	   \emph{Multivariate Behavioral Research, 49(4),} 372-389.}
}
\author{Brian P. O'Connor}
\examples{
\donttest{
# the Harman (1967) correlation matrix
PCAoutput <- PCA(data_Harman, Nfactors = 2, Ncases = 305, rotation='promax', verbose=FALSE)
COMPLEXITY(loadings=PCAoutput$structure, verbose=TRUE)

# Rosenberg Self-Esteem scale items
PCAoutput <- PCA(data_RSE, Nfactors = 2, rotation='promax', verbose=FALSE)
COMPLEXITY(loadings=PCAoutput$structure, verbose=TRUE)

# NEO-PI-R scales
PCAoutput <- PCA(data_NEOPIR, Nfactors = 5, rotation='promax', verbose=FALSE)
COMPLEXITY(loadings=PCAoutput$structure, verbose=TRUE)
}
}
