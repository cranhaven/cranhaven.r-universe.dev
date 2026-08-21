\name{EFA_SCORES}
\alias{EFA_SCORES}
\title{Exploratory factor analysis scores}
\description{Factor scores, and factor score indeterminacy coefficients, for exploratory factor analysis}
\usage{EFA_SCORES(loadings=NULL, loadings_type='structure', data=NULL, cormat=NULL,  
           corkind='pearson', phi=NULL, method = 'Thurstone', verbose = TRUE)}
\arguments{
  \item{loadings}{
  \code{}The factor loadings. Required for all methods except PCA.}

  \item{loadings_type}{
  \code{}(optional) The kind of factor loadings. The options are 'structure' (the default) or 'pattern'.
  Use 'structure' for orthogonal loadings.}

  \item{data}{
  \code{}(optional) An all-numeric dataframe where the rows are cases & the columns are 
  the variables. Required if factor scores for cases are desired.}

  \item{cormat}{
  \code{}(optional) The item/variable correlation matrix. Not required when "data" is provided.}

  \item{corkind}{
  \code{}(optional) The kind of correlation matrix to be used. The options are 'pearson', 'kendall', 
  'spearman', 'gamma', and 'polychoric'. The kind of correlation should be the same as
  the kind that was used to produce the "loadings".}

  \item{phi}{
  \code{}(optional) The factor correlations.}

  \item{method}{
  \code{}(optional) The method to be used for computing the factor scores (e.g., method = 'Thurstone'). 
  The options are:
  \itemize{
    \item\strong{Thurstone} for Thurstone's' (1935) least squares regression approach 
    (Grice Equation 5), which is conducted on the factor structure loadings   
    \item{\strong{Harman}} for Harman's' (1976) idealized variables (Grice Equation 10), 
    which is conducted on the factor pattern loadings.
    \item{\strong{Bartlett}} for Bartlett's (1937) method (Grice Equation 9), 
    which is conducted on the factor pattern loadings.
    \item{\strong{tenBerge}} for ten Berge et al's (1999) method, which is conducted on the 
    factor pattern loadings and which generates scores with 
    correlations that are identical to those in \strong{phi} (Grice Equation 8) 
    \item{\strong{Anderson}} for Anderson & Rubin (1956; see Gorsuch, 1983, p 265) scores,
    which  is conducted on the factor pattern loadings and which is
	only appropriate for orthogonal factor models 
    \item{\strong{PCA}} for unrotated principal component scores (requires only data or cormat).
   }}
                 
  \item{verbose}{
  \code{}(optional) Should detailed results be displayed in console? TRUE (default) or FALSE}
}
\details{
Before using factor scores, it is important to establish that there is
an acceptable degree of "determinacy" for the computed factor scores
(Grice, 2001; Waller, 2023).

The following descriptions of factor score indeterminacy are either taken
directly from, or adapted from, Grice (2001):

"As early as the 1920s researchers recognized that, even if the
correlations among a set of ability tests could be reduced to a subset
of factors, the scores on these factors would be indeterminate
(Wilson, 1928). In other words, an infinite number of ways for scoring
the individuals on the factors could be derived that would be
consistent with the same factor loadings. Under certain conditions,
for instance, an individual with a high ranking on g (general
intelligence), according to one set of factor scores, could receive a
low ranking on the same common factor according to another set of
factor scores, and the researcher would have no way of deciding which
ranking is "true" based on the results of the factor analysis. As
startling as this possibility seems, it is a fact of the mathematics
of the common factor model.

The indeterminacy problem is not that the factor scores cannot be
directly and appropriately computed; it is that an infinite number of
sets of such scores can be created for the same analysis that will all
be equally consistent with the factor loadings.

The degree of indeterminacy will not be equivalent across studies and
is related to the ratio between the number of items and factors in a
particular design (Meyer, 1973; Schonemann, 1971). It may also be
related to the magnitude of the communalities (Gorsuch, 1983). Small
amounts of indeterminacy are obviously desirable, and the con-
sequences associated with a high degree of indeterminacy are
extremely unsettling. Least palatable is the fact that if the maximum
possible proportion of indeterminacy in the scores for a particular
factor meets or exceeds 50%, it becomes entirely possible to con-
struct two orthogonal or negatively correlated sets of factor scores
that will be equally consistent with the same factor loadings
(Guttman, 1955).

\strong{MULTR & RSQR}
MULTR is the multiple correlation between each factor and the original
variables (Green, 1976; Mulaik, 1976). MULTR ranges from 0 to 1, with
high values being desirable, and indicates the maximum possible degree
of determinacy for factor scores. Some authors have suggested that
MULTR values should be substantially higher than .707 which, when
squared, would equal .50. RSQR is the square or MULTR and represents
the maximum proportion of determinacy.

\strong{MINCOR}

the minimum correlation that could be obtained between two sets of
equally valid factor scores for each factor (Guttman, 1955; Mulaik,
1976; Schonemann, 1971). This index ranges from -1 to +1. High
positive values are desirable. When MINCOR is zero, then two sets of
competing factor scores can be constructed for the same common factor
that are orthogonal or even negatively correlated. MINCOR values
approaching zero are distressing, and negative values are disastrous.
MINCOR values of zero or less occur when MULTR <= .707 (at least 50%
indeterminacy). MULTR values that do not appreciably exceed .71 are
therefore particularly problematic. High values that approach 1.0
indicate that the factors may be slightly indeterminate, but the
infinite sets of factor scores that could be computed will yield
highly similar rankings of the individuals. In other words, the
practical impact of the indeterminacy is minimal. MINCOR is the
"Guttman's Indeterminacy Index" that is provided by the
fsIndeterminacy function in the fungible package.

\strong{VALIDITY}

While the MULTR values represent the maximum correlation between the
factor score estimates and the factors, the VALIDITY coefficients
represent the actual correlations between the factor score estimates
and their respective factors, which may be lower than MULTR. The
VALIDITY coefficients may range from -1 to +1. They should be
interpreted in the same manner as MULTR. Gorsuch (1983, p. 260)
recommended values of at least .80, but much larger values (>.90) may
be necessary if the factor score estimates are to serve as adequate
substitutes for the factors themselves.

\strong{Correlational Accuracy}

If the factor score estimates are adequate representations of the
factors, then the correlations between the factor scores should be
similar to the correlations between the factors."
}
\value{A list with the following elements:
 \item{FactorScores}{The factor scores}
 \item{FSCoef}{The factor score coefficients (W)}
 \item{MULTR}{The multiple correlation between each factor and the original variables}
 \item{RSQR}{The square or MULTR, representing the maximum proportion of determinacy}
 \item{MINCOR}{Guttmans indeterminacy index, the minimum correlation that could be obtained 
 	between two sets of equally valid factor scores for each factor.}
 \item{VALIDITY}{The correlations between the factor score estimates and their respective factors}
 \item{UNIVOCALITY}{The extent to which the estimated factor scores are excessively or insufficiently 
 	correlated with other factors in the same analysis}
 \item{FactorScore_Correls}{The correlations between the factor scores}
 \item{phi}{The correlations between the factors}
 \item{pattern}{The pattern matrix}
 \item{pattern}{The structure matrix}
}
\references{ 
	{Anderson, R. D., & Rubin, H. (1956). Statistical inference in factor analysis. 
		\emph{Proceedings of the Third Berkeley Symposium of Mathematical Statistics and Probability, 5,} 111-150.}
\cr\cr{Bartlett, M. S. (1937). The statistical conception of mental factors.
	  	\emph{British Journal of Psychology, 28,} 97-104.}
\cr\cr{Grice, J.  (2001). Computing and evaluating factor scores.
        \emph{Psychological Methods, 6(4),} 430-450.}
\cr\cr {Harman, H. H.  (1976).  \emph{Modern factor analysis.} University of Chicago press.}
\cr\cr{ten Berge, J. M. F., Krijnen, W. P., Wansbeek, T., and Shapiro, A. (1999).  
	Some new results on correlation-preserving factor scores prediction methods.
	\emph{Linear Algebra and its Applications, 289(1-3),} 311-318.}
\cr\cr{Thurstone, L. L. (1935). \emph{The vectors of mind.} Chicago: University of Chicago Press.}
\cr\cr{Waller, N. G. (2023). Breaking our silence on factor score indeterminacy. 
	\emph{Journal of Educational and Behavioral Statistics, 48(2),} 244-261.}
}
\author{Brian P. O'Connor }
\examples{	
efa_out <- EFA(data=data_RSE, extraction = 'ml', Nfactors=2, rotation='promax')

EFA_SCORES(loadings=efa_out$structure, loadings_type='structure', data=data_RSE,  
           phi=efa_out$phi, method = 'tenBerge') 
           
\donttest{
# PCA scores
EFA_SCORES(data=data_NEOPIR, method = 'PCA') 
}
}
