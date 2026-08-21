\name{SMT}
\alias{SMT}
\title{Sequential chi-square model tests}
\description{A test for the number of common factors using the likelihood 
ratio test statistic values from maximum likelihood factor analysis estimations.
}
\usage{SMT(data, corkind, Ncases=NULL, verbose)}
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
\details{
From Auerswald & Moshagen (2019):

"The fit of common factor models is often assessed with the likelihood ratio test 
statistic (Lawley, 1940) using maximum likelihood estimation (ML), which tests 
whether the model-implied covariance matrix is equal to the population covariance 
matrix. The associated test statistic asymptotically follows a Chi-Square distribution 
if the observed variables follow a multivariate normal distribution and other 
assumptions are met (e.g., Bollen, 1989). This test can be sequentially applied to 
factor models with increasing numbers of factors, starting with a zero-factor model. 
If the Chi-Square test statistic is statistically significant (with e.g., p < .05), 
a model with one additional factor, in this case a unidimensional factor model, is 
estimated and tested. The procedure continues until a nonsignificant result is 
obtained, at which point the number of common factors is identified.

"Simulation studies investigating the performance of sequential Chi-Square model 
tests (SMT) as an extraction criterion have shown conflicting results. Whereas 
some studies have shown that SMT has a tendency to overextraction (e.g., Linn, 1968; 
Ruscio & Roche, 2012; Schonemann & Wang, 1972), others have indicated that the SMT 
has a tendency to underextraction (e.g., Green et al., 2015; Hakstian et al., 1982; 
Humphreys & Montanelli, 1975; Zwick & Velicer, 1986). Hayashi, Bentler, and 
Yuan (2007) demonstrated that overextraction tendencies are due to violations of 
regularity assumptions if the number of factors for the test exceeds the true 
number of factors. For example, if a test of three factors is applied to samples 
from a population with two underlying factors, the likelihood ratio test statistic 
will no longer follow a Chi-Square distribution. Note that the tests are applied 
sequentially, so a three-factor test is only employed if the two-factor test was 
incorrectly significant. Therefore, this violation of regularity assumptions does 
not decrease the accuracy of SMT, but leads to (further) overextractions if a 
previous test was erroneously significant. Additionally, this overextraction 
tendency might be counteracted by the lack of power in simulation studies with 
smaller sample sizes. The performance of SMT has not yet been assessed for 
non-normally distributed data or in comparison to most of the other modern techniques 
presented thus far in a larger simulation design." (p. 475) 
}
\value{A list with the following elements:
 \item{NfactorsSMT}{number of factors according to the SMT}
 \item{pvalues}{eigenvalues, chi-square values, & pvalues}
}
\references{
{Auerswald, M., & Moshagen, M. (2019). How to determine the number of factors to 
retain in exploratory factor analysis: A comparison of extraction methods under 
realistic conditions. \emph{Psychological Methods, 24(4),} 468-491.}
}
\author{Brian P. O'Connor }
\examples{
\donttest{
# the Harman (1967) correlation matrix
SMT(data_Harman, Ncases=305, verbose=TRUE)

# Rosenberg Self-Esteem scale items, using Pearson correlations
SMT(data_RSE, corkind='polychoric', verbose=TRUE)

# NEO-PI-R scales
SMT(data_NEOPIR, verbose=TRUE)
}
}
