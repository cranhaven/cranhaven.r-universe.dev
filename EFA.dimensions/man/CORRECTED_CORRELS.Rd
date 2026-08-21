\name{CORRECTED_CORRELS}
\alias{CORRECTED_CORRELS}
\title{Corrected Pearson correlation coefficients}
\description{Corrects Pearson correlation coefficients for attenuation due to 
             unreliability and tests for overall differences between the
             original and corrected correlation matrices.}
\usage{CORRECTED_CORRELS(cormat, Ncases = 100, alphas, 
                  systematic=FALSE, Nperms = 1000, verbose=TRUE)
}  
\arguments{
  \item{cormat}{
  \code{}A square correlation matrix.}

  \item{Ncases}{ 
  \code{}The number of cases upon which cormat is based.
  \cr Example: Ncases = 250}
                  
  \item{alphas}{ 
  \code{}A vector of reliability coefficients (e.g., Cronbach alpha values) for the 
  variables in cormat, in the same order as the variables in cormat.
  \cr Example: alphas <- c(.81, .95, .64, .49, .36)}
                  
  \item{systematic}{ 
  \code{}The kind of permutations for the tests for significance differences
  between the uncorrected and corrected correlation matrices. The options are
  TRUE (for all possible permutations), or FALSE (the default), in which case
  random matrix permutations will be conducted.}
                  
  \item{Nperms}{ 
  \code{}(optional) The number of random matrix permutation for when systematic = FALSE.
  \cr Example: Nperms = 1000}
                  
  \item{verbose}{
  \code{}(optional) Should detailed results be displayed in console? TRUE (default) or FALSE}
}
\details{
This function uses the simple, traditional formula for correcting correlations for 
attentuation due to unreliability, as described in Schmidt and Hunter (1996),
Schmitt (1996), and Trafimow (2016).

Spearman's rho and Kendall's tau correlations between the values in the entered and 
corrected correlation matrices are provided when there are three or more variables.
These are rank correlations that do not assume that similarity increases are linear 
and which are more robust to outliers than are Pearson correlations. The 
corresponding permutation significance tests of overall equality between the entered 
and corrected correlation matrices are "Mantel" tests.

The Steiger (1980) and Jennrich (1970) tests of the overall equality of the entered 
and corrected correlation matrices are conducted using functions from the
psych package (Revelle, 2023).

See the MatrixCorrelation package for additional tests of differences between 
correlation matrices.
}
\value{A list with the following elements:
 \item{cormat}{The entered correlation matrix}
 \item{alphas}{The entered alphas}
 \item{corxd}{The matrix of corrected correlations}
 \item{resids}{The differences between the entered and corrected correlations}
 \item{data_in_rows}{The input data and results in row format}
 \item{cor_spearman}{The Spearman's rho correlation between the entered and 
 corrected correlations}
 \item{cor_kendall}{The Kendall's tau correlation between the entered and corrected 
 correlations}
 \item{Steiger_test}{Results of the Steiger test for equality of the entered and 
 corrected correlation matrices}
 \item{Jennrich_test}{Results of the Jennrich test for equality of the entered and 
 corrected correlation matrices}
}
\references{ 
   {Jennrich, R. I. (1970) An asymptotic test for the equality of two correlation  
   matrices. \emph{Journal of the American Statistical Association, 65,} 904-912.}
\cr\cr{Revelle, W. (2023). psych: Procedures for Psychological, Psychometric, and Personality Research. 
	 R package version 2.3.6, https://CRAN.R-project.org/package=psych}
\cr\cr{Schmidt, F. L., & Hunter, J. E. (1996). Measurement error in psychological 
   research: Lessons from 26 research scenarios. 
   \emph{Psychological Methods, 1(2),} 199223.}
\cr\cr{Schmitt, N. (1996). Uses and abuses of coefficient alpha. 
   \emph{Psychological Assessment, 8(4),} 350353.}
\cr\cr{Steiger, J. H. (1980). Testing pattern hypotheses on correlation matrices: 
   Alternative statistics and some empirical results. 
   \emph{Multivariate Behavioral Research, 15,} 335-352.}
\cr\cr{Trafimow, D. (2016). The attenuation of correlation coefficients: A statistical 
   literacy issue. \emph{Teaching Statistics, 38(1),} 2528.}
}
\author{Brian P. O'Connor }
\examples{
# data from Schmitt (1996)
cormat <- '
 1.0
 .70  1.0
 .51  .47  1.0  
 .52  .55  .45  1.0
 .32  .35  .31  .28  1.0'
cormat_noms <- c('v1', 'v2', 'v3', 'v4', 'v5')
cormat <- data.matrix( read.table(text=cormat, fill=TRUE, col.names=cormat_noms, 
                                  row.names=cormat_noms ))
cormat[upper.tri(cormat)] <- t(cormat)[upper.tri(cormat)]

alphas <- c(.81, .95, .64, .49, .36)

CORRECTED_CORRELS(cormat=cormat, alphas=alphas, 
                  Nperms = 10000, systematic=FALSE, verbose=TRUE)
}
