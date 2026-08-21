\name{DIMTESTS}
\alias{DIMTESTS}
\title{Tests for the number of factors}
\description{Conducts multiple tests for the number of factors
}
\usage{DIMTESTS(data, tests, corkind, Ncases, HULL_method, HULL_gof, HULL_cor_method,
          CD_cor_method, display=2)}
\arguments{
  \item{data}{
  \code{}An all-numeric dataframe where the rows are cases & the columns are the variables, 
         or a correlation matrix with ones on the diagonal. The function internally determines 
         whether the data are a correlation matrix.}

  \item{tests}{
  \code{}A vector of the names of the tests for the number of factors that should
         be conducted. The possibilities are CD, EMPKC, HULL, MAP, NEVALSGT1, 
         RAWPAR, SALIENT, SESCREE, SMT. If tests is not specified, then 
         tests = c('EMPKC', 'HULL', 'RAWPAR') is used as the default.}
         
  \item{corkind}{
  \code{}The kind of correlation matrix to be used if data is not a correlation matrix. 
         The options are 'pearson', 'kendall', 'spearman', 'gamma', and 'polychoric'. Required only if 
         the entered data is not a correlation matrix.}
                  
  \item{Ncases}{
  \code{}The number of cases. Required only if data is a correlation matrix.}

  \item{HULL_method}{
  \code{}From EFAtools: The estimation method to use. One of "PAF" (default), "ULS", or "ML", 
         for principal axis factoring, unweighted least squares, and maximum likelihood}

  \item{HULL_gof}{
  \code{}From EFAtools: The goodness of fit index to use. Either "CAF" (default), "CFI", or "RMSEA", 
         or any combination of them. If method = "PAF" is used, only the CAF can be used as 
         goodness of fit index. For details on the CAF, see Lorenzo-Seva, Timmerman, and Kiers (2011).}

  \item{HULL_cor_method}{
  \code{}From EFAtools: The kind of correlation matrix to be used for the Hull method analyses. 
         The options are 'pearson', 'kendall', and 'spearman'}

  \item{CD_cor_method}{
  \code{}From EFAtools: The kind of correlation matrix to be used for the CD method analyses. 
         The options are 'pearson', 'kendall', and 'spearman'}

  \item{display}{
  \code{}The results to be displayed in the console: 0 = nothing; 1 = only the # of 
         factors for each test; 2 (default) = detailed output for each test}
}
\details{This is a convenience function for running possibly multiple tests 
for the number of factors.
	
Run one the following commands for descriptions of the various tests:

\itemize{

\item{RShowDoc("Number_of_factors_tests_vignettes", package = "EFA.dimensions")}

\item{vignette("Number_of_factors_tests_vignettes")}
}
}
\value{A list with the following elements:
 \item{dimtests}{A matrix with the DIMTESTS results}
 
 \item{NfactorsDIMTESTS}{The number of factors according to the first test method 
 	                     specified in the "tests" vector}
}
\references{
{Auerswald, M., & Moshagen, M. (2019). How to determine the number of factors to 
	retain in exploratory factor analysis: A comparison of extraction methods under 
	realistic conditions. \emph{Psychological Methods, 24(4),} 468-491.}
\cr\cr {Lorenzo-Seva, U., Timmerman, M. E., & Kiers, H. A. (2011). The Hull method 
	for selecting the number of common factors. \emph{Multivariate Behavioral 
	Research, 46(2),} 340-364.}
\cr\cr {O'Connor, B. P. (2000). SPSS and SAS programs for determining 
  the number of components using parallel analysis and Velicer's 
  MAP test. \emph{Behavior Research Methods, Instrumentation, and
  Computers, 32,} 396-402.}
\cr\cr {Ruscio, J., & Roche, B. (2012). Determining the number of factors to retain 
	in an exploratory factor analysis using comparison data of known factorial 
	structure. \emph{Psychological Assessment, 24,} 282292. doi: 10.1037/a0025697}
\cr\cr {Zwick, W. R., & Velicer, W. F. (1986). Comparison of five rules for determining  
	the number of components to retain. \emph{Psychological Bulletin, 99,} 432-442.}
}
\author{Brian P. O'Connor }
\examples{
\donttest{
# the Harman (1967) correlation matrix
DIMTESTS(data_Harman, tests = c('EMPKC','HULL','RAWPAR'), corkind='pearson', 
                                Ncases = 305, display=2)

# Rosenberg Self-Esteem scale items, all possible DIMTESTS
DIMTESTS(data_RSE, 
         tests = c('CD','EMPKC','HULL','MAP','NEVALSGT1','RAWPAR','SALIENT','SESCREE','SMT'), 
      corkind='pearson', display=2)
  
# Rosenberg Self-Esteem scale items, using polychoric correlations
DIMTESTS(data_RSE, corkind='polychoric', display=2)

# NEO-PI-R scales
DIMTESTS(data_NEOPIR, tests = c('EMPKC','HULL','RAWPAR','NEVALSGT1'), display=2)
}
}
