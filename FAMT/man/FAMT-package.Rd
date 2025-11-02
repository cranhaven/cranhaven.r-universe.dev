\name{FAMT-package}
\alias{FAMT-package}
\alias{FAMT}
\docType{package}
\title{Factor Analysis for Multiple Testing (FAMT) : simultaneous tests under dependence in high-dimensional data
}
\description{
The method proposed in this package takes into account the impact of dependence on multiple testing procedures for high-throughput data as proposed by Friguet et al. (2009). The common information shared by all the variables is modeled by a factor analysis structure. The number of factors considered in the model is chosen to reduce the variance of the number of false discoveries. The model parameters are estimated thanks to an EM algorithm. Factor-adjusted tests statistics are derived, as well as the associated p-values. The proportion of true null hypotheses (an important parameter when controlling the false discovery rate) is also estimated from the FAMT model. Diagnostic plots are proposed to interpret and describe the factors.
}
\details{
\tabular{ll}{
Package: \tab FAMT\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2010-05-03\cr
License: \tab GPL \cr
LazyLoad: \tab yes\cr
}

The \code{\link{as.FAMTdata}} function creates a single R object containing the data stored: 
- in one mandatory data-frame: the 'expression' dataset with m rows (if m tests) and n columns (n is the sample size) containing the observations of the responses.
- and two optional data-frames: the 'covariates' dataset with n rows and at least 2 columns, one giving the specification to match 'expression' and 'covariates' and the other one containing the observations of at least one covariate. The optional dataset, 'annotations' can be provided to help interpreting the factors: with m rows and at least one column to identify the variables (ID).

The whole multiple testing procedure is provided in a single function, \code{\link{modelFAMT}}, but you can also choose to apply the procedure step by step, using the functions : 

\code{\link{nbfactors}} (Estimation of the optimal number of factors)
\code{\link{emfa}} (EM fitting of the Factor Analysis model).

The \code{\link{modelFAMT}} also provides the individual test statistics and corresponding p-values like the \code{\link{raw.pvalues}} function.

A function \code{\link{summaryFAMT}} provides some key elements of classical summaries either on 'FAMTdata' or 'FAMTmodel'.

The estimation of the proportion of true null hypotheses from a 'FAMTmodel' is done by the function \code{\link{pi0FAMT}}. 

The \code{\link{defacto}} function provides diagnostic plots to interpret and describe the factors.

}

\author{
David Causeur, Chloe Friguet, Magalie Houee-Bigot, Maela Kloareg.

Maintainer: \email{David.Causeur@agrocampus-ouest.fr}
}

\references{
Causeur D., Friguet C., Houee-Bigot M., Kloareg M. (2011). Factor Analysis for Multiple Testing (FAMT): An R Package for Large-Scale Significance Testing Under Dependence. Journal of Statistical Software, 40(14),1-19. 
\url{https://www.jstatsoft.org/v40/i14}

Friguet C., Kloareg M. and Causeur D. (2009). A factor model approach to multiple testing under dependence. Journal of the American Statistical Association, 104:488, p.1406-1415

\url{http://famt.free.fr/}
}
\concept{Factor analysis}
\concept{Multiple testing}
\concept{False discovery rate}
\concept{Dependence}

