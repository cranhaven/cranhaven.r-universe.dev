\name{SPmlficmcm-package}
\alias{SPmlficmcm-package}
\alias{SPmlficmcm}
\docType{package}
\title{
SemiParametric Maximum Likelihood for interaction in case-mother control-mother designs
}
\description{
Implementation of a method of general semiparametric maximum likelihood estimation for logistic models in case-mother control-mother designs. The method was proposed by \emph{Chen et al., (2012)} for the complete data and \emph{Nguile-Makao et al., (2015)} proposed an extension of the method allowing missing offspring genotype.
}
\details{
The package \pkg{SPmlficmcm} implements the semiparametric maximum likelihood estimation method published by \emph{Chen et al., (2012)}. This method permits to analyze the interaction effects involving genetic variants and environmental exposures on the risk of adverse obstetric and early-life outcomes. \emph{Nguile-Makao et al., (2015)} proposed an extension of this method allowing missing offspring genotype. The package performs the analysis the following way: it builds the nonlinear system from the database, resolves the nonlinear system using the \code{\link[nleqslv]{nleqslv}} function of package \pkg{nleqslv}. It estimates the model parameters and the standard errors using the log profile likelihood function and the one-step method estimation. All this procedure may be done for complete data and also for missing offspring genotype. For more details see \emph{Chen et al., (2012)}, and \emph{Nguile-Makao et al., (2015)} . 
The modeling supposes that the distribution of maternal genotype and offspring genotype satisfy the following assumptions: \emph{random mating}, \emph{Hardy-Weinberg equilibrium} and \emph{Mendelian inheritance}. The package also permits to treat the missing offspring genotype data.
\packageIndices{SPmlficmcm}
}
\author{
Moliere Nguile-Makao and Alexandre Bureau

Maintainer:  Moliere Nguile-Makao <moliere.nguile@gmail.com>
}
\references{
Jinbo Chen,  Dongyu Lin  and Hagit Hochner (2012) Semiparametric Maximum Likelihood Methods for Analyzing Genetic and Environmental Effects with Case-Control Mother-Child Pair Data. \emph{Biometrics} DOI: 10.1111/j.1541-0420.2011.01728.

Moliere Nguile-Makao, Alexandre Bureau (2015), Semi-Parametric Maximum likelihood Method for interaction in Case-Mother Control-Mother designs: Package SPmlficmcm. \emph{Journal of Statistical Software DOI: 10.18637/jss.v068.i10}.
}
\keyword{ Semiparametric }
\keyword{ Maximum Likelihood }
\keyword{ Case-Control }
\keyword{ Mother-Child Pair }
\seealso{
\code{\link{Est.Inpar}}, \code{\link{FtSmlrmCMCM}}, \code{\link{SeltcEch}}, \code{\link{Spmlficmcm}}
}

