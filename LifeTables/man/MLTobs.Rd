\name{MLTobs}
\docType{data}
\alias{MLTobs}

\title{Background objects necessary for certain functions}
\description{ This data set contains objects that are necessary for certain package functions and are described in detail below}

\usage{data(MLTobs)}
\format{ 
	\itemize{
\item{averages.smooth} A 48x7 matrix whose columns contain the seven underlying family patterns. Rows 1:24 are the male pattern and rows 25:48 are the female patterns. Used in \code{\link{mortmod}}.
\item{class5} A vector of length 799 which contains the classification for the 799 HMD mortality rate schedules. Used in \code{\link{hmd.DA.mx}}.
\item{f.csd.weight} A function to determine the weight for the family-specific deviation. Used in \code{\link{mortmod}}.
\item{Mx.svd.scores} A 48x10 matrix whose columns contain the first 10 component score vectors a SVD of the HMD mortality rate schedules. 
\item{flt.mx} A 24x799 matrix whose columns contain the 799 female mortality rate schedules in the Human Mortality Database
\item{flt.mx.info} A 799x28 matrix. The first four columns contain the Location ID, Location Name, Subgroup type and Period. The remaining columns contain the age-specific mortality rates contained in \code{flt.mx}.
\item{hi.devs} A 8x48 matrix whose first five rows contain the family-age-specific high deviations for when alpha is positive. The 8th row contains the age-specific high deviations from the entire HMD data set. Used in \code{\link{mortmod}}.
\item{hmd.1m0.train.f} Training models for females when the input parameters include 1m0. Used in \code{\link{hmd.DA}}.
\item{hmd.1m0.train.m} Training models for males when the input parameters include 1m0. Used in \code{\link{hmd.DA}}.
\item{hmd.1m0a.train.f} Training models for females when the input parameters include 1m0 and 45q15. Used in \code{\link{hmd.DA}}.

\item{hmd.1m0a.train.m} Training models for males when the input parameters include 1m0 and 45q15. Used in \code{\link{hmd.DA}}.

\item{hmd.5m0.train.f} Training models for females when the input parameters include 5m0. Used in \code{\link{hmd.DA}}.

\item{hmd.5m0.train.m} Training models for males when the input parameters include 5m0. Used in \code{\link{hmd.DA}}.

\item{hmd.5m0a.train.f} Training models for females when the input parameters include 5m0 and 45q15. Used in \code{\link{hmd.DA}}.

\item{hmd.5m0a.train.m} Training models for males when the input parameters include 5m0 and 45q15. Used in \code{\link{hmd.DA}}.

\item{hmd.1q0.train.f} Training models for females when the input parameters include 1q0. Used in \code{\link{hmd.DA}}.

\item{hmd.1q0.train.m} Training models for males when the input parameters include 1q0. Used in \code{\link{hmd.DA}}.

\item{hmd.1q0a.train.f} Training models for females when the input parameters include 1q0 and 45q15. Used in \code{\link{hmd.DA}}.

\item{hmd.1q0a.train.m} Training models for males when the input parameters include 1q0 and 45q15. Used in \code{\link{hmd.DA}}.

\item{hmd.5q0.train.f} Training models for females when the input parameters include 5q0. Used in \code{\link{hmd.DA}}.

\item{hmd.5q0.train.m} Training models for males when the input parameters include 5q0. Used in \code{\link{hmd.DA}}.

\item{hmd.5q0a.train.f} Training models for females when the input parameters include 5q0 and 45q15. Used in \code{\link{hmd.DA}}.

\item{hmd.5q0a.train.m} Training models for males when the input parameters include 5q0 and 45q15. Used in \code{\link{hmd.DA}}.

\item{lo.devs} A 8x48 matrix whose first five rows contain the family-age-specific low deviations for when alpha is positive. The 8th row contains the age-specific low deviations from the entire HMD data set. Used in \code{\link{mortmod}}.
\item{mlt.mx} A 24x799 matrix whose columns contain the 799 male mortality rate schedules in the Human Mortality Database
\item{mlt.mx.info} A 799x28 matrix. The first four columns contain the Location ID, Location Name, Subgroup type and Period. The remaining columns contain the age-specific mortality rates contained in \code{mlt.mx}.
\item{opt.alpha.f} A 33x7 matrix whose columns contain the female family-specific alpha values to produce life tables with life expectancies at 33 levels from 20-100 in 2.5 year increments
\item{opt.alpha.m} A 33x7 matrix whose columns contain the male family-specific alpha values to produce life tables with life expectancies at 33 levels from 20-100 in 2.5 year increments
\item{Patterns.final.coeffs} A 5x7 matrix whose columns contain the set of median coefficients for each family that are used to calculate the underlying family patterns
	}	
	}
	
\references{
S. Clark and D. Sharrow (2011) "Contemporary Model Life Tables for Developed Countries: An Application of Model-Based Clustering."  Paper presented at the annual meeting of the Population Association of America. \url{http://paa2011.princeton.edu/abstracts/111808}

C. Fraley and A. E. Raftery (2002) "Model-based clustering, discriminant analysis, and density estimation." \emph{Journal of the American Statistical Association} \bold{97}:611-631.

C. Fraley, A. E. Raftery, T. B. Murphy, L. Scrucca (2012) "mclust Version 4 for R: Normal Mixture Modeling for Model-Based Clustering, Classification, and Density Estimation", Technical Report no. 597, Department of Statistics, University of Washington

\emph{Human Mortality Database} University of California, Berkeley (USA), and Max Planck Institute for Demographic Research (Germany). Available at www.mortality.org or www.humanmortality.de (data downloaded February 2015).
}

\keyword{datasets}
