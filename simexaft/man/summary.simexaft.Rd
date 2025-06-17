\name{summary.simexaft}
\alias{summary.simexaft}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Summarizing Model fits for the AFT model by SIMEX method
}
\description{
Summary method for the class SIMEXAFF.
}
\usage{
\method{summary}{simexaft}(object, \dots)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{ object of class SIMEXAFT.  }
  \item{\dots}{ further arguments. }
}

\value{
  \item{coefficients }{a p x 3 matrix with columns for the estimated coefficient 
                        its standard error, corresponding(two-sided) p-value}
  \item{scalereg }{estimate of the scale }
  \item{extrapolation }{the extrapolation method }
  \item{SIMEXvariable }{character vector of the SIMEXvariable }
}
\references{
Genz, A., Bretz, F., Miwa, T., Mi, X., Leisch, F., Scheipl, F. and Hothorn, T. (2011). mvtnorm: Multivariate Normal and t Distributions. R package version 0.9-9991, URL http://CRAN.
R-project.org/package=mvtnorm.

He, W., Yi, G. Y. and Xiong, J. (2007). Accelerated Failure Time Models with Covariates Subject to Measurement Error. \emph{Statistics in Medicine}, 26, 4817-4832.

Therneau, T. and Lumley, T. (2011). survival: Survival Analysis, Including Penalised Likelihood. R package version 2.36-10, URL http://CRAN.R-project.org/package=survival.
}
\author{ Juan Xiong, Wenqing He and Grace Y. Yi}

\seealso{
\code{\link[simexaft]{simexaft}}
}




