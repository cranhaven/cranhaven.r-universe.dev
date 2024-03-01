\name{summary.simfam}
\alias{summary.simfam}

\title{
Summary method for \code{simfam}
}
\description{
Provides a summary of simulated data.
}
\usage{
\method{summary}{simfam}(object, digits = max(3, getOption("digits") - 3), \ldots)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
An object class of \code{'simfam'} generated from \code{simfam} function
}
 \item{digits}{Number of significant digits to use when printing.}
 \item{...}{Further arguments passed to or from other methods.}


}
\value{
Displays a summary of simulated data and returns the following values:
\item{num.fam}{Number of families simulated.}
\item{avg.num.affected}{Average number of affected individuals per family.}
\item{avg.num.carriers}{Average number of mutation carriers per family.}
\item{avg.family.size}{Average family size.}
\item{ave.ageonset}{Average age of onset for affected individuals.}
}

\author{
Yun-Hee Choi	
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{simfam}}
}
\examples{
set.seed(4321)
fam <- simfam(N.fam = 50, design = "pop", variation = "none", base.dist = "Weibull", 
       base.parms = c(0.01, 3), vbeta = c(-1.13, 2.35))
       
summary(fam)
\dontrun{
Study design:                           pop 
Baseline distribution:                  Weibull 
Number of families:                     50 
Average number of affected per family:  1.24 
Average number of carriers per family:  1.3 
Average family size:                    17.02 
Average age of onset for affected:      40.08 
Sampling weights used:                  1
}

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ summary }
