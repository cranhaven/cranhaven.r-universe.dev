\name{summary.simfam_c}
\alias{summary.simfam_c}

\title{
Summary method for \code{simfam_c}
}
\description{
Provides a summary of simulated data from \code{simfam_c} function.
}
\usage{
\method{summary}{simfam_c}(object, digits = max(3, getOption("digits") - 3), \ldots)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
An object class of \code{'simfam'} generated from \code{simfam_c} function
}
 \item{digits}{Number of significant digits to use when printing.}
 \item{...}{Further arguments passed to or from other methods.}


}
\value{
Displays a summary of simulated data and returns the following values:
\item{num.fam}{Number of families simulated.}
\item{avg.num.affected1}{Average number of affected individuals by event 1 per family.}
\item{avg.num.affected2}{Average number of affected individuals by event 2 per family.}
\item{avg.num.carriers}{Average number of mutation carriers per family.}
\item{avg.family.size}{Average family size.}
\item{ave.ageonset1}{Average age of onset for affected individuals by event 1.}
\item{ave.ageonset2}{Average age of onset for affected individuals by event 2.}
}

\author{
Yun-Hee Choi	
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{simfam_c}}
}
\examples{
set.seed(4321)
fam <- simfam_c(N.fam = 50, design = "pop+", variation = "none", 
        base.dist = "Weibull", 
        base.parms = list(c(0.01, 3), c(0.01, 3)), 
        vbeta = list(c(-1.13, 2.35), c(-1,2)))
       
summary(fam)
\dontrun{
Study design:                           pop+ 
Baseline distribution for event 1:      Weibull 
Baseline distribution for event 2:      Weibull 
Number of families:                     50 
Average number of event 1 per family:   1.24 
Average number of event 2 per family:   0.7 
Average number of carriers per family:  5.54 
Average family size:                    15.58 
Average age of onset for event 1:       42.59 
Average age of onset for event 2:       43.72 
Sampling weights used:                  1
}

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ summary }
