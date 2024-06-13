\name{diabetes}
\alias{diabetes}
\alias{diabetes}

\docType{data}

\title{Studies on the Association of Gestational Diabetes Mellitus (GDM)
  and Type 2 Diabetes Mellitus (T2DM)}

\description{Results from 20 cohort studies investigating the
  association between GDM and T2DM}

\format{
  The data frame contains the following columns:
  \describe{
    \item{y1}{number of subjects who developed T2DM among the
      unexposed subjects (without GDM)}
    \item{n1}{number of unexposed subjects (without GDM)}
    \item{y2}{number of subjects who developed T2DM among the
      exposed subjects (with GDM)}
    \item{n2}{number of exposed subjects (with GDM)}
    \item{\code{studynames}}{The study names indicating the last name of
    the first author and the year of each study}
  }
}


\references{
Chen, Y., Luo, S., Chu, H., Su, X., and Nie, L. (2012a). An empirical
Bayes method for multivariate meta-analysis with an application in
clinical trials. \cr
\emph{Communication in Statistics: Theory and Methods}. \cr
<https://doi.org/10.1080/03610926.2012.700379> \cr

Bellamy, L, Casas, J.P., Hingorani, A.D., Williams, D. (2009) Type 2 diabetes mellitus after gestational
diabetes: a systematic review and meta-analysis. \cr
\emph{The Lancet} 373(9677):1773-1779 \cr
<doi:10.1097/01.aoa.0000370496.77221.05> \cr
}


\note{
The dataset \code{diabetes} is used to conduct exact posterior inference
of relative risk and risk difference for multiple 2X2 tables.
}

\examples{
\donttest{
library(mmeta)
data(diabetes)
summary(diabetes)
}
}

\keyword{datasets}
