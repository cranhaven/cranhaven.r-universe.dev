\name{colorectal}
\alias{colorectal}
\alias{colorectal}

\docType{data}

\title{Studies on the Association of N-acetyltransferase 2 (NAT2) Acetylation
  Status and Colorectal Cancer}

\description{Results from 20 case-control studies investigating the
  association between rapid NAT2 acetylator status and colorectal cancer}

\format{
  The data frame contains the following columns:
  \describe{
    \item{y1}{number of subjects with rapid NAT2 acetylator status in the
      control group}
    \item{n1}{number of subjects in the control group (without
      colorectal cancer)}
    \item{y2}{number of subjects with rapid NAT2 acetylator status in the
      case group}
    \item{n2}{number of subjects in the case group (with
      colorectal cancer)}
    \item{studynames}{The study names indicating the last name of
    the first author of each study}
  }
}

\references{
Chen, Y., Chu, H., Luo, S., Nie, L., and Chen, S. (2011b). Bayesian
analysis on meta-analysis of case-control studies accounting for
within-study correlation. \cr 
\emph{Statistical Methods in Medical Research},Published online on Dec 4, 2011, PMID: 22143403. \cr
<doi:10.1177/0962280211430889>. \cr

Ye, Z. and Parry, J. (2002) Meta-analysis of 20 case-control studies
on the N -acetyltransferase 2 acetylation status and
colorectal cancer risk. \cr 
\emph{Med Sci Monit 8}, CR558-65. \cr
\url{https://medscimonit.com/abstract/index/idArt/13598}. \cr
}

\note{
The dataset \code{colorectal} is used to conduct exact posterior
inference of odds ratio for multiple 2X2 tables. 
}

\examples{
\donttest{
library(mmeta)
data(colorectal)
summary(colorectal)
}
}

\keyword{datasets}
