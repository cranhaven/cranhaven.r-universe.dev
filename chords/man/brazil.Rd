\name{brazil}
\alias{brazil}
\docType{data}
\title{Heavy Drug Users in Curitiba}

\description{
A respondent driven sample of heavy drug users in Curitiba. 
}

\usage{data("brazil")}

\format{
  A data frame with 303 observations on the following 8 variables.
  \describe{
    \item{\code{MyUniID}}{Subject's ID.}
    \item{\code{NS1}}{Subject's self reported degree.}
    \item{\code{refCoupNum}}{Reference coupon no.}
    \item{\code{coup1}}{Supplied coupon.}
    \item{\code{coup2}}{Supplied coupon.}
    \item{\code{coup3}}{Supplied coupon.}
    \item{\code{interviewDt}}{Time of interview. See details.}
    \item{\code{interviewDt2}}{Deprecated.}
  }
}

\details{
The format of the data is essentially that of the RDS file format as specified in page 7 in the RDS Analysis tool manual: \url{http://www.respondentdrivensampling.org/reports/RDSAT_7.1-Manual_2012-11-25.pdf}.

The RDS format has been augmented with the time of interview (\code{interviewDt} variable) required for the methodology in [1].

The \code{interviewDt} variable encodes the time of interview.
For the purpose of calling \code{\link{Estimate.b.k}} the scale and origin are imaterial. We thus use an arbitrary efficient encoding which might not adhere to the original scale.

For full details see the Source section.
}

\source{
  [1] Salganik, M.J., Fazito, D., Bertoni, N., Abdo, A.H., Mello, M.B., and Bastos, F.I. (2011). 
  "Assessing Network Scale-up Estimates for Groups Most at Risk of HIV/AIDS: Evidence From a Multiple-Method Study of Heavy Drug Users in Curitiba,
  Brazil." 
  American Journal of Epidemiology, 174(10): 1190-1196. 
  
  And \url{http://opr.princeton.edu/archive/nsum/}
}

\references{
  [1] Berchenko, Y., Rosenblatt J.D., and S.D.W. Frost. 
  "Modeling and Analyzing Respondent Driven Sampling as a Counting Process." 
  arXiv:1304.3505
}

\keyword{datasets}
