\name{ThomasConfirmation}
\alias{ThomasConfirmation}
\docType{data}
\title{US Senate Votes on Clarence Thomas Confirmation}
\description{
Votes in the US Senate on Clarence Thomas nomination for the US Supreme Court
}
\format{
  A data frame with 100 observations on the following 6 variables.
  \describe{
    \item{\code{State}}{State name}
    \item{\code{Senator}}{Senator name}
    \item{\code{Party}}{Party affiliation (\code{D}=Democrat or \code{R}=Republican)}
    \item{\code{ConfVote}}{Confirmation vote (\code{Nay} or \code{Yea})}
    \item{\code{StateOpinion}}{Percentage of state residents supporting the choice}
    \item{\code{Vote}}{Numeric coding for vote (\code{1}=for or \code{0}=against)}
  }
}
\details{
Data from the U.S. Senate vote on October 15, 1991 to confirm Clarence Thomas to a position on the Supreme Court.
}
\source{
These numbers are taken from Kastellec, J.P., Lax, J.R., and Phillips, J. (2010), "Public Opinion and Senate Confirmation of Supreme Court Nominees," Journal of Politics, 72(3): 767-84. In this paper the authors used opinion polls and an advanced statistical method known as multilevel regression and poststratification to determine the StateOpinion levels.
}
\keyword{datasets}
