\name{AlitoConfirmation}
\alias{AlitoConfirmation}
\docType{data}
\title{US Senate Votes on Samuel Alito for the Supreme Court}
\description{
US Senate party affiliatoin and votes on confirming Samuel Alito for the Supreme Court
}
\format{
  A data frame with 100 observations on the following 6 variables.
  \describe{
    \item{\code{State}}{State name}
    \item{\code{Senator}}{Senator's name}
    \item{\code{Party}}{Party affiliation  (\code{D}=Democrat, \code{R}=Republican)}
    \item{\code{ConfVote}}{Confirmation vote (\code{Nay}=against or \code{Yea}=for)}
    \item{\code{StateOpinion}}{Percentage of state residents supporting the choice}
    \item{\code{Vote}}{1=for or 0=against}
  }
}
\details{
Data from the U.S. Senate vote on January 31, 2006 to confirm Samuel Alito to a position on the Supreme Court.
}
\source{
These numbers are taken from Kastellec, J.P., Lax, J.R., and Phillips, J. (2010), "Public Opinion and Senate Confirmation of Supreme Court Nominees," Journal of Politics, 72(3): 767-84. In this paper the authors used opinion polls and an advanced statistical method known as multilevel regression and poststratification to determine the StateOpinion levels.
}
\keyword{datasets}
