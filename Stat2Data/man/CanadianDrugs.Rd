\name{CanadianDrugs}
\alias{CanadianDrugs}
\docType{data}
\title{Canadian Drugs Senate Vote}
\description{
US Senate vote on Klobuchar amendment to lower drug prices
}
\format{
  A data frame with 94 observations on the following 6 variables.
  \describe{
    \item{\code{Senator}}{Name of the Senator}
    \item{\code{Contributions}}{Amount of money received from the pharmaceutical industry over 6 years}
    \item{\code{Party}}{\code{D}=Democrat or \code{R}=Republican}
    \item{\code{State}}{Abbreviation for Senator's state}
    \item{\code{RollCall}}{ \code{Nay} or \code{Yea}}
    \item{\code{Vote}}{\code{Against} or \code{With} what drug makers wanted}
  }
}
\details{
January 2017 vote in the U.S. Senate related to repeal part of ObamaCare. The "Klobuchar amendment" to a bill was introduced with the purpose of lowering drug prices by allowing prescription drugs to be imported from Canada.

The data exclude two senators who did not vote on the amendment and four senators who were new to Congress and thus had received no money from the drug industry. The remaining 94 senators represent 49 states (every state except California) and each of these senators had received at least $3,000.
}
\source{
Data obtained from:

http://www.senate.gov/legislative/LIS/roll_call_lists/roll_call_vote_cfm.cfm?congress=115&session=1&vote=00020

http://maplight.org/us-congress/interest/H4300/view/all

}
\keyword{datasets}
