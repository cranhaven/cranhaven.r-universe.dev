\name{SampleFG}
\alias{SampleFG}
\docType{data}
\title{Field Goal Attempts in the NFL}
\description{
A sample of 30 field goal attempts in the National Football League
}
\format{
  A dataset with 30 observations on the following 13 variables.
  \tabular{rl}{
    \code{ID} \tab {ID number}\cr
    \code{PlayerID} \tab {Code for player}\cr
    \code{LastName} \tab {Last name}\cr
    \code{FirstName} \tab {First name}\cr
    \code{Year} \tab {Year}\cr
    \code{Team} \tab {Abbreviation for team name}\cr
    \code{Date} \tab {Code for date:  mmddyy}\cr
    \code{FGAttempts} \tab {Field goals attempted by the kicker that game}\cr
    \code{FGMade} \tab {Field goals made by the kicker that game}\cr
    \code{Attempt} \tab {Which attempt during the game?}\cr
    \code{Result} \tab {\code{1}=made the field goal or \code{0}=missed}\cr
    \code{Yards} \tab {Number of yards for the field goal attempt}\cr
    \code{Block} \tab {\code{1}=attempt blocked or \code{0}=not blocked}\cr
  }
}
\details{
This is a subset  of just 30 field goal attempts selected at random from the larger sample of attempts made by NFL kickers
that is summarized in FGByDistance.
}
\source{
We thank Sean Forman and Doug Drinen of Sports Reference LLC for providing us with the NFL field goal data
set.
}

\keyword{datasets}
