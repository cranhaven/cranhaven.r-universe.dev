\name{italy2006}
\alias{italy2006}
\docType{data}
\title{
2006 Italian General Election survey
}
\description{
2006 Italian General Election survey, with quadratic ideological proximity.
}
\usage{data(italy2006)}
\format{
  A data frame with 438 observations on the following 18 variables.
  \describe{
    \item{\code{country}}{country name}
    \item{\code{id}}{id of respondent}
    \item{\code{vote}}{a factor with levels \code{FI} \code{UL} \code{AN} \code{UDC} \code{RC} for each party voted}
    \item{\code{self}}{self-placement of respondent on a 0 to 10
left-right scale}
    \item{\code{prox_FI}}{see Details.}
    \item{\code{prox_UL}}{see Details.}
    \item{\code{prox_AN}}{see Details.}
    \item{\code{prox_UDC}}{see Details.}
    \item{\code{prox_RC}}{see Details.}
    \item{\code{partyID_FI}}{see Details.}
    \item{\code{partyID_UL}}{see Details.}
    \item{\code{partyID_AN}}{see Details.}
    \item{\code{partyID_UDC}}{see Details.}
    \item{\code{partyID_RC}}{see Details.}
    \item{\code{sex}}{gender variable \code{1} = female}
    \item{\code{age}}{see Details.}
    \item{\code{education}}{see Details.}
    \item{\code{gov_perf}}{see Details.}
  }
}
\details{
In this survey respondents were asked to indicate which party they voted for in the 2006 Election. The data concerns 5 parties: UL (Ulivo), RC (Communist Refoundation party), FI (Forza Italia), AN (National Alliance) and UDC (Union of Christian Democrats).


\code{prox_*} quadratic ideological distance between the respondent and a party \code{*} placement 

\code{partyID_*} binary variable equals to \code{1} if the respondent declares to feel herself close to party \code{*}

\code{age} : 1 = "18-24 years", 2 = "25-34", 3 = "35-44", 4 = "45-54", 5 = "55-64", 6 =  "65 +"

\code{education} : 0 = "up to primary school", 1 = "incomplete secondary", 2 = "secondary completed", 3 = "post-secondary trade",  4 = "university undergraduate degree inc", 5 = "university undergraduate degree comp"


\code{gov_perf} : 1 = "very good job", 2 = "good job", 3 = "bad job", 4 = "very bad job"
}
\source{
CSES - Comparative Study of Electoral Systems: \url{https://cses.org/}). 
}
\examples{
data(italy2006)
head(italy2006)
}
\keyword{datasets}
