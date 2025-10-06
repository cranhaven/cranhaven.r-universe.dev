\name{italy2006.wide}
\alias{italy2006.wide}
\docType{data}
\title{
2006 Italian General Election survey
}
\description{
2006 Italian General Election survey - wide format
}
\usage{data(italy2006.wide)}
\format{
  A data frame with 524 observations on the following 15 variables.
  \describe{
    \item{\code{country}}{country name}
    \item{\code{id}}{id of respondent}
    \item{\code{vote}}{a factor with levels \code{FI} \code{UL} \code{AN} \code{UDC} \code{RC} for each party voted}
    \item{\code{self}}{self-placement of respondent on a 0 to 10
left-right scale}
    \item{\code{FI}}{see Details.}
    \item{\code{DS}}{see Details.}
    \item{\code{AN}}{see Details.}
    \item{\code{DL}}{see Details.}
    \item{\code{UDC}}{see Details.}
    \item{\code{RC}}{see Details.}
    \item{\code{pID}}{see Details.}
    \item{\code{sex}}{gender variable \code{1} = female}
    \item{\code{age}}{see Details.}
    \item{\code{education}}{see Details.}
    \item{\code{gov_perf}}{see Details.}
  }
}
\details{
In this survey respondents were asked to indicate which party they voted for in the 2006 Election. The data concerns 5 parties: UL (Ulivo), RC (Communist Refoundation party), FI (Forza Italia), AN (National Alliance) and UDC (Union of Christian Democrats). The dataset is in wide format.

variable from \code{FI} to \code{RC} identify the placement of those parties, on a 0 to 10 left-right scale, as perceived by the respondent.



\code{pID} is a variable that identifies the partisanship of the respondent (where 0=stands for no partyID, 1 = FI partyID, 23 = UL partyID, 3 = AN partyID, 4 = UDC partyID, 6 = RC partyID)


\code{age} : 1 = "18-24 years", 2 = "25-34", 3 = "35-44", 4 = "45-54", 5 = "55-64", 6 =  "65 +"

\code{education} : 0 = "up to primary school", 1 = "incomplete secondary", 2 = "secondary completed", 3 = "post-secondary trade",  4 = "university undergraduate degree inc", 5 = "university undergraduate degree comp"


\code{gov_perf} : 1 = "very good job", 2 = "good job", 3 = "bad job", 4 = "very bad job"

}
\source{
CSES - Comparative Study of Electoral Systems: \url{https://cses.org/}). 
}
\examples{
data(italy2006.wide)
head(italy2006.wide)
## maybe str(italy2006.wide) ; plot(italy2006.wide) ...
}
\keyword{datasets}
