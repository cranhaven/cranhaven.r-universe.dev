\name{elec92}
\alias{elec92}
\docType{data}
\title{
Public Opinion During the 1992 U.S. Presidential Election
}
\description{
This data set contains several variables from the Center for
   Political Studies 1992 National Election Study.
   Observations with missing values on any of the variables have
   been deleted.
}
\usage{data(elec92)}
\format{
  A data frame with 1653 observations on the following 7 variables.
  \describe{
    \item{\code{caseid}}{NES case identification number}
    \item{\code{bush}}{ Respondents feeling thermometer rating of George H. W. Bush}
    \item{\code{ideol}}{Respondents ideological self-placement, seven-point scale
       ranging from 1=extremely liberal to 7=extremely conservative}
    \item{\code{econ4yr}}{Respondents judgment whether national economy has gotten 
       better or worse over preceding four years, 
       five-point scale ranging from 1=much better to 5=much worse}
    \item{\code{party}}{Respondents party identification, seven-point scale ranging
       from 0=strong Democrat to 6=strong Republican}
    \item{\code{choice}}{Difference in respondents feeling thermometer ratings of
       Bush and Clinton}
    \item{\code{clinton}}{Respondent''s feeling thermometer rating of Bill Clinton}
  }
}

\source{
The full data set from which these observations and variables were extracted is
   available on the Study Page for the American National Election Studies
   1992 Time Series Study, at 
   \url{https://electionstudies.org/data-center/1992-time-series-study/}.
}
\references{
Jacoby, William G. (1999) \dQuote{Levels of Measurement and Political Research: An Optimistic View.}
   \emph{American Journal of Political Science} 43: 271-301.
}
\examples{
library(optiscale)
data(elec92)
summary(lm(choice ~ party + ideol + econ4yr, data = elec92))
}
\keyword{datasets}
