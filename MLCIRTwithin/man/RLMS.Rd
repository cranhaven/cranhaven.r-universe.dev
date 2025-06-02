\name{RLMS}
\alias{RLMS}
\docType{data}
\title{RLMS dataset}

\description{
This dataset contains the data about job satisfaction described in:
Bartolucci, F., Bacci, S., and Gnaldi, M. (2015), Statistical Analysis of Questionnaires: A Unified Approach Based on R and Stata, Chapman and Hall/CRC press}

\usage{data(RLMS)}
\format{
  A data frame with 1485 observations about four polytomous items with covariates:
  \describe{
    \item{\code{marital}}{marital status of the respondent}
    \item{\code{education}}{educational level of the respondent}
    \item{\code{gender}}{gender of the respondent}
    \item{\code{age}}{age of the respondent}
    \item{\code{work}}{work status of the respondent}
    \item{\code{Y1}}{1st item response}
    \item{\code{Y2}}{2nd item response}
    \item{\code{Y3}}{3rd item response}
    \item{\code{Y4}}{4th item response}
  }
}

%\details{
%%%  ~~ If necessary, more details than the __description__ above ~~
%}

%\source{
%}

 \references{
Bartolucci, F., Bacci, S., and Gnaldi, M. (2015), \emph{Statistical Analysis of Questionnaires: A Unified Approach Based on R and Stata}, Chapman and Hall/CRC press
 }
 \examples{
 data(RLMS)
 ## maybe str(RLMS)
 str(RLMS)
 }

\keyword{datasets}
\keyword{education}