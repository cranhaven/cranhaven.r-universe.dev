\name{SF12}
\alias{SF12}
\docType{data}
\title{SF12 dataset}

\description{
 This data set contains the responses  of 620 oncological patients to 12 ordinal polytomous items that measure the health-related quality of life, according to   the Italian release of Short-Form 12 version 2 (SF-12v2); patient's age is also provided.}

\usage{data(SF12)}
\format{
A dataframe with  620 observations on 12 items and one covariate: 


\describe{
    \item{\code{Y1}}{general health}
    \item{\code{Y2}}{limits in moderate activities}
    \item{\code{Y3}}{limits in climbing several flights of stairs}
    \item{\code{Y4}}{accomplished less than he/she would like, as a result of his/her physical health}
    \item{\code{Y5}}{limited in the kind of work or daily activities, as a result of his/her physical health}
    \item{\code{Y6}}{accomplished less than he/she would like, as a result of his/her emotional health}
    \item{\code{Y7}}{did work less carefully than usual, as a result of his/her emotional health}
    \item{\code{Y8}}{how much did pain interfere with normal work}
    \item{\code{Y9}}{how much of the time have he/she felt calm and peaceful}
    \item{\code{Y10}}{how much of the time did he/she have a lot of energy}
    \item{\code{Y11}}{how much of the time have he/she felt downhearted and depressed}
    \item{\code{Y12}}{how much of the time physical health or emotional health interfered with social activities}
    \item{\code{age}}{age of the respondent}    
  }
}

\details{
All items have 5 response categories, with the exception of items Y2 and Y3 having 3 response categories:  the minimum value 0 correspond to a low  level of quality of life, whereas the maximum value corresponds to a high level of quality of life. A proportion of 0.205 patients (127 out of 620) has missing responses (NA) on one or more items.
}

%\source{
%%%  ~~ reference to a publication or URL from which the data were obtained ~~
%}

\references{
Ware, J., Kosinski, M., Turner-Bowker, D. and Gandek, B. (2002), \emph{SF-12v2. How to score version 2 of the SF-12 health survey}, QualityMetric Incorporated: Lincoln.
}
 \examples{
 data(SF12)
 dim(SF12)
 ## maybe str(SF12)
 str(SF12)
 }

\keyword{datasets}
\keyword{health-related quality of life}