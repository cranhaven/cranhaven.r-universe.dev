\name{hads}
\alias{hads}
\docType{data}
\title{Dataset about measurement of anxiety and depression in oncological patients}

\description{This data set contains the responses of 201 oncological patients to 14 ordinal polytomous items that measure
anxiety (7 items) and depression (7 items), according to the Hospital Anxiety and Depression Scale questionnaire.
}
\usage{data(hads)}
\format{
  A data frame with 201 observations on 14 items:
  \describe{
    \item{\code{item1}}{measure of depression}
    \item{\code{item2}}{measure of anxiety}
    \item{\code{item3}}{measure of depression}
    \item{\code{item4}}{measure of depression}
    \item{\code{item5}}{measure of depression}
    \item{\code{item6}}{measure of anxiety}
    \item{\code{item7}}{measure of anxiety}
    \item{\code{item8}}{measure of anxiety}
    \item{\code{item9}}{measure of depression}
    \item{\code{item10}}{measure of anxiety}
    \item{\code{item11}}{measure of anxiety}
    \item{\code{item12}}{measure of anxiety}
    \item{\code{item13}}{measure of depression}
    \item{\code{item14}}{measure of depression}
  }
}

\details{All items have 4 response categories: the minimum value 0 corresponds to a low level of anxiety or depression, 
whereas the maximum value 3 corresponds to a high level of anxiety or depression.}

%\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
%}
\references{
Zigmond, A. and Snaith, R. (1983), The hospital anxiety and depression scale, \emph{Acta Psychiatrika Scandinavica},
\bold{67}, 361-370.
}
\examples{
data(hads)
## maybe str(hads) 
str(hads)
}
\keyword{datasets}
\keyword{psychology}