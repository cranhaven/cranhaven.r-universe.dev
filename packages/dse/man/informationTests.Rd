\name{informationTests}
\alias{informationTests}
\title{Tabulates selection criteria}
\description{Tabulates several model selection criteria.}
\usage{
    informationTests(..., sample.start=1,sample.end=NULL, Print=TRUE, warn=TRUE)}
\arguments{
    \item{...}{At least one object of class TSestModel.}
    \item{sample.start}{The start of the period to use for criteria calculations.}
    \item{sample.end}{
    The end of the period to use for criteria calculations. If omitted
    the end of the sample is used.}
    \item{Print}{If FALSE then printing suppressed.}
    \item{warn}{If FALSE then some warning messages are suppressed.}
}

\value{A matrix of the value for each model on each test returned invisibly.}
\section{Side Effects}{Criteria are tabulated for all models in the list.}
\seealso{
    \code{\link{informationTestsCalculations}}
}
\examples{
    data("eg1.DSE.data.diff", package="dse")
    model1 <- estVARXls(eg1.DSE.data.diff)
    model2 <- estVARXar(eg1.DSE.data.diff)
    informationTests(model1, model2)}
\concept{DSE}
\keyword{ts}

