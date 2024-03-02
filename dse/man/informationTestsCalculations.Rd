\name{informationTestsCalculations}
\alias{informationTestsCalculations}

\title{Calculate selection criteria}
\description{Calculates several model selection criteria.}
\usage{
    informationTestsCalculations(lst, sample.start=1,sample.end=NULL, warn=TRUE)
}
\arguments{
    \item{lst}{One or more objects of class TSestModel.}
    \item{sample.start}{
        The start of the period to use for criteria calculations.
    }
    \item{sample.end}{
        The end of the period to use for criteria calculations. 
        If omitted the end of the sample is used.
    }
    \item{warn}{If FALSE then some warning messages are suppressed.}
}
\value{
   The calculated values are returned in a vector with names: port, like, 
   aic, bic, gvc, rice, fpe, taic, tbic, tgvc, trice and tfpe. These
   correspond to values for the  Portmanteau test, likelihood, 
   Akaike Information Criterion, Bayes  Information Criterion, 
   Generalized Cross Validation, Rice Criterion, and Final Prediction Error. 
   The preceeding 't' indicates that the theoretical parameter space 
   dimension has been used, rather than the number of 
   coefficient (parameter) values. Methods which select a model based on some
   information criterion calculated by \code{informationTestsCalculations}
   should use the name of the vector element to specify the test value which
   is to be used.
}
\seealso{
    \code{\link{informationTests}}
}
\examples{
    data("eg1.DSE.data.diff", package="dse")
    model <- estVARXls(eg1.DSE.data.diff)
    informationTestsCalculations(model)
}
\concept{DSE}
\keyword{ts}

