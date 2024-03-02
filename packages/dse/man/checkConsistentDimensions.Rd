\name{checkConsistentDimensions}
\alias{checkConsistentDimensions}
\alias{checkConsistentDimensions.default}
\alias{checkConsistentDimensions.ARMA}
\alias{checkConsistentDimensions.SS}
\alias{checkConsistentDimensions.TSdata}
\alias{checkConsistentDimensions.TSestModel}

\title{Check Consistent Dimensions}
\description{Check that dimensions of a model and data agree.}
\usage{
    checkConsistentDimensions(obj1, obj2=NULL)
    \method{checkConsistentDimensions}{default}(obj1, obj2=NULL)
    \method{checkConsistentDimensions}{ARMA}(obj1, obj2=NULL)
    \method{checkConsistentDimensions}{SS}(obj1, obj2=NULL)
    \method{checkConsistentDimensions}{TSdata}(obj1, obj2=NULL)
    \method{checkConsistentDimensions}{TSestModel}(obj1, obj2=NULL)
    }
\arguments{
    \item{obj1}{An object containing a TSmodel, TSdata, or TSestModel,
       depending on the method}
    \item{obj2}{Another object containing TSdata corresponding to the TSmodel in
       obj1, or a TSmodel corresponding to the TSdata in obj1.}
}
\value{logical}
\details{
    Check that dimensions of a model and data agree. If \code{obj1} is a
    \code{TSestModel} then if \code{obj2} is \code{NULL, TSdata} is 
    taken from \code{obj1}. 
}

\examples{
    data("eg1.DSE.data.diff", package="dse")
    model <- estVARXls(eg1.DSE.data.diff)
    checkConsistentDimensions(model)
}
\concept{DSE}
\keyword{ts}

