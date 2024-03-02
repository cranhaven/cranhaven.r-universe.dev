\name{fixF}
\alias{fixF}
\title{Set SS Model F Matrix to Constants}
\description{
    Set any parameters of the F matrix to constants. The same values are
    retained but they are considered to be constants rather than parameters.
    This will not change the model but will affect some estimation 
    techniques and information criteria results.
}
\usage{
    fixF(model)
}
\arguments{
    \item{model}{An object of class TSmodel.}

}
\value{
    An SS TSmodel object.
}

\examples{
    data("eg1.DSE.data.diff", package="dse")
    model <- toSS(estVARXls(eg1.DSE.data.diff))
    model <- fixF(model)
}
\seealso{
    \code{\link{fixConstants}}
}
\concept{DSE}
\keyword{ts}

