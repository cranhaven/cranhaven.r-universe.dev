\name{fixConstants}
\alias{fixConstants}
\title{Fix TSmodel Coefficients (Parameters) to Constants}
\description{
    Fix specified coefficients to constant values or any coefficients within 
    fuzz of 0.0 or 1.0 to exactly 0.0 or 1.0.
    This will not change the model much but will affect some estimation 
    techniques and information criteria results, as these are considered to 
    be constants rather than coefficients.
}
\usage{
    fixConstants(model, fuzz=1e-5, constants=NULL)
}
\arguments{
    \item{model}{an object of class TSmodel.}
    \item{fuzz}{absolute difference to be considered equivalent.}
    \item{constants}{NULL or a list of logical arrays.}
}
\value{
    An object of class 'SS' 'TSmodel' with some array entries set to
    constants 0.0 or 1.0.
}
\details{If constants is not NULL then parameters within fuzz of 0.0 or 1.0 are 
  set as constants 0.0 or 1.0. If constants is not NULL then it should be a 
  list with logical arrays named F, G ..., with TRUE corresponding to any 
       array elements which are to  be treated as constant.
}
\examples{
    f <- array(c(.5,.3,.2,.4),c(2,2))
    h <- array(c(1,0,0,1),c(2,2))
    k <- array(c(.5,.3,.2,.4),c(2,2))
    ss <- SS(F=f,G=NULL,H=h,K=k)
    ss
    coef(ss)
    ss <- fixConstants(ss, constants=list(
              F = matrix(c(TRUE, FALSE, FALSE, FALSE), 2,2)))
    ss
    coef(ss)
    data("eg1.DSE.data.diff", package="dse")
    model <- toARMA(toSS(estVARXls(eg1.DSE.data.diff)))
    model <- fixConstants(model)
}
\seealso{
    \code{\link{fixF}}
}
\concept{DSE}
\keyword{ts}


