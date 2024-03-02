\name{ARMA}
\alias{ARMA}
\alias{is.ARMA}

\title{ARMA Model Constructor}
\description{Constructs an ARMA TSmodel object as used by the DSE package.}
\usage{
    ARMA(A=NULL, B=NULL, C=NULL, TREND=NULL, 
          constants=NULL,
          description=NULL, names=NULL, input.names=NULL, output.names=NULL)
    is.ARMA(obj)
}
\arguments{
    \item{A}{The auto-regressive polynomial, an axpxp array.}
    \item{B}{The moving-average polynomial, an bxpxp array.}
    \item{C}{The  input polynomial, an cxpxm array. C should be NULL if 
    there is no input}
    \item{TREND}{A matrix, p-vector, or NULL.}
    \item{constants}{NULL or a list of logical arrays with the same names as 
          arrays above, indicating which elements should be considered constants.}
    \item{description}{An arbitrary string.}
    \item{names}{A list with elements input and output, each a vector of 
       strings. Arguments input.names and output.names should not be used if
       argument names is used.}
    \item{input.names}{A vector of strings. }
    \item{output.names}{A vector of strings.}
    \item{obj}{Any object.}
}
\value{An ARMA TSmodel}
\details{ 
The ARMA model is defined by:

      A(L)y(t) =  B(L)w(t) + C(L)u(t) + TREND(t)

where
\describe{
    \item{A}{(axpxp) is the auto-regressive polynomial array.}
    \item{B}{(bxpxp) is the moving-average polynomial array.}
    \item{C}{(cxpxm) is the  input polynomial array. C should be NULL if 
    there is no input}
    \item{y}{is the p dimensional output data.}
    \item{u}{is the m dimensional control (input) data.}
    \item{TREND}{is a matrix the same dimension as y, a p-vector
      (which gets replicated for each time period), or NULL.}
}

This is sometime called a vector ARMA (VARMA) model, but the univariate case is
also handled by this structure. VAR models are a  special case 
where \code{B(L) = I}.  ARIMA models are also special cases where the 
polynomial arrays have unit roots, but these are not distinguished in a
separate term as is sometimes done in other programs.

The name of last term, \code{TREND}, is misleading. If it is NULL it is treated
as zero. If it is a p-vector, then this constant vector is added to the
the p-vector \code{y(t)} at each period. For a stable model this would give the
none zero mean, and might more appropriately be called the constant or intercept
rather than trend. If the model is for differenced data, then this mean is the
trend of the undifferenced model. The more general case is when \code{TREND} is
a time series matrix of the same dimension as \code{y}. In this case it is added
to \code{y}. This allows for a very general deterministic component, which may
or may not be a traditional trend.

By default, elements in parameter arrays are treated as constants if they
are exactly 1.0 or 0.0, and as parameters otherwise. A value of 1.001 would
be treated as a parameter, and this is the easiest way to initialize an
element which is not to be treated as a constant of value 1.0. Any array
elements can be fixed to constants by specifying the list \code{constants}.
Arrays which are not specified in the list will be treated in the default 
way. An alternative for fixing constants is the function \code{fixConstants}.

The function \code{ARMA} sets up a model but does not estimate it. See 
\code{\link{estVARXls}} for one possibility for estimating VAR models and
\code{\link{estMaxLik}} for one possibility for estimating ARMA models.
}
\seealso{
    \code{\link{TSmodel}},
    \code{\link{simulate.ARMA}},
    \code{\link{fixConstants}}
    \code{\link{estVARXls}}
    \code{\link{estMaxLik}}
}
\examples{
    mod1 <- ARMA(A=array(c(1,-.25,-.05), c(3,1,1)), B=array(1,c(1,1,1)))
    AR   <- array(c(1, .5, .3, 0, .2, .1, 0, .2, .05, 1, .5, .3) ,c(3,2,2))
    VAR  <- ARMA(A=AR, B=diag(1,2))
    C    <- array(c(0.5,0,0,0.2),c(1,2,2))
    VARX <- ARMA(A=AR, B=diag(1,2), C=C) 
    MA   <- array(c(1, .2, 0, .1, 0, 0, 1, .3), c(2,2,2)) 
    ARMA  <- ARMA(A=AR, B=MA, C=NULL) 
    ARMAX <- ARMA(A=AR, B=MA, C=C) 
}

\concept{DSE}
\keyword{ts}

