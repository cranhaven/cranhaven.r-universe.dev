\name{regDataGen}
\alias{regDataGen}
\title{Artificial data for testing regression algorithms}
\description{
  The generator produces regression data data with 4 discrete and 7 numeric attributes.
  }
\usage{
  regDataGen(noInst, t1=0.8, t2=0.5, noise=0.1)
}
\arguments{
  \item{noInst}{Number of instances to generate.}
  \item{t1, t2}{ Parameters controlling the shape of the distribution.}
  \item{noise}{Parameter controlling the amount of noise. If \code{noise=0}, there is no noise. If noise = 1, then the level
               of the signal and noise are the same.}
}
\value{
Returns a \code{\link{data.frame}} with \code{noInst} rows and 11 columns. 
Range of values of the attributes and response are
\item{a1}{0,1}
\item{a2}{ a,b,c,d}
\item{a3}{ 0,1 (irrelevant)}
\item{a4}{ a,b,c,d (irrelevant)}
\item{x1}{ numeric (gaussian with different sd for each class)}
\item{x2}{ numeric (gaussian with different sd for each class)}
\item{x3}{ numeric (gaussian, irrelevant)}
\item{x4}{ numeric from [0,1]}
\item{x5}{ numeric from [0,1]}
\item{x6}{ numeric from [0,1]}
\item{response}{ numeric}
}
\details{

The response variable is derived from x4, x5, x6 using two different
functions. The choice depends on a hidden variable, which determines weather the 
response value would follow a linear dependency \eqn{f=x_4-2x_5+3x_6},
or a nonlinear one \eqn{f=cos(4\pi x_4)(2x_5-3x_6)}.

Attributes a1, a2, x1, x2 carry some information on the hidden
variables depending on parameters t1, t2. Extreme values of the
parameters are t1=0.5 and t2=1, when there is no information.
On the other hand, if t1=0 or t1=1 then each of the attributes
a1, a2 carries full information. If t2=0, then each of x1, x2
carries full information on the hidden variable.

The attributes x4, x5, x6 are available with a noise level depending
on parameter \code{noise}. If \code{noise=0}, there is no noise. If \code{noise=1}, then the level
of the signal and noise are the same.

}
\author{ Petr Savicky }

\seealso{
\code{\link{classDataGen}},\code{\link{ordDataGen}},\code{\link{CoreModel}},
}

\examples{
#prepare a regression data set
regData <-regDataGen(noInst=200)

# build regression tree similar to CART
modelRT <- CoreModel(response ~ ., regData, model="regTree", modelTypeReg=1)
print(modelRT)

destroyModels(modelRT) # clean up

}

\keyword{ data }
\keyword{datagen}
