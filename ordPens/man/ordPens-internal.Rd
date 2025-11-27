\name{ordPens-internal}
\title{Internal ordPens functions}
\alias{cd}
\alias{coding}
\alias{genRidge}
\alias{ordAOV1}
\alias{ordAOV2}
\alias{crO}
\alias{penALS}
\alias{ord.glasso}
\alias{ordglasso_control}
\alias{invlink}
\alias{nloglik}
\alias{nscore}
\alias{nhessian}
\alias{smooth.construct.ordinal.smooth.spec}
\alias{Predict.matrix.ordinal.smooth}
\alias{plot.ordinal.smooth}
\description{Internal ordPens functions}
\usage{
cd(x)
coding(x, constant=TRUE, splitcod=TRUE)
genRidge(x, y, offset, omega, lambda, model, delta=1e-6, maxit=25)
ordAOV1(x, y, type, nsim, null.sample, ...)
ordAOV2(x, y, type, nsim, null.sample, ...)
crO(k, d=2)
penALS(H, p, lambda, qstart, crit, maxit, Ks, constr)
ord.glasso(x, y, lambda, weights = rep(1, length(y)), penscale = sqrt, 
              standardize = TRUE, restriction = c("refcat", "effect"), 
              nonpenx = NULL, control=ordglasso_control())
ordglasso_control(control = list())
invlink(eta)


\method{smooth.construct}{ordinal.smooth.spec}(object, data, knots)
\method{Predict.matrix}{ordinal.smooth}(object, data)
\method{plot}{ordinal.smooth}(x, P=NULL, data=NULL, label="", se1.mult=1, se2.mult=2,
partial.resids=FALSE, rug=TRUE, se=TRUE, scale=-1, n=100, n2=40, n3=3,
pers=FALSE, theta=30, phi=30, jit=FALSE, xlab=NULL, ylab=NULL, main=NULL,
ylim=NULL, xlim=NULL, too.far=0.1, shade=FALSE, shade.col="gray80",
shift=0, trans=I, by.resids=FALSE, scheme=0, ...)
}
\author{Jan Gertheiss, Fabian Scheipl, Aisouda Hoshiyar}
\details{
  These are not to be called by the user.
}
\keyword{internal}
