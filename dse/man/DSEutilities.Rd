\name{DSEutilities}
\alias{DSE.ar}
\alias{criteria.table.heading}
\alias{criteria.table.legend}
\alias{criteria.table.nheading}
\alias{dseclass}
\alias{dseclass<-}
\alias{estVARXmean.correction}
\alias{fake.TSestModel.missing.data}
\alias{printTestValue}
\alias{svd.criteria}

%%\alias{genda_sym}
%%\alias{gendk_sym}

\title{DSE Utilities}
\description{
    These functions are used by other functions in DSE and are not
    typically called directly by the user.}
\usage{
   estVARXmean.correction(X, y, bbar, fuzz=sqrt(.Machine$double.eps), warn=TRUE)
   fake.TSestModel.missing.data(model,data, residual, max.lag, warn=TRUE)   
   printTestValue(x, digits=16)
   svd.criteria(sv)
   criteria.table.heading()
   criteria.table.legend()
   criteria.table.nheading()
   DSE.ar(data, ...)
%%   genda_sym
%%   gendk_sym
}
\concept{DSE}
\keyword{internal}


