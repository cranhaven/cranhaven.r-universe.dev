\name{FLXMRnlm}
\alias{FLXMRnlm}
\alias{FLXnlm}
\title{flexmixNL Interface for Generalized Nonlinear Models}
\description{
  This is the main driver for \code{\link{flexmixNL}} interfacing the 
  family of Generalized Nonlinear Models.
}
\usage{
FLXMRnlm(formula = . ~ ., 
         family = c("gaussian", "Gamma"), 
         start = list(), 
         offset = NULL)
}
\arguments{
  \item{formula}{A model \code{\link{formula}} decribing the nonlinear predictor 
  and including variables and  regression parameters. }
  \item{family}{A character string naming a family function 
  (\code{family="gaussian"} or \code{family="Gamma"} available).}
  \item{start}{A list of starting values for the regression parameters.}
  \item{offset}{Specification of an \emph{a priori} known
    component to be included in the nonlinear predictor during fitting.}
}
\details{
Models for \code{\link{FLXMRnlm}} are specified by a model formula 
(\code{\link{formula}} argument) relating the response to a 
nonlinear predictor. 
When fitting normal mixture models (\code{family="gaussian"}) the nonlinear 
predictor is explicitely formulated (see also \code{\link{nls}}). 
When fitting gamma mixture models (\code{family="Gamma"}) the nonlinear 
predictor is specified by a symbolic description (see also \code{\link{gnm}}). 

Variables not included in the data frame (see also \code{\link{flexmix}}) 
are identified as the regression parameters. Starting values are required for 
every regression parameter. 

See \code{\link{flexmixNL}} for examples.
}
\value{Returns an object of class \code{FLXMRnlm}.}

\author{Sanela Omerovic}


\seealso{\code{\link{flexmixNL}}, \code{\link{formula}}, 
\code{\link{gnm}}, \code{\link{nls}}}

\keyword{regression}
\keyword{nonlinear}
\keyword{models}