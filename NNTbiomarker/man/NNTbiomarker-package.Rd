\name{NNTbiomarker-package}
\alias{NNTbiomarker-package}
\alias{NNTbiomarker}
\docType{package}
\title{
Plan a biomarker validation study by focusing on desired clinical actionability.
}
\description{
Clarifying what performance would suffice if the test is to improve medical care makes it possible to design meaningful validation studies.
}
\details{
\tabular{ll}{
Package: \tab NNTbiomarker\cr
Type: \tab Package\cr
Version: \tab 0.1\cr
Date: \tab 2015-03-21\cr
License: \tab What license is it under?\cr
}
This package bases the design of a biomarker study on the idea of "number needed to treat" (NNT). It postulates a "range of discomfort" for NNT, within which the clinical decision is uncomfortable for a treating physician.  provides a \code{shiny} window for eliciting the boundaries of the range of number needed to  }
\author{
Roger Day

Maintainer: Roger Day <day01@pitt.edu>
}
\references{
See author for unpublished manuscript.
}
\keyword{ package }
\examples{
NNT.to.pv(NNTpos=5, NNTneg=28)
NNT.from.sesp(se=0.7, sp=0.9, prev=0.1)
pv.to.sesp(pv = cbind(ppv=seq(.5, .9, .1), npv=0.9), prev = 0.2)

}
