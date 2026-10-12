\name{clr.test}
\alias{clr.test}
\title{Conditional Likelihood Ratio Test}
\description{Likelihood ratio test for objects of class SYMARMA.}

\usage{
clr.test(model1, model2)
}

\arguments{
 \item{model1, model2}{two models of class SYMARMA having the same set of records and the same type (\dQuote{family}).}
}

\details{
Likelihood ratio test checks the difference between -2*logLikelihood of the two models against the change in degrees of freedom
using a chi-squared test. The records used in the dataset for both models MUST be the same.
}

\author{
Vinicius Quintas Souto Maior and Francisco Jose A. Cysneiros

Maintainer: Vinicius Quintas Souto Maior <vinicius@de.ufpe.br>
}

\examples{
serie <- symarma.sim(model=list(ar=c(0.2,0.5)),n=70,family="Normal",
 varphi=1)
model0 <- elliptical.ts(serie,order=c(2,0,0))
model1 <- elliptical.ts(serie,order=c(1,0,0))
clr.test(model0,model1)
}