\name{cc.ROC}
\alias{cc.ROC}
\title{
  Computes the optimal number of controls per case in hypothesis tests
  involving the ROC. Included here with the intent that it can be
  used in conjunction with \code{\link{pwrFDR}} to allow power/sample
  size calculation for multiple tests of ROC curve based hypothesis.
  See details. 
}
\description{
  In hypothesis tests of TPR_1 vs TPR_0 at fixed FPR, or
  FPR_1 vs FPR_0 at fixed TPR, this computes the optimal number
  of controls per case. Required by \code{\link{es.ROC}}
}
\usage{
  cc.ROC(FPR0, FPR1 = NULL, TPR0, TPR1 = NULL, b = NULL)
  }
\arguments{
  \item{FPR0}{
    When the TPR is fixed, the FPR under the null. Otherwise the fixed FPR.
  }
  \item{FPR1}{
    When the TPR is fixed, the FPR under the alternative. Otherwise left blank.
  }
  \item{TPR0}{
    When the FPR is fixed, the TPR under the null. Otherwise the fixed TPR.
  }
  \item{TPR1}{
    When the FPR is fixed, the TPR under the alternative. Otherwise left blank.
  }
  \item{b}{
    Nominal slope of the ROC at FPR0. Taken to be 1 by default.
  }
}
\value{
    The optimal number of controls per case.
}
\references{
  Pepe M. S., Feng Z, Janes, H Bossuyt P. M. and Potter J. D. Pivotal
  evaluation of the accuracy of a biomarker used for classification or
  prediction. Supplement. J Natl Cancer Inst 2008;100: 1432--1438
}
\author{
  Grant Izmirlian <izmirlian at nih dot gov>
}
\seealso{
  \code{\link{es.ROC}}
}
\examples{
    cc.ROC(FPR0=0.15, TPR0=0.80, TPR1=0.90)
}
\keyword{FDR}
\keyword{Benjamini}
\keyword{Hochberg}
\keyword{microarrays}
\keyword{Multiple.Testing}
\keyword{average.power}
\keyword{k.power}
\keyword{lambda.power}
