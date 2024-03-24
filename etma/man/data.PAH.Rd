\name{data.PAH}
\alias{data.PAH}
\docType{data}
\title{
The data of metabolic pathway of PAH interaction on oral cancer.
}
\description{
This data used the data from a meta-analysis included about 50 studies investigating the associations between CYP1A1/GSTM1 and oral cancer, and saved the studies both reported the genotype of GSTM1 and CYP1A1 rs4646903. Finally, there are 13 studies included in this data.
}
\usage{data("data.PAH")}
\format{
  A data frame with 13 observations on the following 11 variables.
  \describe{
    \item{\code{Athour}}{the first author in included studies.}
    \item{\code{Year}}{the published year in included studies.}
    \item{\code{Country}}{the location of study.}
    \item{\code{case.CYP1A1.0}}{the number of AA genotype (rs4646903) in cases.}
    \item{\code{case.CYP1A1.1}}{the number of AC/CC genotype (rs4646903) in cases (risk type).}
    \item{\code{ctrl.CYP1A1.0}}{the number of AA genotype (rs4646903) in controls.}
    \item{\code{ctrl.CYP1A1.1}}{the number of AC/CC genotype (rs4646903) in controls (risk type).}
    \item{\code{case.GSTM1.0}}{the number of GSTM1 functional type in cases.}
    \item{\code{case.GSTM1.1}}{the number of GSTM1 null type in cases (risk type).}
    \item{\code{ctrl.GSTM1.0}}{the number of GSTM1 functional type in controls.}
    \item{\code{ctrl.GSTM1.1}}{the number of GSTM1 null type in controls (risk type).}
  }
}

\references{
Liu H, Jia J, Mao X, Lin Z. (2015) Association of CYP1A1 and GSTM1 Polymorphisms With Oral Cancer Susceptibility: A Meta-Analysis. Medicine 94(27): e895. doi: 10.1097/MD.0000000000000895
%%  ~~ possibly secondary sources and usages ~~
}
\seealso{
\code{\link{ETMA}}
}
\examples{

data(data.PAH)
head(data.PAH)

}
\keyword{datasets}
