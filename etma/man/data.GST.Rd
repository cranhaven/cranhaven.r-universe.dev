\name{data.GST}
\alias{data.GST}
\docType{data}
\title{
The data of GSTs family interaction on cancer.
}
\description{
This data used the data from a meta-analysis included about 500 studies investigating the associations between GSTM1/GSTT1 and cancer, and saved the studies both reported the genotype of GSTM1 and GSTT1. Finally, there are 360 studies (375 populations) included in this data.
}
\usage{data("data.GST")}
\format{
  A data frame with 375 observations on the following 12 variables.
  \describe{
    \item{\code{Study}}{the first author and published year in included studies.}
    \item{\code{Ethnicity}}{the ethnicity of each population.}
    \item{\code{Country}}{the location of study.}
    \item{\code{Cancer}}{the subtype of cancer in each study.}
    \item{\code{case.GSTM1.0}}{the number of GSTM1 functional type in cases.}
    \item{\code{ctrl.GSTM1.0}}{the number of GSTM1 null type in cases (risk type).}
    \item{\code{case.GSTM1.1}}{the number of GSTM1 functional type in controls.}
    \item{\code{ctrl.GSTM1.1}}{the number of GSTM1 null type in controls (risk type).}
    \item{\code{case.GSTT1.0}}{the number of GSTT1 functional type in cases.}
    \item{\code{ctrl.GSTT1.0}}{the number of GSTT1 null type in cases (risk type).}
    \item{\code{case.GSTT1.1}}{the number of GSTT1 functional type in controls.}
    \item{\code{ctrl.GSTT1.1}}{the number of GSTT1 null type in controls (risk type).}
  }
}

\references{
Fang J, Wang S, Zhang S, Su S, Song Z, Deng Y, et al. (2013) Association of the Glutathione S-Transferase M1, T1 Polymorphisms with Cancer: Evidence from a Meta-Analysis. PLoS ONE 8(11): e78707. doi:10.1371/journal.pone.0078707
%%  ~~ possibly secondary sources and usages ~~
}
\seealso{
\code{\link{ETMA}}
}
\examples{

data(data.GST)
head(data.GST)

}
\keyword{datasets}
