\name{aspirin}
\docType{data}
\alias{aspirin}
\title{Aspirin use and breast cancer risk}
\description{
This data set can be used to examine the recent epidemiological studies on aspirin use and breast cancer risk published from 2001 to 2007 within a meta-analysis, and to investigate reasons for heterogeneity between the individual studies. \cr

We systematically searched for cohort-studies and case-control-studies from 2001-2007, which evaluated the association between aspirin and breast cancer risk. A total of 15 studies (seven cohort studies and eight case-control studies) met the inclusion criteria.
}
\usage{data("aspirin")}
\format{A data frame consisting of 15 data sets (rows) and 11 attributes (columns)}
\examples{
#Example
#Homogeneous Metaregression adjusting for study type and year of publication

data(aspirin)
wgt <- 1/aspirin$var# calculate weights
m0 <- mixcov(dep="logrr", fixed=c("type","yearc"), random="", weight=wgt,
             k=1,family="gaussian",data=aspirin)

}
\references{
Schlattmann, P.(2009) \emph{Medical Applications of Finite Mixture Models.} Berlin: Springer.
}
\keyword{datasets}
