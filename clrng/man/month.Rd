\docType{data}
\name{month}
\alias{month}
\title{Sample dataset: month.RData}
\format{
A data frame with 12 rows and 12 columns:
\describe{
    \item{Ane}{anencephaly}
    \item{Men}{meningomyelocele/spina bifida}
    \item{Cya}{cyanotic congenital heart disease}
    \item{Her}{congenital diaphragmatic hernia}
    \item{Omp}{omphalocele}
    \item{Gas}{gastrochisis}
    \item{Lim}{limb reduction defect}
    \item{Cle}{cleft lip with or without cleft palate}
    \item{Pal}{cleft palate alone}
    \item{Dow}{Down syndrome}
    \item{Chr}{suspected chromosomal disorder}
    \item{Hyp}{hypospadias.}
  }
}
\source{
Centers for Disease Control and Prevention (CDC)
}
\usage{
data(month)
}
\examples{
data(month)
head(month)
}
\description{
Counts of birth anomalies by birth month and 12 congenital 
anomalies categories in 2018 within the United States.
}
\references{
https://www.cdc.gov/nchs/data_access/VitalStatsOnline.htm
}
\keyword{datasets}
