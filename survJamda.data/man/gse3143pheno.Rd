\name{gse3143pheno}
\alias{gse3143pheno}
\docType{data}
\title{
Phenotype data of GSE3143
}
\description{
Data such as patients' information, tumor characteristics, samples information related to GSE3143.
}
\usage{data(gse3143pheno)}
\format{
  A data frame with 158 observations on the following 4 variables.
  \describe{
    \item{\code{TTBNO}}{a factor with levels \code{KF-067} \code{KF-069} \code{KF-070} \code{KF-076} \code{KF-077} \code{KF-079} \code{KF-090} \code{KF-094} \code{KF-095} \code{KF-097} \code{KF-098} \code{KF-104} \code{KF-105} \code{KF-106} \code{KF-110} \code{KF-113} \code{KF-117} \code{KF-120} \code{KF-123} \code{KF-124} \code{KF-125} \code{KF-126} \code{KF-127} \code{KF-128} \code{KF-129} \code{KF-130} \code{KF-131} \code{KF-132} \code{KF-133} \code{KF-134} \code{KF-135} \code{KF-136} \code{KF-137} \code{KF-138} \code{KF-139} \code{KF-140} \code{KF-141} \code{KF-142} \code{KF-143} \code{KF-144} \code{KF-145} \code{KF-146} \code{KF-147} \code{KF-149} \code{KF-150} \code{KF-151} \code{KF-152} \code{KF-153} \code{KF-154} \code{KF-155} \code{KF-157} \code{KF-159} \code{KF-160} \code{KF-176} \code{KF-178} \code{KF-182} \code{KF-183} \code{KF-184} \code{KF-187} \code{KF-188} \code{KF-189} \code{KF-193} \code{KF-194} \code{KF-195} \code{KF-196} \code{KF-200} \code{KF-201} \code{KF-203} \code{KF-204} \code{KF-207} \code{T00-0050} \code{T00-0064} \code{T00-0070} \code{T00-0109} \code{T00-0214} \code{T00-0454} \code{T00-0622} \code{T01-0047} \code{T01-0052} \code{T01-0145} \code{T01-0154} \code{T01-0156} \code{T01-0158} \code{T01-0166} \code{T01-0194} \code{T01-0202} \code{T01-0204} \code{T01-0255} \code{T01-0310} \code{T01-0344} \code{T01-0352} \code{T01-0356} \code{T01-0357} \code{T01-0366} \code{T01-0378} \code{T01-0384} \code{T01-0387} \code{T01-0407} \code{T01-0410} \code{T01-0428} \code{T01-0439} \code{T01-0452} \code{T01-0461} \code{T01-0463} \code{T01-0466} \code{T01-0468} \code{T01-0469} \code{T01-0472} \code{T01-0476} \code{T01-0495} \code{T01-0496} \code{T01-0502} \code{T01-0503} \code{T01-0514} \code{T01-0522} \code{T01-0534} \code{T01-0537} \code{T01-0569} \code{T01-0585} \code{T01-0591} \code{T01-0605} \code{T01-0618} \code{T01-0659} \code{T01-0748} \code{T01-0772} \code{T01-0838} \code{T92-0037} \code{T92-0058} \code{T93-0002} \code{T94-0071} \code{T94-0072} \code{T94-0074} \code{T95-0028} \code{T95-0031} \code{T95-0040} \code{T95-0074} \code{T95-0162} \code{T95-0168} \code{T95-0194} \code{T96-0085} \code{T96-0094} \code{T96-0127} \code{T96-0136} \code{T96-0159} \code{T96-0167} \code{T96-0195} \code{T96-0250} \code{T97-0017} \code{T97-0019} \code{T97-0043} \code{T97-0051} \code{T97-0071} \code{T97-0075} \code{T97-0080} \code{T97-0090} \code{T97-0094} \code{T97-0191} \code{T97-0281}}
    \item{\code{ERlev}}{a numeric vector}
    \item{\code{Status(0=alive,1=dead)}}{a numeric vector}
    \item{\code{Survival_Time(months)}}{a numeric vector}
  }
}

\source{
\url{http://www.ncbi.nlm.nih.gov/projects/geo/query/acc.cgi?acc=GSE3143}
}
\references{
Yasrebi H,  Sperisen P,  Praz V,  Bucher P, 2009 Can Survival Prediction Be Improved By Merging Gene Expression Data Sets?. PLoS ONE 4(10): e7431. doi:10.1371/journal.pone.0007431.
}
\seealso{
 \code{\link{gse3143}} 
}

\examples{
data(gse3143pheno)
}
\keyword{datasets}
