\name{SNPlocations}
\alias{SNPlocations}
\docType{data}
\title{
Three GWAS studies SNPs locations and data
}
\description{
\code{SNPlocations} includes the locations of SNPs in chromosomes 1 to 4.
Data was simulated to the SNPs with HAPGEN2 for three studies and a sample of it was taken (Chromosomes 1 to 4) for the examples. The data is summarized as z-scores(transformed p-values, with inverse standard normal cumulative distribution).
The z-scores matrix can be download from the web (see example).
}
\usage{data(SNPlocations)}
\format{\code{SNPlocations} data.frame of 249024 SNPs' names, chromosome number and location on the chromosomes.
\code{zmat} Matrix of 249024 SNPs' z-scores (in rows) in each of the 3 studies (columns).
}
\source{
See:
Su, Zhan, Jonathan Marchini, and Peter Donnelly. "HAPGEN2: simulation of multiple disease SNPs." Bioinformatics 27.16 (2011): 2304-2305.
}
\examples{
data(SNPlocations)
head(SNPlocations)

\dontrun{
download.file('http://www.math.tau.ac.il/~ruheller/repfdr_RData/zmat.RData',destfile = "zmat.RData")
load(file = "zmat.RData")

input.to.repfdr <- ztobins(zmat, 3, df= 15)
pbz <- input.to.repfdr$pdf.binned.z
bz  <- input.to.repfdr$binned.z.mat
}
}
\keyword{datasets}
\keyword{Replicability}
\keyword{GWAS}
