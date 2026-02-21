\name{scRNAtools_PEA}
\alias{scRNAtools_PEA}
\title{Pathway enrichment analysis}
\description{
Pathway enrichment analysis using the interested gene set or differentially expressed gene set provided by users. This data contains two column (Enterz ID and gene sybmols)
}
\usage{
scRNAtools_PEA(DEGs,number)
}
\arguments{
  \item{DEGs}{
Interested gene set of differentially expressed gene set.
}
  \item{number}{
The number of random, for example, users can set 1000, 5000 or more.
}
}
\details{This function integrated method to do the pathway enrichment analysis, TPEA.}
\value{The significant pathways are wrote in the occurrent path.}
\references{
Wei Jiang (2017). TPEA: A Novel Topology-Based Pathway Enrichment Analysis Approach.
}
\author{Qian Yang}
\examples{
data(DEGs)
number<-10
pdf(file=file.path(tempdir(), "enrichment analysis.pdf"))
scRNAtools_PEA(DEGs,number)
dev.off()
}