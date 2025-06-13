\name{getBackgroundLnc}
\alias{getBackgroundLnc}
\title{
Get the background of molecules
}
\description{
getBackgroundLnc attempts to get the background of user-specified molecules.
}
\usage{
getBackgroundLnc(type = "gene_lncRNA")
}
\arguments{
  \item{type}{
A character string. Should be one of "gene", "lncRNA" or "gene_lncRNA".}
}
\details{
The default background is obtained from the environment variable. For human, the reference gene background is all human genes in KEGG pathways. The reference lncRNA background is collected from GeneCode database. 
}
\value{
A character vector. 
}

\author{
Xinrui Shi, Chunquan Li and Xia Li
}

\seealso{\code{\link{identifyLncGraphW}}}
\examples{
\dontrun{
## get all background of genes
 bgGene <- getBackgroundLnc(type="gene")

## get all background of lncRNAs
 bgLncRNA  <- getBackgroundLnc(type="lncRNA")

## get all background of lncRNAs and genes
 bgGL <- getBackgroundLnc(type="gene_lncRNA")
}
}
\keyword{file}
