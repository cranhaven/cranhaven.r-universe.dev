\name{GetGeneSets}
\alias{GetGeneSets}
\title{Get Pre-Built Gene Sets to be applied to CAMML}
\description{
GetGeneSets takes in a one of the following: "immune.cells","skin.immune.cells","T.subset.cells", or "mouse.cells" and a Seurat Object that will be cell-typed using CAMML. The function will then build a gene.set.collection and a list of gene.weights based on one of the pre-built gene sets.
}
\usage{
    GetGeneSets(data = "immune.cells")
}
\arguments{
\item{data}{One of the following: "immune.cells","skin.immune.cells","T.subset.cells", or "mouse.cells". 
\itemize{
\item "immune.cells" provides gene sets for 5 human immune cells: B, T, NK, Monocytes, and HSCs. 
\item "skin.immune.cells" provides gene sets for 6 human cell types: B, T, NK, Endothelial, Fibroblast, and Monocytes.
\item "T.subset.cells" provides 6 gene sets for 5 human T cell subtypes: naive CD4, CD4, naive CD8, CD8, memory, and regulatory T cells.
\item "mouse.cells" provides gene sets for 7 mouse cell types: B, T, NK, DC, Endothelial, Fibroblasts, and Macrophages.
}
All datasets were built using differential expression of data in the package celldex using the package EdgeR.} 
}
\value{
A \code{data.frame} with the cell type, gene name, ensembl ID, and weight for each gene in each gene set.
}
\examples{
GetGeneSets("immune.cells")
}
\seealso{\code{\link{org.Hs.eg.db},\link{org.Mm.eg.db}}}
