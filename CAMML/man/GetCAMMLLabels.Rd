\name{GetCAMMLLabels}
\alias{GetCAMMLLabels}
\title{Single- and Multi-Labelling for CAMML}
\description{
This function takes in the Seurat Object output from the CAMML function and returns one of four labelling options. "top1" will return the top cell type for each cell. "top2" will return the top two highest scoring cell types for each cell. "top10p" will return the top scoring cell type and all other cell types with 10\% of that score for each cell. "2xmean" will return all cell types with scores greater than twice the mean of all scores for a given cell.
}
\usage{
GetCAMMLLabels(seurat, labels = "top1")
}
\arguments{
  \item{seurat}{A Seurat Object with a CAMML assay with weighted VAM scores for each cell type in each query cell. This is the output from the CAMML function.}
  \item{labels}{One of the following: "top1", "top2", "top10p", or "top2xmean". 
  "top1" will return the single-label for the top-scoring cell type for each cell.
  "top2" will return the labels for the two top-scoring cell types for each cell.
  "top10p" will return the top scoring cell type and any other cell types with scores within 10\% of the top score for each cell.
  "top2xmean" will return any cell types with scores two times the average of all cell type scores for each cell.}
  }

\value{
A list with the labels designated by the "labels" argument.
}
\seealso{\code{\link{CAMML}}}
\examples{
# Only run example code if Seurat and CAMML packages are available
if (requireNamespace("Seurat", quietly=TRUE) & 
requireNamespace("SeuratObject", quietly=TRUE) & 
requireNamespace("CAMML", quietly=TRUE)) {
  # See vignettes for more examples
  seurat <- CAMML(seurat=SeuratObject::pbmc_small,
  gene.set.df=data.frame(cbind(ensembl.id = c("ENSG00000172005",
  "ENSG00000173114","ENSG00000139187"),
  cell.type = c("T cell","T cell","T cell"))))
  GetCAMMLLabels(seurat, labels = "top1")
}
}