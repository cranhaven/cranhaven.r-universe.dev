\name{CAMML}
\alias{CAMML}
\title{Cell-typing using variance Adjusted Mahalanobis distances with Multi-Labeling (CAMML)}
\description{
Multi-label cell-typing method for single-cell RNA-sequencing data. CAMML takes in cell type-specific gene sets, with weights for each gene, and builds weighted Variance-Adjusted Mahalanobis (VAM) scores for each of them. CAMML then outputs a Seurat Object with an assay for CAMML that has the weighted VAM score for each cell type in each cell. CAMML takes in several arguments: \code{seurat}: a Seurat Object of the scRNA-seq data, \code{gene.set.df}: a data frame with a row for each gene and the following required columns: "ensemb.id" and "cell.type" and optional columns of "gene.weight" and "gene.symbol".
}
\usage{
    CAMML(seurat, gene.set.df)
}
\arguments{
\item{seurat}{A Seurat Object that has previously been normalized and scaled.} 
\item{gene.set.df}{A list of lists of genes in each gene set, with each gene set list named for the cell type it represents.}
}
\value{
A \code{SeuratObject} with a CAMML assay with the scores for each cell type in each cell. This will be in the form of a matrix with columns for each cell and rows for each cell type that was scored.
}
\examples{
# Only run example code if Seurat package is available
if (requireNamespace("Seurat", quietly=TRUE) & requireNamespace("SeuratObject", quietly=TRUE)) {
  # See vignettes for more examples
  seurat <- CAMML(seurat=SeuratObject::pbmc_small,
  gene.set.df = data.frame(cbind(ensembl.id = c("ENSG00000172005",
  "ENSG00000173114","ENSG00000139187"),
  cell.type = c("T cell","T cell","T cell"))))
  seurat@assays$CAMML@data
}
}
\seealso{\code{\link{vamForSeurat}}}

