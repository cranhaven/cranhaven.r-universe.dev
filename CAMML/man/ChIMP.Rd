\name{ChIMP}
\alias{ChIMP}
\title{CAMML with the Integration of Marker Proteins (ChIMP)}
\description{
ChIMP takes in the output of CAMML and a list of the CITE-seq markers designated for each cell type. For each marker, a k=2 means clustering will be applied to discretize their presence, resulting in a 0 in cells where the marker expression is in the lower value cluster and a 1 in cells where the marker expression is in the higher value cluster. Additionally, if a quantile cutoff is desired instead, this method can be designated and a cutoff can be given (the default is .5). These discretized scores are then multiplied by the CAMML score for each cell type in each cell. The function also takes in a vector of booleans the length of the number of cell types being evaluated that designates whether each cell type is required to have all markers score 1 or any marker score a 1 in order for the CAMML score to be maintained. If the boolean is true, ChIMP will weight CAMML by the maximum marker score for each cell type. For example, if both CD4 and CD8 are listed markers for T cells and either marker scoring a 1 is sufficient, the boolean will be true. If it is false, all markers designated for a cell type need to be in the higher value cluster for a given cell. ChIMP can also use the absence of a CITE-seq marker as support for a cell type by designating it "FALSE" with the greater argument. For example, if one is looking to identify non-immune cell types, CD45 can be used with greater = FALSE to support cell-type scores for a non-immune cell type.
}
\usage{
    ChIMP(seurat, citelist, method = "k", cutoff = .5, 
    anyMP = rep(T, length(rownames(seurat))),
    greater = rep(T, length(unlist(citelist))))
}
\arguments{
\item{seurat}{A Seurat Object that has previously been run on CAMML.} 
\item{citelist}{A list of all the surface markers for each cell type, named by their cell type.}
\item{method}{Either a "k" or a "q" to designate the desired method. "k" will use a k=2 k-means clustering method for discretization. "q" will use a quantile cutoff method.} 
\item{cutoff}{A value between 0 and 1 designating the cutoff to be used if the quantile method is selected.} 
\item{anyMP}{A vector of booleans regarding whether the CITE-seq weighting will take any positive marker protein score (TRUE) or requires all positive marker scores (FALSE)}
\item{greater}{A vector of booleans for every CITE-seq marker designating whether to evaluate it as present (TRUE) in a cell type or absent (FALSE) in a cell type.}
}
\value{
A \code{SeuratObject} with a ChIMP assay with the scores for each cell type in each cell, weighted by their CITE-seq score. This will be in the form of a matrix with columns for each cell and rows for each cell type that was scored.
}
\examples{
# Only run example code if Seurat package is available
if (requireNamespace("Seurat", quietly=TRUE) && 
requireNamespace("SeuratObject", quietly=TRUE)) {
  # See vignettes for more examples
  seurat <- CAMML(seurat=SeuratObject::pbmc_small,
  gene.set.df = data.frame(cbind(ensembl.id = c("ENSG00000172005",
  "ENSG00000173114","ENSG00000139187"),
  cell.type = c("T cell","T cell","T cell"))))
  cite <- matrix(c(rnorm(40), rnorm(40,2,1)),
  nrow = length(rownames(seurat@assays$CAMML)),
  ncol = length(colnames(seurat@assays$CAMML)))
  rownames(cite) <- "marker"
  colnames(cite) <- colnames(seurat)
  assay <- SeuratObject::CreateAssayObject(counts = cite)
  seurat[["ADT"]] <- assay
  citelist <- list()
  citelist[[1]] = "marker"
  names(citelist) = "T cell"
  seurat <- ChIMP(seurat, citelist)
  seurat@assays$ChIMP@data
}
}
\seealso{\code{\link{vamForSeurat}}}