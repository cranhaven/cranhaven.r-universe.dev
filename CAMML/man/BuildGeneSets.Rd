\name{BuildGeneSets}
\alias{BuildGeneSets}
\title{Build Gene Sets for reference data to be applied to CAMML}
\description{
BuildGeneSets takes in an expression matrix, either as a Seurat Object or as a simple matrix (where cells are the columns, and genes the rows), and labels for each of the cells. EdgeR differential expression analysis is then run within the function and gene sets are built based on a log fold change (FC) cut-off (the default is 2). Cut-offs can also be set by FC and -log10(p-value). Of note, when more than one cut-off is given, genes must meet ALL criteria. Gene weights are recorded for each gene in the gene set as the log2(FC), FC, or -log10(p-value) (the default is log2(FC)). Each gene in the gene set is converted into its corresponding Ensembl ID, necessitating that users also provide the species of interest. Currently humans "Hs", mice "Mm", and zebrafish "Dr" are accepted (the default is humans).
}
\usage{
    BuildGeneSets(exp.data, labels = as.character(Idents(exp.data)), 
    cutoff.type = "logfc", cutoff = 2, species = "Hs", weight.type = "logfc")
}
\arguments{
\item{exp.data}{A Seurat Object or expression matrix for the reference data that has previously been normalized and scaled.} 
\item{labels}{A vector of the cell type labels for each cell in the expression matrix. This must have a label for each cell and be in the order the cells appear as columns in the expression matrix. The default will be the Idents of the Seurat Object.} 
\item{cutoff.type}{One or more of the following: "logfc", "fc", and "-logp". This value will determine what value(s) genes must have to be included in a given gene set. "logfc" and "fc" cut-off genes based on their log fold change and fold change values respectively. "-logp" will cut-off genes based on the significance of their differential expression according to the -log10(p-value). When more than one cut-off is given, genes must meet ALL criteria.}
\item{cutoff}{A number or vector of numbers that correspond to the cutoff.types. The value should be greater than 0 if cutoff.type is "logfc", greater than 1 if cutoff.type is "fc", and greater than 1 if cutoff.type is "-logp". The number of values given should match the number of cutoff types listed and should be in the same order as the cutoff types vector.}
\item{species}{Either "Hs", "Mm", or "Dr" for human, mouse, and zebrafish respectively. Used to convert gene symbols into Ensembl IDs.}
\item{weight.type}{Can be one of the following: "logfc","fc", or "-logp". The former two options will assign gene weights by their log2FC or FC respectively in differential expression analysis. The final option will assign weight by the negative log10 of the p-values for each gene from differential expression analysis.}
}
\value{
A \code{data.frame} with the cell type, gene name, ensembl ID, and weight for each gene in each gene set.
}
\examples{
#Only run code if Seurat package is available
if (requireNamespace("Seurat", quietly=TRUE) & requireNamespace("SeuratObject", quietly=TRUE)) {
  #See vignettes for more examples
  BuildGeneSets(exp.data=SeuratObject::pbmc_small, 
  labels = c(rep(1,40),rep(2,40)), cutoff.type = "logfc", 
  cutoff = 2, species = "Hs", weight.type = "logfc")
}
}
\seealso{\code{\link{edgeR},\link{org.Hs.eg.db},\link{org.Mm.eg.db}}}
