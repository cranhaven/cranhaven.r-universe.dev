#' Identification of Cancer Dysfunctional Subpathway by integrating DNA methylation, copy number variation, and gene expression data
#' @description Identify Cancer Dysfunctional Subpathway by integrating gene expression, DNA methylation and copy number variation, and pathway topological information. 1)We firstly calculate the gene risk scores by integrating three kinds of data: DNA methylation, copy number variation, and gene expression.  2)Secondly, we perform a greedy search algorithm to identify the key dysfunctional subpathways within the pathways for which the discriminative scores were locally maximal. 3)Finally, the permutation test was used to calculate statistical significance level for these key dysfunctional subpathways.
#' @name ICDS-package
#' @aliases ICDS-package ICDS
#' @docType package
"_PACKAGE"
