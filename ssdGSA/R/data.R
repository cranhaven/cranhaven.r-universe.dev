#' This is data to be included in package
#' @name data_matrix
#' @docType data
#' @format An example data matrix of gene expressions with gene ensembl ID as row names and
#' columns corresponding to different samples.
#' \describe{
#'   \item{S1}{gene expression for this gene from the 1st sample}
#'   \item{S2}{gene expression for this gene from the 2nd sample}
#'   \item{S3}{gene expression for this gene from the 3rd sample}
#'   \item{S4}{gene expression for this gene from the 4th sample}
#'   \item{S5}{gene expression for this gene from the 5th sample}
#'   \item{S6}{gene expression for this gene from the 6th sample}
#'   \item{S7}{gene expression for this gene from the 7th sample}
#'   \item{S8}{gene expression for this gene from the 8th sample}
#'   \item{S9}{gene expression for this gene from the 9th sample}
#'   \item{S10}{gene expression for this gene from the 10th sample}
#'
#' }
#'
"data_matrix"



#' This is data to be included in package
#' @name data_matrix_entrezID
#' @docType data
#' @format An example data matrix of gene expressions with gene entrez ID as row names and
#' columns corresponding to different samples.
#' \describe{
#'   \item{S1}{gene expression for this gene from the 1st sample}
#'   \item{S2}{gene expression for this gene from the 2nd sample}
#'   \item{S3}{gene expression for this gene from the 3rd sample}
#'   \item{S4}{gene expression for this gene from the 4th sample}
#'   \item{S5}{gene expression for this gene from the 5th sample}
#'   \item{S6}{gene expression for this gene from the 6th sample}
#'   \item{S7}{gene expression for this gene from the 7th sample}
#'   \item{S8}{gene expression for this gene from the 8th sample}
#'   \item{S9}{gene expression for this gene from the 9th sample}
#'   \item{S10}{gene expression for this gene from the 10th sample}
#'
#' }
#'
"data_matrix_entrezID"


#' This is data to be included in package
#' @name direction_matrix
#' @docType data
#' @format An example direction matrix containing directionality information from summary statistics
#' such as effect size (ES) or p value, with each row for one gene.
#' \describe{
#'   \item{gene}{gene entrez ID}
#'   \item{ES}{effect size (SE) for this gene from summary statistics}
#'   \item{pval}{p value for this gene from summary statistics}
#' }
#'
"direction_matrix"


#' This is data to be included in package
#' @name gene_sets
#' @docType data
#' @format An example disease gene sets in the form of a list, with gene set names as list component names,
#' and each component is a vector of gene entrez ID. In this sample gene sets list, there are 10 gene sets in total.
#' \describe{
#'   \item{TCELL.KEGG_T_CELL_RECEPTOR_SIGNALING_PATHWAY}{gene entrez ID related to this pathway}
#'   \item{TLR.KEGG_TOLL_LIKE_RECEPTOR_SIGNALING_PATHWAY}{gene entrez ID related to this pathway}
#'   \item{BCELL.KEGG_B_CELL_RECEPTOR_SIGNALING_PATHWAY}{gene entrez ID related to this pathway}
#'   \item{NEUROTROPHIN.KEGG_NEUROTROPHIN_SIGNALING_PATHWAY}{gene entrez ID related to this pathway}
#'   \item{ERBB.KEGG_ERBB_SIGNALING_PATHWAY}{gene entrez ID related to this pathway}
#'   \item{CALCIUM.KEGG_CALCIUM_SIGNALING_PATHWAY}{gene entrez ID related to this pathway}
#'   \item{CHEMOKINE.KEGG_CHEMOKINE_SIGNALING_PATHWAY}{gene entrez ID related to this pathway}
#'   \item{GNRH.KEGG_GNRH_SIGNALING_PATHWAY}{gene entrez ID related to this pathway}
#'   \item{VEGF.KEGG_VEGF_SIGNALING_PATHWAY}{gene entrez ID related to this pathway}
#' }
#'
"gene_sets"
