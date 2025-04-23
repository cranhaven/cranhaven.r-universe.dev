# Documentation and definitions for data and constants

#### Data ####

#' Human IGHV germlines
#'
#' A \code{character} vector of all 498 human IGHV germline gene segment alleles
#' in IMGT Gene-db release July 2022, with an additional 25 undocumented alleles from VDJbase.
#'
#' @name HVGERM
#' @docType data
#' @format Values correspond to IMGT-gaped nuceltoide sequences (with
#' nucleotides capitalized and gaps represented by '.').
#'
#' @references Xochelli \emph{et al}. (2014) Immunoglobulin heavy variable
#' (IGHV) genes and alleles: new entities, new names and implications for
#' research and prognostication in chronic lymphocytic leukaemia.
#' \emph{Immunogenetics}. 67(1):61-6.
#' @keywords data
"HVGERM"

#' Human IGHV germlines functionality description
#'
#' A \code{data.table} of all 498 human IGHV germline gene segment alleles
#' in IMGT Gene-db release July 2022, with an additional 25 undocumented alleles from VDJbase.
#' The first column is the allele name, the second column is the functionality annotation, the 
#' third column is the nt sequence and the last column is the aa sequence.
#'
#' @name hv_functionality
#' @docType data
#'
#' @references Xochelli \emph{et al}. (2014) Immunoglobulin heavy variable
#' (IGHV) genes and alleles: new entities, new names and implications for
#' research and prognostication in chronic lymphocytic leukaemia.
#' \emph{Immunogenetics}. 67(1):61-6.
#' @keywords data
"hv_functionality"


#' Allele thresholds table
#'
#' A \code{data.table} of the allele thresholds table. The V alleles are based on the
#' \code{HVGERM} and \code{hv_functionality} germline reference set. The D, and the J are based on 
#' the AIRR-C reference set (https://zenodo.org/records/10489725). The table contains these columns: allele - the IUIS allele name,
#' asc_allele - the allele name based on allele similarity clusters (only for V), threshold = the genotype threshold for the alleles.
#'
#' @name allele_threshold_table
#' @docType data
#'
#' @references Peres, et al (2022) <doi:10.1101/2022.12.26.521922>
#' @keywords asc table
"allele_threshold_table"


#' Allele similarity cluster table
#'
#' A \code{data.table} of the allele similarity cluster table based on the 
#' \code{HVGERM} and \code{hv_functionality} germlie reference set. This is not the latest
#' version of the allele similarity cluster table. For the latest version please refer either to the
#' zenodo doi or you can use the \code{recentAlleleClusters}
#'
#' @name allele_cluster_table
#' @docType data
#'
#' @references Peres, et al (2022) <doi:10.1101/2022.12.26.521922>
#' @keywords asc table
"allele_cluster_table"
