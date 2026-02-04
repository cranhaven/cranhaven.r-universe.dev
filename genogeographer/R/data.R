#' Kenn Kidd Lab markers
#'
#' List of markers identified by Kenn Kidd lab.
#'
#' @format List of 55 markers
#' \describe{
#'   \item{locus}{Locus/Marker names}
#' }
#' @source K.K. Kidd et al.Progress toward an efficient panel of SNPs for ancestry inference.
#' Forensic Science International: Genetics 10 (2014) 23–32
"kidd_loci"

#' Seldin Lab markers
#'
#' List of markers identified by Seldin lab.
#'
#' @format List of 122 markers
#' \describe{
#'   \item{locus}{Locus/Marker names}
#' }
#' @source Kosoy et al.  
#' Ancestry Informative Marker Sets for Determining Continental Origin and Admixture Proportions in Common Populations in America. 
#' HUMAN MUTATION, Vol. 30, No. 1, 69–78, 2009.
"seldin_loci"

#' AIMs markers in Precision ID Ancestry Panel (Thermo Fisher Scientific)
#'
#' List of markers with their main and alternative allele. 
#' The markers is the union of Seldin's and Kidd's markers.
#'
#' @format List of 164 markers
#' \describe{
#'   \item{locus}{Locus/Marker names}
#'   \item{main_allele}{The main allele (alleles are in lexicographic order)}
#'   \item{other_allele}{The other variant}
#' }
"main_alleles"