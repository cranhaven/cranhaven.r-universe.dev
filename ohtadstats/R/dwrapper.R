#' Ohta D Statistic Wrapper
#' 
#' Pairwise computation of Ohta's D Statistics for each pair of polymorphisms in
#' a given dataset.
#' 
#' @param data_set Matrix containing genotype data with individuals as rows and
#' loci as columns. Genotypes should be coded as 0 (homozygous), 1 (heterozygous),
#' or 2 (homozygous). Rownames must be subpopulation names and column names
#' should be marker names.
#' @param tot_maf Minimum minor allele frequency across the total population for
#' a marker to be included in the analysis.
#' @param pop_maf Minimum minor allele frequency across a subpopulation for
#' that subpopulation to be included in analysis.
#'
#' @return A list of matrices containing the pairwise comparisons for each D statistic.
#' Also included is the number of subpopulations evaluated in each comparison
#' and the ratio of d2is_mat to d2st_mat (ratio1) and dp2st_mat to dp2is_mat (ratio2).
#' The result of a comparison between marker M and marker N will be found in the Mth row
#' at the Nth column.
#'
#' @details
#' This wrapper implements the dstat function for all pairs of loci in a genotype
#' matrix. If the input matrix includes n loci, choose(n,2) pairs are evaluated. Therefore,
#' the computaiton time scales quadratically, and is not feasible for large datasets.
#' We suggest manual parallelization across computational nodes for a large-scale
#' (ie thousands of markers) implementation.
#' 
#' @examples
#' 
#' data(beissinger_data)
#' beissinger_subset <- beissinger_data[,1:15]
#' dwrapper(beissinger_subset, tot_maf = 0.05, pop_maf = 0.01)
#' 
#' \dontrun{
#' data(beissinger_data)
#' dwrapper(beissinger_data, tot_maf = 0.05, pop_maf = 0.01)
#' }
#' @export
dwrapper <- function(data_set, tot_maf = 0.1, pop_maf = 0.05){
  dimnames <- list(colnames(data_set), colnames(data_set))
  d2it_mat <- matrix(NA,nrow = ncol(data_set), ncol = ncol(data_set), dimnames = dimnames)
  d2is_mat <- matrix(NA, nrow = ncol(data_set), ncol = ncol(data_set), dimnames = dimnames)
  d2st_mat <- matrix(NA, nrow = ncol(data_set), ncol = ncol(data_set), dimnames = dimnames)
  dp2st_mat <- matrix(NA, nrow = ncol(data_set), ncol = ncol(data_set), dimnames = dimnames)
  dp2is_mat <- matrix(NA, nrow = ncol(data_set), ncol = ncol(data_set), dimnames = dimnames)
  npops_mat <- matrix(NA, nrow = ncol(data_set), ncol = ncol(data_set), dimnames = dimnames)
  for (i in 1:ncol(data_set)){
    for (j in i:ncol(data_set)){
      d_stats <- dstat(index = c(i,j), data_set = data_set, tot_maf = tot_maf, pop_maf = pop_maf)
      d2it_mat[i,j] <- d_stats[2]
      d2is_mat[i,j] <- d_stats[3]
      d2st_mat[i,j] <- d_stats[4]
      dp2st_mat[i,j] <- d_stats[5]
      dp2is_mat[i,j] <- d_stats[6]
      npops_mat[i,j] <- d_stats[1]
    }
  }
  ratio1 <- d2is_mat / d2st_mat
  ratio2 <- dp2st_mat / dp2is_mat
  dstat_matrices <- list(d2it_mat=d2it_mat, d2is_mat=d2is_mat, d2st_mat=d2st_mat, dp2st_mat=dp2st_mat, dp2is_mat=dp2is_mat, npops_mat = npops_mat,
                         ratio1 = ratio1, ratio2 = ratio2)
  return(dstat_matrices)
}
