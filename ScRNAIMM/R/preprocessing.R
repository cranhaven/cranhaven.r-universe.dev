#' Remove genes which are not expressed in at least one cell
#'
#' Filter out the genes that are not expressed in at least one cell type
#'
#' @usage filter_ScRNA(ScRNA_mat)
#'
#' @param ScRNA_mat ScRNA-Seq matrix where genes are in rows and cells are in columns
#'
#' @export
#'
#' @author Mohamed Soudy \email{Mohmedsoudy2009@gmail.com}
#'
#' @returns Filtered ScRNA-seq matrix that contains genes that are at least expressed in one cell type
#'
filter_ScRNA <- function(ScRNA_mat)
{
  return(ScRNA_mat[rowSums(ScRNA_mat[])>0,])
}

#' Prepare the data set for the imputation
#'
#' This function aims to get the indices where all genes are zeros or genes that is expressed on only one sample per cell
#'
#' @usage prepare_dataset(filtered_data, cluster_labels)
#'
#' @param filtered_data ScRNA-seq data set generate by filter_ScRNA function
#'
#' @param cluster_labels cell labels
#'
#' @export
#'
#' @author Mohamed Soudy \email{Mohmedsoudy2009@gmail.com}
#'
#' @returns a data frame that contains the processed ScRNA-seq data
#'
prepare_dataset <- function(filtered_data, cluster_labels)
{
  prepared_list <- list()
  i <- 1
  #assign column names based on clsuter label
  colnames(filtered_data) <- paste0(colnames(filtered_data), ";", cluster_labels)
  labels_unique <- unique(cluster_labels)
  for (label in labels_unique)
  {
    #Get each cell
    population_columns <- colnames(filtered_data)[sapply(strsplit(colnames(filtered_data), ";"), "[", 2) == label]
    cell_population <- filtered_data %>% select(all_of(population_columns))    #Get cells where not all values are zeros
    zero_inx <- which(rowSums(cell_population) == 0)
    cell_population[cell_population == 0] <- NA
    if (length(zero_inx)!=0)
      cell_population[zero_inx,] <- 0
    prepared_list[[i]] <- cell_population
    i <- i + 1
  }
  prepared_df <- do.call(cbind, prepared_list)
  return(prepared_df)
}
