#'@title Gene Network Reconstruction and Alignment
#'
#'@description Reconstruct gene networks via epsilon-NN and compare conditions using manifold alignment implemented in scTenifoldNet.
#'@name align_embedding
#'@importFrom scTenifoldNet manifoldAlignment dRegulation
#'@importFrom Matrix writeMM
#'@importFrom methods as
#'@importFrom tictoc tic toc
#'@importFrom utils write.csv write.table
#'@param gene_data1 The information for genes in state1, must have a col names "gene_name".
#'@param gene_node1 Gene ids that are associated with other peaks or genes in state1.
#'@param E1 Embedding representations of peaks and genes in state1.
#'@param gene_data2 The information for genes in state2, must have a col names "gene_name".
#'@param gene_node2 Gene ids that are associated with other peaks or genes in state2.
#'@param E2 Embedding representations of peaks and genes in state2.
#'@param dirpath The folder path to read or write file
#'@param save_file Logical, whether to save the output to a file.
#'@param d The dimension of latent space.
#'@return A list containing the following elements:
#' \describe{
#'   \item{\code{E_g2}}{Low-dimensional embedding representations of genes under the two conditions.}
#'   \item{\code{common_genes}}{Genes shared between both conditions and used in the analysis.}
#'   \item{\code{diffRegulation}}{A list of differential regulatory information for each gene.}
#' }
#'@examples
#'\donttest{
#' library(scPOEM)
#' library(monocle)
#' dirpath <- "./example_data"
#' # Download compare mode example data
#' data(example_data_compare)
#' data_S1 <- example_data_compare$S1
#' data_S2 <- example_data_compare$S2
#' gg_net1 <- GGN(data_S1$Y, file.path(dirpath, "compare/S1"), save_file=FALSE)
#' pp_net1 <- PPN(data_S1$X, data_S1$peak_data, data_S1$cell_data,
#'                data_S1$genome, file.path(dirpath, "compare/S1"), save_file=FALSE)
#'
#' net_Lasso1 <- PGN_Lasso(data_S1$X, data_S1$Y,
#'                         data_S1$gene_data, data_S1$neibor_peak,
#'                         file.path(dirpath, "compare/S1"), save_file=FALSE)
#' net_RF1 <- PGN_RF(data_S1$X, data_S1$Y, data_S1$gene_data,
#'                   data_S1$neibor_peak, file.path(dirpath, "compare/S1"), save_file=FALSE)
#' net_XGB1 <- PGN_XGBoost(data_S1$X, data_S1$Y,
#'                         data_S1$gene_data, data_S1$neibor_peak,
#'                         file.path(dirpath, "compare/S1"), save_file=FALSE)
#' pg_net_list1 <- list(net_Lasso1, net_RF1, net_XGB1)
#' E_result_S1 <- pg_embedding(gg_net1, pp_net1, pg_net_list1,
#'                file.path(dirpath, "compare/S1"), save_file=FALSE)
#'
#' gg_net2 <- GGN(data_S2$Y, file.path(dirpath, "compare/S2"), save_file=FALSE)
#' pp_net2 <- PPN(data_S2$X, data_S2$peak_data,
#'                data_S2$cell_data, data_S2$genome,
#'                file.path(dirpath, "compare/S2"), save_file=FALSE)
#' net_Lasso2 <- PGN_Lasso(data_S2$X, data_S2$Y,
#'                         data_S2$gene_data, data_S2$neibor_peak,
#'                         file.path(dirpath, "compare/S2"), save_file=FALSE)
#' net_RF2 <- PGN_RF(data_S2$X, data_S2$Y, data_S2$gene_data,
#'                   data_S2$neibor_peak, file.path(dirpath, "compare/S2"), save_file=FALSE)
#' net_XGB2 <- PGN_XGBoost(data_S2$X, data_S2$Y,
#'                         data_S2$gene_data, data_S2$neibor_peak,
#'                         file.path(dirpath, "compare/S2"), save_file=FALSE)
#' pg_net_list2 <- list(net_Lasso2, net_RF2, net_XGB2)
#' E_result_S2 <- pg_embedding(gg_net2, pp_net2, pg_net_list2,
#'                file.path(dirpath, "compare/S2"), save_file=FALSE)
#'
#' compare_result <- align_embedding(data_S1$gene_data,
#'                                   E_result_S1$gene_node,
#'                                   E_result_S1$E,
#'                                   data_S2$gene_data,
#'                                   E_result_S2$gene_node,
#'                                   E_result_S2$E,
#'                                   file.path(dirpath, "compare/compare"),
#'                                   save_file=FALSE)
#'}
#'
#' @export

align_embedding <- function(gene_data1,
                            gene_node1,
                            E1,
                            gene_data2,
                            gene_node2,
                            E2,
                            dirpath=tempdir(),
                            save_file=TRUE,
                            d=100){

  used_genes1 <- gene_data1$gene_name[gene_node1+1]
  used_genes2 <- gene_data2$gene_name[gene_node2+1]

  # Find common gene names
  common_genes <- intersect(used_genes1, used_genes2)
  if((length(common_genes) != length(used_genes1)) | (length(common_genes) != length(used_genes2))){
    warning("Different genes used in the two states. Only common genes are used in this step.\n")
  }

  # Find their indices in eNN matrices
  idx1 <- which(used_genes1 %in% common_genes)
  idx2 <- which(used_genes2 %in% common_genes)

  E1_gg <- E1[(nrow(E1)-length(gene_node1)+1):nrow(E1), ]
  E1_gg <- E1_gg[idx1, ]
  E2_gg <- E2[(nrow(E2)-length(gene_node2)+1):nrow(E2), ]
  E2_gg <- E2_gg[idx2, ]

  eNN1 <- eNN(E1_gg)
  eNN2 <- eNN(E2_gg)
  rownames(eNN1) <- common_genes
  colnames(eNN1) <- common_genes
  rownames(eNN2) <- common_genes
  colnames(eNN2) <- common_genes
  message("Manifold alignment for genes in two states...\n")
  tic()
  E_g2 <- manifoldAlignment(eNN1, eNN2, d = as.integer(d))
  toc()
  #embedding_sparse <- readMM("sctenifold/sctenifoldnet_embedding.mtx")
  rownames(E_g2) <- c(paste0('X_', common_genes),paste0('y_', common_genes))
  # Differential regulation testing
  dR <- dRegulation(manifoldOutput = E_g2)
  outputResult <- list()
  outputResult$E_g2 <- E_g2
  outputResult$common_genes <- common_genes
  outputResult$diffRegulation <- dR

  if (save_file==TRUE){
    outputdir <- file.path(dirpath, "compare")
    if (!dir.exists(outputdir)) {
      dir.create(outputdir, showWarnings = FALSE, recursive = TRUE)
    }
    sparse_matrix <- as(E_g2, "CsparseMatrix")
    writeMM(sparse_matrix, file = file.path(outputdir, "E_g2.mtx"))
    write.csv(common_genes, file=file.path(outputdir, "common_genes.csv"))
    write.csv(outputResult$diffRegulation, file=file.path(outputdir, "gene_regulation.csv"))
    write.table(outputResult$diffRegulation$gene[outputResult$diffRegulation$p.adj<0.05],
                file = file.path(outputdir, "gene_sig.txt"),
                sep = "\t", quote = FALSE, row.names = FALSE)
    message("Manifold Alignment resylts are saved in:", outputdir, "\n")
  }

  return(outputResult)
}

#args <- commandArgs(trailingOnly = TRUE)
#dirpath <- NULL
#state1 <- NULL
#state2 <- NULL

#for (i in seq(1, length(args))) {
#  if (args[i] == "--dirpath") {
#    dirpath <- args[i+1]
#  }
#  if (args[i] == "--state1") {
#    dirpath <- args[i+1]
#  }
#  if (args[i] == "--state2") {
#    dirpath <- args[i+1]
#  }
#}

#if (is.null(dirpath)){
#  dirpath <- "data_example/compare"
#}
#if (is.null(state1)){
#  dirpath <- "S1"
#}

#if (is.null(state2)){
#  dirpath <- "S2"
#}

#manifold_alignmnet(dirpath, state1, state2)
