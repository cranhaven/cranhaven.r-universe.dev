#'@title Peak-Gene Network via XGBoost
#'
#'@description Construct the peak-gene network via XGBoost.
#'@name PGN_XGBoost
#'@import Matrix
#'@importFrom xgboost xgboost xgb.importance
#'@importFrom parallel makeCluster stopCluster
#'@importFrom doParallel registerDoParallel
#'@importFrom foreach foreach %dopar%
#'@importFrom tictoc tic toc
#'@param X The scATAC-seq data, sparse matrix.
#'@param Y The scRNA-seq data, sparse matrix.
#'@param gene_data The information for genes, must have a col names "gene_name".
#'@param neibor_peak The peak IDs within a certain range of each gene, must have cols c("gene_name", "start_use", "end_use"). The id numbers in "start_use" and "end_use" are start from 0.
#'@param dirpath The folder path to read or write file.
#'@param count_device The number of cpus used to train the Lasso model.
#'@param rebuild_PGN_XGB Logical. Whether to rebuild the peak-gene network via XGBoost from scratch. If FALSE, the function will attempt to read from `PGN_XGB.mtx` under \cr `dirpath/test` in single mode or `dirpath/state_name/test` in compare mode.
#'@param save_file Logical, whether to save the output to a file.
#'@return The PGN_XGBoost network.
#'@examples
#'\donttest{
#' library(scPOEM)
#' dirpath <- "./example_data"
#' # Download single mode example data
#' data(example_data_single)
#' # Construct PGN net via XGBoost.
#' net_XGB <- PGN_XGBoost(example_data_single$X,
#'                        example_data_single$Y,
#'                        example_data_single$gene_data,
#'                        example_data_single$neibor_peak,
#'                        file.path(dirpath, "single"),
#'                        save_file=FALSE)
#'}
#'
#' @export


PGN_XGBoost <- function(X, Y, gene_data, neibor_peak, dirpath=tempdir(), count_device=1, rebuild_PGN_XGB=TRUE, save_file=TRUE) {
  if (!rebuild_PGN_XGB){
    message("Load peak-gene network constructed by XGBoost\n")
    net_XGB <- readMM(file.path(dirpath, "test/PGN_XGB.mtx"))
    return(net_XGB)
  }

  PGN_XGBoost_shapley <- function(X, Y, feature_set = NULL, gene_name = NULL, count_device = 1) {
    g <- ncol(Y)
    p <- ncol(X)

    func_fit_xgb <- function(ii) {
      gene <- gene_name[ii]
      coefficient_extension <- rep(0, p)
      if (!is.null(feature_set)) {
        gene_info <- subset(feature_set, gene_name == gene)
        if (nrow(gene_info) == 0) return(Matrix(0, nrow = p, ncol = 1, sparse = TRUE))
        start <- gene_info$start_use[1]
        end <- gene_info$end_use[1]
        if (start == -1) return(Matrix(0, nrow = p, ncol = 1, sparse = TRUE))
        if ((end - start + 1) <= 5) end <- min(start + 5, p-1)
        X_g <- as.matrix(X[, (start+1):(end+1)])
      }
      else {
        start <- 0
        end <- p - 1
        X_g <- as.matrix(X)
      }
      Y_g <- as.vector(Y[, ii])
      if (sum(Y_g) == 0) return(Matrix(0, nrow = p, ncol = 1, sparse = TRUE))

      model <- xgboost(data = X_g, label = Y_g, nrounds = 100,
                                objective = "reg:squarederror", verbose = 0)
      imp <- tryCatch({
        xgb.importance(model = model)[, c("Feature", "Gain")]
      }, error = function(e) return(NULL))
      if (is.null(imp) || nrow(imp) == 0) return(Matrix(0, nrow = p, ncol = 1, sparse = TRUE))

      imp_index <- as.numeric(gsub("f", "", imp$Feature)) + start
      coefficient_extension[imp_index + 1] <- imp$Gain
      coefficient_extension <- Matrix(coefficient_extension, nrow = p, sparse = TRUE)
      return(coefficient_extension)
    }

    cl <- makeCluster(count_device)
    registerDoParallel(cl)
    tic()
    ii <- NULL
    result.pre <- foreach(ii=1:g,
                          .combine = "cbind",
                          .packages = c("Matrix", "xgboost")
    ) %dopar% func_fit_xgb(ii)
    stopCluster(cl)
    toc()
    PGN_net_matrix <- drop0(result.pre)
    return(PGN_net_matrix)
  }
  process_PGN_XGBoost <- function(sparse_matrix, threshold = 0.9) {
    if (!inherits(sparse_matrix, "dgCMatrix")) {
      sparse_matrix <- as(sparse_matrix, "dgCMatrix")
    }

    rows <- nrow(sparse_matrix)
    cols <- ncol(sparse_matrix)
    result <- Matrix(0, nrow = rows, ncol = cols, sparse = TRUE)

    for (col in 1:cols) {
      col_data <- sparse_matrix[, col]
      non_zero_indices <- which(col_data != 0)
      if (length(non_zero_indices) == 0) next
      non_zero_values <- col_data[non_zero_indices]

      sorted_indices <- order(non_zero_values, decreasing = TRUE)
      sorted_values <- non_zero_values[sorted_indices]

      cumulative_sum <- cumsum(sorted_values)
      cutoff_index1 <- which(cumulative_sum >= threshold)[1]
      cutoff_index2 <- which(sorted_values < 0.05)[1]
      if (!is.na(cutoff_index2)) {
        cutoff_index2 <- cutoff_index2 - 1
      } else {
        cutoff_index2 <- length(sorted_values)
      }

      cutoff_index <- max(cutoff_index1, cutoff_index2, na.rm = TRUE)
      selected_indices <- sorted_indices[1:cutoff_index]

      result[non_zero_indices[selected_indices], col] <- col_data[non_zero_indices[selected_indices]]
    }

    return(drop0(result))
  }
  message("Construct peak-gene network via XGBoost from scratch...\n")

  neibor_peak$start_use <- as.integer(neibor_peak$start_use)
  neibor_peak$end_use <- as.integer(neibor_peak$end_use)
  colnames(X) <- NULL

  PGN_XGBoost <- PGN_XGBoost_shapley(X, Y, neibor_peak, gene_name = gene_data$gene_name, count_device = as.integer(count_device))

  net_XGB <- process_PGN_XGBoost(PGN_XGBoost, threshold = 0.9)

  if (save_file==TRUE){
    if (!dir.exists(file.path(dirpath, "test"))) {
      dir.create(file.path(dirpath, "test"), showWarnings = FALSE, recursive = TRUE)
    }
    writeMM(net_XGB, file.path(dirpath, "test/PGN_XGB.mtx"))
    message("PGN_XGB is saved in:", file.path(dirpath, "test/PGN_XGB.mtx"), "\n")
  }

  return(net_XGB)

}
