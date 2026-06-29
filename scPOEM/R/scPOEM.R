#'@title Main Function.
#'
#'@description This function takes paired single-cell ATAC-seq (scATAC-seq) and RNA-seq (scRNA-seq) data to embed peaks and genes into a shared low-dimensional space. It integrates regulatory relationships from peak-peak interactions (via Cicero), peak-gene interactions (via Lasso, random forest, and XGBoost), and gene-gene interactions (via principal component regression). Additionally, it supports gene-gene network reconstruction using epsilon-NN projections and compares networks across conditions through manifold alignment (scTenifoldNet).
#'@name scPOEM
#'@importFrom Matrix readMM
#'@importFrom utils read.csv
#'@param mode The mode indicating whether to analyze data from a single condition or to compare two conditions.
#'@param input_data A list of input data.
#'
#'If \code{mode = "single"}, \code{input_data} must be a list containing the following **seven objects**:
#' \itemize{
#'   \item \code{X}: The scATAC-seq data, sparse matrix.
#'   \item \code{Y}: The scRNA-seq data, sparse matrix.
#'   \item \code{peak_data}: A data.frame containing peak information.
#'   \item \code{gene_data}: A data.frame containing gene information (must contain a column "gene_name").
#'   \item \code{cell_data}: A data.frame containing cell metadata.
#'   \item \code{neibor_peak}: The peak IDs within a certain range of each gene, must have cols c("gene_name", "start_use", "end_use"). The id numbers in "start_use" and "end_use" are start from 0.
#'   \item \code{genome}: The genome length for the species.
#' }
#'
#'If \code{mode = "compare"}, \code{input_data} must be a **named list of two elements**, with names corresponding to two state names (e.g., "S1" and "S2"). Each element must itself be a list containing the same seven components as described above for \code{mode = "single"}.
#'@param dirpath The folder path to read or write file.
#'@param count_device The number of cpus used to train models.
#'@param nComp The number of PCs used for regression in constructing GGN.
#'@param d The dimension of latent space. Default is 100.
#'@param numwalks Number of random walks per node. Default is 5.
#'@param walklength Length of walk depth. Default is 3.
#'@param epochs Number of training epochs. Default is 100.
#'@param neg_sample Number of negative samples per positive sample. Default is 5.
#'@param batch_size Batch size for training. Default is 32.
#'@param weighted Whether the sampling network is weighted. Default is TRUE.
#'@param exclude_pos Whether to exclude positive samples from negative sampling. Default is FALSE.
#'@param seed An integer specifying the random seed to ensure reproducible results.
#'@param rebuild_GGN Logical. Whether to rebuild the gene-gene network from scratch. If FALSE, the function will attempt to read from `GGN.mtx` under `dirpath/test` in `single` mode or `dirpath/state_name/test` in compare mode.
#'@param rebuild_PPN Logical. Whether to rebuild the peak-peak network from scratch. If FALSE, the function will attempt to read from `PPN.mtx` under `dirpath/test` in `single` mode or `dirpath/state_name/test` in compare mode.
#'@param rebuild_PGN_Lasso Logical. Whether to rebuild the peak-gene network via Lasso from scratch. If FALSE, the function will attempt to read from `PGN_Lasso.mtx` under \cr `dirpath/test` in single mode or `dirpath/state_name/test` in compare mode.
#'@param rebuild_PGN_RF Logical. Whether to rebuild the peak-gene network via random forest from scratch. If FALSE, the function will attempt to read from `PGN_RF.mtx` under `dirpath/test` in single mode or `dirpath/state_name/test` in compare mode.
#'@param rebuild_PGN_XGB Logical. Whether to rebuild the peak-gene network via XGBoost from scratch. If FALSE, the function will attempt to read from `PGN_XGB.mtx` under \cr `dirpath/test` in single mode or `dirpath/state_name/test` in compare mode.
#'@param relearn_pg_embedding Logical. Whether to relearn the low-dimensional representations for peaks and genes from scratch. If FALSE, the function will attempt to read from \cr`node_embeddings.mtx`, `node_used_peak.csv`, `node_used_gene.csv` \cr under `dirpath/embedding` in single mode or \cr `dirpath/state_name/embedding` in compare mode.
#'@param save_file Logical, whether to save the output to a file.
#'@param pg_method The vector of methods used to construct peak-gene net. Default is c("Lasso", "RF", "XGBoost").
#'@param python_env Name or path of the Python environment to be used.
#'@return The scPOEM result. \describe{
#'  \item{Single Mode}{Returns a list containing the following elements:
#'    \describe{
#'      \item{\code{E}}{Low-dimensional representations of peaks and genes.}
#'      \item{\code{peak_node}}{Peak IDs that are associated with other peaks or genes.}
#'      \item{\code{gene_node}}{Gene IDs that are associated with other peaks or genes.}
#'    }
#'  }
#'  \item{Compare Mode}{Returns a list containing the following elements:
#'    \describe{
#'      \item{\code{state1 name}}{The single-mode result for the first condition.}
#'      \item{\code{state2 name}}{The single-mode result for the second condition.}
#'      \item{\code{compare}}{A summary list containing:
#'        \describe{
#'          \item{\code{E_g2}}{Low-dimensional embedding representations of genes under the two conditions.}
#'          \item{\code{common_genes}}{Genes shared between both conditions and used in the analysis.}
#'          \item{\code{diffRegulation}}{A list of differential regulatory information for each gene.}
#'        }
#'      }
#'    }
#'  }
#'}
#'@examples
#'\donttest{
#' library(scPOEM)
#' library(monocle)
#' dirpath <- "./example_data"
#' # An example for analysing a single dataset.
#' # Download and read data.
#' data(example_data_single)
#' single_result <- scPOEM(mode = "single",
#'                         input_data=example_data_single,
#'                         dirpath=file.path(dirpath, "single"),
#'                         save_file=FALSE)
#'
#' # An example for analysing and comparing datasets from two conditions.
#' # Download compare mode example data
#' data(example_data_compare)
#' compare_result <- scPOEM(mode = "compare",
#'                          input_data=example_data_compare,
#'                          dirpath=file.path(dirpath, "compare"),
#'                          save_file=FALSE)
#'
#'}
#'@export

scPOEM <- function(mode = c("single", "compare"), input_data, dirpath=tempdir(), count_device=1, nComp=5, seed=NULL, numwalks=5, walklength=3, epochs=100, neg_sample=5, batch_size=32, weighted = TRUE, exclude_pos=FALSE, d=100, rebuild_GGN=TRUE, rebuild_PPN=TRUE, rebuild_PGN_Lasso=TRUE, rebuild_PGN_RF=TRUE, rebuild_PGN_XGB=TRUE, relearn_pg_embedding=TRUE, save_file=TRUE, pg_method=c("Lasso", "RF", "XGBoost"), python_env = "scPOEM_env") {

  tryCatch({
    reticulate::use_condaenv(python_env, required = TRUE)
  }, error = function(e) {
    stop(paste("Python environment", python_env, "not found. Please create it or set a valid one using the `python_env` parameter."))
  })
  required_modules <- c(
    os = "os",
    random = "random",
    numpy = "numpy",
    scipy = "scipy",
    sklearn = "scikit-learn",
    matplotlib = "matplotlib",
    tqdm = "tqdm",
    ray = "ray",
    tensorflow = "tensorflow"
  )
  missing <- sapply(names(required_modules), function(pkg) !reticulate::py_module_available(pkg))
  if (any(missing)) {
    missing_install_names <- required_modules[missing]

    stop(
      paste0(
        "The following Python packages are missing from environment '", python_env, "':\n",
        paste0("- ", missing_install_names, collapse = "\n"), "\n",
        "Please install them manually using conda or pip, for example:\n",
        "  conda activate ", python_env, "\n",
        "  pip install ", paste(missing_install_names, collapse = ","), "\n"
      )
    )
  }

  required_keys <- c("X", "Y", "peak_data", "gene_data", "cell_data", "neibor_peak", "genome")

  check_keys <- function(data, data_name = NULL) {
    missing_keys <- setdiff(required_keys, names(data))
    if (length(missing_keys) > 0) {
      stop(paste("Missing keys in", ifelse(is.null(data_name), "input", data_name), ":",
                 paste(missing_keys, collapse = ", ")))
    }
  }

  check_rebuild <- function(rebuild_GGN, rebuild_PPN, rebuild_PGN_Lasso, rebuild_PGN_RF, rebuild_PGN_XGB, relearn_pg_embedding, expected_dims, dirpath){
    # pg_embedding
    if (!relearn_pg_embedding) {
      if (!file.exists(file.path(dirpath, "embedding/node_embeddings.mtx"))) {
        stop("File 'node_embeddings.mtx' not found. Either set reblearn_pg_embedding = TRUE or provide this file.")
      }
      if (!file.exists(file.path(dirpath, "embedding/node_used_peak.csv"))) {
        stop("File 'node_used_peak.csv' not found. Either set reblearn_pg_embedding = TRUE or provide this file.")
      }
      if (!file.exists(file.path(dirpath, "embedding/node_used_gene.csv"))) {
        stop("File 'node_used_gene.csv' not found. Either set reblearn_pg_embedding = TRUE or provide this file.")
      }
      E <- readMM(file.path(dirpath, "embedding/node_embeddings.mtx"))
      peak_node <- read.csv(file.path(dirpath, "embedding/node_used_peak.csv"), header = FALSE)
      gene_node <- read.csv(file.path(dirpath, "embedding/node_used_gene.csv"), header = FALSE)
      if (!all.equal(dim(E), c(dim(peak_node)[1]+dim(gene_node)[1], d))) {
        stop(sprintf("pg_embedding dimension incorrect. Expected %s * %s.", dim(peak_node)[1]+dim(gene_node)[1], d))
      }
    }

    # GGN
    if (!rebuild_GGN) {
      ggn_path <- file.path(dirpath, "test/GGN.mtx")
      if (!file.exists(ggn_path)) {
        stop("File 'GGN.mtx' not found. Either set rebuild_GGN = TRUE or provide this file.")
      }
      gg_net <- readMM(ggn_path)
      if (!identical(dim(gg_net), expected_dims$GGN)) {
        stop(sprintf("GGN dimension incorrect. Expected %s * %s.", expected_dims$GGN[1], expected_dims$GGN[2]))
      }
    }

    # PPN
    if (!rebuild_PPN) {
      ppn_path <- file.path(dirpath, "test/PPN.mtx")
      if (!file.exists(ppn_path)) {
        stop("File 'PPN.mtx' not found. Either set rebuild_PPN = TRUE or provide this file.")
      }
      pp_net <- readMM(ppn_path)
      if (!identical(dim(pp_net), expected_dims$PPN)) {
        stop(sprintf("PPN dimension incorrect. Expected %s * %s.", expected_dims$PPN[1], expected_dims$PPN[2]))
      }
    }

    # PGN_Lasso
    if ((!rebuild_PGN_Lasso) & ("Lasso" %in% pg_method)) {
      lasso_path <- file.path(dirpath, "test/PGN_Lasso.mtx")
      if (!file.exists(lasso_path)) {
        stop("File 'PGN_Lasso.mtx' not found. Either set rebuild_PGN_Lasso = TRUE or provide this file.")
      }
      net_lasso <- readMM(lasso_path)
      if (!identical(dim(net_lasso), expected_dims$PGN_Lasso)) {
        stop(sprintf("PGN_Lasso dimension incorrect. Expected %s * %s.", expected_dims$PGN_Lasso[1], expected_dims$PGN_Lasso[2]))
      }
    }

    # PGN_RF
    if ((!rebuild_PGN_RF) & ("RF" %in% pg_method)) {
      rf_path <- file.path(dirpath, "test/PGN_RF.mtx")
      if (!file.exists(rf_path)) {
        stop("File 'PGN_RF.mtx' not found. Either set rebuild_PGN_RF = TRUE or provide this file.")
      }
      net_RF <- readMM(rf_path)
      if (!identical(dim(net_RF), expected_dims$PGN_RF)) {
        stop(sprintf("PGN_RF dimension incorrect. Expected %s * %s.", expected_dims$PGN_RF[1], expected_dims$PGN_RF[2]))
      }
    }

    # PGN_XGB
    if ((!rebuild_PGN_XGB) & ("XGBoost" %in% pg_method)) {
      xgb_path <- file.path(dirpath, "test/PGN_XGB.mtx")
      if (!file.exists(xgb_path)) {
        stop("File 'PGN_XGB.mtx' not found. Either set rebuild_PGN_XGB = TRUE or provide this file.")
      }
      net_XGB <- readMM(xgb_path)
      if (!identical(dim(net_XGB), expected_dims$PGN_XGB)) {
        stop(sprintf("PGN_XGB dimension incorrect. Expected %s * %s.", expected_dims$PGN_XGB[1], expected_dims$PGN_XGB[2]))
      }
    }

  }

  if (mode == "single") {
    if (!is.list(input_data)) stop("For mode='single', input_data must be a list.")
    check_keys(input_data)
    message("Processing single state...\n")
    n_peaks <- dim(input_data$X)[2]
    n_genes <- dim(input_data$Y)[2]
    expected_dims <- list(
      GGN = c(n_genes, n_genes),
      PPN = c(n_peaks, n_peaks),
      PGN_Lasso = c(n_peaks, n_genes),
      PGN_RF = c(n_peaks, n_genes),
      PGN_XGB = c(n_peaks, n_genes)
    )
    message("Check whether to rebuild data or load from existing files with correct dimensions.\n")
    check_rebuild(rebuild_GGN, rebuild_PPN, rebuild_PGN_Lasso, rebuild_PGN_RF, rebuild_PGN_XGB, relearn_pg_embedding, expected_dims, dirpath)
    message("Construct gene-gene net.\n")
    gg_net <- GGN_use(input_data$Y, dirpath, count_device, nComp, rebuild_GGN, save_file)
    message("Construct peak-peak net.\n")
    pp_net <- PPN(input_data$X, input_data$peak_data, input_data$cell_data, input_data$genome, dirpath, rebuild_PPN, save_file, seed=seed)
    pg_net_list <- list()
    message("Construct peak-gene net via Lasso.\n")
    if ("Lasso" %in% pg_method){
      net_lasso <- PGN_Lasso(input_data$X, input_data$Y, input_data$gene_data, input_data$neibor_peak, dirpath, count_device, rebuild_PGN_Lasso, save_file)
      pg_net_list <- c(pg_net_list, list(net_lasso))
    }
    message("Construct peak-gene net via random forest.\n")
    if ("RF" %in% pg_method){
      net_RF <- PGN_RF_use(input_data$X, input_data$Y, input_data$gene_data, input_data$neibor_peak, dirpath, count_device, rebuild_PGN_RF, save_file, seed=seed)
      pg_net_list <- c(pg_net_list, list(net_RF))
    }
    message("Construct peak-gene net via XGBoost.\n")
    if ("XGBoost" %in% pg_method){
      net_XGB <- PGN_XGBoost(input_data$X, input_data$Y, input_data$gene_data, input_data$neibor_peak, dirpath, count_device, rebuild_PGN_XGB, save_file)
      pg_net_list <- c(pg_net_list, list(net_XGB))
    }
    message("Learn low-dimensional representations for peaks and genes.\n")
    single_result <- pg_embedding_use(gg_net, pp_net, pg_net_list, dirpath, relearn_pg_embedding=relearn_pg_embedding, save_file=save_file, d=d, numwalks=numwalks, walklength=walklength, epochs=epochs, neg_sample=neg_sample, batch_size=batch_size, weighted=weighted, exclude_pos=exclude_pos, seed=seed)
    return(single_result)

  } else if (mode == "compare") {
    if (!is.list(input_data) || is.null(names(input_data))) {
      stop("For mode='compare', input_data must be a named list of states.")
    }
    state_names <- names(input_data)
    compare_result <- list()
    for (state_name in state_names) {
      dirpath_s <- file.path(dirpath, state_name)
      state_data <- input_data[[state_name]]
      if (!is.list(state_data)) stop(paste("State", state_name, "must be a list."))
      check_keys(state_data, state_name)
      message(paste("Processing state:", state_name, "\n"))
      n_peaks <- dim(state_data$X)[2]
      n_genes <- dim(state_data$Y)[2]
      expected_dims <- list(
        GGN = c(n_genes, n_genes),
        PPN = c(n_peaks, n_peaks),
        PGN_Lasso = c(n_peaks, n_genes),
        PGN_RF = c(n_peaks, n_genes),
        PGN_XGB = c(n_peaks, n_genes)
      )
      message("Check whether to rebuild data or load from existing files with correct dimensions.\n")
      check_rebuild(rebuild_GGN, rebuild_PPN, rebuild_PGN_Lasso, rebuild_PGN_RF, rebuild_PGN_XGB, relearn_pg_embedding, expected_dims, dirpath_s)
      message("Construct gene-gene net.\n")
      gg_net <- GGN_use(state_data$Y, dirpath_s, count_device, nComp, rebuild_GGN, save_file)
      message("Construct peak-peak net.\n")
      pp_net <- PPN(state_data$X, state_data$peak_data, state_data$cell_data, state_data$genome, dirpath_s, rebuild_PPN, save_file, seed=seed)
      pg_net_list <- list()
      message("Construct peak-gene net via Lasso.\n")
      if ("Lasso" %in% pg_method){
        net_lasso <- PGN_Lasso(state_data$X, state_data$Y, state_data$gene_data, state_data$neibor_peak, dirpath_s, count_device, rebuild_PGN_Lasso, save_file)
        pg_net_list <- c(pg_net_list, list(net_lasso))
      }
      message("Construct peak-gene net via random forest.\n")
      if ("RF" %in% pg_method){
        net_RF <- PGN_RF_use(state_data$X, state_data$Y, state_data$gene_data, state_data$neibor_peak, dirpath_s, count_device, rebuild_PGN_RF, save_file, seed=seed)
        pg_net_list <- c(pg_net_list, list(net_RF))
      }
      message("Construct peak-gene net via XGBoost.\n")
      if ("XGBoost" %in% pg_method){
        net_XGB <- PGN_XGBoost(state_data$X, state_data$Y, state_data$gene_data, state_data$neibor_peak, dirpath_s, count_device, rebuild_PGN_XGB, save_file)
        pg_net_list <- c(pg_net_list, list(net_XGB))
      }
      message("Learn low-dimensional representations for peaks and genes.\n")
      compare_result[[state_name]] <- pg_embedding_use(gg_net, pp_net, pg_net_list, dirpath_s, relearn_pg_embedding=relearn_pg_embedding, save_file=save_file, d=d, numwalks=numwalks, walklength=walklength, epochs=epochs, neg_sample=neg_sample, batch_size=batch_size, weighted=weighted, exclude_pos=exclude_pos, seed=seed)
    }
    message("Align genes between two states and identify differentially regulated genes.\n")
    compare_result$compare <- align_embedding(gene_data1 = input_data[[state_names[1]]]$gene_data,
                                              gene_node1 = compare_result[[state_names[1]]]$gene_node,
                                              E1 = compare_result[[state_names[1]]]$E,
                                              gene_data2 = input_data[[state_names[2]]]$gene_data,
                                              gene_node2 = compare_result[[state_names[2]]]$gene_node,
                                              E2 = compare_result[[state_names[2]]]$E,
                                              dirpath = dirpath,
                                              save_file = save_file,
                                              d=d)
    return(compare_result)
  }
  message("mode is wrong!\n")
}
