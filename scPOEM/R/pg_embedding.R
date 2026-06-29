utils::globalVariables(c("metapath2vec_pg"))
#'@title Co-embeddings of Peaks and Genes.
#'
#'@description Learn the low-dimensional representations for peaks and genes with a meta-path based method.
#'@name pg_embedding
#'@import Matrix
#'@importFrom reticulate source_python
#'@importFrom tictoc tic toc
#'@importFrom utils read.csv
#'@param gg_net The gene-gene network.
#'@param pp_net The peak-peak network.
#'@param pg_net_list A list of peak-gene networks, constructed via different methods.
#'@param dirpath The folder path to read or write file.
#'@param relearn_pg_embedding Logical. Whether to relearn the low-dimensional representations for peaks and genes from scratch. If FALSE, the function will attempt to read from \cr`node_embeddings.mtx`, `node_used_peak.csv`, `node_used_gene.csv` \cr under `dirpath/embedding` in single mode or \cr `dirpath/state_name/embedding` in compare mode.
#'@param save_file Logical, whether to save the output to a file.
#'@param d Dimension of the latent space. Default is 100.
#'@param numwalks Number of random walks per node. Default is 5.
#'@param walklength Length of walk depth. Default is 3.
#'@param epochs Number of training epochs. Default is 100.
#'@param neg_sample Number of negative samples per positive sample. Default is 5.
#'@param batch_size Batch size for training. Default is 32.
#'@param weighted Whether the sampling network is weighted. Default is TRUE.
#'@param exclude_pos Whether to exclude positive samples from negative sampling. Default is FALSE.
#'@param seed An integer specifying the random seed to ensure reproducible results.
#'@param python_env Name or path of the Python environment to be used.
#'@return A list containing the following: \describe{
#'\item{\code{E}}{Low-dimensional representations of peaks and genes}
#'\item{\code{peak_node}}{Peak ids that are associated with other peaks or genes.}
#'\item{\code{gene_node}}{Gene ids that are associated with other peaks or genes.}
#'}
#'@examples
#'\donttest{
#' library(scPOEM)
#' library(monocle)
#' dirpath <- "./example_data"
#' # Download single mode example data
#' data(example_data_single)
#' gg_net <- GGN(example_data_single$Y,
#'               file.path(dirpath, "single"),
#'               save_file=FALSE)
#' pp_net <- PPN(example_data_single$X, example_data_single$peak_data,
#'               example_data_single$cell_data, example_data_single$genome,
#'               file.path(dirpath, "single"), save_file=FALSE)
#' net_Lasso <- PGN_Lasso(example_data_single$X, example_data_single$Y,
#'                        example_data_single$gene_data, example_data_single$neibor_peak,
#'                        file.path(dirpath, "single"), save_file=FALSE)
#' net_RF <- PGN_RF(example_data_single$X, example_data_single$Y,
#'                  example_data_single$gene_data, example_data_single$neibor_peak,
#'                  file.path(dirpath, "single"), save_file=FALSE)
#' net_XGB <- PGN_XGBoost(example_data_single$X, example_data_single$Y,
#'                        example_data_single$gene_data, example_data_single$neibor_peak,
#'                        file.path(dirpath, "single"), save_file=FALSE)
#' E_result <- pg_embedding(gg_net, pp_net, list(net_Lasso, net_RF, net_XGB),
#'                          file.path(dirpath, "single"), save_file=FALSE)
#'}
#'
#' @export

pg_embedding <- function(gg_net, pp_net, pg_net_list, dirpath=tempdir(), relearn_pg_embedding=TRUE, save_file=TRUE, d=100, numwalks=5, walklength=3, epochs=100, neg_sample=5, batch_size=32, weighted = TRUE, exclude_pos=FALSE, seed=NULL, python_env = "scPOEM_env") {
  if (!relearn_pg_embedding) {
    message("Load embedding results\n")
    E <- readMM(file.path(dirpath, "embedding/node_embeddings.mtx"))
    peak_node <- read.csv(file.path(dirpath, "embedding/node_used_peak.csv"), header = FALSE)[[1]]
    gene_node <- read.csv(file.path(dirpath, "embedding/node_used_gene.csv"), header = FALSE)[[1]]
    return(list(E=E, peak_node=peak_node, gene_node=gene_node))
  }

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
    matplotlib = "matplotlib",
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
  if(!is.null(seed)){
    seed = as.integer(seed)
  }

  message("Learn representations of peaks and genes...\n")
  tic()
  py_script <- system.file("python/pg_embedding.py", package = "scPOEM")

  source_python(py_script)

  E_result <- metapath2vec_pg(gg_net, pp_net, pg_net_list, dirpath, save_file, as.integer(d), as.integer(numwalks), as.integer(walklength), as.integer(epochs), as.integer(neg_sample), as.integer(batch_size), weighted, exclude_pos, seed)
  toc()
  E <- E_result[[1]]
  peak_node <- as.vector(E_result[[2]])
  gene_node <- as.vector(E_result[[3]])
  return(list(E=E, peak_node=peak_node, gene_node=gene_node))
}


pg_embedding_use <- function(gg_net, pp_net, pg_net_list, dirpath=tempdir(), relearn_pg_embedding=TRUE, save_file=TRUE, d=100, numwalks=5, walklength=3, epochs=100, neg_sample=5, batch_size=32, weighted = TRUE, exclude_pos=FALSE, seed=NULL) {
  if (!relearn_pg_embedding) {
    message("Load embedding results\n")
    E <- readMM(file.path(dirpath, "embedding/node_embeddings.mtx"))
    peak_node <- read.csv(file.path(dirpath, "embedding/node_used_peak.csv"), header = FALSE)[[1]]
    gene_node <- read.csv(file.path(dirpath, "embedding/node_used_gene.csv"), header = FALSE)[[1]]
    return(list(E=E, peak_node=peak_node, gene_node=gene_node))
  }

  if(!is.null(seed)){
    seed = as.integer(seed)
  }

  message("Learn representations of peaks and genes...\n")
  tic()
  py_script <- system.file("python/pg_embedding.py", package = "scPOEM")

  source_python(py_script)

  E_result <- metapath2vec_pg(gg_net, pp_net, pg_net_list, dirpath, save_file, as.integer(d), as.integer(numwalks), as.integer(walklength), as.integer(epochs), as.integer(neg_sample), as.integer(batch_size), weighted, exclude_pos, seed)
  toc()
  E <- E_result[[1]]
  peak_node <- as.vector(E_result[[2]])
  gene_node <- as.vector(E_result[[3]])
  return(list(E=E, peak_node=peak_node, gene_node=gene_node))
}
