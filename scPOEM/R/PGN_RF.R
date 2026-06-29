utils::globalVariables(c("make_PGN_RF"))
#'@title Peak-Gene Network via Random Forest
#'
#'@description Construct the peak-gene network via random forest.
#'@name PGN_RF
#'@import Matrix
#'@importFrom reticulate source_python
#'@importFrom tictoc tic toc
#'@param X The scATAC-seq data, sparse matrix.
#'@param Y The scRNA-seq data, sparse matrix.
#'@param gene_data The information for genes, must have a col names "gene_name".
#'@param neibor_peak The peak IDs within a certain range of each gene, must have cols c("gene_name", "start_use", "end_use"). The id numbers in "start_use" and "end_use" are start from 0.
#'@param dirpath The folder path to read or write file.
#'@param count_device The number of cpus used to train the Lasso model.
#'@param rebuild_PGN_RF Logical. Whether to rebuild the peak-gene network via random forest from scratch. If FALSE, the function will attempt to read from `PGN_RF.mtx` under `dirpath/test` in single mode or `dirpath/state_name/test` in compare mode.
#'@param save_file Logical, whether to save the output to a file.
#'@param seed An integer specifying the random seed to ensure reproducible results.
#'@param python_env Name or path of the Python environment to be used.
#'@return The PGN_RF network.
#'@examples
#'\donttest{
#' library(scPOEM)
#' dirpath <- "./example_data"
#' # Download single mode example data
#' data(example_data_single)
#' # Construct PGN net via random forest (RF).
#' net_RF <- PGN_RF(example_data_single$X,
#'                  example_data_single$Y,
#'                  example_data_single$gene_data,
#'                  example_data_single$neibor_peak,
#'                  file.path(dirpath, "single"),
#'                  save_file=FALSE)
#'}
#'
#' @export

PGN_RF <- function(X, Y, gene_data, neibor_peak, dirpath=tempdir(), count_device=1, rebuild_PGN_RF=TRUE, save_file=TRUE, seed=NULL, python_env = "scPOEM_env") {
  if (!rebuild_PGN_RF){
    message("Load peak-gene network constructed by random forest\n")
    net_RF <- readMM(file.path(dirpath, "test/PGN_RF.mtx"))
    return(net_RF)
  }

  tryCatch({
    reticulate::use_condaenv(python_env, required = TRUE)
  }, error = function(e) {
    stop(paste("Python environment", python_env, "not found. Please create it or set a valid one using the `python_env` parameter."))
  })
  required_modules <- c(
    os = "os",
    numpy = "numpy",
    scipy = "scipy",
    sklearn = "scikit-learn",
    tqdm = "tqdm"
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

  message("Construct peak-gene network via random forest from scratch...\n")
  tic()
  py_script <-system.file("python/PGN_RF.py", package = "scPOEM")

  source_python(py_script)

  net_RF <- make_PGN_RF(X, Y, gene_data, neibor_peak, dirpath, save_file, as.integer(count_device), seed)
  toc()
  if (save_file==TRUE){
    message("PGN_RF is saved in:", file.path(dirpath, "test/PGN_RF.mtx"), "\n")
  }
  return(net_RF)
}

PGN_RF_use <- function(X, Y, gene_data, neibor_peak, dirpath=tempdir(), count_device=1, rebuild_PGN_RF=TRUE, save_file=TRUE, seed=NULL) {
  if (!rebuild_PGN_RF){
    message("Load peak-gene network constructed by random forest\n")
    net_RF <- readMM(file.path(dirpath, "test/PGN_RF.mtx"))
    return(net_RF)
  }
  if(!is.null(seed)){
    seed = as.integer(seed)
  }

  message("Construct peak-gene network via random forest from scratch...\n")
  tic()
  py_script <-system.file("python/PGN_RF.py", package = "scPOEM")

  source_python(py_script)

  net_RF <- make_PGN_RF(X, Y, gene_data, neibor_peak, dirpath, save_file, as.integer(count_device), seed)
  toc()
  if (save_file==TRUE){
    message("PGN_RF is saved in:", file.path(dirpath, "test/PGN_RF.mtx"), "\n")
  }
  return(net_RF)
}
