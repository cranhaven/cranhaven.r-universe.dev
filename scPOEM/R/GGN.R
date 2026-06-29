utils::globalVariables(c("make_GGN"))
#'@title Construct Gene-Gene Network
#'
#'@description Construct the gene-gene network via principle component regression.
#'@name GGN
#'@importFrom Matrix readMM
#'@importFrom reticulate source_python
#'@importFrom tictoc tic toc
#'@importFrom utils globalVariables
#'@param Y The scRNA-seq data, sparse matrix.
#'@param dirpath The folder path to read or write file.
#'@param count_device The number of cpus used to train the Lasso model.
#'@param nComp The number of PCs used for regression
#'@param rebuild_GGN Logical. Whether to rebuild the gene-gene network (GGN) from scratch. If FALSE, the function will attempt to read from `GGN.mtx` under `dirpath/test` in single mode or `dirpath/state_name/test` in compare mode.
#'@param save_file Logical, whether to save the output to a file.
#'@param python_env Name or path of the Python environment to be used.
#'@return The GGN network.
#'@examples
#'\donttest{
#' library(scPOEM)
#' dirpath <- "./example_data"
#' # Download single mode example data
#' data(example_data_single)
#' # Construct GGN net.
#' gg_net <- GGN(example_data_single$Y,
#'               file.path(dirpath, "single"),
#'               save_file=FALSE)
#'}
#'
#' @export
GGN <- function(Y, dirpath=tempdir(), count_device=1, nComp=5, rebuild_GGN=TRUE, save_file=TRUE, python_env = "scPOEM_env") {
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
    ray = "ray"
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

  if (!rebuild_GGN){
    message("Load gene-gene network\n")
    gg_net <- readMM(file.path(dirpath, "test/GGN.mtx"))
    return(gg_net)
  }
  if (!requireNamespace("reticulate", quietly = TRUE)) {
    stop("The 'reticulate' package is required. Please install it with install.packages('reticulate').")
  }

  message("Construct gene-gene network from scratch...\n")
  tic()
  py_script <- system.file("python/GGN.py", package = "scPOEM")

  source_python(py_script)

  gg_net <- make_GGN(Y, dirpath, save_file, as.integer(nComp), as.integer(count_device))
  toc()
  if (save_file==TRUE){
    message("GGN is saved in:", file.path(dirpath, "test/GGN.mtx"), "\n")
  }

  return(gg_net)
}


GGN_use <- function(Y, dirpath=tempdir(), count_device=1, nComp=5, rebuild_GGN=TRUE, save_file=TRUE) {
  if (!rebuild_GGN){
    message("Load gene-gene network\n")
    gg_net <- readMM(file.path(dirpath, "test/GGN.mtx"))
    return(gg_net)
  }

  message("Construct gene-gene network from scratch...\n")
  tic()
  py_script <- system.file("python/GGN.py", package = "scPOEM")

  source_python(py_script)

  gg_net <- make_GGN(Y, dirpath, save_file, as.integer(nComp), as.integer(count_device))
  toc()
  if (save_file==TRUE){
    message("GGN is saved in:", file.path(dirpath, "test/GGN.mtx"), "\n")
  }
  return(gg_net)
}
