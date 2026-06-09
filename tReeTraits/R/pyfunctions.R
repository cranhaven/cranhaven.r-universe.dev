
#' Run TreeQSM on a LAS/LAZ file using PyTLidar
#'
#' Processes a point cloud, filters and normalizes it, then runs the PyTLidar TreeQSM model
#' to reconstruct cylinders representing the tree structure.
#'
#' @param file Path to the input LAS or LAZ file.
#' @param output_dir Directory to save output files; temporary if NULL.
#' @param intensity_threshold Minimum point intensity to retain.
#' @param resolution Thinning voxel size (m).
#' @param patch_diam1 Numeric vector of patch diameter 1 parameters.
#' @param patch_diam2min Numeric vector of minimum patch diameter 2.
#' @param patch_diam2max Numeric vector of maximum patch diameter 2.
#' @param optimizing_metrics Character vector of  metric names to average and
#' minimize when selecting the best QSM fit. See Details
#' @param verbose Logical; whether to print details during processing.
#' @return A list with elements:
#'   \itemize{
#'     \item \code{qsm_pars}: Data frame of selected patch parameters and fit metrics.
#'     \item \code{qsm}: Data frame of cylinder-level QSM output.
#'   }
#' @details
#' The \code{optimizing_metrics} argument controls which point-to-cylinder distance
#' summaries are used to evaluate TreeQSM fits. These metrics quantify how closely
#' reconstructed cylinders match the underlying point cloud and are computed for
#' different structural components of the tree.
#'
#' Available metrics include:
#' \itemize{
#'   \item \code{median}, \code{mean}, \code{max}, \code{std}: Overall point–to–cylinder distances.
#'   \item \code{TrunkMedian}, \code{TrunkMean}, \code{TrunkMax}, \code{TrunkStd}: Trunk-only distances.
#'   \item \code{BranchMedian}, \code{BranchMean}, \code{BranchMax}, \code{BranchStd}: All branch distances.
#'   \item \code{Branch1Median}, \code{Branch1Mean}, \code{Branch1Max}, \code{Branch1Std}: First-order branch distances.
#'   \item \code{Branch2Median}, \code{Branch2Mean}, \code{Branch2Max}, \code{Branch2Std}: Second-order branch distances.
#' }
#'
#' When multiple metrics are supplied, their row-wise mean is computed and minimized
#' to select the best-fitting QSM, allowing users to balance fit quality across
#' different tree components.
#' @return A list with elements:
#'   \describe{
#'     \item{qsm_pars}{Data frame of patch parameters and fit metrics.}
#'     \item{qsm}{Data frame of cylinder-level QSM output.}
#'   }
#' @export
#' @importFrom lidR readLAS filter_poi las_update
#' @importFrom reticulate import
#' @importFrom dplyr mutate rowwise arrange select
#' @examples
#' \dontrun{
#' file <- system.file("extdata", "tree_0744.laz", package="tReeTraits")
#' run_treeqsm(file)
#' }
run_treeqsm <- function(
    file,
    output_dir = NULL,
    intensity_threshold = 40000,
    resolution = 0.02,
    patch_diam1 = c(0.05, 0.10),
    patch_diam2min = c(0.04, 0.05),
    patch_diam2max = c(0.12, 0.14),
    optimizing_metrics = c('TrunkMean', 'Branch1Mean'),
    verbose = TRUE
) {
  stopifnot(
    length(patch_diam1) > 0,
    length(patch_diam2min) > 0,
    length(patch_diam2max) > 0
  )
  if (length(optimizing_metrics) == 0) {
    stop("optimizing_metrics must contain at least one valid metric name.")
  }
  optimizing_metric_choices = c("median", "mean", "max", "std", "TrunkMedian",
                                "TrunkMean", "TrunkMax", "TrunkStd", "BranchMedian",
                                "BranchMean", "BranchMax", "BranchStd", "Branch1Median",
                                "Branch1Mean", "Branch1Max", "Branch1Std", "Branch2Median",
                                "Branch2Mean", "Branch2Max", "Branch2Std")
  optimizing_metrics = match.arg(optimizing_metrics,
                                 choices = optimizing_metric_choices,
                                 several.ok=TRUE)

  # check and setup pytlidar environemnt as needed.
  .check_pytlidar()

  message("Preparing output directory...")
  if (is.null(output_dir)) {
    output_dir <- file.path(tempdir(check = TRUE), paste0("QSM_tmp_", Sys.getpid()))
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
    on.exit({
      try(unlink(output_dir, recursive = TRUE, force = TRUE))
    }, add = TRUE)
  } else if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  output_dir <- normalizePath(output_dir, winslash = "/", mustWork = TRUE)

  # Thin, intensity-filter, normalize, and recenter point cloud
  message("Reading and preprocessing LAS file...")
  las <- readLAS(file, filter = paste0("-thin_with_voxel ", resolution))
  las <- filter_poi(las, .data$Intensity > intensity_threshold)
  las <- normalize_las(las)
  las <- recenter_las(las, height = 1)
  las <- lidR::las_update(las)

  # las_points is your normalized, recentered LAS point matrix
  np <- import("numpy")
  P <- as.matrix(las@data[, c("X", "Y", "Z")])
  P <- np$array(P)  # This is what you pass to Python

  # Build inputs dictionary for PyTLidar
  define_input <- import("PyTLidar.Utils.define_input", delay_load = TRUE)$define_input
  inputs_list <- define_input(P, 1, 1, 1)  # 1 tree, 1 model, 1? (use standard args)
  stopifnot(
    length(patch_diam1) > 0,
    length(patch_diam2min) > 0,
    length(patch_diam2max) > 0
  )
  inputs <- inputs_list[[1]]  # get first dict

  # overwrite just the patch diameters
  patch_diam1 <- c(patch_diam1)
  patch_diam2min <- c(patch_diam2min)
  patch_diam2max <- c(patch_diam2max)
  inputs$PatchDiam1 <- np$array(patch_diam1)
  inputs$PatchDiam2Min <- np$array(patch_diam2min)
  inputs$PatchDiam2Max <- np$array(patch_diam2max)
  inputs$BallRad1 <- np$array(patch_diam1 + 0.01)
  inputs$BallRad2 <- np$array(patch_diam2max + 0.01)
  inputs$savemat = 0
  inputs$savepdf=0
  if(!verbose) inputs$disp=1

  message("Running PyTLidar TreeQSM...")
  # import necessary modules
  pytlidar <- import("PyTLidar", delay_load = TRUE)
  treeqsm <- import("PyTLidar.treeqsm", delay_load = TRUE)

  res <- tryCatch(
    treeqsm$treeqsm(P, inputs, results_location = output_dir),
    error = function(e) stop("Error running PyTLidar TreeQSM: ", e$message)
  )

  results = res[[1]]
  # Ensure results is always a list of fits
  if (!all(sapply(results, function(x) "pmdistance" %in% names(x)))) {
    results <- list(results)
  }
  metrics = setdiff(names(results[[1]]$pmdistance), "CylDist")
  fits <- as.data.frame(
    do.call(
      rbind.data.frame,
      lapply(results, function(x) {
        c(
          list(
            PatchDiam1    = x$PatchDiam1,
            PatchDiam2Max = x$PatchDiam2Max,
            PatchDiam2Min = x$PatchDiam2Min,
            file          = x$file_id
          ),
          as.list(
            vapply(metrics, function(m) x$pmdistance[[m]], numeric(1))
          )
        )
      })
    )
  )
  fits$distance <- rowMeans(fits[, optimizing_metrics, drop = FALSE], na.rm = TRUE)
  fits = dplyr::arrange(fits, .data$distance)
  parameters = dplyr::select(dplyr::slice(fits, 1),
                             dplyr::starts_with("PatchDiam"), .data$distance, dplyr::all_of(optimizing_metrics))
  best_qsm = file.path(output_dir, 'results', paste0('cylinder_', fits$file[1], '.txt'))
  qsm <- .read_qsm_raw(normalizePath(best_qsm))
  list(qsm_pars = parameters, qsm = qsm)
}


#' Read a PyTLidar QSM file
#'
#' Reads a PyTLidar-generated cylinder file and converts it into a tidy data frame
#' with start/end coordinates, radius, length, volume, and branching order.
#'
#' @param cyl_file Path to the PyTLidar cylinder output file (.txt).
#' @return A data frame with columns startX, startY, startZ, endX, endY, endZ,
#'   cyl_ID, parent_ID, extension_ID, radius_cyl, length, volume, branching_order.
#' @importFrom stringr str_split
#' @importFrom readr read_delim
#' @importFrom dplyr mutate select row_number
.read_qsm_raw <- function(cyl_file) {
  headers <- stringr::str_split(readLines(cyl_file, n = 1), "\t")[[1]]
  cyl <- readr::read_delim(cyl_file, skip = 1, col_names = FALSE, delim = "\t", show_col_types = FALSE)

  new_headers <- c(
    headers[1:2],                       # radius, length
    paste0("start", c("X","Y","Z")),    # start coordinates
    paste0("dir", c("X","Y","Z")),      # direction vector
    headers[5:length(headers)]
  )
  colnames(cyl) <- new_headers

  cyl <- dplyr::mutate(cyl,
                       endX = .data$startX + .data$dirX * .data$`length (m)`,
                       endY = .data$startY + .data$dirY * .data$`length (m)`,
                       endZ = .data$startZ + .data$dirZ * .data$`length (m)`,
                       cyl_ID = dplyr::row_number(),
                       parent_ID = .data$parent,
                       extension_ID = .data$extension,
                       length = .data$`length (m)`,
                       radius_cyl = .data$`radius (m)`,
                       volume = pi * .data$radius_cyl^2 * .data$`length (m)`,
                       branching_order = .data$branch_order)

  cyl <- dplyr::select(cyl, dplyr::all_of(c(
    "startX", "startY", "startZ",
    "endX", "endY", "endZ",
    "cyl_ID", "parent_ID", "extension_ID",
    "radius_cyl", "length", "volume",
    "branching_order"
  ))
  )
  cyl
}

#' @keywords internal
#' @noRd
.parse_patch_params <- function(file_path) {
  lines <- readLines(file_path)
  lines <- lines[nzchar(lines)]  # remove empty lines

  start_idx <- which(grepl("^PatchDiam1", lines))
  if (length(start_idx) == 0) stop("PatchDiam1 not found in file.")

  lines <- lines[start_idx:length(lines)]
  lines <- lines[!grepl("^AverageSurfaceCoverage_", lines)]

  params <- list()
  for (line in lines) {
    split_line <- strsplit(line, "\\s+")[[1]]
    name <- split_line[1]
    value <- suppressWarnings(as.numeric(split_line[-1]))
    if (all(is.na(value))) value <- paste(split_line[-1], collapse = " ")
    params[[name]] <- value
  }

  as.data.frame(params, stringsAsFactors = FALSE)
}

#' Save QSM results and patch parameters
#'
#' Writes QSM cylinder data and parameter summaries to tab-delimited text files.
#'
#' @param qsm Output from \code{run_treeqsm()}; a list with \code{qsm} and \code{qsm_pars}.
#' @param name Base name to use for output files.
#' @param output_dir Directory where files are written (defaults to current working directory).
#' @importFrom utils write.table
#' @return Invisibly returns a list with file paths:
#'   \itemize{
#'     \item \code{qsm}: Path to the QSM cylinder file.
#'     \item \code{pars}: Path to the parameter summary file.
#'   }
#' @examples
#' \dontrun{
#' # Run and Load a qsm from a laz file.
#' # ---- Step 1. Define input file  ----
#' # Input file
#' file <- system.file("extdata", "tree_0744.laz", package = "tReeTraits")
#' tree_id <- tools::file_path_sans_ext(basename(file))
#'
#' # ---- Step 2. Run TreeQSM ----
#' # Multiple parameter combinations can be supplied; TreeQSM optimizes across them
#' qsm_result <- run_treeqsm(
#'   file = file,
#'   intensity_threshold = 40000,
#'   resolution = 0.02,
#'   patch_diam1 = c(0.05, 0.1),
#'   patch_diam2min = c(0.04, 0.05),
#'   patch_diam2max = c(0.12, 0.14),
#'   verbose = TRUE
#' )
#'
#' # ---- Step 3. Save results ----
#' write_qsm(
#'   qsm_result,
#'   name = tree_id,
#'   output_dir = tempdir()
#' )
#'
#' # ---- Step 4. Reload QSM ----
#' qsm_path <- file.path(tempdir(), paste0(tree_id, "_qsm.txt"))
#' qsm <- load_qsm(qsm_path)
#'
#' # ---- Step 5. Visualize ----
#' plot_qsm2d(qsm, scale = 50)
#' plot_qsm3d(qsm)}
#' @export
write_qsm <- function(qsm, name, output_dir = getwd()) {
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

  qsm_file <- file.path(output_dir, paste0(name, "_qsm.txt"))
  pars_file <- file.path(output_dir, paste0(name, "_qsm_pars.txt"))

  # Write cylinder data
  write.table(qsm$qsm, file = qsm_file, row.names = FALSE, sep = "\t", quote = FALSE)

  # Write parameters
  pars <- qsm$qsm_pars
  df <- as.data.frame(t(unlist(pars)))
  colnames(df) <- names(pars)
  utils::write.table(df, file = pars_file, row.names = FALSE, sep = "\t", quote = FALSE)

  message("Outputs written to:\n", qsm_file, "\n", pars_file)
  invisible(list(qsm = qsm_file, pars = pars_file))
}
