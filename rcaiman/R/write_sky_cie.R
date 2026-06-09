#' Write and read out-of-the-box CIE sky model and raster
#'
#' @description
#' Persist and restore the out-of-the-box CIE sky model, its diagnostics, and
#' related rasters/points. The writer produces a human-readable `.txt` manifest
#' plus CSV and GeoPackage sidecar files. The reader reconstructs a list object
#' compatible with the out-of-the-box pipeline.
#'
#' @section Functions:
#' \describe{
#'   \item{`write_sky_cie`}{No return value. Writes six files to disk with the
#'   prefix `name` (see below).}
#'   \item{`read_sky_cie`}{Returns a `list` similar to the output of
#'   `ootb_sky_cie()` and suitable as input to `ootb_sky_above()`.}
#' }
#'
#' @section Files written by `write_sky_cie`:
#' \itemize{
#'   \item Plain text manifest: `name.txt`
#'   \item CSV with sky radiance samples: `name_rr.csv`
#'   \item CSV with sky radiance samples for the upward pass: `name_rr_up.csv` (optional)
#'   \item GeoPackage with sky sample points: `name_sky_points.gpkg`
#'   \item GeoPackage with the sun disk location: `name_sun_disk.gpkg`
#'   \item CSV with validation pairs: `name_val.csv`
#' }
#'
#' @section Text file keys:
#'
#' \describe{
#'   \item{`format_version:`}{Semantic version of the manifest.}
#'   \item{`sun_theta:`}{Solar zenith (deg).}
#'   \item{`sun_phi:`}{Solar azimuth (deg).}
#'   \item{`method_sun:`}{Method used to optimize sun coordinates.}
#'   \item{`zenith_dn:`}{Reference DN at zenith.}
#'   \item{`start_a:`…`start_e:`}{Initial CIE coefficients.}
#'   \item{`is_from_detected_sky_dn:`}{Enables `_rr_up.csv` if `TRUE`.}
#'   \item{`fit_a:`…`fit_e:`}{Fitted CIE coefficients.}
#'   \item{`method:`}{Method used to fit CIE coefficients.}
#'   \item{`dist_to_black:`}{Argument passed to `extract_sky_points()`.}
#'   \item{`grid:`}{Sky grid parameters (`angle_width`, `first_ring_different`).}
#'   \item{`min_spherical_dist:`}{Sampling buffer distance (deg).}
#'   \item{`sky_points_no:`}{Number of sky points.}
#'   \item{`outliers_no:`}{Number of sky points that were detected as outliers.}
#'   \item{`rmse:`}{Validation metrics. Root mean squared error.}
#'   \item{`r_squared:`}{Validation metric. Coefficient of determination.}
#'   \item{`mae:`}{Validation metrics. Mean absolute error.}
#'   \item{`[Tested: …]`}{Enumerates tested methods/grids/distances.}
#' }
#'
#' @details
#' Encoding is UTF-8. Decimal point is `.`. Unknown keys are
#' ignored with a warning. Missing required keys trigger an error. The manifest
#' begins with `format_version:` which is checked for basic compatibility.
#'
#' When `read_sky_cie()` detects manual edits (moved sun disk or changed sky
#' points) and `refit_allowed = TRUE`, it re-fits the CIE model using the
#' current `r`, `z`, and `a`, then revalidates.
#'
#' @param name character vector of length one. File base name without extension.
#'   A path can be included, e.g., `"C:/Users/Doe/Documents/DSCN4500"`.
#' @param sky_cie list. Object holding the fitted CIE model, diagnostics, and
#'   derived rasters, as produced by the out-of-the-box workflow.
#' @param r numeric [terra::SpatRaster-class] of one layer. The canopy image
#'   used in the out-of-the-box workflow (used by `read_sky_cie()` when
#'   refitting).
#' @inheritParams sky_grid_segmentation
#' @param refit_allowed logical vector of length one. If `TRUE`, allow automatic
#'   re-fit when manual edits are detected.
#'
#' @return See *Functions*.
#'
#' @name write_sky_cie
#' @rdname write_sky_cie
#' @aliases read_sky_cie
#'
#' @export
#'
#' @examples
#' \dontrun{
#' caim <- read_caim()
#' z <- zenith_image(ncol(caim), lens())
#' a <- azimuth_image(z)
#'
#' # Read a previously written model
#' path <- system.file("external/example.txt", package = "rcaiman")
#' sky_cie <- read_sky_cie(gsub(".txt", "", path), r = caim$Blue, z = z, a = a,
#'                         refit_allowed = TRUE)
#' }
write_sky_cie <- function(sky_cie, name) {
  if (!is.list(sky_cie)) {
    stop("`sky_cie` must be a list returned by `ootb_sky_cie()`. ")
  }
  required_names <- c(
                      "rr_raster",
                      "model",
                      "model_validation",
                      "dist_to_black",
                      "use_window",
                      "min_spherical_dist",
                      "sky_points",
                      "sun_row_col",
                      "g",
                      "tested_grids",
                      "tested_distances",
                      "tested_methods",
                      "optimal_start",
                      "model_up"
                      )
  if (!all(required_names %in% names(sky_cie))) {
    stop(sprintf("`sky_cie` must contain %s.",
                 paste(sprintf('"%s"', required_names), collapse = ", ")))
  }
  .check_vector(name, "character", 1)

  # helpers ---------------------------------------------------------------
  .print_line <- function(...) cat(paste0(...), "\n")
  .hr <- function(ch = "-", n = 80) paste0(rep(ch, n), collapse = "")

  # manifest --------------------------------------------------------------
  con <- file(paste0(name, ".txt"), open = "wt", encoding = "UTF-8")
  on.exit(close(con), add = TRUE)
  sink(con)
  .print_line(.hr("-"))
  .print_line("format_version:", " 1.0")
  .print_line("generated_by:", " rcaiman::write_sky_cie()  # do not edit by hand")
  .print_line(.hr("-"))

  # sun and methods
  .print_line("sun_theta:", unname(sky_cie$model$sun_angles["z"]))
  .print_line("sun_phi:",   unname(sky_cie$model$sun_angles["a"]))
  if (!is.null(sky_cie$model$method_sun))
    .print_line("method_sun:", sky_cie$model$method_sun)
  for (m in sky_cie$tested_methods) .print_line("[Tested:", m, "]")


  .print_line(.hr("."))
  # starts
  .print_line("zenith_dn:", sky_cie$model$rr$zenith_dn)
  starts <- unname(sky_cie$optimal_start)
  .print_line("start_a:", starts[1]); .print_line("start_b:", starts[2])
  .print_line("start_c:", starts[3]); .print_line("start_d:", starts[4])
  .print_line("start_e:", starts[5])
  .print_line("is_from_detected_sky_dn:",
              unname(isTRUE(attr(sky_cie$optimal_start, "is_from_detected_sky_dn"))))

  .print_line(.hr("."))
  # fit
  co <- unname(sky_cie$model$coef[1:5])
  .print_line("fit_a:", co[1]); .print_line("fit_b:", co[2])
  .print_line("fit_c:", co[3]); .print_line("fit_d:", co[4])
  .print_line("fit_e:", co[5])
  .print_line("method:", unname(sky_cie$model$method))
  for (m in sky_cie$tested_methods) .print_line("[Tested:", m, "]")


  .print_line(.hr("."))
  # grid and sampling
  .print_line("dist_to_black:", if (!is.null(sky_cie$dist_to_black)) sky_cie$dist_to_black else NA)
  .print_line("grid:", paste(attr(sky_cie$g, "angle_width"),
                             attr(sky_cie$g, "first_ring_different")))
  for (m in sky_cie$tested_grids) .print_line("[Tested:", m, "]")

  .print_line("min_spherical_dist:", if (!is.null(sky_cie$min_spherical_dist)) sky_cie$min_spherical_dist else NA)
  for (m in sky_cie$tested_distances) .print_line("[Tested:", m, "]")

  .print_line(.hr("."))
  # counts and validation
  .print_line("sky_points_no:", nrow(sky_cie$sky_points))
  .print_line("outliers_no:",   sum(sky_cie$sky_points$is_outlier, na.rm = TRUE))
  .print_line("rmse:", sky_cie$model_validation$rmse)
  .print_line("r_squared:", sky_cie$model_validation$r_squared)
  .print_line("mae:", sky_cie$model_validation$mae)
  .print_line(.hr("-"))
  sink()

  # sidecar files ---------------------------------------------------------
  # sky points to GPKG
  cells <- terra::cellFromRowCol(sky_cie$rr_raster,
                                 sky_cie$sky_points$row,
                                 sky_cie$sky_points$col)
  xy <- terra::xyFromCell(sky_cie$rr_raster, cells)
  p <- terra::vect(xy, "points")
  terra::crs(p) <- terra::crs(sky_cie$rr_raster)
  terra::writeVector(p, paste0(name, "_sky_points.gpkg"), filetype = "GPKG")

  # sun disk to GPKG
  cells <- terra::cellFromRowCol(sky_cie$rr_raster,
                                 sky_cie$sun_row_col$row,
                                 sky_cie$sun_row_col$col)
  xy <- terra::xyFromCell(sky_cie$rr_raster, cells)
  p <- terra::vect(xy, "points")
  terra::crs(p) <- terra::crs(sky_cie$rr_raster)
  terra::writeVector(p, paste0(name, "_sun_disk.gpkg"), filetype = "GPKG")

  # validation CSV
  utils::write.csv2(data.frame(pred = sky_cie$model_validation$pred,
                               obs  = sky_cie$model_validation$obs),
                    paste0(name, "_val.csv"),
                    row.names = FALSE)

  # rr CSV
  if (!is.null(sky_cie$model$rr$sky_points)) {
    utils::write.csv2(sky_cie$model$rr$sky_points,
                      paste0(name, "_rr.csv"),
                      row.names = FALSE)
  }

  # rr_up CSV (optional)
  if (isTRUE(attr(sky_cie$optimal_start, "is_from_detected_sky_dn")) &&
      !is.null(sky_cie$model_up$rr$sky_points)) {
    utils::write.csv2(sky_cie$model_up$rr$sky_points,
                      paste0(name, "_rr_up.csv"),
                      row.names = FALSE)
  }

  invisible(NULL)
}

#' @rdname write_sky_cie
#' @export
read_sky_cie <- function(name, r, z, a, refit_allowed = FALSE) {
  .check_vector(name, "character", 1)
  .assert_file_exists(paste0(name, ".txt"))
  .check_r_z_a_m(r, z, a, r_type = "single")
  .check_vector(refit_allowed, "logical", 1)

  # helpers ---------------------------------------------------------------
  .read_manifest <- function(path) {
    if (!file.exists(path)) stop("Manifest not found: ", path)
    readLines(path, encoding = "UTF-8", warn = FALSE)
  }
  .get_token_after <- function(lines, key, cast = c("char","num","log"), required = TRUE) {
    cast <- match.arg(cast)
    pat <- paste0("^", key, "\\s*:\\s*(.*)$")
    hit <- grep(pat, lines)
    if (!length(hit)) {
      if (required) stop("Missing key: ", key) else return(NA)
    }
    val <- sub(pat, "\\1", lines[hit[1]])
    if (cast == "num") return(suppressWarnings(as.numeric(val)))
    if (cast == "log") return(tolower(trimws(val)) %in% c("true","t","1"))
    trimws(val)
  }
  .get_two_tokens_after <- function(lines, key) {
    pat <- paste0("^", key, "\\s*:\\s*(.*)$")
    hit <- grep(pat, lines)
    if (!length(hit)) stop("Missing key: ", key)
    val <- strsplit(sub(pat, "\\1", lines[hit[1]]), "\\s+")[[1]]
    list(tok1 = suppressWarnings(as.numeric(val[1])),
         tok2 = tolower(val[2]) %in% c("true","t","1"))
  }
  .get_tested <- function(lines) {
    x <- trimws(gsub("^\\[Tested:\\s*|\\]$", "", grep("^\\[Tested:", lines, value = TRUE)))
    unique(x[nzchar(x)])
  }

  # init structure --------------------------------------------------------
  sky_cie <- list()
  sky_cie$io <- list()
  sky_cie$model <- list()
  sky_cie$model$rr <- list()
  sky_cie$model_up <- list()
  sky_cie$model_up$rr <- list()
  sky_cie$model_validation <- list()

  # parse manifest --------------------------------------------------------
  lines <- .read_manifest(paste0(name, ".txt"))

  sky_cie$io$format_version <- .get_token_after(lines, "format_version", "char", required = TRUE)
  if (!grepl("^1\\.", sky_cie$io$format_version)) {
    warning("Unknown manifest format_version: ", sky_cie$io$format_version)
  }

  sky_cie$model$sun_angles <- c(
    z = .get_token_after(lines, "sun_theta", "num", TRUE),
    a = .get_token_after(lines, "sun_phi",   "num", TRUE)
  )
  msun <- .get_token_after(lines, "method_sun", "char", required = FALSE)
  if (!is.na(msun)) sky_cie$model$method_sun <- msun

  sky_cie$tested_methods <- .get_tested(lines)

  sky_cie$model$rr$zenith_dn <- .get_token_after(lines, "zenith_dn", "num", TRUE)

  sky_cie$optimal_start <- stats::setNames(
    c(
      .get_token_after(lines, "start_a", "num", TRUE),
      .get_token_after(lines, "start_b", "num", TRUE),
      .get_token_after(lines, "start_c", "num", TRUE),
      .get_token_after(lines, "start_d", "num", TRUE),
      .get_token_after(lines, "start_e", "num", TRUE)
    ),
    c("a","b","c","d","e")
  )
  attr(sky_cie$optimal_start, "is_from_detected_sky_dn") <-
    .get_token_after(lines, "is_from_detected_sky_dn", "log", TRUE)

  sky_cie$model$coef <- c(
    .get_token_after(lines, "fit_a", "num", TRUE),
    .get_token_after(lines, "fit_b", "num", TRUE),
    .get_token_after(lines, "fit_c", "num", TRUE),
    .get_token_after(lines, "fit_d", "num", TRUE),
    .get_token_after(lines, "fit_e", "num", TRUE)
  )
  sky_cie$model$method <- .get_token_after(lines, "method", "char", TRUE)

  # grids and distances
  sky_cie$dist_to_black <- suppressWarnings(.get_token_after(lines, "dist_to_black", "num", required = FALSE))
  sky_cie$use_window <- length(sky_cie$dist_to_black) == 1 && !is.na(sky_cie$dist_to_black)

  gd <- .get_two_tokens_after(lines, "grid")
  sky_cie$g <- tryCatch(
    sky_grid_segmentation(z, a, gd$tok1, gd$tok2),
    error = function(e) NA
  )

  msd <- suppressWarnings(.get_token_after(lines, "min_spherical_dist", "num", required = FALSE))
  if (!is.na(msd)) sky_cie$min_spherical_dist <- msd

  # validation metrics
  sky_cie$model_validation$r_squared <- .get_token_after(lines, "r_squared", "num", TRUE)
  sky_cie$model_validation$rmse      <- .get_token_after(lines, "rmse",      "num", TRUE)
  sky_cie$model_validation$mae       <- .get_token_after(lines, "mae",       "num", TRUE)

  # sidecar files ---------------------------------------------------------
  # validation pairs
  df_val <- utils::read.csv2(paste0(name, "_val.csv"))
  sky_cie$model_validation$pred <- df_val$pred
  sky_cie$model_validation$obs  <- df_val$obs

  # rr
  sky_cie$model$rr$sky_points <- utils::read.csv2(paste0(name, "_rr.csv"))

  # rr_up (optional)
  if (isTRUE(attr(sky_cie$optimal_start, "is_from_detected_sky_dn"))) {
    f_up <- paste0(name, "_rr_up.csv")
    sky_cie$model_up$rr$sky_points <- if (file.exists(f_up)) utils::read.csv2(f_up) else NULL
  } else {
    sky_cie$model_up$rr$sky_points <- NULL
  }

  # sky_points from GPKG → row/col
  sp <- terra::vect(paste0(name, "_sky_points.gpkg"))
  ext <- terra::extract(z, sp, cells = TRUE)
  rc  <- terra::rowColFromCell(z, ext$cell)
  sky_cie$sky_points <- data.frame(row = rc[,1], col = rc[,2])

  # sun disk from GPKG → row/col and angles
  sd <- terra::vect(paste0(name, "_sun_disk.gpkg"))
  ext <- terra::extract(z, sd, cells = TRUE)
  rc  <- terra::rowColFromCell(z, ext$cell)
  sun_row_col <- data.frame(row = rc[,1], col = rc[,2])

  # consistency checks and optional refit --------------------------------
  # angular distance between stored sun and GPKG sun
  if (nrow(sun_row_col) != 1) {
    warning("Sun is outside the raster grid; cannot verify manual edit of sun position.")
    delta <- 0
  } else {
    sza_saa <- zenith_azimuth_from_row_col(z, a, sun_row_col$row, sun_row_col$col)
    sza_saa <- as.numeric(sza_saa)
    delta <- calc_spherical_distance(
      .degree2radian(sky_cie$model$sun_angles["z"]),
      .degree2radian(sky_cie$model$sun_angles["a"]),
      .degree2radian(sza_saa[1]),
      .degree2radian(sza_saa[2])
    )
    names(sza_saa) <- c("z","a")
  }

  # sort_df <- function(df) df[order(df$row, df$col), c("row","col")]
  # points_changed <- !identical(
  #   sort_df(sky_cie$model$rr$sky_points),
  #   sort_df(sky_cie$sky_points)
  # )
  .norm_points <- function(x) {
    x <- x[, c("row","col"), drop = FALSE]
    x$row <- as.integer(x$row)
    x$col <- as.integer(x$col)
    x <- x[order(x$row, x$col), , drop = FALSE]
    rownames(x) <- NULL
    x
  }

  points_changed <- !identical(
    .norm_points(sky_cie$model$rr$sky_points),
    .norm_points(sky_cie$sky_points)
  )

  if ( (delta > pi/180) || points_changed ) {
    msg <- "Detected manual changes. "
    if (isTRUE(refit_allowed)) {
      rr <- extract_rr(r, z, a, sky_cie$sky_points, use_window = sky_cie$use_window)
      sky_cie$model <- fit_cie_model(rr, sza_saa,
                                     custom_sky_coef = sky_cie$model$coef)
      sky_cie$model_validation <- validate_cie_model(sky_cie$model, k = 10)
      message(msg, "Automatically re-fitting the CIE model.")
    } else {
      warning(msg, "Re-fitting the CIE model is recommended.")
    }
  }

  # regenerate rr_raster for convenience
  sky_cie$rr_raster <- cie_image(z, a, sky_cie$model$sun_angles, sky_cie$model$coef)

  sky_cie$io$source <- name
  sky_cie
}
