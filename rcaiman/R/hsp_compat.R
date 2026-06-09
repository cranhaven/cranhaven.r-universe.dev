#' HSP compatibility functions
#'
#' @description
#' Read and write legacy files from HSP (HemiSPherical Project Manager) projects
#' to interoperate with existing workflows. Intended for legacy support; not
#' required when working fully within `rcaiman`.
#'
#' @section About HSP software:
#'
#' HSP (introduced in \insertCite{Lang2013}{rcaiman}, based on the method in
#' \insertCite{Lang2010}{rcaiman}) runs exclusively on Windows. HSP stores
#' pre-processed images as PGM files in the `manipulate` subfolder of each
#' project (itself inside the `projects` folder).
#'
#' @section Functions:
#'
#' \describe{
#'   \item{\code{hsp_read_manual_input()}}{read sky marks and sun position
#'   defined manually within an HSP project; returns a named list with
#'   components \code{weight}, \code{max_points}, \code{angle},
#'   \code{point_radius}, \code{sun_row_col}, \code{sky_points}, and
#'   \code{zenith_dn}.}
#'
#'   \item{\code{hsp_read_opt_sky_coef()}}{read optimized CIE sky coefficients
#'   from an HSP project; returns a numeric vector of length five.}
#'
#'   \item{\code{hsp_write_sky_points()}}{write a file with sky point
#'   coordinates compatible with HSP; creates a file on disk.}
#'
#'   \item{\code{hsp_write_sun_coord()}}{write a file with solar disk
#'   coordinates compatible with HSP; creates a file on disk.}
#' }
#'
#' @param sky_points `data.frame` with columns `row` and `col`.
#' @param path_to_HSP_project character vector of length one. Path to the HSP
#'   project folder (e.g., `"C:/Users/johndoe/Documents/HSP/projects/my_prj/"`).
#' @param img_name character vector of length one (e.g., `"DSCN6342.pgm"` or
#'   `"DSCN6342"`). See *About HSP software*.
#' @param sun_row_col numeric vector of length two. Raster coordinates (row,
#'   column) of the solar disk.
#'
#' @return See *Functions*
#'
#' @references \insertAllCited{}
#'
#' @name hsp_compat
#' @rdname hsp_compat
#' @aliases hsp_read_manual_input hsp_read_opt_sky_coef hsp_write_sky_points hsp_write_sun_coord
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # NOTE: assumes the working directory is the HSP project folder (e.g., an RStudio project).
#'
#' # From HSP to R in order to compare ---------------------------------------
#' r <- read_caim("manipulate/IMG_1013.pgm")
#' z <- zenith_image(ncol(r), lens())
#' a <- azimuth_image(z)
#' manual_input <- read_manual_input(".", "IMG_1013")
#' sun_row_col <- manual_input$sun_row_col
#' sun_angles <- zenith_azimuth_from_row_col(
#'   z, a,
#'   sun_row_col[1],
#'   sun_row_col[2]
#' )
#' sun_angles <- as.vector(sun_angles)
#'
#' sky_points <- manual_input$sky_points
#' rr <- extract_rr(r, z, a, sky_points)
#' model <- fit_cie_model(rr, sun_angles)
#' sky <- cie_image(
#'   z, a,
#'   model$sun_angles,
#'   model$coef
#' ) * model$rr$zenith_dn
#' plot(r / sky)
#'
#' r <- read_caim("manipulate/IMG_1013.pgm")
#' sky_coef <- read_opt_sky_coef(".", "IMG_1013")
#' sky_m <- cie_image(z, a, sun_angles, sky_coef)
#' sky_m <- cie_sky_manual * manual_input$zenith_dn
#' plot(r / sky_m)
#'
#' # From R to HSP  ----------------------------------------------------------
#' r <- read_caim("manipulate/IMG_1014.pgm")
#' z <- zenith_image(ncol(r), lens())
#' a <- azimuth_image(z)
#' m <- !is.na(z)
#' g <- sky_grid_segmentation(z, a, 10)
#' bin <- binarize_with_thr(caim$Blue, thr_isodata(caim$Blue[m]))
#' bin <- select_sky_region(z, 0, 85) & bin
#'
#' sun_angles <- estimate_sun_angles(r, z, a, bin, g)
#' sun_row_col <- row_col_from_zenith_azimuth(
#'   z, a,
#'   sun_angles["z"],
#'   sun_angles["a"]
#' ) %>% as.numeric()
#' write_sun_coord(sun_row_col, ".", "IMG_1014")
#'
#' sky_points <- extract_sky_points(r, bin, g)
#' write_sky_points(sky_points, ".", "IMG_1014")
#' }
hsp_read_manual_input <- function(path_to_HSP_project, img_name) {
  .check_vector(path_to_HSP_project, "character", 1)
  .check_vector(path_to_HSP_project, "img_name", 1)

  files <- dir(file.path(path_to_HSP_project, "manipulate"),
               pattern = "settings", full.names = TRUE)
  file <- files[grep(img_name, files)]
  settings <- scan(file, "character")
  settings <- settings[c(9, 11:13)]
  settings <- data.frame(
    name = Map(function(x) x[1], strsplit(settings, "=")) %>% unlist(),
    value = Map(function(x) x[2], strsplit(settings, "=")) %>% unlist()
  )

  files <- dir(file.path(path_to_HSP_project, "manipulate"),
               pattern = "sun", full.names = TRUE)
  file <- files[grep(img_name, files)]
  sun <- scan(file, "character")
  sun <- strsplit(sun, "\\.") %>% unlist() %>% as.numeric()
  sun_mark <- list()
  sun_row_col <- rev(sun)

  files <- dir(file.path(path_to_HSP_project, "manipulate"),
               pattern = "points", full.names = TRUE)
  file <- files[grep(img_name, files)]
  sky_marks <- scan(file, "character", skip = 1)
  sky_marks <- strsplit(sky_marks, "\\.") %>%
    unlist() %>%
    as.numeric() %>%
    matrix(., ncol = 3, byrow = TRUE) %>%
    as.data.frame(.)
  names(sky_marks) <- c("col", "row", "type" )

  files <- dir(file.path(path_to_HSP_project, "manipulate"),
               pattern = "statistics", full.names = TRUE)
  file <- files[grep(img_name, files)]
  content <- scan(file, "character", skip = 1, sep = "\n")
  zenith_dn <- content[grep( "Zenith", content)]
  zenith_dn <- strsplit(zenith_dn, "=")[[1]][2] %>%
    sub(",", ".", .) %>% as.numeric()

  list(weight = settings[1,2] %>% as.numeric(),
       max_points = settings[2,2] %>% as.numeric(),
       angle = settings[3,2] %>% as.numeric(),
       point_radius = settings[4,2] %>% as.numeric(),
       sun_row_col = sun_row_col,
       sky_points = sky_marks,
       zenith_dn = zenith_dn)
}

#' @rdname hsp_compat
#' @export
hsp_read_opt_sky_coef <- function(path_to_HSP_project, img_name) {
  .check_vector(path_to_HSP_project, "character", 1)
  .check_vector(path_to_HSP_project, "img_name", 1)

  files <- dir(file.path(path_to_HSP_project, "manipulate"),
               pattern = "opt-parameters", full.names = TRUE)
  file <- files[grep(img_name, files)]
  sky_coef <- scan(file, "character", skip = 1)
  sky_coef <- data.frame(
    name = Map(function(x) x[1], strsplit(sky_coef, "=")) %>% unlist(),
    value = Map(function(x) x[2], strsplit(sky_coef, "=")) %>% unlist()
  )
  sky_coef[c(2, 1, 5, 4, 3), 2] %>% sub(",", ".", .) %>% as.numeric()
}

#' @rdname hsp_compat
#' @export
hsp_write_sky_points <- function(sky_points, path_to_HSP_project, img_name) {
  .check_sky_points(sky_points)
  .check_vector(path_to_HSP_project, "character", 1)
  .check_vector(path_to_HSP_project, "img_name", 1)

  no <- nrow(sky_points)

  col.row_coordinates <- paste(sky_points$col, sky_points$row, "3", sep = ".")
  col.row_coordinates <- paste(col.row_coordinates, collapse = " ")

  sky_points <- c(no, col.row_coordinates)
  sky_points <- data.frame(sky_points)

  img_name <- filenamer::as.filename(img_name)
  img_name <- filenamer::trim_ext(img_name) %>% as.character()

  utils::write.table(sky_points, file.path(path_to_HSP_project,
                                           "manipulate",
                                           paste0(img_name, "_points.conf")),
                     quote = FALSE, row.names = FALSE, col.names = FALSE,
                     fileEncoding = "UTF-8", eol = "\n")
}

#' @rdname hsp_compat
#' @export
hsp_write_sun_coord <- function(sun_row_col, path_to_HSP_project, img_name) {
  .check_vector(sun_row_col, "numeric", 2, sign = "positive")
  .check_vector(path_to_HSP_project, "character", 1)
  .check_vector(path_to_HSP_project, "img_name", 1)

  sun_col_row <- paste(sun_row_col[c(2,1)], collapse = ".")

  img_name <- filenamer::as.filename(img_name)
  img_name <- filenamer::trim_ext(img_name) %>% as.character()

  utils::write.table(sun_col_row, file.path(path_to_HSP_project,
                                            "manipulate",
                                            paste0(img_name, "_sun.conf")),
                     quote = FALSE, row.names = FALSE, col.names = FALSE,
                     fileEncoding = "UTF-8", eol = "\n")
}
