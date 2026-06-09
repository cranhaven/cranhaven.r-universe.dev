#' Read a canopy image from a raw file
#'
#' @description
#' Read unprocessed sensor data from a camera RAW file and split the signal by
#' spectral band according to the in-camera color filter array (CFA). Use this
#' to obtain images with precise radiometry.
#'
#' @details
#' Uses Python [`rawpy`](https://pypi.org/project/rawpy/) through `reticulate`
#' to access sensor data and black-level metadata. Optionally extracts only the
#' blue/cyan band.
#'
#' @section Check Python Accessibility:
#'
#' To ensure that R can access a Python installation, run the following test:
#'
#' ````
#' reticulate::py_eval("1+1")
#'
#' ````
#'
#' If R can access Python successfully, you will see `2` in the console. If not,
#' you will receive instructions on how to install Python.
#'
#' @section Create a Virtual Environment:
#'
#' After passing the Python accessibility test, create a virtual environment
#' using the following command:
#'
#' ````
#' reticulate::virtualenv_create()
#'
#' ````
#'
#' @section Install `rawpy`:
#'
#' Install the rawpy package within the virtual environment:
#'
#' ````
#' reticulate::py_install("rawpy")
#'
#' ````
#'
#' @section For RStudio Users:
#'
#' If you are an RStudio user who works with projects, you will need a
#' _.Renviron_ file in the root of each project. To create a _.Renviron_ file,
#' follow these steps:
#'
#' * Create a "New Blank File" named ".Renviron" (without an extension) in the
#' project's root directory.
#'
#' * Run bellow code:
#'
#' ````
#' path <- file.path(reticulate::virtualenv_root(),
#' reticulate::virtualenv_list(), "Scripts", "python.exe")
#' paste("RETICULATE_PYTHON =", path)
#'
#' ````
#'
#' * Copy/paste the line from the console (the string between the quotes) into
#' the .Renviron file. This is an example `RETICULATE_PYTHON =
#' ~/.virtualenvs/r-reticulate/Scripts/python.exe`
#'
#' * Do not forget to save the changes
#'
#' By following these steps, users can easily set up their environment to access
#' raw data efficiently, but it is not the only way of doing it, you might know
#' an easier or better one.
#'
#' See the help page of [read_caim()] and [fisheye_to_equidistant()] as a
#' complement to this help page. Further details about raw files can be found in
#' \insertCite{Diaz2024;textual}{rcaiman}.
#'
#' @param path character vector of length one. Path to a file with raw data
#'   (including file extension).
#' @param only_blue logical vector of length one. If `TRUE`, return only the blue/cyan band.
#' @param offset_value numeric vector of length one. Optional black level offsets to replace
#'   [`black_level_per_channel`](https://www.libraw.org/docs/API-datastruct-eng.html#datastream_data:~:text=Per%2Dchannel%20black%20level%20correction)
#'   metadata obtained with `rawpy`.
#'
#' @references \insertAllCited{}
#'
#' @return Numeric [terra::SpatRaster-class]:
#' \itemize{
#'   \item single-layer if `only_blue = TRUE`.
#'   \item multi-layer if `only_blue = FALSE`, with one layer per color per CFA
#'   color (e.g., R, G, B).
#' }
#' Layers are named according to metadata in the raw file.
#'
#' @seealso [read_caim()]
#'
#' @export
#'
#' @examples
#' \dontrun{
#' file_name <- tempfile(fileext = ".NEF")
#' download.file("https://osf.io/s49py/download", file_name, mode = "wb")
#'
#' # Geometric and radiometric corrections -----------------------------------
#' zenith_colrow <- c(1290, 988)/2
#' diameter <- 756
#' z <- zenith_image(diameter, lens("Nikon_FCE9"))
#' a <- azimuth_image(z)
#' m <- !is.na(z)
#' caim <- read_caim_raw(file_name, only_blue = TRUE)
#' caim <- crop_caim(caim, zenith_colrow - diameter/2, diameter, diameter)
#' caim <- correct_vignetting(caim, z, c(0.0638, -0.101))
#' caim <- fisheye_to_equidistant(caim, z, a, m, radius = 300,
#'                                k = 1, p = 1, rmax = 100)
#' }
read_caim_raw <- function(path,
                          only_blue = FALSE,
                          offset_value = NULL) {
  .check_vector(path, "character", 1)
  .assert_file_exists(path)
  .check_vector(only_blue, "logical", 1)
  .check_vector(offset_value, "numeric", 1, allow_null = TRUE, sign = "positive")

  if (!requireNamespace("reticulate", quietly = TRUE)) {
    stop(paste("Package \"reticulate\" needed for this function to work.",
               "Please install it."),
         call. = FALSE)
  }

  rawpy <- reticulate::import("rawpy")

  img <- rawpy$imread(path)
  r <- crop_caim(rast(img$raw_image))
  m <- crop_caim(rast(img$raw_colors))
  black_level <- img$black_level_per_channel
  if (!is.null(offset_value)) black_level[] <- offset_value

  .fun <- function(label) {
    if (length(label) == 1) {
      r[m == label] <-  r[m == label] - black_level[label+1]
      foo <- rast(r) %>% terra::aggregate(., 2)
      foo[] <- r[m == label]
      r <- crop_caim(foo)
    } else {
      r[m == label[1]] <- r[m == label[1]] -
        black_level[label[1]+1]
      r[m == label[2]] <- r[m == label[2]] -
        black_level[label[2]+1]
      foo1 <- foo2 <- rast(r) %>% terra::aggregate(., 2)
      foo1[] <- r[m == label[1]]
      foo2[] <- r[m == label[2]]
      r <- crop_caim(mean(foo1, foo2))
    }
    r
  }

  color_desc <- as.character(img$color_desc)
  color_desc <- strsplit(color_desc, "") %>% unlist()
  raw_pattern <- c(img$raw_pattern[1,1],
                   img$raw_pattern[1,2],
                   img$raw_pattern[2,2],
                   img$raw_pattern[2,1])
  if (only_blue) {
    i <- grep("B", color_desc)
    if (length(i) == 0) {i <- grep("C", color_desc)}
    if (length(i) == 0) {stop("There is no blue or cyan filter.")}
    label <- raw_pattern[i]
    r <- .fun(label)
  } else {
    l <- list()
    unique_color_desc <- unique(color_desc)
    for (i in seq_along(unique_color_desc)) {
      color_filter <- grep(unique_color_desc[i], color_desc)
      l[[i]] <- raw_pattern[color_filter]
    }
    r <- Map(.fun, l)
    r <- terra::rast(r)
    names(r) <- unique_color_desc
  }
  img$close()
  r
}
