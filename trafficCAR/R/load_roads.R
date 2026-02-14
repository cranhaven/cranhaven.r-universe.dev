
#' Load road geometries from sf object or file
#'
#' Supports sf objects, .rda files containing a single sf object,
#' or GeoJSON files.
#'
#' @param x sf object or file path
#' @keywords internal
load_roads <- function(x) {
  if (inherits(x, "sf")) return(x)

  if (!is.character(x) || length(x) != 1L) {
    stop("`roads` must be an sf object or a file path")
  }

  if (!file.exists(x)) {
    stop("File does not exist: ", x)
  }

  ext <- tolower(tools::file_ext(x))

  if (ext == "rda") {
    e <- new.env(parent = emptyenv())
    load(x, envir = e)
    objs <- as.list(e)
    if (length(objs) != 1L || !inherits(objs[[1L]], "sf")) {
      stop(".rda must contain exactly one sf object")
    }
    return(objs[[1L]])
  }

  if (ext %in% c("geojson", "json")) {
    return(sf::st_read(x, quiet = TRUE))
  }

  stop("Unsupported file type: ", ext)
}
