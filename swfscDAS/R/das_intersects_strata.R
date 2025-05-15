#' DAS strata - points
#'
#' Determine if swfscDAS outputs intersect with strata polygons
#'
#' @param x a data frame (such as an object of class \code{das_df}) or a list.
#'   If \code{x} is a list, then it must be the output of
#'   \code{\link{das_effort}} or \code{\link{das_effort_sight}}.
#'   If \code{x} is a data frame, the user must also specify
#'   the coordinate columns of \code{x} using \code{x.lon} and \code{x.lat}
#' @param ... ignored
#' @param strata.files list of path(s) of the CSV file(s) with points defining each stratum.
#'   The CSV files must contain headers and be a closed polygon.
#'   The list may be named; see 'Value' section for how these names are used
#' @param x.lon character; name of the longitude column of \code{x}.
#'   Ignored if \code{x} is a list; default is "Lon"
#' @param x.lat character; name of the latitude column of \code{x}.
#'   Ignored if \code{x} is a list; default is "Lat"
#' @param strata.which logical; indicates if the numeric column 'strata_which' should
#'   be included in the output data frame.
#'   Ignored if \code{x} is a list; default is \code{FALSE}.
#'   See 'Value' section for more details
#'
#' @details Assigns DAS event points or segment midpoints to strata polygons
#'   using \code{\link[sf:geos_binary_pred]{st_intersects}}.
#'
#'   If \code{x} is a list, then 1) it must be the output of
#'   \code{\link{das_effort}} or \code{\link{das_effort_sight}} and
#'   2) the segment midpoints (column names mlon and mlat, respectively)
#'   are the points checked if they intersect with each provided stratum.
#'   If \code{x} is a data frame, then the user must provide the columns
#'   that specify the point coordinates to check.
#'
#'   \code{x} should not be an object of class \code{das_dfr},
#'   or an object of class \code{das_df} created with \code{add.dtll.sight = FALSE},
#'   because the ? and numeric event codes will have NA latitude and longitude values.
#'
#' @return If \code{x} is a data frame, then logical columns are added to \code{x}
#'   indicating if each point intersected with the corresponding stratum polygon.
#'   The names of these columns are the names of \code{strata.files};
#'   the element(s) of \code{strata.files} will have the name InPoly#,
#'   where '#' is the index of that stratum polygon in \code{strata.files}.
#'   If \code{strata.which}, then the column 'strata_which' is added to the end of the data frame.
#'   This column contains either a 0 if the point intersects with no strata or
#'   2) a numeric indicating the index (in \code{strata.files}) of
#'   the (first) strata polygon that the point intersects with.
#'
#'   Otherwise, i.e. if \code{x} is a list and thus the output of one of the effort functions,
#'   then the stratum columns are added to both the segdata and sightinfo data frames.
#'   However, note that the columns added to the sightinfo data frame still indicate
#'   whether or not the segment midpoint was in the corresponding stratum,
#'   rather than the sighting point itself.
#'
#' @examples
#' y <- system.file("das_sample.das", package = "swfscDAS")
#' y.proc <- das_process(y)
#' y.eff <- das_effort(y.proc, method = "section", num.cores = 1)
#'
#' stratum.file <- system.file("das_sample_stratum.csv", package = "swfscDAS")
#' das_intersects_strata(y.eff, list(InPoly = stratum.file), x.lon = "Lon", x.lat = "Lat")
#'
#' das_intersects_strata(y.proc, list(stratum.file))
#'
#' # Visualize effort midpoints and stratum polygon
#' require(sf)
#' y.eff.strata <- das_intersects_strata(y.eff, list(InPoly = stratum.file))
#' segdata <- st_as_sf(y.eff.strata$segdata, coords = c("mlon", "mlat"), crs = 4326)
#'
#' # Make stratum polygon
#' stratum.df <- read.csv(stratum.file)
#' stratum.sfc <- st_sfc(
#'   st_polygon(list(matrix(c(stratum.df$Lon, stratum.df$Lat), ncol = 2))),
#'   crs = 4326
#' )
#'
#' plot(segdata["InPoly"], axes = TRUE, reset = FALSE,
#'      xlim = c(-137, -142.5), ylim = c(42, 47))
#' plot(stratum.sfc, add = TRUE)
#'
#' @export
das_intersects_strata <- function(x, ...) UseMethod("das_intersects_strata")

#' @name das_intersects_strata
#' @export
das_intersects_strata.list <- function(x, strata.files, ...) {
  if (!identical(names(x), c("segdata", "sightinfo", "randpicks")))
    stop("If x is a list, then it must be the output of das_effort or ",
         "das_effort_sight. ",
         "Thus, it must contain exactly three named elements: ",
         "segdata, sightinfo, and randpicks (in order)")

  x1 <- das_intersects_strata(x$segdata, strata.files, "mlon", "mlat")
  names.new <- base::setdiff(names(x1), names(x$segdata))
  x1.tojoin <- x1 %>% select(.data$segnum, !!names.new)

  x2 <- left_join(x$sightinfo, x1.tojoin, by = "segnum")
  # x2 <- das_intersects_strata(x$sightinfo, strata.files, "mlat", "mlon")

  list(segdata = x1, sightinfo = x2, randpicks = x$randpicks)
}

#' @name das_intersects_strata
#' @export
das_intersects_strata.data.frame <- function(x, strata.files, x.lon = "Lon", x.lat = "Lat",
                                             strata.which = FALSE, ...) {
  # Make x into an sfc object
  if (!all(c(x.lat, x.lon) %in% names(x)))
    stop("x.lon and x.lat must be the names of numeric columns in x")

  x.coords <- data.frame(lon = x[[x.lon]], lat = x[[x.lat]])
  if (!(inherits(x.coords$lon, "numeric") & inherits(x.coords$lat, "numeric")))
    stop("x.lon and x.lat must be the names of numeric columns in x")

  if (any(is.na(x.coords$lon)) | any(is.na(x.coords$lat)))
    stop("The longitude and latitude columns of x must not have NA values")

  x.sfc <- st_wrap_dateline(
    st_geometry(st_as_sf(x.coords, coords = c("lon", "lat"), crs = 4326))
  )

  # Read and coerce strata.files into an sfc object (polygon)
  if (!inherits(strata.files, "list"))
    stop("strata.files must be a list of (potentially named) file paths")
  strata.files.list <- lapply(strata.files, .das_pts2poly_vertices)

  if (is.null(names(strata.files.list)))
    names(strata.files.list) <- paste0("InPoly", seq_along(strata.files.list))
  which.blank <- which(names(strata.files.list) == "")
  if (length(which.blank) > 0)
    names(strata.files.list)[which.blank] <- paste0("InPoly", which.blank)
  rm(which.blank)

  # x in strata.files
  x.in.df <- data.frame(lapply(strata.files.list, function(j, x.sfc) {
    tryCatch({
      stopifnot(inherits(x.sfc, "sfc"), inherits(j, "sfc"))
      vapply(st_intersects(x.sfc, j), length, 1)
    }, error = "Error intersecting x with the elemnts of strata.files")
  }, x.sfc = x.sfc))

  if (strata.which) {
    tmp <- apply(x.in.df, 1, function(i) {
      ifelse(all(i == 0), 0, head(which(i != 0), 1))
    })
    x.in.df <- cbind(x.in.df, strata_which = tmp)
  }

  cbind(x, x.in.df)
}
