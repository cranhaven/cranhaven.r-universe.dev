utils::globalVariables(c("..colsToKeep", ".N", ".SD", "row_number"))

################################################################################
#' Convert reduced representation to full raster
#'
#' @param reduced `data.frame` or `data.table` that has at least one
#' column of codes that are represented in the `fullRaster`.
#'
#' @param fullRaster `RasterLayer`/`SpatRaster` of codes used in `reduced` that
#'                   represents a spatial representation of the data. Note that
#'                   if `fullRaster` is a `factor` `SpatRaster`, the active category
#'                   level values are used, not the IDs (see `terra::activeCat` and
#'                   `terra::cats`)
#'
#' @param newRasterCols Character vector, length 1 or more, with the name(s) of
#'                      the column(s) in `reduced` whose value will be
#'                      returned as a `RasterLayer`/`SpatRaster` or list
#'                      of `RasterLayer`/`SpatRaster`s.
#'
#' @param mapcode a character, length 1, with the name of the column in `reduced`
#'                that is represented in `fullRaster`.
#'
#' @param ... Other arguments. None used yet.
#'
#' @return A `RasterLayer`/`SpatRaster` or list of
#'  `RasterLayer`/`SpatRaster` of with same dimensions as `fullRaster` representing
#'  `newRasterCols` spatially, according to the join between the `mapcode`
#'  contained within `reduced` and `fullRaster`
#'
#' @seealso [terra::rast()]
#'
#' @author Eliot McIntire
#' @export
#' @importFrom data.table := data.table key setkeyv setnames
#' @importFrom terra ext levels rast res values
#' @rdname rasterizeReduced
#'
#' @example inst/examples/example_mapReduce.R
#'
rasterizeReduced <- function(reduced, fullRaster, newRasterCols, mapcode = names(fullRaster), ...) {
  if (!inherits(fullRaster, c("Raster", "SpatRaster"))) {
    stop("fullRaster must be a Raster or SpatRaster")
  }

  ## don't use rasterRead; rasterizeReduced can be used independently of reproducible
  if (is(fullRaster, "Raster")) {
    rasterFUN <- function(...) {
      raster::raster(...)
    }
  } else {
    rasterFUN <- function(...) {
      rast(...)
    }
  }

  if (!is.data.table(reduced))
    reduced <- data.table::setDT(reduced)

  if (!is.null(key(reduced))) {
    if (key(reduced) != mapcode) {
      setkeyv(reduced, mapcode)
    }
  } else {
    setkeyv(reduced, mapcode)
  }

  ## instead of `.as.vector(values(fullRaster))` extract by pix ID so that
  ## for factor rasters the value/label of the active category (not its code/level) is extracted
  ## presumably this is the value in reduced.
  fullRasterVals <- as.data.table(fullRaster[1:ncell(fullRaster)])
  setnames(fullRasterVals, 1, new = mapcode)

  ## with RasterLayer we need to use factorValues to convert the mapcodes
  ## to the levels.
  if (is(fullRaster, "Raster")) {
    if (raster::is.factor(fullRaster)) {
      fullRasterVals <- as.data.table(raster::factorValues(fullRaster, fullRasterVals[[mapcode]]))
      setnames(fullRasterVals, 1, new = mapcode)
    }
  }

  if (is.factor(fullRasterVals[[mapcode]])) {
    fullRasterVals[, (mapcode) := lapply(.SD, as.character), .SDcols = mapcode]
  }

  set(fullRasterVals, NULL, "row_number", seq(ncell(fullRaster)))
  setkeyv(fullRasterVals, mapcode)

  colsToKeep <- c(mapcode, newRasterCols)
  BsumVec <- reduced[, ..colsToKeep][fullRasterVals] |> unique()
  setkeyv(BsumVec, "row_number")

  if (length(newRasterCols) > 1) {
    ras <- list()
    for (i in newRasterCols) {
      ras[[i]] <- rasterFUN(fullRaster)
      names(ras[[i]]) <- names(rasterFUN(fullRaster))

      if (is.factor(BsumVec[[i]]) && is(ras, "SpatRaster")) {
        ras[[i]][] <- as.numeric(BsumVec[[i]])
        levs <- unique(data.frame(id = na.omit(as.numeric(BsumVec[[i]])),
                                  values = na.omit(BsumVec[[i]])))
        levels(ras[[i]][]) <- levs
      } else {
        ## if factor values are attributed to a RasterLayer,
        ## the attributes table is automatically added
        ras[[i]][] <- BsumVec[[i]]
      }
    }
  } else {
    ras <- rasterFUN(fullRaster)
    names(ras) <- names(rasterFUN())

    if (is.factor(BsumVec[[newRasterCols]]) && is(ras, "SpatRaster")) {
      ras[] <- as.numeric(BsumVec[[newRasterCols]])
      levs <- unique(data.frame(id = na.omit(as.numeric(BsumVec[[newRasterCols]])),
                                values = na.omit(BsumVec[[newRasterCols]])))
      levels(ras) <- levs
    } else {
      ## if factor values are attributed to a RasterLayer,
      ## the attributes table is automatically added
      ras[] <- BsumVec[[newRasterCols]]
    }
  }
  return(ras)
}
