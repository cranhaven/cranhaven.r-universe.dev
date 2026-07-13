#' Split and re-merge `RasterLayer`(s)
#'
#' `splitRaster` divides up a raster into an arbitrary number of pieces (tiles).
#' Split rasters can be recombined using `do.call(merge, y)` or `mergeRaster(y)`,
#' where `y <- splitRaster(x)`.
#'
#' This function is parallel-aware using the same mechanism as used in \pkg{raster}:
#' NOTE: This may not work as expected as we transition away from `raster`.
#' Specifically, if you start a cluster using `raster::beginCluster()`,
#' then this function will automatically use that cluster.
#' It is always a good idea to stop the cluster when finished, using `raster::endCluster()`.
#'
#' @param r       The raster to be split.
#'
#' @param nx      The number of tiles to make along the x-axis.
#'
#' @param ny      The number of tiles to make along the y-axis.
#'
#' @param buffer  Numeric vector of length 2 giving the size of the buffer along the x and y axes.
#'                If values greater than or equal to `1` are used, this
#'                is interpreted as the number of pixels (cells) to use as a buffer.
#'                Values between `0` and `1` are interpreted as proportions
#'                of the number of pixels in each tile (rounded up to an integer value).
#'                Default is `c(0, 0)`, which means no buffer.
#'
#' @param path    Character specifying the directory to which the split tiles will be saved.
#'                If missing, the function will write to memory.
#' @param cl      A cluster object. Optional. This would generally be created using
#'                [parallel::makeCluster()] or equivalent. This is an alternative way, instead
#'                of `beginCluster()`, to use parallelism for this function, allowing for
#'                more control over cluster use.
#' @param rType   Data type of the split rasters. Defaults to FLT4S.
#'
#' @param fExt    file extension (e.g., `".grd"` or `".tif"`) specifying the file format.
#'
#' @return `splitRaster` returns a list (length `nx*ny`) of cropped raster tiles.
#'
#' @seealso [do.call()], [terra::merge()].
#'
#' @author Alex Chubaty and Yong Luo
#' @export
#' @importFrom parallel clusterApplyLB
#' @importFrom terra crop crs<- ext writeRaster
#' @importFrom reproducible checkPath
#' @importFrom terra ext unwrap wrap xmax xmin xres yres
#' @rdname splitRaster
#'
#' @example inst/examples/example_splitRaster.R
#'
splitRaster <- function(r, nx = 1, ny = 1, buffer = c(0, 0), path = NA, cl, rType = "FLT4S",
                        fExt = ".tif") {
  if (!is.numeric(nx) || !is.numeric(ny) || !is.numeric(buffer)) {
    stop("nx, ny, and buffer must be numeric")
  }
  if (!is.integer(nx)) nx <- as.integer(nx)
  if (!is.integer(ny)) ny <- as.integer(ny)
  if (is.integer(buffer)) buffer <- as.numeric(buffer)

  if (!is.na(path)) {
    path <- checkPath(path, create = TRUE)
  }

  isRasterLayer <- is(r, "RasterLayer")
  if (isRasterLayer) {
    r <- terra::rast(r)
  }

  if (missing(cl)) {
    cl <- tryCatch(raster::getCluster(), error = function(e) NULL)
    on.exit(if (!is.null(cl)) raster::returnCluster(), add = TRUE)
  }

  if (length(buffer) > 2) {
    warning("buffer contains more than 2 elements - only the first two will be used.")
    buffer <- buffer[1:2]
  } else if (length(buffer) == 1) {
    buffer <- c(buffer, buffer)
  }
  if (buffer[1] < 1) {
    buffer[1] <- ceiling((buffer[1] * (terra::xmax(r) - terra::xmin(r)) / nx) / terra::xres(r))
  }
  if (buffer[2] < 1) {
    buffer[2] <- ceiling((buffer[2] * (terra::ymax(r) - terra::ymin(r)) / ny) / terra::yres(r))
  }

  ext <- terra::ext(r)

  extents <- vector("list", length = nx * ny)
  n <- 1L
  for (i in seq_len(nx) - 1L) {
    for (j in seq_len(ny) - 1L) {
      x0 <- terra::xmin(ext) + i * ((terra::xmax(ext) - terra::xmin(ext)) / nx) -
        buffer[1] * terra::xres(r)
      x1 <- terra::xmin(ext) + (i + 1L) * ((terra::xmax(ext) - terra::xmin(ext)) / nx) +
        buffer[1] * terra::xres(r)
      y0 <- terra::ymin(ext) + j * ((terra::ymax(ext) - terra::ymin(ext)) / ny) -
        buffer[2] * terra::yres(r)
      y1 <- terra::ymin(ext) + (j + 1L) * ((terra::ymax(ext) - terra::ymin(ext)) / ny) +
        buffer[2] * terra::yres(r)
      extents[[n]] <- list(x0, x1, y0, y1) # don't make an SpatExtent  yet because it doesn't copy to parallel
      n <- n + 1L
    }
  }

  tiles <- if (!is.null(cl)) {
    clusterApplyLB(cl = cl, x = seq_along(extents), fun = .croppy, e = extents,
                   r = terra::wrap(r), path = path, rType = rType, fExt = fExt) #|>
      # lapply(terra::unwrap)

  } else {
    lapply(X = seq_along(extents),
           FUN = .croppy,
           e = extents,
           r = r,
           path = path,
           rType = rType,
           fExt = fExt)
  }
  tiles <- if (isRasterLayer) {
    lapply(tiles, function(t) {
      withCallingHandlers({
        t <- raster::raster(t)
      },
      warning = function(w) {
        if (getRversion() <= "4.1.3")
          if (grepl("NAs introduced by coercion", w$message))
            invokeRestart("muffleWarning")
      })
      raster::dataType(t) <- rType
      t
    })
  } else {
    if (is.character(tiles[[1]]))
      tiles <- lapply(tiles, terra::rast)
    if (grepl("INT", rType))
      for (i in seq_along(tiles)) {
        tiles[[i]][] <- as.integer(tiles[[i]][])
      }
    tiles
  }

  # The crs doesn't always stick with `raster::raster` --> it will add LongLat if it is ""
  #  Override here
  if (!identical(terra::crs(tiles[[1]]), terra::crs(r)))
    for (i in seq(tiles))
      terra::crs(tiles[[i]]) <- terra::crs(r)

  return(tiles)
}

#' @importFrom terra crop crs rast unwrap wrap writeRaster
#' @keywords internal
.croppy <- function(i, e, r, path, rType, fExt) {
  isWrapped <- is(r, "PackedSpatRaster")
  if (isWrapped) {
    r <- terra::unwrap(r)
  }

  if (is(e[[i]], "list"))
    e[[i]] <- do.call(terra::ext, e[[i]])
  ri <- terra::crop(r, e[[i]], datatype = rType)
  terra::crs(ri) <- terra::crs(r)
  if (is.na(path)) {
    return(ri)
  } else {
    filename <- paste0(file.path(path, paste0(names(r), "_tile", i)), fExt)
    terra::writeRaster(ri, filename, overwrite = TRUE, datatype = rType)

    # if (isWrapped) {
    return(filename)
    # } else {
    #   return(terra::rast(filename))
    # }
  }
}
