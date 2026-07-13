#' @details `mergeRaster` differs from `merge` in how overlapping tile regions
#' are handled: `merge` retains the values of the first raster in the list.
#' This has the consequence of retaining the values from the buffered
#' region in the first tile in place of the values from the neighbouring tile.
#' On the other hand, `mergeRaster` retains the values of the tile region,
#' over the values in any buffered regions.
#' This is useful for reducing edge effects when performing raster operations involving
#' contagious processes.
#'
#' @param x    A list of split raster tiles (i.e., from `splitRaster`).
#' @param fun  Function (e.g. `mean`, `min`, or `max` that
#'             accepts a `na.rm` argument. The default is `mean`.
#'
#' @return `mergeRaster` returns a `RasterLayer` object.
#'
#' @seealso [terra::merge()], [terra::mosaic()]
#'
#' @author Yong Luo, Alex Chubaty, Tati Micheletti & Ian Eddy
#' @export
#' @importFrom terra align crop ext merge mosaic origin project rast xmax xmin ymax ymin
#' @rdname splitRaster
#'
setGeneric("mergeRaster", function(x, fun = NULL) {
  standardGeneric("mergeRaster")
})

#' @export
#' @rdname splitRaster
setMethod(
  "mergeRaster",
  signature = signature(x = "list"),
  definition = function(x, fun) {
    isRaster <- is(x[[1]], "Raster")

    if (isTRUE(isRaster)) {
      x <- lapply(x, terra::rast)
    }

    if (length(x) > 1) {
      xminExtent <- sapply(x, terra::xmin) |> unique() |> sort()
      xmaxExtent <- sapply(x, terra::xmax) |> unique() |> sort()
      yminExtent <- sapply(x, terra::ymin) |> unique() |> sort()
      ymaxExtent <- sapply(x, terra::ymax) |> unique() |> sort()
      xBuffer <- if (any(length(xminExtent) == 1, length(xmaxExtent) == 1)) {
        0.0
      } else {
        unique((xmaxExtent[-length(xmaxExtent)] - xminExtent[-1]) / 2) # nolint
      }
      yBuffer <- if (any(length(yminExtent) == 1, length(ymaxExtent) == 1)) {
        0.0
      } else {
        unique((ymaxExtent[-length(ymaxExtent)] - yminExtent[-1]) / 2) # nolint
      }

      ## check that all rasters share same origin (i.e., are aligned)
      origins <- sapply(x, terra::origin)
      if (!((max(origins[1, ]) - min(origins[1, ])) %==% 0) |
          !((max(origins[1, ]) - min(origins[2, ])) %==% 0)) {
        x <- lapply(x[-1], function(r) {
          template <- terra::project(x = r, y = x[[1]], align = TRUE)
          terra::project(x = r, y = template)
        }) |>
          rev() |>
          append(x[[1]]) |>
          rev()
      }

      if (any(length(xBuffer) > 1, length(yBuffer) > 1)) {
        message(paste0("The tiles present different buffers (likely due to resampling).",
                       " mergeRaster() will use terra::mosaic()."))

        rTemplate <- x[[1]]
        for (i in seq_along(x)) {
          if (i == 1) next
          extnt(x[[i]]) <- terra::align(terra::ext(x[[i]]), rTemplate, snap = "near")
        }

        mosaicArgs <- x
        if (!is.null(fun)) {
          mosaicArgs$fun <- fun
        } else {
          mosaicArgs$fun <- "mean"
        }
        y <- do.call(terra::mosaic, mosaicArgs) ## TODO: avoid do.call
      } else {
        for (i in seq_along(x)) {
          r <- x[[i]]
          if (terra::xmin(r) != min(xminExtent)) {
            xminCut <- terra::xmin(r) + xBuffer
          } else {
            xminCut <- terra::xmin(r)
          }
          if (xmax(r) != max(xmaxExtent)) {
            xmaxCut <- terra::xmax(r) - xBuffer
          } else {
            xmaxCut <- terra::xmax(r)
          }
          if (terra::ymin(r) != min(yminExtent)) {
            yminCut <- terra::ymin(r) + yBuffer
          } else {
            yminCut <- terra::ymin(r)
          }
          if (terra::ymax(r) != max(ymaxExtent)) {
            ymaxCut <- terra::ymax(r) - yBuffer
          } else {
            ymaxCut <- terra::ymax(r)
          }
          x[[i]] <- terra::crop(r, ext(xminCut, xmaxCut, yminCut, ymaxCut))
        }

        y <- do.call(merge, x) ## TODO: avoid do.call
      }

      if (isTRUE(isRaster)) {
        if (terra::nlyr(y) > 1) {
          return(raster::stack(y))
        } else {
          return(raster::raster(y))
        }
      } else {
        return(y)
      }
    } else {
      return(x[[1]]) ## original raster if it's the only one in the list
    }
})
