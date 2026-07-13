utils::globalVariables(c("angles", "indices", "to", "x", "y", "rasterVal"))

##############################################################
#' Fast `adjacent` function, and Just In Time compiled version
#'
#' Faster function for determining the cells of the 4, 8 or bishop
#'  neighbours of the `cells`. This is a hybrid function that uses
#'  matrix for small numbers of loci (<1e4) and data.table for larger numbers of loci
#'
#' Between 4x (large number loci) to 200x (small number loci) speed gains over
#' `adjacent` in raster package. There is some extra speed gain if
#' `NumCol` and `NumCells` are passed rather than a raster.
#' Efficiency gains come from:
#'  1. use `data.table` internally
#'     - no need to remove NAs because wrapped or outside points are
#'       just removed directly with data.table
#'     - use data.table to sort and fast select (though not fastest possible)
#'  2. don't make intermediate objects; just put calculation into return statement
#'
#' The steps used in the algorithm are:
#' 1. Calculate indices of neighbouring cells
#' 2. Remove "to" cells that are
#'    - `< 1` or `> numCells` (i.e., they are above or below raster), using a single modulo
#'      calculation
#'    - where the modulo of "to" cells is equal to 1 if "from" cells are 0 (wrapped right
#'      to left)
#'    - or where the modulo of the "to" cells is equal to 0 if "from" cells are 1 (wrapped
#'      left to right)
#'
#' @param x `SpatRaster` object for which adjacency will be calculated.
#'
#' @param cells vector of cell numbers for which adjacent cells should be found.
#'              Cell numbers start with 1 in the upper-left corner and increase
#'              from left to right and from top to bottom.
#'
#' @param directions the number of directions in which cells should be connected:
#'                   4 (rook's case), 8 (queen's case), or `"bishop"` to connect
#'                   cells with one-cell diagonal moves.
#'                   Or a neighbourhood matrix (see Details).
#'
#' @param sort logical. Whether the outputs should be sorted or not, using cell ids
#'             of the `from` cells (and `to` cells, if `match.adjacent`
#'             is `TRUE`).
#'
#' @param pairs logical. If `TRUE`, a matrix of pairs of adjacent cells is returned.
#'              If `FALSE`, a vector of cells adjacent to cells is returned
#'
#' @param include logical. Should the focal cells be included in the result?
#'
#' @param target a vector of cells that can be spread to. This is the inverse of a mask.
#'
#' @param numCol numeric indicating number of columns in the raster.
#'               Using this with `numCell` is a bit faster execution time.
#'
#' @param numCell numeric indicating number of cells in the raster.
#'                Using this with `numCol` is a bit faster execution time.
#'
#' @param match.adjacent logical. Should the returned object be the same as
#'                       `raster::adjacent`.
#'                       Default `FALSE`, which is faster.
#'
#' @param cutoff.for.data.table numeric. If the number of cells is above this value,
#'                              the function uses data.table which is faster with
#'                              large numbers of cells. Default is 5000, which appears
#'                              to be the turning point where data.table becomes faster.
#'
#' @param torus Logical. Should the spread event wrap around to the other side of the raster?
#'                       Default is `FALSE`.
#'
#' @param id numeric If not `NULL` (default), then function will return `"id"` column.
#'
#' @param numNeighs A numeric scalar, indicating how many neighbours to return. Must be
#'                  less than or equal to `directions`; which neighbours are random
#'                  with equal probabilities.
#' @param returnDT A logical. If TRUE, then the function will return the result
#'                 as a `data.table`, if the internals used `data.table`,
#'                 i.e., if number of cells is greater than `cutoff.for.data.table`.
#'                 User should be warned that this will therefore cause the output
#'                 format to change depending `cutoff.for.data.table`.
#'                 This will be faster for situations where `cutoff.for.data.table = TRUE`.
#'
#' @return Either a matrix (if more than 1 column, i.e., `pairs = TRUE`,
#' and/or `id` is provided), a vector (if only one column), or a `data.table`
#' (if `cutoff.for.data.table` is less than `length(cells)` *and*
#' `returnDT` is `TRUE`.
#' To get a consistent output, say a matrix, it would be wise to test the output
#' for its class.
#' The variable output is done to minimize coercion to maintain speed.
#' The columns will be one or more of `id`, `from`, `to`.
#'
#' @seealso [terra::adjacent()]
#'
#' @author Eliot McIntire
#' @export
#' @importFrom data.table := data.table key set setcolorder setkeyv
#' @importFrom terra ncell ncol nrow
#' @importFrom stats na.omit
#' @rdname adj
#'
#' @examples
#' library(terra)
#'
#' origDTThreads <- data.table::setDTthreads(2L)
#' origNcpus <- options(Ncpus = 2L)
#'
#' a <- rast(ext(0, 1000, 0, 1000), res = 1)
#' sam <- sample(1:ncell(a), 1e4)
#' numCol <- ncol(a)
#' numCell <- ncell(a)
#' adj.new <- adj(numCol = numCol, numCell = numCell, cells = sam, directions = 8)
#' adj.new <- adj(numCol = numCol, numCell = numCell, cells = sam, directions = 8,
#'                include = TRUE)
#'
#' # clean up
#' data.table::setDTthreads(origDTThreads)
#' options(Ncpus = origNcpus)
#'
adj <- function(x = NULL, cells, directions = 8, sort = FALSE, pairs = TRUE,
                    include = FALSE, target = NULL, numCol = NULL, numCell = NULL,
                    match.adjacent = FALSE, cutoff.for.data.table = 2e3,
                    torus = FALSE, id = NULL, numNeighs = NULL, returnDT = FALSE) {
  to <- NULL
  J <- NULL # nolint
  cells <- as.integer(cells)

  if (is.null(numCol) || is.null(numCell)) {
    if (is.null(x)) stop("must provide either numCol & numCell or a x")
    numCol <- as.integer(ncol(x))
    numCell <- as.integer(ncell(x))
  }

  if (is.character(directions)) {
    if (directions == "bishop")  {
      dirs <- 4
      needCorners <- TRUE
    } else {
      stop("directions must be numeric or 'bishop'")
    }
} else {
    needCorners <- if (directions == 8) TRUE else FALSE
    dirs <- directions
  }

  numToCells <- dirs + include
  fromCells <- rep.int(cells, times = numToCells)

  if (is.numeric(directions)) {
    top <- cells - numCol
    lef <- cells - 1L
    rig <- cells + 1L
    bot <- cells + numCol
  }
  if (needCorners) {
    topl <- cells - numCol - 1L
    topr <- cells - numCol + 1L
    botl <- cells + numCol - 1L
    botr <- cells + numCol + 1L
  }

  toCells <- if (directions == 8) {

    if (match.adjacent)
      if (include)
        c(cells, topl, lef, botl, topr, rig, botr, top, bot)
      else
        c(topl, lef, botl, topr, rig, botr, top, bot)
    else
      if (include)
        c(topl, top, topr, lef, cells, rig, botl, bot, botr)
      else
        c(topl, top, topr, lef, rig, botl, bot, botr)
  } else if (directions == 4) {
    if (match.adjacent)
      if (include)
        c(cells, lef, rig, top, bot)
      else
        c(lef, rig, top, bot)
    else
      if (include)
        c(top, lef, cells, rig, bot)
      else
        c(top, lef, rig, bot)
  } else if (directions == "bishop") {
    if (match.adjacent)
      if (include)
        c(cells, topl, botl, topr, botr)
      else
        c(topl, botl, topr, botr)
    else
      if (include)
        c(topl, topr, cells, botl, botr)
      else
        c(topl, topr, botl, botr)
  } else {
    stop("directions must be 4 or 8 or \'bishop\'")
  }

  if (!is.null(numNeighs)) {
    lenCells <- length(cells)
    if (length(numNeighs) == 1) numNeighs <- rep(numNeighs, lenCells)
    ind <- unlist(sampleV(1:(directions + include), size = numNeighs))
    minusVal <- lenCells - rep.int(seq_along(cells), numNeighs)
    indFull2 <- ind * lenCells - minusVal

    toCells <- toCells[indFull2]
    fromCells <- fromCells[indFull2]
  }

  useMatrix <- (length(cells) < cutoff.for.data.table)
  if (useMatrix) {
    adj <- cbind(from = fromCells, to = toCells)
    if (!is.null(id)) adj <- cbind(adj, id = rep.int(id, times = numToCells))
  } else {
    adj <- data.table(from = fromCells, to = toCells)
    if (!is.null(id)) set(adj, NULL, "id", rep.int(id, times = numToCells))
  }

  if (useMatrix) {
    # Remove all cells that are not target cells, if target is a vector of cells
    if (!is.null(target)) {
      adj <- adj[na.omit(adj[, "to"] %in% target), , drop = FALSE]
    }

    if (sort) {
      if (pairs) {
        if (match.adjacent) {
          adj <- adj[order(adj[, "from"], adj[, "to"]), , drop = FALSE]
        } else {
          adj <- adj[order(adj[, "from"]), , drop = FALSE]
        }
      } else {
        adj <- adj[order(adj[, "to"]), , drop = FALSE]
      }
    }

    # Remove the "from" column if pairs is FALSE
    # Good time savings if no intermediate object is created
    keepCols <- if (is.null(id)) "to" else c("to", "id")
    if (!torus) {
      if (pairs) {
        return(adj[
          !((adj[, "to"] <= 0 | adj[, "to"] > numCell)  | # top or bottom of raster
              ((adj[, "from"] %% numCol + adj[, "to"] %% numCol) == 1)) # right & left edge cells, with neighbours wrapped
          , , drop = FALSE])
      } else {
        adj <- adj[
          !((((adj[, "to"] - 1) %% numCell + 1) != adj[, "to"]) | # top or bottom of raster
              ((adj[, "from"] %% numCol + adj[, "to"] %% numCol) == 1)) # right & left edge cells, with neighbours wrapped
          , keepCols, drop = FALSE]
        if (match.adjacent) {
          adj <- unique(adj[, "to"])
        }
        return(adj)
      }
    } else {
      whLefRig <- (adj[, "from"] %% numCol + adj[, "to"] %% numCol) == 1
      adj[whLefRig, "to"] <- adj[whLefRig, "to"] +
        numCol * (adj[whLefRig, "from"] - adj[whLefRig, "to"])
      whBotTop <- ((adj[, "to"] - 1) %% numCell + 1) != adj[, "to"]
      adj[whBotTop, "to"] <- adj[whBotTop, "to"] +
        sign(adj[whBotTop, "from"] - adj[whBotTop, "to"]) * numCell
      if (pairs) {
        return(adj)
      } else {
        if (match.adjacent) {
          adj <- unique(adj[, "to", drop = TRUE])
        } else {
          adj <- adj[, keepCols, drop = FALSE]
        }
        return(adj)
      }
    }
  } else {
    ## use data.table
    # Remove all cells that are not target cells, if target is a vector of cells
    if (!is.null(target)) {
      set(adj, NULL, "ord", seq_len(NROW(adj)))
      setkeyv(adj, "to")
      adj <- adj[J(target)]
      adj <- na.omit(adj)
      setkeyv(adj, "ord")
      set(adj, NULL, "ord", NULL)
    }

    if (sort) {
      if (pairs) {
        if (match.adjacent) {
          setkeyv(adj, c("from", "to"))
        } else {
          setkeyv(adj, "from")
        }
      } else {
        setkeyv(adj, "to")
      }
    }

    # Remove the "from" column if pairs is FALSE
    if (!pairs) {
      from <- as.integer(adj$from)
      set(adj, NULL, "from", NULL)
    }

    if (!torus) {
      if (!pairs) {
        adj <- adj[
          !((((to - 1) %% numCell + 1) != to) |  #top or bottom of raster
              ((from %% numCol + to %% numCol) == 1))# | #right & left edge cells, with neighbours wrapped
          ]
        if (match.adjacent) {
          if (returnDT)
            return(unique(adj[, list(to)]))
          else
            return(unique(adj$to))
        }
        if (returnDT)
          return(adj)
        else
          return(as.matrix(adj))
      } else {
        if (returnDT)
          return(adj[
            # top or bottom of raster | right & left edge cells, with neighbours wrapped
            !((((to - 1) %% numCell + 1) != to) | ((from %% numCol + to %% numCol) == 1))
          ])
        else
          return(as.matrix(adj[
            # top or bottom of raster | right & left edge cells, with neighbours wrapped
            !((((to - 1) %% numCell + 1) != to) | ((from %% numCol + to %% numCol) == 1))
          ]))
        return()
      }
    } else {
      if (!pairs) {
        whLefRig <- (from %% numCol + adj$to %% numCol) == 1
        toWhLefRig <- adj$to[whLefRig]
        set(adj, which(whLefRig), "to", toWhLefRig + numCol * (from[whLefRig] - toWhLefRig))
        whBotTop <- ((adj$to - 1) %% numCell + 1) != adj$to
        toWhBotTop <- adj$to[whBotTop]
        set(adj, which(whBotTop), "to", toWhBotTop +
              as.integer(sign(from[whBotTop] - toWhBotTop) * numCell))

        if (match.adjacent) {
          if (returnDT)
            return(unique(adj[, list(to)]))
          else
            return(unique(adj$to))
        }
      } else {
        whLefRig <- (adj$from %% numCol + adj$to %% numCol) == 1
        toWhLefRig <- adj$to[whLefRig]
        set(adj, which(whLefRig), "to", toWhLefRig + numCol * (adj$from[whLefRig] - toWhLefRig))
        whBotTop <- ((adj$to - 1) %% numCell + 1) != adj$to
        toWhBotTop <- adj$to[whBotTop]
        set(adj, which(whBotTop), "to", toWhBotTop +
              as.integer(sign(adj$from[whBotTop] - toWhBotTop) * numCell))
      }
      if (returnDT)
        return(adj)
      else
        return(as.matrix(adj))
    }
  }
}

##############################################################
#' Identify pixels in a circle or ring (doughnut) around an object.
#'
#' Identify the pixels and coordinates that are at a (set of) buffer distance(s)
#' of the objects passed into `coords`.
#' This is similar to `sf::st_buffer` but much faster and without the georeferencing information.
#' In other words, it can be used for similar problems, but where speed is important.
#' This code is substantially adapted from `PlotRegionHighlighter::createCircle`.
#'
#' @param landscape Raster on which the circles are built.
#'
#' @param coords Either a matrix with 2 (or 3) columns, `x` and `y` (and `id`), representing the
#'               coordinates (and an associated id, like cell index),
#'               or a `SpatialPoints*` object around which to make circles. Must be same
#'               coordinate system as the `landscape` argument. Default is missing,
#'               meaning it uses the default to `loci`.
#'
#' @param loci   Numeric. An alternative to `coords`.
#'               These are the indices on `landscape` to initiate this function (see `coords`).
#'               Default is one point in centre of `landscape`.
#'
#' @param maxRadius  Numeric vector of length 1 or same length as `coords`
#'
#' @param minRadius  Numeric vector of length 1 or same length as `coords`. Default is
#'                   `maxRadius`, meaning return all cells that are touched
#'                   by the narrow ring at that exact radius. If smaller than `maxRadius`,
#'                   then this will create a buffer or doughnut or ring.
#'
#' @param allowOverlap Logical. Should duplicates across id be removed or kept. Default TRUE.
#'
#' @param allowDuplicates Logical. Should duplicates within id be removed or kept. Default FALSE.
#'                        This is useful if the actual x, y coordinates are desired, rather
#'                        than the cell indices. This will increase the size of the returned
#'                        object.
#'
#' @param includeBehavior Character string. Currently accepts only `"includePixels"`, the default,
#'                        and `"excludePixels"`. See details.
#'
#' @param returnDistances Logical. If `TRUE`, then a column will be added to the returned
#'                        data.table that reports the distance from `coords` to every
#'                        point that was in the circle/doughnut surrounding `coords`.
#'                        Default `FALSE`, which is faster.
#'
#' @param angles Numeric. Optional vector of angles, in radians, to use. This will create
#'               "spokes" outward from `coords.` Default is `NA`, meaning, use internally
#'               derived angles that will "fill" the circle.
#'
#' @param returnAngles Logical. If `TRUE`, then a column will be added to the returned
#'                     data.table that reports the angle from `coords` to every
#'                     point that was in the circle/doughnut surrounding `coords`. Default `FALSE.`
#'
#' @param closest Logical. When determining non-overlapping circles, should the function
#'                give preference to the closest `loci` or the first one (much faster).
#'                Default is `FALSE`, meaning the faster, though maybe not desired behaviour.
#'
#' @param simplify logical. If `TRUE`, then all duplicate pixels are removed.
#'                 This means that some `x`, `y` combinations will disappear.
#'
#' @inheritParams spread
#'
#' @details This function identifies all the pixels as defined by a donut
#' with inner radius `minRadius` and outer radius of `maxRadius`.
#' The `includeBehavior` defines whether the cells that intersect the radii
#' but whose centres are not inside the donut are included `includePixels`
#' or not `excludePixels` in the returned pixels identified.
#' If this is `excludePixels`, and if a `minRadius` and
#' `maxRadius` are equal, this will return no pixels.
#'
#'
#' @return A `matrix` with 4 columns, `id`, `indices`,
#' `x`, `y`. The `x` and `y` indicate the exact coordinates of
#' the `indices` (i.e., cell number) of the `landscape`
#' associated with the ring or circle being identified by this function.
#'
#' @seealso [rings()] which uses `spread` internally.
#' `cir` tends to be faster when there are few starting points, `rings` tends to be faster
#' when there are many starting points. `cir` scales with `maxRadius^2` and `coords`.
#' Another difference between the two functions is that `rings` takes the centre of the pixel
#' as the centre of a circle, whereas `cir` takes the exact coordinates.
#' See example. For the specific case of creating distance surfaces from specific
#' points, see [distanceFromEachPoint()], which is often faster.
#' For the more general GIS buffering, see `sf::st_buffer`.
#'
#' @example inst/examples/example_cir.R
#'
#' @export
#' @importFrom data.table data.table set setkeyv
#' @importFrom fpCompare %==%
#' @importFrom terra cellFromXY extract res xyFromCell ncell ncol
#' @rdname cir
cir <- function(landscape, coords, loci,
                maxRadius = ncol(landscape) / 4, minRadius = maxRadius,
                allowOverlap = TRUE, allowDuplicates = FALSE,
                includeBehavior = "includePixels", returnDistances = FALSE,
                angles = NA_real_,
                returnAngles = FALSE, returnIndices = TRUE,
                closest = FALSE, simplify = TRUE)  {

  origClass <- class(landscape)
  if (missing(coords)) {
    if (missing(loci)) {
      ncells <- terra::ncell(landscape)
      middleCell <- if (identical(ncells / 2, floor(ncells / 2))) {
        ncells / 2 - terra::ncol(landscape) / 2
      } else {
        round(ncells / 2)
      }
      coords <- terra::xyFromCell(landscape, middleCell)
    } else if (is.numeric(loci)) {
      coords <- terra::xyFromCell(landscape, loci)
      coords <- cbind(coords, id = loci)
    } else {
      stop("Need either a numeric loci or coords")
    }
  } else if (inherits(coords, "Spatial")) {
    .requireNamespace("sp")
    coords <- sp::coordinates(coords)
  } else if (inherits(coords, "SpatVector")) {
    coords <- crds(coords)
  } else if (!is.numeric(coords)) {
    stop("Need either a numeric loci or coords with matrix or SpatialPoints or SpatVector")
  }

  ### adapted from createCircle of the package PlotRegionHighlighter

  if (!all(xycolNames %in% colnames(coords))) {
    stop("coords must have columns named x and y")
  }
  suppliedAngles <- if (all(!is.na(angles))) TRUE else FALSE

  scaleRaster <- terra::res(landscape)
  if (!isTRUE(all.equal(scaleRaster[1], scaleRaster[2]))) {
    stop("cir function only accepts rasters with identical resolution in x and y dimensions")
  }

  if (!any(includeBehavior == c("includePixels", "excludePixels"))) {
    stop("includeBehavior can only be \"includePixels\" or \"excludePixels\"")
  }

  scaleRaster <- scaleRaster[1]

  moreThanOne <- NROW(coords) > 1

  equalRadii <- TRUE
  if (suppliedAngles) {
    # if provided with angles, then problem is easier
    seqNumInd <- seq_len(NROW(coords))
    maxRadius <- c(seq(minRadius, maxRadius, by = max(0.68, 0.75 - maxRadius / 3e3)), maxRadius)
    numAngles <- length(angles)
    rads <- rep(rep(maxRadius, each = numAngles), NROW(coords))
    x <- kronecker(coords[, "x"], c(cos(angles) %o% maxRadius), "+")
    y <- kronecker(coords[, "y"], c(sin(angles) %o% maxRadius), "+")
    id <- rep(rep(seqNumInd, each = numAngles), each = length(maxRadius))
  } else {
    if (moreThanOne) {
      # create an index sequence for the number of individuals
      seqNumInd <- seq_len(NROW(coords))

      if (length(maxRadius) == 1) maxRadius <- rep(maxRadius, NROW(coords))
      if (length(minRadius) == 1) minRadius <- rep(minRadius, NROW(coords))
      equalRadii <- sum(maxRadius - maxRadius[1]) %==% 0

      # The goal of maxRadius and numAngles is to identify every cell within the circle
      #  The 0.68 and 0.75 were found by trial and error to minimize the number of
      #  pixels selected that are duplicates of each other.
      if (any(minRadius != maxRadius)) {
        if (any(minRadius > maxRadius)) stop("minRadius must be less than or equal to maxRadius")
        maxRadiusList <- lapply(seqNumInd, function(x) {
          ## 0.75 was the maximum that worked with 1e4 pixels, 1e2 maxRadius
          ## 0.66 was the maximum that worked with 4e6 pixels, 1.3e3 maxRadius
          a <- seq(minRadius[x], maxRadius[x], by = max(0.68, 0.75 - maxRadius[x] / 3e3))
          if (a[length(a)] != maxRadius[x]) a <- c(a, maxRadius[x])
          a
        })

        if (equalRadii) {
          maxRadius <- do.call(cbind, maxRadiusList)
        } else {
          lengths <- unlist(lapply(maxRadiusList, length))
          maxLen <- max(lengths)
          maxRadius <- do.call(cbind, lapply(seq_along(maxRadiusList), function(y) {
            c(maxRadiusList[[y]], rep(NA_real_, maxLen - lengths[y]))
          }))
        }
      }
    } else {
      seqNumInd <- 1
      if (any(minRadius != maxRadius)) {
        ## 0.66 was the maximum that worked with 4e6 pixels, 1.3e3 maxRadius
        a <- seq(minRadius, maxRadius, by = max(0.68, 0.75 - maxRadius / 3e3))
        if (a[length(a)] != maxRadius) a <- c(a, maxRadius)
        maxRadius <- a
      }
    }

    numAngles <- ceiling(maxRadius / scaleRaster * 2.6 * pi) + 1

    if (moreThanOne) {
      if (is.matrix(numAngles)) {
        nAngles <- apply(numAngles, 2, sum, na.rm = TRUE)
      } else {
        nAngles <- numAngles
      }
    } else {
      nAngles <- sum(numAngles)
    }

    # create individual IDs for the number of points that will be done for their circle
    if (!c("id") %in% colnames(coords)) {
      if (moreThanOne) {
        id <- rep.int(seqNumInd, times = nAngles)
      } else {
        id <- 1L
      }
    } else {
      id <- as.integer(rep(coords[, "id"], times = nAngles))
    }

    # create vector of radius for the number of points that will be done for
    # each individual circle
    if (equalRadii)
      rads <- rep.int(maxRadius, times = numAngles)
    else
      rads <- rep.int(na.omit(as.vector(maxRadius)), times = na.omit(as.vector(numAngles)))

    # extract the individuals' current coords
    xs <- rep.int(coords[, "x"], times = nAngles)
    ys <- rep.int(coords[, "y"], times = nAngles)

    angles <- if (all(is.na(angles))) {
      if (!is.null(dim(numAngles))) {
        if (equalRadii) {
          rep(unlist(lapply(numAngles[, 1], function(na) {
            seq_len(na) * (pi * 2 / na)
          })), ncol(numAngles))
        } else {
          unlist(lapply(na.omit(as.vector(numAngles)), function(na) {
            seq_len(na) * (pi * 2 / na)
          }))
        }
      } else {
        unlist(lapply(numAngles, function(na) seq.int(na) * (pi * 2 / na)))
      }
    } else {
      rep(angles, length(numAngles))
    }
    x <- cos(angles) * rads + xs
    y <- sin(angles) * rads + ys
  }

  indices <- as.integer(cellFromXY(landscape, cbind(x, y)))

  if (moreThanOne && allowOverlap && !closest) {
    matDT <- data.table(id, indices, rads, angles, x = x, y = y)
    setkeyv(matDT, c("id", "indices"))
    if (!equalRadii) {
      matDT[, maxRad := rep(apply(maxRadius, 2, max, na.rm = TRUE), nAngles)]
      matDT[, minRad := rep(apply(maxRadius, 2, min, na.rm = TRUE), nAngles)]
    }
    if (!allowDuplicates) {
      matDT <- unique(matDT, by = c("id", "indices"))
    }
    matDT <- na.omit(matDT)
    matDT <- as.matrix(matDT)
  } else {
    matDT <- cbind(id, rads, angles, x, y, indices)
    if (!closest && !allowDuplicates) {
      notDups <- !duplicatedInt(indices)
      matDT <- matDT[notDups, , drop = FALSE]
    }
    matDT <- na.omit(matDT)
  }
  rm(id, indices, rads, x, y)

  # only need to calculate distances for these two cases
  if (includeBehavior == "excludePixels" || returnDistances || closest) {
    if (equalRadii) {
      maxRad <- maxRadius[NROW(maxRadius)]
      minRad <- maxRadius[1]
    }

    # if distances are not required, then only need the inner circle and outer
    # circle distances. Don't waste resources on calculating all distances.
    if (returnDistances || closest) {
      matDT2 <- matDT
    } else {
      if (equalRadii) {
        # 0.71 is the sqrt of 1, so keep
        matDT2 <- matDT[matDT[, "rads"] >= (maxRad - 0.71) | matDT[, "rads"] <=
                          (minRad + 0.71), , drop = FALSE]
      } else {
        # 0.71 is the sqrt of 1, so keep
        matDT2 <- matDT[matDT[, "rads"] >= (matDT[, "maxRad"] - 0.71) | matDT[, "rads"] <=
                          (matDT[, "minRad"] + 0.71), , drop = FALSE]
      }
    } #  only pixels that are in inner or outer ring of pixels

    if (suppliedAngles) {
      a <- cbind(id = matDT2[, "id"], rads = matDT2[, "rads"], angles = matDT2[, "angles"],
                 x = matDT2[, "x"], y = matDT2[, "y"], to = matDT2[, "indices"])

    } else {
      xyC <- xyFromCell(landscape, matDT2[, "indices"])
      a <- cbind(id = matDT2[, "id"], rads = matDT2[, "rads"], angles = matDT2[, "angles"],
                 x = xyC[, "x"], y = xyC[, "y"], to = matDT2[, "indices"])
    }
    if (!equalRadii)
      a <- cbind(a, maxRad = matDT2[, "maxRad"], minRad = matDT2[, "minRad"])

    b <- cbind(coords, id = seq_len(NROW(coords)))

    colnames(b)[1:2] <- xycolNames
    d <- distanceFromEachPoint(b, a)

    if (closest) {
      d <- d[order(d[, "rads"]), , drop = FALSE]
      dups <- duplicated(d[, "to", drop = FALSE])
      d <- d[!dups, , drop = FALSE]
    }

    if (includeBehavior == "excludePixels") {
      if (equalRadii) {
        d <- d[d[, "dists"] %<=% maxRad & d[, "dists"] %>=% minRad, , drop = FALSE]
      } else {
        d <- d[d[, "dists"] %<=% d[, "maxRad"] & d[, "dists"] %>=% d[, "minRad"], , drop = FALSE]
      }
    }

    colnames(d)[which(colnames(d) == "to")] <- "indices"
    if (!returnDistances)
      d <- d[, -which(colnames(d) == "dists"), drop = FALSE]

    if (!returnAngles) {
      d <- d[, -which(colnames(d) == "angles"), drop = FALSE]
      matDT <- matDT[, -which(colnames(matDT) == "angles"), drop = FALSE]
    } else {
      ## convert 'd' and 'matDT' to geographic
      d[, "angles"] <- (pi / 2 - d[, "angles"]) %% (2 * pi)
      matDT[, "angles"] <- pi / 2 -  matDT[, "angles", drop = FALSE] %% (2 * pi)
    }

    if (returnDistances) {
      wh <- na.omit(match("rads", colnames(d)))
      if (length(wh) > 0) matDT <- d[, -wh, drop = FALSE]
    } else if (closest) {
      wh <- na.omit(match(c("rads", "dists"), colnames(d)))
      if (length(wh) > 0) matDT <- d[, -wh, drop = FALSE]
    } else {
      if (equalRadii)
        matDTinterior <- matDT[matDT[, "rads"] < (maxRad - 0.71) &
                                 matDT[, "rads"] > (minRad + 0.71), , drop = FALSE]
      else
        matDTinterior <- matDT[matDT[, "rads"] < (matDT[, "maxRad"] - 0.71) &
                                 matDT[, "rads"] > (matDT[, "minRad"] + 0.71), , drop = FALSE]

      matDT <- rbind(d[, colnames(matDTinterior), drop = FALSE], matDTinterior)
      matDT <- matDT[, -which(colnames(matDT) == "rads"), drop = FALSE]
    }
  } else {
    if (!returnAngles) {
      matDT <- matDT[, -which(colnames(matDT) == "angles"), drop = FALSE]
    }
    matDT <- matDT[, -which(colnames(matDT) == "rads"), drop = FALSE]
  }
  if (!(returnIndices > 0)) {
    if (isTRUE(origClass == "SpatRaster"))
      ras <- terra::rast(landscape)
    else
      ras <- raster::raster(landscape)
    ras[] <- 0
    if (!allowOverlap) {
      if (!returnDistances) {
        ras[matDT[, "indices"]] <- matDT[, "id"]
      } else {
        ras[matDT[, "indices"]] <- matDT[, "dists"]
      }
    } else {
      matDT <- data.table(matDT, key = "indices")
      if (!returnDistances) {
        matDT <- matDT[, sum(id), by = indices]
      } else {
        matDT <- matDT[, sum(1 / dists), by = indices]
      }
      ras[matDT$indices] <- matDT$V1
    }
    return(ras)
  }
  return(matDT)
}

################################################################################
#' Wrap coordinates or pixels in a torus-like fashion
#'
#' Generally useful for model development purposes. Primarily used internally
#' in e.g., `crw` if `torus = TRUE`.
#'
#' If `withHeading` used, then `X` must be an `sf` or `SpatVector` object
#' that contains two columns, `x1` and `y1`, with the immediately
#' previous agent locations.
#'
#' @param X `SpatVector`, `sf`, or matrix of coordinates.
#'
#' @param bounds Either a `SpatRaster*`, `Extent`, or `bbox` object defining bounds to wrap around.
#'
#' @param withHeading logical. If `TRUE`, the previous points must be wrapped
#'                    also so that the subsequent heading calculation will work.
#'                    Default `FALSE`. See details.
#'
#' @return Object of the same class as `X`, but with coordinates updated to reflect the wrapping.
#'
#' @author Eliot McIntire
#' @export
#'
#' @examples
#' origDTThreads <- data.table::setDTthreads(2L)
#' origNcpus <- options(Ncpus = 2L)
#'
#' xrange <- yrange <- c(-50, 50)
#' hab <- terra::rast(terra::ext(c(xrange, yrange)))
#' hab[] <- 0
#'
#' # initialize agents
#' N <- 10
#'
#' # previous points
#' x1 <- y1 <- rep(0, N)
#' # initial points
#' starts <- cbind(x = stats::runif(N, xrange[1], xrange[2]),
#'                 y = stats::runif(N, yrange[1], yrange[2]))
#'
#' # create the agent object # the x1 and y1 are needed for "previous location"
#' agent <- terra::vect(data.frame(x1, y1, starts), geom = c("x", "y"))
#'
#' ln <- rlnorm(N, 1, 0.02) # log normal step length
#' sd <- 30 # could be specified globally in params
#'
#' if (interactive()) {
#'   # clearPlot()
#'   terra::plot(hab, col = "white")
#' }
#'
#' for (i in 1:10) {
#'   agent <- crw(agent = agent, extent = terra::ext(hab), stepLength = ln,
#'                stddev = sd, lonlat = FALSE, torus = FALSE) # don't wrap
#'   if (interactive()) terra::plot(agent[, 1], add = TRUE, col = 1:10)
#' }
#' terra::crds(agent) # many are "off" the map, i.e., beyond the extent of hab
#' agent <- SpaDES.tools::wrap(agent, bounds = terra::ext(hab))
#' terra::plot(agent, add = TRUE, col = 1:10) # now inside the extent of hab
#'
#' # clean up
#' data.table::setDTthreads(origDTThreads)
#' options(Ncpus = origNcpus)
#'
wrap <- function(X, bounds, withHeading = FALSE) {
  classX <- is(X)

  # if (is(X, "matrix")) {
  #   X <- terra::vect(data.frame(x1 = X[, 1], y1 = X[, 2], X), geom = xycolNames)
  # } else
  if (is(X, "sf")) {
    X <- terra::vect(X)
  }

  crdsStart <- coords(X)
  ext <- extnt(bounds)
  if (isTRUE(withHeading)) {
    Xmin <- terra::xmin(ext)
    Xmax <- terra::xmax(ext)
    Ymin <- terra::ymin(ext)
    Ymax <- terra::ymax(ext)
    # This requires that previous points be "moved" as if they are
    #  off the bounds, so that the heading is correct
    xmins <- crdsStart[, 1] < Xmin
    ymins <- crdsStart[, 2] < Ymin
    xmaxs <- crdsStart[, 1] > Xmax
    ymaxs <- crdsStart[, 2] > Ymax

    # SpatVector returns data.frame; sf returns vector
    if (is.matrix(X)) {
      x1 <- X@.Data[, "x1"] # using @.Data accommodates NetLogoR's agentMatrix as well as normal matrix
      y1 <- X@.Data[, "y1"] # using @.Data accommodates NetLogoR's agentMatrix as well as normal matrix
    } else {
      x1 <- if (is.data.frame(X[["x1"]])) X[["x1"]][, 1] else X[["x1"]]
      y1 <- if (is.data.frame(X[["y1"]])) X[["y1"]][, 1] else X[["y1"]]
    }

     # if (any(c(xmins, ymins, xmaxs, ymaxs)))
     #   browser()
    if (any(xmins))
      X[xmins, "x1"] <- (x1[xmins] - Xmin) %% (Xmax - Xmin) + Xmax
    if (any(xmaxs))
      X[xmaxs, "x1"] <- (x1[xmaxs] - Xmax) %% (Xmin - Xmax) + Xmin
    if (any(ymins))
      X[ymins, "y1"] <- (y1[ymins] - Ymin) %% (Ymax - Ymin) + Ymax
    if (any(ymaxs))
      X[ymaxs, "y1"] <- (y1[ymaxs] - Ymax) %% (Ymin - Ymax) + Ymin

  }
  # signature(X = "matrix", bounds = "Extent", withHeading = "missing"),
  # definition = function(X, bounds) {
  if (identical(tolower(colnames(crdsStart)), xycolNames)) {
    # terra::vect uses capitals X Y
    crds <- cbind(
      x = (crdsStart[, 1] - terra::xmin(bounds)) %% (terra::xmax(bounds) - terra::xmin(bounds)) +
        terra::xmin(bounds),
      y = (crdsStart[, 2] - terra::ymin(bounds)) %% (terra::ymax(bounds) - terra::ymin(bounds)) +
        terra::ymin(bounds)
    )
  } else {
    stop("When X is a matrix, it must have 2 columns, x and y,",
         "as from say, coordinates(SpatialPointsObj)")
  }
  if (!(isTRUE(identical(crdsStart[, 1], crds[, 1])) &&
        isTRUE(identical(crdsStart[, 2], crds[, 2])))) {
    coords(X) <- crds
  }

  if ("matrix" %in% classX) {
    return(X)
  } else if ("sf" %in% classX) {
    .requireNamespace("sf")
    return(sf::st_as_sf(X))
  } else if ("SpatVector" %in% classX) {
    return(X)
  }
}

# setMethod(
#   "wrap",
#   signature(X = "SpatialPoints", bounds = "ANY", withHeading = "missing"),
#   definition = function(X, bounds) {
#     X@coords <- wrap(X@coords, bounds = bounds)
#     return(X)
# })

# setMethod(
#   "wrap",
#   signature(X = "matrix", bounds = "Raster", withHeading = "missing"),
#   definition = function(X, bounds) {
#     X <- wrap(X, bounds = extent(bounds))
#     return(X)
# })

# setMethod(
#   "wrap",
#   signature(X = "matrix", bounds = "Raster", withHeading = "missing"),
#   definition = function(X, bounds) {
#     X <- wrap(X, bounds = extent(bounds))
#     return(X)
# })

# setMethod(
#   "wrap",
#   signature(X = "matrix", bounds = "matrix", withHeading = "missing"),
#   definition = function(X, bounds) {
#     if (identical(colnames(bounds), c("min", "max")) &
#          (identical(rownames(bounds), c("s1", "s2")))) {
#       X <- wrap(X, bounds = extent(bounds))
#       return(X)
#     } else {
#       stop("Must use either a bbox, Raster*, or Extent for 'bounds'")
#     }
# })

# setMethod(
#   "wrap",
#   signature(X = "SpatialPointsDataFrame", bounds = "Extent", withHeading = "logical"),
#   definition = function(X, bounds, withHeading) {
#     if (withHeading) {
#       # This requires that previous points be "moved" as if they are
#       #  off the bounds, so that the heading is correct
#       X@data[coordinates(X)[, "x"] < bounds@xmin, "x1"] <-
#         (X@data[coordinates(X)[, "x"] < bounds@xmin, "x1"] - bounds@xmin) %%
#         (bounds@xmax - bounds@xmin) + bounds@xmax
#       X@data[coordinates(X)[, "x"] > bounds@xmax, "x1"] <-
#         (X@data[coordinates(X)[, "x"] > bounds@xmax, "x1"] - bounds@xmax) %%
#         (bounds@xmin - bounds@xmax) + bounds@xmin
#       X@data[coordinates(X)[, "y"] < bounds@ymin, "y1"] <-
#         (X@data[coordinates(X)[, "y"] < bounds@ymin, "y1"] - bounds@ymin) %%
#         (bounds@ymax - bounds@ymin) + bounds@ymax
#       X@data[coordinates(X)[, "y"] > bounds@ymax, "y1"] <-
#         (X@data[coordinates(X)[, "y"] > bounds@ymax, "y1"] - bounds@ymax) %%
#         (bounds@ymin - bounds@ymax) + bounds@ymin
#     }
#     return(wrap(X, bounds = bounds))
# })

# setMethod(
#   "wrap",
#   signature(X = "SpatialPointsDataFrame", bounds = "Raster", withHeading = "logical"),
#   definition = function(X, bounds, withHeading) {
#       X <- wrap(X, bounds = extent(bounds), withHeading = withHeading)
#       return(X)
# })

# setMethod(
#   "wrap",
#   signature(X = "SpatialPointsDataFrame", bounds = "matrix", withHeading = "logical"),
#   definition = function(X, bounds, withHeading) {
#     if (identical(colnames(bounds), c("min", "max")) &
#         identical(rownames(bounds), c("s1", "s2"))) {
#       X <- wrap(X, bounds = extent(bounds), withHeading = withHeading)
#       return(X)
#     } else {
#       stop("Must use either a bbox, Raster*, or Extent for 'bounds'")
#     }
# })

################################################################################
#' Identify outward radiating spokes from initial points
#'
#' This is a generalized version of a notion of a viewshed.
#' The main difference is that there can be many "viewpoints".
#'
#' @inheritParams cir
#'
#' @param stopRule A function. If the spokes are to stop. This can be a function
#'                 of `landscape`, `fromCell`, `toCell`, `x`
#'                 (distance from coords cell), or any other named argument passed
#'                 into the `...` of this function. See examples.
#'
#' @param nAngles Numeric, length one. Alternative to angles. If provided, the function
#'                will create a sequence of angles from `0` to `2*pi`,
#'                with a length `nAngles`, and not including `2*pi`.
#'                Will not be used if `angles` is provided, and will show
#'                warning of both are given.
#'
#' @param ... Objects to be used by `stopRule()`. See examples.
#'
#' @return A matrix containing columns id (representing the row numbers of `coords`),
#' angles (from `coords` to each point along the spokes), x and y coordinates
#' of each point along the spokes, the corresponding indices on the `landscape`
#' Raster, dists (the distances between each `coords` and each point along the
#' spokes), and stop, indicating if it was a point that caused a spoke to stop
#' going outwards due to `stopRule`.
#'
#' @author Eliot McIntire
#' @export
#' @importFrom fpCompare %<<%
#' @rdname spokes
#'
#' @example inst/examples/example_spokes.R
#'
spokes <- function(landscape, coords, loci, maxRadius = ncol(landscape) / 4,
           minRadius = maxRadius, allowOverlap = TRUE, stopRule = NULL,
           includeBehavior = "includePixels", returnDistances = FALSE,
           angles = NA_real_, nAngles = NA_real_, returnAngles = FALSE,
           returnIndices = TRUE, ...) {
  message("This function is very experimental and may not behave as expected")
#  signature(landscape = "RasterLayer", coords = "SpatialPoints", loci = "missing"),
  if (!missing(nAngles)) {
    if (missing(angles)) {
    angles <- seq(0, pi * 2, length.out = 17)
    angles <- angles[-length(angles)]
    } else {
      warning("Both angles and nAngles are provided. Using angles only.")
    }
  }

  aCir <- cir(landscape, coords = coords, minRadius = minRadius, maxRadius = maxRadius,
              returnAngles = TRUE, returnDistances = TRUE,
              allowOverlap = allowOverlap, allowDuplicates = TRUE,
              angles = angles, returnIndices = returnIndices)

  if (!is.null(stopRule)) {
    forms <- names(formals(stopRule))
    fromC <- "fromCell" %in% forms
    if (fromC) fromCell <- cellFromXY(landscape, terra::crds(coords))
    toC <- "toCell" %in% forms
    if (toC) toCell <- cellFromXY(landscape, to[, xycolNames])
    land <- "landscape" %in% forms
    listArgs <- if (land) list(landscape = landscape[aCir[, "indices"]][[1]]) else NULL
    if (length(list(...)) > 0) listArgs <- append(listArgs, list(...))
    xDist <- "x" %in% forms

    a <- cbind(aCir, do.call(stopRule, args = listArgs))
    colnames(a)[ncol(a)] <- "stop"
    a <- cbind(a, stopDist = a[, "stop"] * a[, "dists"])
    a[a[, "stop"] %==% 0, "stopDist"] <- maxRadius

    sortedUniqAngles <- sort(unique(a[, "angles"]))
    dxx <- lapply(sort(unique(a[, "id"])), function(id) {
      aID <- a[a[, "id"] == id, , drop = FALSE]
      b <- tapply(aID[, "stopDist"], aID[, "angles"], min, na.rm = TRUE)
      d1 <- lapply(sortedUniqAngles, function(x) {
        a1 <- aID[aID[, "angles"] %==% x, , drop = FALSE]
        if (includeBehavior == "excludePixels")
          a1[a1[, "dists"] %<<% b[as.numeric(names(b)) %==% x], , drop = FALSE]
        else
          a1[a1[, "dists"] %<=% b[as.numeric(names(b)) %==% x], , drop = FALSE]
      })
      do.call(rbind, d1)
    })
    d2xx <- do.call(rbind, dxx)
    whDrop <- match(c("stopDist"), colnames(d2xx))
    d2xx[, -whDrop, drop = FALSE]
  }
}

#' This is a very fast version of `cir` with `allowOverlap = TRUE`,
#' `allowDuplicates = FALSE`, `returnIndices = TRUE`, `returnDistances = TRUE`, and
#' `includeBehavior = "excludePixels"`.
#' It is used inside `spread2`, when asymmetry is active.
#' The basic algorithm is to run `cir` just once, then add to the x,y coordinates of every locus.
#'
#' @name cirSpecialQuick
#' @inheritParams cir
.cirSpecialQuick <- function(landscape, loci, maxRadius, minRadius) {
  bb <- xyFromCell(landscape, loci)
  middleCell <- if (identical(ncell(landscape) / 2, floor(ncell(landscape) / 2))) {
    ncell(landscape) / 2 - ncol(landscape) / 2
  } else {
    round(ncell(landscape) / 2)
  }
  xy <- xyFromCell(landscape, middleCell)

  # just run one, central locus with cir
  pureCircle2 <- cir(landscape,
                     #loci = attributes(dt)$spreadState$clusterDT$initialPixels,
                     allowOverlap = TRUE, allowDuplicates = FALSE,
                     maxRadius = maxRadius,
                     minRadius = minRadius,
                     returnIndices = TRUE,
                     returnDistances = TRUE,
                     includeBehavior = "excludePixels")
  pureCircle2 <- pureCircle2[order(pureCircle2[, "indices"]), ]
  cc <- cbind(pureCircle2[, "x"] - xy[,"x"], pureCircle2[, "y"] - xy[, "y"])
  dd <- cbind(x = rep(bb[, "x"], each = NROW(pureCircle2)),
              y = rep(bb[, "y"], each = NROW(pureCircle2))) +
    matrix(rep(t(cc), NROW(bb)), ncol = 2, byrow = TRUE)
  lociAll <- rep(loci, each = NROW(pureCircle2))
  distsAll <- rep(pureCircle2[, "dists"], nrow(bb))
  dd <- cbind(id = lociAll, dd, indices = cellFromXY(landscape, dd[, xycolNames]),
              dists = distsAll)

  dd[!as.logical(dd[, "x"] > terra::xmax(landscape) | dd[, "x"] < terra::xmin(landscape) |
                       dd[, "y"] > terra::ymax(landscape) | dd[, "y"] < terra::ymin(landscape)), ]

}
