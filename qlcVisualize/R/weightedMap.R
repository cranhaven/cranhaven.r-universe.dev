weightedMap <- function(x, y = NULL, window = NULL, crs = NULL, weights = "equal",
                        grouping = NULL, holes = NULL, concavity = 2, expansion = 1000,
                        method = "dcn", maxit = 10, verbose = 0) {

  # prepare result
  result <- list()
  # set crs
  result <- .setCRS(result, x, window, crs)
  # set points to proper sf format
  result <- .setPoints(result, x, y)
  # set grouping
  result <- .setGrouping(result, grouping)
  # prepare window
  if (is.null(window)) {
    # without provided window, make new window around points
    result <- .makeWindow(result, concavity, expansion)
    # add holes inside window
    if (!is.null(holes)) {
      result <- .makeHoles(result, holes, expansion)
    }
  } else {
    # set provided window to proper sf format
    result <- .setWindow(result, window)
    # check window for empty polygons and outside points
    result <- .checkWindow(result)
  }
  # make voronoi
  result <- .makeVoronoi(result)
  # make cartogram, but only when weights are provided
  if (!is.null(weights)) {
    # prepare weights
    result = .setWeights(result, weights)
    # make cartogram, first round just to prevent errors
    result <- .makeCarto(result, method = "dcn", maxit = 2, verbose = 0)
    result <- .makeCarto(result, method, maxit, verbose)
  }
  return(result)
}

# shorter alternative name
wmap <- weightedMap

# ======
# setCRS
# ======

.setCRS <- function(result, x, window, crs) {
  if (!is.null(crs)) {
    # use crs provided in call
    crs <- sf::st_crs(crs)
  } else {
    # respect crs from points or window
    if ((is(x, "sf") | is(x, "sfc")) && !is.na(sf::st_crs(x))) {
      crs_x <- sf::st_crs(x)
    } else {
      crs_x <- NULL
    }
    if ((is(window, "sf") | is(window, "sfc")) && !is.na(sf::st_crs(window))) {
      crs_window <- sf::st_crs(window)
    } else {
      crs_window <- NULL
    }
    if (is.null(crs_x) & is.null(crs_window)) {
      warning("No crs provided. EPSG:3857 is assumed")
      crs <- 3857
    } else if (!is.null(crs_x) & !is.null(crs_window)) {
      if (crs_x == crs_window) {
        crs <- crs_x
      } else {
        stop("crs from x and window are different")
      }
    } else if (!is.null(crs_x)) {
      crs <- crs_x
    } else {
      crs <- crs_window
    }
  }
  result$crs <- crs
  return(result)
}

# =========
# setPoints
# =========

.setPoints <- function(result, x, y) {
  if (!is.null(y)) {
    # combine x and y values
    x <- data.frame(x, y)
  }
  if (!is(x, "sf") && !is(x, "sfc")) {
    # turn coordinates into sf
    colnames(x) <- c("X","Y")
    x <- sf::st_as_sf(x, coords=c("X", "Y"), crs = 4326)
  }
  if (is(st_geometry(x),"sfc_POINT")) {
    x <- sf::st_transform(x, result$crs)
    x <- sf::st_jitter(x, factor = 1e-8)
    result$points <- sf::st_geometry(x)
  } else {
    stop("cannot interpret x as coordinates")
  }
  return(result)
}

# ===========
# setGrouping
# ===========

.setGrouping <- function(result, grouping) {
  if (is.null(grouping)) {
    grouping <- rep("1", times = length(result$points))
  }
  result$grouping <- as.character(grouping)
  return(result)
}

# =========
# setWindow
# =========

.setWindow <- function(result, window) {
  # try to convert window from spatstat::owin
  if (is(window, "owin") | is(window, "SpatVector") | is(window, "Spatial")) {
    window <- sf::st_as_sf(window)
    if (is.na(sf::st_crs(window))) {
      sf::st_crs(window) <- 4326
    }
  }
  if (is(window, "sf") || is(window, "sfc")) {
    # make valid window
    window <- sf::st_make_valid(sf::st_set_precision(window, 1e6))
    window <- sf::st_transform(window, result$crs)
    window <- sf::st_make_valid(window)
    window <- sf::st_cast(window, "MULTIPOLYGON")
  } else {
    stop("Provided window cannot be interpreted: try to use sf format")
  }
  result$window <- sf::st_geometry(window)
  return(result)
}

.checkWindow <- function(result) {
  # check intersection of points and window
  filled <- sf::st_intersects(result$points, result$window, sparse = FALSE)
  # remove polygons from window that have no points
  empty <- which(colSums(filled) == 0)
  if (length(empty) > 0) {
    # new window
    result$window <- result$window[-empty]
    # add removed indices to output
    result$emptyPolygons <- empty
    warning(
      "Some polygons do not contain any points and are removed, see $emptyPolygons.",
      call. = FALSE
    )
  }
  # remove points outside of the window
  outside <- which(rowSums(filled) == 0)
  if (length(outside) > 0) {
    # new points
    result$points <- result$points[-outside]
    # add to output
    result$outsideWindow <- outside
    warning(
      "Some points lie outside of the window and are ignored, see $outsideWindow.",
      call. = FALSE
    )
  }
  # make grouping of points from provided window
  #filled <- sf::st_intersects(result$points, result$window)
  result$grouping <- as.character(unlist(apply(filled, 1, which)))
  return(result)
}

# ==========
# makeWindow
# ==========

.makeWindow <- function(result, concavity, expansion) {
  # construct various windows for groups in grouping vector
  groups <- names(table(result$grouping))
  window <- sapply(groups, .getWindow
                   , grouping = result$grouping
                   , points = result$points
                   , concavity = concavity
                   , expansion = expansion
                   , crs = result$crs
                   , simplify = FALSE
  )
  result$window <- sf::st_sfc(sapply(window, sf::st_geometry), crs = result$crs)
  return(result)
}

.getWindow <- function(group, grouping, points, concavity, expansion, crs) {
  ids <- which(grouping == group)
  if (length(ids) == 1) {
    # circle
    w <- sf::st_geometry(sf::st_buffer(points[ids], dist = expansion))
  } else if (length(ids) == 2) {
    # squirkle
    line <- sf::st_cast(sf::st_combine(points[ids]), "LINESTRING")
    w <- sf::st_buffer(line, dist = expansion)
  } else {
    # concave window
    w <- .getConcaveWindow(sf::st_as_sf(points[ids]), concavity, expansion, crs)
    # test window: unexplicably sometimes a point falls outside
    filled <- sf::st_intersects(points[ids], w, sparse = FALSE)
    # solution: just try again
    while (min(rowSums(filled)) == 0) {
      w <- .getConcaveWindow(points[ids])
      filled <- sf::st_intersects(points[ids], w, sparse = FALSE)
    }
  }
  return(w)
}

.getConcaveWindow <- function(points, concavity, expansion, crs) {
  # concaveman hull
  w <- concaveman::concaveman(points, concavity = concavity)
  w <- sf::st_transform(w, crs)
  # use spatstat::exand.owin to make nice expansion
  if (expansion > 0) {
    w <- spatstat.geom::as.owin(w)
    w <- spatstat.random::expand.owin(w, distance = expansion)
    w <- sf::st_as_sf(w)
    sf::st_crs(w) <- crs
  }
  return(sf::st_geometry(w))
}

# =========
# makeHoles
# =========

.makeHoles <- function(result, holes, expansion) {
  # combine points for holes
  points <- do.call(rbind, holes)
  points <- sf::st_multipoint(points)
  points <- sf::st_sfc(points, crs = 4326)
  points <- sf::st_cast(points, "POINT")
  # project points
  points <- sf::st_transform(points, crs = result$crs)
  # combine with other points
  coor <- sf::st_coordinates(result$points)
  coor <- rbind(sf::st_coordinates(points), coor)
  # get closest points
  w <- spatstat.geom::as.owin(result$window)
  points <- suppressWarnings(
    spatstat.geom::ppp(coor[,1], coor[,2], window = w)
  )
  dist <- spatstat.geom::delaunayDistance(points)
  # make holes
  allHoles <- sapply(1:length(holes), .getHole
                     , coor = coor
                     , d = dist
                     , expansion = expansion
                     , crs = result$crs
  )
  # combine holes
  allHoles <- do.call(c, allHoles)
  allHoles <- sf::st_make_valid(allHoles)
  allHoles <- sf::st_union(allHoles)
  allHoles <- sf::st_sfc(allHoles, crs = result$crs)
  # remove holes from original window
  result$window <- sf::st_sfc(sf::st_difference(result$window, allHoles))
  return(result)
}

.getHole <- function(point, coor, d, expansion, crs) {
  # make holes inside closest points to provided hole-point
  hole <- spatstat.geom::convexhull.xy(coor[which(d[point,] ==  1),])
  hole <- sf::st_as_sfc(hole, crs = crs)
  #sf::st_crs(hole) <- crs
  hole <- sf::st_buffer(hole, -2*expansion)
  hole <- sf::st_buffer(hole, expansion)
  return(hole)
}

# ===========
# makeVoronoi
# ===========

.makeVoronoi <- function(result) {
  if (length(result$window) == 1) {
    voronoi <- .getVoronoi(result$points, result$window)
  } else {
    # combine separate voronoi parts into one geometry
    distr <- sf::st_intersects(result$points, result$window, sparse = FALSE)
    separate <- sapply(1:ncol(distr), function(i) {
      .getVoronoi(result$points[which(distr[,i] != 0)], result$window[i])
    }, simplify = FALSE)
    voronoi <- do.call(c, separate)
  }
  # get voronoi in right order
  order <- unlist(sf::st_intersects(result$points, voronoi))
  voronoi <- voronoi[order,]
  # make right format
  voronoi <- sf::st_make_valid(sf::st_set_precision(voronoi, 1e6))
  result$voronoi <- sf::st_cast(voronoi, "MULTIPOLYGON")
  return(result)
}

.getVoronoi <- function(points, window) {
  if (length(points) == 1) {
    # do nothing with just one point in window
    v <- sf::st_geometry(window)
  } else {
    # make voronoi inside window
    v <- sf::st_voronoi(sf::st_union(points), envelope = sf::st_geometry(window))
    v <- sf::st_cast(v)
    v <- sf::st_intersection(v, window)
    v <- sf::st_cast(v, "MULTIPOLYGON")

    # split non-contiguous polygons and union loose parts
    v <- sf::st_cast(v, "POLYGON")
    empty <- sf::st_intersects(v, points, sparse = F)
    empty <- which(!apply(empty, 1, any))
    if (length(empty) > 0) {
      near <- sf::st_nearest_feature(v)[empty]
      for (i in 1:length(empty)) {
        v[near[i]] <- sf::st_union(v[near[i]], v[empty[i]])
      }
      v <- v[-empty]
    }
    v <- sf::st_cast(v)
  }
  return(v)
}

# =============
# makeCartogram
# =============

.setWeights <- function(result, weights) {
  if (length(weights) ==  1 && weights == "equal") {
    # default to equally-sized polygons
    weights <- rep(1, times = length(result$points))
  } else if (!is.null(result$outsideWindow)) {
    # remove weights for points outside window
    weights <- weights[-result$outsideWindow]
  }
  result$weights <- weights
  return(result)
}

.makeCarto <- function(result, method, maxit, verbose) {
  # check if this is first run or second run
  if (hasName(result, "weightedVoronoi")) {
    voronoi <- result$weightedVoronoi
  } else {
    voronoi <- result$voronoi
  }
  # use package cartogramR
  v <- sf::st_sf(geometry = voronoi, weights = result$weights)
  carto <- cartogramR::cartogramR(v, count = "weights", method = method
                                  , options = list(maxit = maxit, verbose = verbose))
  # make valid polygons
  cartogram <- sf::st_set_precision(sf::st_geometry(carto$cartogram), 1e6)
  valid_carto <- sf::st_make_valid(cartogram)
  valid_carto <- sf::st_difference(valid_carto)
  # add grouping to polygons and unionize
  groups <- names(table(result$grouping))
  w <- sapply(groups, function(i) {
    sf::st_union(valid_carto[as.character(result$grouping) == i,])
  })
  w <- sf::st_sfc(w, crs = result$crs)
  result$weightedWindow <- sf::st_cast(w)
  # new points
  result$weightedPoints <- sf::st_point_on_surface(valid_carto)
  # repeat voronoi to regularize map
  tmp <- list(points = result$weightedPoints, window = result$weightedWindow)
  result$weightedVoronoi <- .makeVoronoi(tmp)$voronoi
  return(result)
}
