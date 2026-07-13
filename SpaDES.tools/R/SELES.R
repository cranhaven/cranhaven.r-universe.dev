################################################################################
#' `SELES` - Transitioning to next time step
#'
#' @description
#' Describes the probability of an agent successfully persisting until next
#' time step. THIS IS NOT YET FULLY IMPLEMENTED.
#'
#' A `SELES`-like function to maintain conceptual backwards compatibility
#' with that simulation tool. This is intended to ease transitions from
#' [SELES](http://www.gowlland.ca/).
#'
#' You must know how to use SELES for these to be useful.
#'
#' @param p realized probability of persisting (i.e., either 0 or 1).
#'
#' @param agent `SpatialPoints*` object.
#'
#' @return Returns new `SpatialPoints*` object with potentially fewer agents.
#'
#' @export
#' @include heading.R
#' @rdname SELEStransitions
#'
#' @author Eliot McIntire
transitions <- function(p, agent) {
  .requireNamespace("sp")
  sp::coordinates(agent)[which(p == 0), ] <- NA
  return(agent)
}

##############################################################
#' SELES - Number of Agents to initiate
#'
#' @description
#' Sets the the number of agents to initiate. THIS IS NOT YET FULLY IMPLEMENTED.
#'
#' A `SELES`-like function to maintain conceptual backwards compatibility
#' with that simulation tool. This is intended to ease transitions from
#' [SELES](http://www.gowlland.ca/).
#'
#' You must know how to use SELES for these to be useful.
#'
#' @param N         Number of agents to initiate (integer scalar).
#' @param probInit  Probability of initializing an agent at the location.
#'
#' @return A numeric, indicating number of agents to start
#'
#' @author Eliot McIntire
#' @export
#' @include heading.R
#' @rdname SELESnumAgents
#'
numAgents <- function(N, probInit) {
  stopifnot(length(N) == 1, is.numeric(N))
  return(N)
}

##############################################################
#' `SELES` - Initiate agents
#'
#' @description
#' Sets the the number of agents to initiate. THIS IS NOT FULLY IMPLEMENTED.
#'
#' A `SELES`-like function to maintain conceptual backwards compatibility
#' with that simulation tool. This is intended to ease transitions from
#' [SELES](http://www.gowlland.ca/).
#'
#' You must know how to use SELES for these to be useful.
#'
#' @param map `RasterLayer` with extent and resolution of desired return object
#'
#' @param numAgents numeric resulting from a call to [numAgents()]
#'
#' @param probInit a `Raster` resulting from a [probInit()] call
#'
#' @param asSpatialPoints logical or `"sf"`. Should returned object be `RasterLayer`
#'                        or `SpatVector` default (or legacy `TRUE` `SpatialPointsDataFrame`)
#'
#' @param indices numeric. Indices of where agents should start
#'
#' @return A `SpatialPointsDataFrame`, with each row representing an individual agent
#'
#' @example inst/examples/example_initiateAgents.R
#'
#' @author Eliot McIntire
#' @export
#' @include heading.R
#' @importFrom terra values ncell xyFromCell
#' @importFrom stats runif
#' @rdname initiateAgents
initiateAgents <- function(map, numAgents, probInit, asSpatialPoints = TRUE, indices) {
  if (missing(numAgents) && missing(probInit) && missing(indices))
    indices <- 1:ncell(map)
  if (missing(numAgents) && (inherits(probInit, "Raster") || inherits(probInit, "SpatRaster")))
    indices <- which(runif(ncell(probInit)) < terra::values(probInit))
  if (is.numeric(numAgents) && (missing(probInit)))
    indices <- sample(1:ncell(map), size = numAgents, replace = !(asSpatialPoints %in% FALSE))
  if (is.numeric(numAgents)  && (inherits(probInit, "Raster") || inherits(probInit, "SpatRaster"))) {
    vals <- terra::values(probInit)
    indices <- sample(1:ncell(probInit), numAgents, replace = !(asSpatialPoints %in% FALSE),
                            prob = vals / sum(vals))
  }
  if (!asSpatialPoints %in% FALSE) {
    if (length(indices > 0)) {
      xys <- terra::xyFromCell(map, indices)
      diffs <- cbind(runif(length(indices), -res(map)[1] / 2, res(map)[1] / 2),
                     runif(length(indices), -res(map)[2] / 2, res(map)[2] / 2))
      if (isTRUE(asSpatialPoints)) {
        .requireNamespace("sp")
        xys <- sp::SpatialPoints(xys)
        xys@coords <- xys@coords + diffs
        xys@bbox <- cbind(apply(sp::coordinates(xys), 2, min), apply(sp::coordinates(xys), 2, max))

      } else {
        xys <- terra::vect(xys + diffs)
      }
      terra::crs(xys) <- terra::crs(map)
      return(xys)
    } else {
      stop("uh oh! no indices specified.") # TODO: need error msg
    }
  } else {
    tmp <- terra::rast(terra::ext(map), res = res(map), vals = 0)
    tmp[indices] <- 1
    return(tmp)
  }
}



################################################################################
#' `SELES` - Agent Location at initiation
#'
#' @description
#' Sets the the location of the initiating agents. NOT YET FULLY IMPLEMENTED.
#'
#' A `SELES`-like function to maintain conceptual backwards compatibility
#' with that simulation tool. This is intended to ease transitions from
#' [SELES](http://www.gowlland.ca/).
#'
#' You must know how to use SELES for these to be useful.
#'
#' @param map A `SpatialPoints*`, `SpatialPolygons*`, or `Raster*` object.
#'
#' @return Object of same class as provided as input.
#'          If a `Raster*`, then zeros are converted to `NA`.
#'
#' @author Eliot McIntire
#' @include heading.R
#' @export
#' @rdname SELESagentLocation
agentLocation <- function(map) {
  if (length(grep(pattern = "Raster", class(map))) == 1) {
    map[map == 0] <- NA
  } else if (length(grep(pattern = "SpatialPoints", class(map))) == 1) {
    map
  } else if (!is.na(pmatch("SpatialPolygons", class(map)))) {
    map
  } else {
    stop("only raster, Spatialpoints or SpatialPolygons implemented")
  }
  return(map)
}

##############################################################
#' `SELES` - Probability of Initiation
#'
#' @description
#' Describes the probability of initiation of agents or events.
#' *THIS IS NOT FULLY IMPLEMENTED.*
#'
#' A `SELES`-like function to maintain conceptual backwards compatibility
#' with that simulation tool. This is intended to ease transitions from
#' [SELES](http://www.gowlland.ca/).
#'
#' You must know how to use SELES for these to be useful.
#'
#' @param map A `spatialObjects` object. Currently, only provides CRS and, if p is not
#' a raster, then all the raster dimensions.
#'
#' @param p probability, provided as a numeric or raster
#'
#' @param absolute logical. Is `p` absolute probabilities or relative?
#'
#' @return A `RasterLayer` with probabilities of initialization.
#'        There are several combinations of inputs possible and they each result
#'        in different behaviours.
#'
#' If `p` is numeric or `Raster` and between 0 and 1, it is treated as an
#' absolute probability, and a map will be produced with the p value(s) everywhere.
#'
#' If `p` is numeric or `Raster` and not between 0 and 1, it is treated as a
#' relative probability, and a map will be produced with `p/max(p)` value(s) everywhere.
#'
#' If `absolute` is provided, it will override the previous statements, unless
#' `absolute = TRUE` and p is not between 0 and 1 (i.e., is not a probability).
#'
#' @author Eliot McIntire
#' @export
#' @importFrom terra crs ext setValues
#' @include heading.R
#' @rdname SELESprobInit
#'
probInit <- function(map, p = NULL, absolute = NULL) {
  if (all(inRange(p, 0, 1))) {
    if (is.null(absolute)) {
      absolute <- TRUE
    }
  } else {
    absolute <- FALSE
  }
  if (is.numeric(p)) {
    probInit <- terra::rast(terra::ext(map), nrows = nrow(map), ncols = ncol(map), crs = crs(map))
    p <- rep(p, length.out = ncell(map))
    probInit <- setValues(probInit, p / (sum(p) * (1 - absolute) + 1 * (absolute)))
  } else if (is(p, "RasterLayer")) {
    .requireNamespace("raster")
    probInit <- p / (raster::cellStats(p, sum) * (1 - absolute) + 1 * (absolute))
  } else if (is(map, "SpatialPolygonsDataFrame")) {
    probInit <- p / sum(p)
  } else {
    stop("Error initializing probability map: bad inputs")
  }
  return(probInit)
}

#' Patch size
#'
#' @param patches Number of patches.
#'
#' @importFrom terra freq
#'
patchSize <- function(patches) {
  return(freq(patches))
}
