utils::globalVariables("num.in.pop")

################################################################################
#' Produce a `raster` of a random Gaussian process.
#'
#' Defunct.
#'
#' @param x        A spatial object (e.g., a `RasterLayer`).
#'
#' @param scale    The spatial scale in map units of the Gaussian pattern.
#'
#' @param var      Spatial variance.
#'
#' @param method   The type of model used to produce the Gaussian pattern.
#'                 Should be one of `"RMgauss"` (Gaussian covariance model),
#'                 `"RMstable"` (the stable powered exponential model),
#'                 or the default, `"RMexp"` (exponential covariance model).
#'
#' @param speedup  An numeric value indicating how much faster than 'normal'
#'                 to generate maps. It may be necessary to give a value larger
#'                 than 1 for large maps. Default is 1.
#'
#' @param alpha    A required parameter of the `"RMstable"` model.
#'                 Should be in the interval `[0,2]` to provide a valid covariance function.
#'                 Default is 1.
#'
#' @param inMemory Should the `RasterLayer` be forced to be in memory?
#'                 Default `FALSE`.
#'
#' @param ... Additional arguments to `raster`.
#'
#' @return A raster map with same extent as `x`, with a Gaussian random pattern.
#'
#' @importFrom terra res
#' @export
#' @rdname gaussMap
#'
gaussMap <- function(x, scale = 10, var = 1, speedup = 1, method = "RMexp",
                     alpha = 1, inMemory = FALSE, ...) {
  .Defunct(msg = paste(
    "Random landscape generation functionality has been removed",
    "because the RandomFields packages is no longer maintained.\n",
    "See neutralLandscapeMap() or use the NLMR package for tools to generate various",
    "random/neutral landscapes."
  ))
}

################################################################################
#' Find factors
#'
#' Internal function (used in [gaussMap()]).
#' Finds the integer factors of an integer.
#'
#' @param x An integer to factorize
#'
#' @return A vector of integer factors
#'
#' @keywords internal
#' @rdname findFactors
.findFactors <- function(x) {
  x <- as.integer(x)
  div <- seq_len(abs(x))
  return(div[x %% div == 0L])
}

#' Produce a `SpatRaster` of  random polygons
#'
#' These are built with the [spread()] function internally.
#'
#' @param ras A raster that whose extent will be used for the random polygons.
#'
#' @param numTypes Numeric value. The number of unique polygon types to use.
#'
#' @param ...      Other arguments passed to `spread`. No known uses currently.
#'
#' @return A map of extent `ext` with random polygons.
#'
#' @seealso [spread()], [randomPolygons()]
#'
#' @export
#' @importFrom terra cellFromXY ext xmax xmin ymax ymin
#' @rdname randomPolygons
#'
#' @examples
#' origDTThreads <- data.table::setDTthreads(2L)
#' origNcpus <- options(Ncpus = 2L)
#'
#' set.seed(1234)
#' Ras <- randomPolygons(numTypes = 5)
#' if (interactive() ) {
#'   terra::plot(Ras, col = c("yellow", "dark green", "blue", "dark red"))
#' }
#'
#' # more complex patterning, with a range of patch sizes
#' r <- terra::rast(terra::ext(0, 50, 0, 50), resolution = 1, vals = 0)
#' a <- randomPolygons(numTypes = 400, r)
#' a[a < 320] <- 0
#' a[a >= 320] <- 1
#' clumped <- terra::patches(a)
#' if (interactive()) {
#'   terra::plot(a)
#' }
#'
#' # clean up
#' data.table::setDTthreads(origDTThreads)
#' options(Ncpus = origNcpus)
#'
randomPolygons <- function(ras = rast(ext(0, 15, 0, 15), res = 1, vals = 0),
                           numTypes = 2, ...) {
  args <- list(...)
  if (any(c("p", "A", "speedup", "minpatch") %in% names(args))) {
    warning("Arguments p, A, speedup, and minpatch have been deprecated. ",
            "See new function definition.")
  }

  xy <- cbind(x = stats::runif(numTypes, xmin(ras), xmax(ras)),
              y = stats::runif(numTypes, ymin(ras), ymax(ras)))
  starts <- terra::vect(xy)
  loci <- terra::cellFromXY(xy, object = ras)
  a <- spread(landscape = ras, spreadProb = 1, loci, allowOverlap = FALSE, id = TRUE, ...)
  return(a)
}

#' Create a single random polygon object
#'
#' Produces a `SpatVector` polygons object with 1 feature that will have approximately an area
#' equal to `area` (expecting area in hectares), #' and a centre at approximately `x`.
#'
#' @param x Either a `SpatVector`, or `SpatialPoints` (deprecated), `SpatialPolygons` (deprecated),
#'          or `matrix` with two dimensions, 1 row, with the approximate centre of the new random
#'          polygon to create.
#'          If `matrix`, then longitude and latitude are assumed (`epsg:4326`).
#'
#' @param area A numeric, the approximate area in `meters squared` of the random polygon.
#'
#' @param hectares Deprecated. Use `area` in meters squared.
#'
#' @return A `SpatVector` polygons object, with approximately the area request,
#'         centred approximately at the coordinates requested, in the projection of `x`.
#'
#' @importFrom terra crs crs<-
#' @importFrom stats rbeta runif
#' @importFrom terra crs
#' @export
#' @docType methods
#' @rdname randomPolygons
#'
#' @examples
#' latLong <- terra::crs("+proj=longlat +datum=WGS84 +no_defs")
#' # "epsg:4326"
#' a1 <- terra::vect(cbind(-110, 59), crs = latLong)
#' a2 <- randomPolygon(a1, area = 1e7)
#'
#' if (interactive()) {
#'   terra::plot(a1)
#'   terra::points(a2, pch = 19)
#' }
#'
#' if (require("sf", quietly = TRUE)) {
#'   latLong <- sf::st_crs("+proj=longlat +datum=WGS84 +no_defs")
#'   # "epsg:4326"
#'
#'   b1 <- list(cbind(
#'     x = c(-122.98, -116.1, -99.2, -106, -122.98),
#'     y = c(59.9, 65.73, 63.58, 54.79, 59.9)
#'   )) |>
#'     sf::st_polygon() |>
#'     sf::st_sfc() |>
#'     sf::st_sf(geometry = _, ID = 1L, shinyLabel = "zone2", crs = latLong)
#'   b2 <- randomPolygon(b1, area = 1e10)
#'
#'   if (interactive()) {
#'     plot(sf::st_geometry(b1))
#'     plot(sf::st_geometry(b2), add = TRUE)
#'   }
#' }
randomPolygon <- function(x, hectares, area) {
  UseMethod("randomPolygon")
}

#' @export
#' @rdname randomPolygons
randomPolygon.default <- function(x, hectares, area) {
  if (inherits(x, "SpatialPoints")) {
    rndmPolygonSpatialPoints(x = x, hectares = hectares, area = area)
  } else if (inherits(x, "SpatVector")) {
    rndmPolygonSpatVector(x, hectares, area)
  } else if (inherits(x, "matrix")) {
    rndmPolygonMatrix(x, hectares, area)
  } else if (inherits(x, "SpatialPolygons")) {
    rndmPolygonSpatialPolygons(x, hectares, area)
  } else if (inherits(x, "sf")) {
    rndmPolygonSf(x, hectares, area)
  }
}

rndmPolygonSpatialPoints <- function(x, hectares, area) {
  .Deprecated("User should convert to using SpatVector rather that SpatialPoints")
  .requireNamespace("sp")
  if (!missing(hectares)) {
    message("hectares argument is deprecated; please use area")
    if (missing(area))
      area <- hectares * 1e4
  }

  units <- gsub(".*units=(.) .*", "\\1", crs(x))

  areaM2 <- area * 1.304 # rescale so mean area is close to hectares
  radius <- sqrt(areaM2 / pi)
  if (!identical(units, "m")) {
    origCRS <- raster::crs(x)
    crsInUTM <- utmCRS(x)
    if (is.na(crsInUTM))
      stop("Can't calculate areas with no CRS provided. Please give a CRS to x. See example.")
    x <- sp::spTransform(x, CRSobj = crsInUTM)
    message("The CRS provided is not in meters; ",
            "converting internally to UTM so area will be approximately correct.")
  }

  meanX <- mean(sp::coordinates(x)[, 1]) - radius
  meanY <- mean(sp::coordinates(x)[, 2]) - radius

  minX <- meanX - radius
  maxX <- meanX + radius
  minY <- meanY - radius
  maxY <- meanY + radius

  # Add random noise to polygon
  xAdd <- round(runif(1, radius * 0.8, radius * 1.2))
  yAdd <- round(runif(1, radius * 0.8, radius * 1.2))
  nPoints <- 20
  betaPar <- 0.6
  X <- c(jitter(sort(rbeta(nPoints, betaPar, betaPar) * (maxX - minX) + minX)),
         jitter(sort(rbeta(nPoints, betaPar, betaPar) * (maxX - minX) + minX, decreasing = TRUE)))
  Y <- c(jitter(sort(rbeta(nPoints / 2, betaPar, betaPar) * (maxY - meanY) + meanY)),
         jitter(sort(rbeta(nPoints, betaPar, betaPar) * (maxY - minY) + minY, decreasing = TRUE)),
         jitter(sort(rbeta(nPoints / 2, betaPar, betaPar) * (meanY - minY) + minY)))

  Sr1 <- sp::Polygon(cbind(X + xAdd, Y + yAdd))
  Srs1 <- sp::Polygons(list(Sr1), "s1")
  outPolygon <- sp::SpatialPolygons(list(Srs1), 1L)
  terra::crs(outPolygon) <- terra::crs(x)
  wasSpatial <- is(outPolygon, "Spatial")
  if (exists("origCRS", inherits = FALSE))  {
    if (requireNamespace("sf", quietly = TRUE)) {
      outPolygon <- sf::st_as_sf(outPolygon)
      outPolygon <- sf::st_transform(outPolygon, origCRS)
      outPolygon <- as(outPolygon, "Spatial")
    } else {
      ## TODO: this should use reproducible:::suppressWarningsSpecific
      outPolygon <- suppressWarnings(sp::spTransform(outPolygon, origCRS))
    }
  }
  return(outPolygon)
}

#' @importFrom terra geomtype is.related spatSample
rndmPolygonSpatVector <- function(x, hectares, area) {
  if (!missing(hectares)) {
    message("hectares argument is deprecated; please use area")
    if (missing(area))
      area <- hectares * 1e4
  }

  if (geomtype(x) == "polygons") {
    need <- TRUE
    while (need) {
      sp1 <- spatSample(x, 1, "random")
      sp2 <- .randomPolygonSpatPoint(sp1, area)
      contain <- is.related(sp1, sp2, relation = "within")
      if (isTRUE(contain))
        need <- FALSE
    }
  } else if (geomtype(x) == "points") {
    sp2 <- .randomPolygonSpatPoint(x, area)
  } else {
    stop("if x is a SpatVector, geom type must be points or polygons")
  }
  sp2
}

#' @importFrom terra vect crs
rndmPolygonMatrix <- function(x, hectares, area) {
  if (!missing(hectares)) {
    message("hectares argument is deprecated; please use area")
    if (missing(area))
      area <- hectares
  }
  latLong <- terra::crs("+proj=longlat +datum=WGS84 +no_defs")
  # latLong <- crs("epsg:4326")
  message("Assuming matrix is in latitude/longitude")
  x <- vect(x, type = "points")
  crs(x) <- latLong
  randomPolygon(x, area = area)
}

rndmPolygonSpatialPolygons <- function(x, hectares, area) {
  .Deprecated("User should convert to using SpatVector rather than SpatialPoints")
  .requireNamespace("sf")
  .requireNamespace("sp")

  if (!missing(hectares)) {
    message("hectares argument is deprecated; please use area")
    if (missing(area))
      area <- hectares
  }
  need <- TRUE
  while (need) {
    sp1 <- sp::spsample(x, 1, "random")
    sp2 <- randomPolygon(sp1, area)
    contain <- sf::st_contains(sf::st_as_sf(sp2), sf::st_as_sf(sp1))
    if (isTRUE(contain))
      need <- FALSE
  }
  sp2
}

rndmPolygonSf <- function(x, hectares, area) {
  .requireNamespace("sf")

  terra::vect(x) |>
    rndmPolygonSpatVector(hectares, area) |>
    sf::st_as_sf()
}

#' @importFrom terra project crs crds vect
.randomPolygonSpatPoint <- function(x, area) {
  units <- gsub(".*units=(.) .*", "\\1", crs(x, proj = TRUE))

  areaM2 <- area * 1.304 # rescale so mean area is close to hectares
  radius <- sqrt(areaM2 / pi)
  if (!identical(units, "m")) {
    origCRS <- crs(x)
    crsInUTM <- utmCRS(x)
    if (is.na(crsInUTM))
      stop("Can't calculate areas with no CRS provided. Please give a CRS to x. See example.")
    x <- project(x, crsInUTM)
    message("The CRS provided is not in meters; ",
            "converting internally to UTM so area will be approximately correct.")
  }

  #y <- spTransform(x, areaCRS)

  meanX <- mean(crds(x)[, 1]) - radius
  meanY <- mean(crds(x)[, 2]) - radius

  minX <- meanX - radius
  maxX <- meanX + radius
  minY <- meanY - radius
  maxY <- meanY + radius

  # Add random noise to polygon
  xAdd <- round(runif(1, radius * 0.8, radius * 1.2))
  yAdd <- round(runif(1, radius * 0.8, radius * 1.2))
  nPoints <- 20
  betaPar <- 0.6
  X <- c(jitter(sort(rbeta(nPoints, betaPar, betaPar) * (maxX - minX) + minX)),
         jitter(sort(rbeta(nPoints, betaPar, betaPar) * (maxX - minX) + minX, decreasing = TRUE)))
  Y <- c(jitter(sort(rbeta(nPoints / 2, betaPar, betaPar) * (maxY - meanY) + meanY)),
         jitter(sort(rbeta(nPoints, betaPar, betaPar) * (maxY - minY) + minY, decreasing = TRUE)),
         jitter(sort(rbeta(nPoints / 2, betaPar, betaPar) * (meanY - minY) + minY)))

  outPolygon <- vect(cbind(X + xAdd, Y + yAdd), type = "polygons")
  crs(outPolygon) <- crs(x)

  if (exists("origCRS", inherits = FALSE))  {
    outPolygon <- project(outPolygon, origCRS)
  }
  outPolygon
}

################################################################################
#' Initiate a specific number of agents in a map of patches
#'
#' Instantiate a specific number of agents per patch.
#' The user can either supply a table of how many to initiate in each patch,
#' linked by a column in that table called `pops`.
#'
#' @param patches `SpatRaster` of patches, with some sort of a patch id.
#'
#' @param numPerPatchTable A `data.frame` or `data.table` with a
#'  column named `pops` that matches the `patches` patch ids, and a
#'  second column `num.in.pop` with population size in each patch.
#'
#' @param numPerPatchMap A `SpatRaster` exactly the same as `patches`
#' but with agent numbers rather than ids as the cell values per patch.
#'
#' @return A raster with 0s and 1s, where the 1s indicate starting locations of
#' agents following the numbers above.
#'
#' @examples
#' library(data.table)
#'
#' origDTThreads <- data.table::setDTthreads(2L)
#' origNcpus <- options(Ncpus = 2L)
#'
#' set.seed(1234)
#' Ntypes <- 4
#' ras <- randomPolygons(numTypes = Ntypes)
#' if (interactive()) {
#'   terra::plot(ras)
#' }
#'
#' # Use numPerPatchTable
#' patchDT <- data.table(pops = 1:Ntypes, num.in.pop = c(1, 3, 5, 7))
#' rasAgents <- specificNumPerPatch(ras, patchDT)
#' rasAgents[is.na(rasAgents)] <- 0
#'
#' if (require(testthat))
#'   expect_true(all(unname(table(ras[rasAgents])) == patchDT$num.in.pop))
#'
#' # Use numPerPatchMap
#' rasPatches <- ras
#' for (i in 1:Ntypes) {
#'   rasPatches[rasPatches==i] <- patchDT$num.in.pop[i]
#' }
#' if (interactive()) {
#'   terra::plot(c(ras, rasPatches))
#' }
#' rasAgents <- specificNumPerPatch(ras, numPerPatchMap = rasPatches)
#' rasAgents[is.na(rasAgents)] <- 0
#' if (interactive()) {
#'   terra::plot(rasAgents)
#' }
#'
#' # clean up
#' data.table::setDTthreads(origDTThreads)
#' options(Ncpus = origNcpus)
#'
#' @export
#' @importFrom data.table data.table setkey
#' @importFrom stats na.omit
#' @importFrom terra ext rast values
#' @rdname specnumperpatch-probs
specificNumPerPatch <- function(patches, numPerPatchTable = NULL, numPerPatchMap = NULL) {
  isRaster <- inherits(patches, "RasterLayer")
  if (isRaster) {
    patches <- terra::rast(patches)
    if (!is.null(numPerPatchMap) && inherits(numPerPatchMap, "RasterLayer")) {
      numPerPatchMap <- terra::rast(numPerPatchMap)
    }
  }
  patchids <- as.numeric(terra::values(patches, na.rm = TRUE))
  wh <- which(as.logical(terra::values(patches)))
  if (!is.null(numPerPatchTable)) {
    dt1 <- data.table(wh, pops = patchids)
    setkeyv(dt1, "pops")
    if (is(numPerPatchTable, "data.table")) {
      numPerPatchTable <- data.table(numPerPatchTable)
    }
    setkeyv(numPerPatchTable, "pops")
    dt2 <- dt1[numPerPatchTable]
  } else if (!is.null(numPerPatchMap)) {
    numPerPatchTable <- as.numeric(na.omit(terra::values(numPerPatchMap)))
    dt2 <- data.table(wh, pops = patchids, num.in.pop = numPerPatchTable)
  } else {
    stop("need numPerPatchMap or numPerPatchTable")
  }

  dt3 <- dt2[, list(cells = resample(wh, unique(num.in.pop))), by = "pops"]
  dt3$ids <- rownames(dt3)

  al <- terra::rast(terra::ext(patches), res = res(patches), vals = 0)
  al[dt3$cells] <- 1

  if (isRaster) {
    al <- raster::raster(al)
  }

  return(al)
}

###
# ### INCORPORATE RELEVANT PARTS OF THIS OLD INIT FUNCTION INTO INITCOORDS()
# ###
# #' initialize mobileAgent
# #'
# #' @param agentlocation The initial positions of the agents
# #'                      (currently only \code{RasterLayer} or
# #'                      \code{SpatialPolygonsDataFrame}) accepted.
# #'
# #' @param numagents The number of agents to initialize.
# #'
# #' @param probinit The probability of placing an agent at a given initial position.
# #'
# #' @export
# setMethod("initialize", "mobileAgent", function(.Object, ...,
#           agentlocation = NULL, numagents = NULL, probinit = NULL) {
#   if (is(agentlocation, "Raster")){
#     ext <- extent(agentlocation)
#     if (!is.null(probinit)) {
#       #            nonNAs <- !is.na(getvalue(probinit))
#       nonNAs <- !is.na(getValues(probinit))
#       wh.nonNAs <- which(nonNAs)
#       #            ProbInit.v <- cumsum(getvalue(probinit)[nonNAs])
#       ProbInit.v <- cumsum(getValues(probinit)[nonNAs])
#       if (!is.null(numagents)) {
#         ran <- runif(numagents,0,1)
#         fI <- findInterval(ran, ProbInit.v)+1
#         fI2 <- wh.nonNAs[fI]
#         last.ran <- runif(numagents,0,1)
#         last.fI <- findInterval(last.ran, ProbInit.v)+1
#         last.fI2 <- wh.nonNAs[last.fI]
#       } else {
#         #                va <- getvalue(probinit)[nonNAs]
#         va <- getValues(probinit)[nonNAs]
#         ran <- runif(length(va), 0, 1)
#         fI2 <- wh.nonNAs[ran<va]
#
#         last.ran <- runif(length(fI2), 0, 1)
#         last.fI <- findInterval(last.ran, ProbInit.v) + 1
#         last.fI2 <- wh.nonNAs[last.fI]
#
#         #                last.ran <- runif(length(fI2),0,1)
#         #                last.fI2 <- wh.nonNAs[last.ran<va]
#       }
#       if (length(grep(pattern = "Raster",class(agentlocation))) == 1) {
#         position <- xyFromCell(agentlocation,fI2,spatial = TRUE)
#       } else if (length(grep(pattern = "SpatialPoints",class(agentlocation))) == 1) {
#         position <- coordinates(agentlocation)
#       } else {
#         stop("need raster layer or Spatial Points object")
#       }
#       numagents <- length(position)
#     } else {
#       # probinit is NULL - start exactly the number of agents as there
#       # are pixels in agentlocation
#       if (!is.null(numagents)) {
#         if (is(agentlocation,"Raster")) {
#           xy = matrix(runif(numagents*2, c(xmin(ext), ymin(ext)),
#                             c(xmax(ext), ymax(ext))), ncol = 2, byrow = TRUE)
#           colnames(xy) = xycolNames
#           position <- SpatialPoints(xy)
#           position <- SpatialPoints(sampleRandom(agentlocation, numagents, xy = TRUE, sp = TRUE))
#         } else if (is(agentlocation,"SpatialPoints")) {
#           sam <- sample(1:length(agentlocation),numagents)
#           position <- SpatialPoints(agentlocation[sam,])
#         } else {
#           stop("need raster layer or Spatial Points object")
#         }
#       } else { # for numagents also NULL
#         if (length(grep(pattern = "Raster",class(agentlocation))) == 1) {
#           position <- SpatialPoints(xyFromCell(agentlocation,
#                                                Which(agentlocation, cells = TRUE)))
#         } else if (length(grep(pattern = "SpatialPoints", class(agentlocation))) == 1) {
#           position <- SpatialPoints(agentlocation)
#         } else {
#           stop("need raster layer or Spatial Points object")
#         }
#         numagents <- length(position)
#       }
#     }
#   } else if (is(agentlocation,"SpatialPolygonsDataFrame")) {
#     if (!is.null(numagents)) {
#       if (!is.null(pri) ) {
#         position <- SpatialPoints(dotsInPolys(agentlocation,
#                                               as.integer(round(numagents*pri,0))))
#         numagents <- length(position)
#       } else {stop("with SpatialPolygonsDataFrame, probinit is required")}
#     } else {stop("with SpatialPolygonsDataFrame, numagents is required")}
#   } else if (is.null(agentlocation)) {
#     stop("Need to provide agentlocation, which can be a map layer")
#   }
#   heading1 <- runif(numagents, 0, 360)
#   distance <- runif(numagents, 0.1, 10)
#
#   .Object@ID <- as.character(1:numagents)
#   .Object@spatial <- position
#   .Object@heading <- heading1
#   .Object@distance <- distance
#
#   return(.Object)
# })

#' @importFrom terra crs
utmCRS <- function(x) {
  zone <- long2UTM(mean(c(xmax(x), xmin(x))))
  crs(paste0("+proj=utm +zone=", zone, " +datum=WGS84"), proj = TRUE)
}

long2UTM <- function(long) {
  (floor((long + 180) / 6) %% 60) + 1
}

#' Produce a neutral landscape using a midpoint displacement algorithm
#'
#' This is a wrapper for the `nlm_mpd` function in the `NLMR` package.
#' The main addition is that it makes sure that the output raster conforms
#' in extent with the input raster `x`, since `nlm_mpd` can output a smaller raster.
#'
#' @param x        A `RasterLayer`/`SpatRaster` to use as a template.
#'
#' @param pad      Integer. Number of cells by which to pad `x` internally to ensure
#'                 `nlm_mpd` produces a raster corresponding to the dimensions of `x`.
#'
#' @param type     One of the supported `NLMR` functions.
#'
#' @param ...      Further arguments passed to `NLMR` function specified in `type`
#'  (except `ncol`, `nrow` and `resolution`, which are extracted from `x`).
#'
#' @importFrom terra res ncol nrow ext extend focal
#' @export
#' @rdname neutralLandscapeMap
#'
#' @return A `RasterLayer`/`SpatRaster` with same extent as `x`, with randomly generated values.
#'
#' @seealso `nlm_mpd`
#'
#' @examples
#' \donttest{
#'   if (requireNamespace("NLMR", quietly = TRUE) &&
#'       requireNamespace("raster", quietly = TRUE)) {
#'     library(terra)
#'     nx <- ny <- 100L
#'     r <- rast(nrows = ny, ncols = nx,
#'               xmin = -nx/2, xmax = nx/2,
#'               ymin = -ny/2, ymax = ny/2)
#'     ## or with raster package:
#'     # r <- raster::raster(nrows = ny, ncols = nx,
#'     #                     xmn = -nx/2, xmx = nx/2,
#'     #                     ymn = -ny/2, ymx = ny/2)
#'     map1 <- neutralLandscapeMap(r,
#'                                 type = "nlm_mpd",
#'                                 roughness = 0.65,
#'                                 rand_dev = 200,
#'                                 rescale = FALSE,
#'                                 verbose = FALSE)
#'     if (interactive()) plot(map1)
#'   }
#' }
neutralLandscapeMap <- function(x, pad = 10L,
                                type = c("nlm_mpd", "nlm_gaussianfield", "nlm_distancegradient",
                                         "nlm_edgegradient", "nlm_fbm", "nlm_mosaicfield",
                                         "nlm_mosaicgibbs", "nlm_mosaictess", "nlm_neigh",
                                         "nlm_percolation", "nlm_planargradient", "nlm_random",
                                         "nlm_randomrectangularcluster"),
                                ...) {
  if (requireNamespace("NLMR", quietly = TRUE)) {
    .requireNamespace("raster")
    type <- match.arg(type)
    typeFun <- getFromNamespace(type, ns = "NLMR")

    dummyVals <- typeFun(
      ncol = ncol(x) + pad, ## pad the raster so any lost cols won't affect crop etc.
      nrow = nrow(x) + pad, ## pad the raster so any lost rows won't affect crop etc.
      resolution = unique(res(x)),
      ...
    )

    ## NOTE: dummyVals doesn't match dimensions etc. of x:
    ## - no CRS and its extent doesn't match x's (but the resolution does);
    ## - we can't reproject or use x to define the extent;
    ## - need to add rows/cols by multiplying the final number by res;
    ## - crop to ensure the extra rows/cols are removed
    dummyExt <- c(0, ncol(x) * unique(res(x)), 0, nrow(x) * unique(res(x)))
    dummyVals <- extend(dummyVals, dummyExt, values = NA)
    dummyVals <- crop(dummyVals, dummyExt)

    # ## now replace added NAs
    # maxIts <- max(nMissingColsRows) + 1L
    # it <- 1L
    # while (any(is.na(dummyVals[])) & (it < maxIts)) {
    #   dummyVals <- focal(dummyVals, w = matrix(1, 3, 3),
    #                              fun = mean, NAonly = TRUE, na.rm = TRUE)
    #   it <- it + 1L
    # }

    dummyLandscape <- x
    dummyLandscape[] <- as.vector(dummyVals[])

    return(dummyLandscape)
  } else {
    stop("Package 'NLMR' not available. Please install it using:\n",
         "  install.packages('NLMR', repos = 'https://predictiveecology.r-universe.dev')")
  }
}
