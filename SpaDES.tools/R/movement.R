################################################################################
#' Move
#'
#' Wrapper for selecting different animal movement methods.
#'
#' @param hypothesis  Character vector, length one, indicating which movement
#'                    hypothesis/method to test/use. Currently defaults to
#'                    'crw' (correlated random walk) using `crw`.
#'
#' @param ... arguments passed to the function in `hypothesis`
#'
#' @author Eliot McIntire
#' @export
#' @rdname crw
#'
move <- function(hypothesis = "crw", ...) {
     if (hypothesis == "crw") move <- "crw"
     if (is.null(hypothesis)) stop("Must specify a movement hypothesis")
     get(move)(...)
 }

################################################################################
#' Simple Correlated Random Walk
#'
#' This version uses just turn angles and step lengths to define the correlated random walk.
#'
#' This simple version of a correlated random walk is largely the version that
#' was presented in Turchin 1998, but it was also used with bias modifications
#' in McIntire, Schultz, Crone 2007.
#'
#' @param agent       A `SpatVector` points geometry or a `SpatialPoints*` (deprecated) object.
#'                    If is has attributes, e.g., `SpatialPointsDataFrame`,
#'                    2 of the columns must
#'                    be `x1` and `y1`, indicating the previous location.
#'                    If it does not have these columns as attributes, `x1` and
#'                    `y1` will be assigned randomly.
#'
#' @param stepLength  Numeric vector of length 1 or number of agents describing
#'                    step length.
#'
#' @param extent      An optional `Extent` object that will be used for `torus`.
#'
#' @param torus       Logical. Should the movement be wrapped to the opposite
#'                    side of the map, as determined by the `extent` argument.
#'                    Default `FALSE`.
#'
#' @param stddev      Numeric vector of length 1 or number of agents describing
#'                    standard deviation of wrapped normal turn angles.
#'
#' @param lonlat      Logical. If `TRUE`, coordinates should be in degrees.
#'                    If `FALSE` coordinates represent planar ('Euclidean')
#'                    space (e.g. units of meters)
#' @param returnMatrix If `TRUE` then the return object will be a `matrix`. This will
#'                     be MUCH faster than retaining the `sp` or `SpatVector` class,
#'                     and thus will be much more effective for iterative `crw` calls
#'
#' @return A `SpatVector` points object with updated spatial position defined
#'         by a single occurrence of step length(s) and turn angle(s).
#'
#' @seealso [terra::distance()]
#'
#' @references Turchin, P. 1998. Quantitative analysis of movement: measuring and
#'             modeling population redistribution in animals and plants.
#'             Sinauer Associates, Sunderland, MA.
#'
#' @references McIntire, E. J. B., C. B. Schultz, and E. E. Crone. 2007.
#'             Designing a network for butterfly habitat restoration: where
#'             individuals, populations and landscapes interact.
#'             Journal of Applied Ecology 44:725-736.
#'
#' @author Eliot McIntire
#' @export
#' @importFrom stats rnorm
#' @rdname crw
#' @seealso [wrap()]
#' @examples
#' origDTThreads <- data.table::setDTthreads(2L)
#' origNcpus <- options(Ncpus = 2L)
#'
#' # using just matrix
#' N <- 10
#' xrange <- yrange <- c(-50, 50)
#' starts <- cbind(x = stats::runif(N, xrange[1], xrange[2]),
#'                 y = stats::runif(N, yrange[1], yrange[2]))
#' moved <- crw(starts, stepLength = 5, stddev = 10)
#' plot(starts, col = rainbow(10), pch = 19)
#' points(moved, col = rainbow(10))
#'
#' # as SpatVector
#' agent <- terra::vect(starts)
#' moved <- crw(agent, stepLength = 5, stddev = 10)
#' movedAgain <- crw(moved, stepLength = 5, stddev = 10)
#' terra::plot(agent)
#' terra::plot(moved, add = TRUE, col = "red")
#' terra::plot(movedAgain, add = TRUE, col = "green")
#'
#' # 1000x faster!! -- returnMatrix = TRUE
#' agentOrig <- agent
#' reps <- 1e2
#' system.time({
#'   for (i in 1:reps) agent <- crw(agent, stepLength = 5, stddev = 10, returnMatrix = TRUE)
#' })
#' agent <- agentOrig
#' system.time({
#'   for (i in 1:reps) agent <- crw(agent, stepLength = 5, stddev = 10)
#' })
#'
#' # as sp
#' if (requireNamespace("sp")) {
#'   agent <- sp::SpatialPoints(starts)
#'   spdf <- crw(agent, stepLength = 5, stddev = 10)
#'   spdfNew <- crw(spdf, stepLength = 5, stddev = 10)
#'   terra::plot(spdf, pch = 19)
#'   terra::points(spdfNew, col = "blue", pch = 19)
#' }
#'
#' # clean up
#' data.table::setDTthreads(origDTThreads)
#' options(Ncpus = origNcpus)
#'
crw <- function(agent, extent, stepLength, stddev, lonlat = FALSE, torus = FALSE,
                returnMatrix = FALSE) {
  crds <- coords(agent)
  xycolNames <- colnames(crds)

  if (!is.matrix(agent)) {
    if (!any(vapply(c("SpatialPoints", "SpatVector"), inherits, x = agent,
                    FUN.VALUE = logical(1)))) {
      if (is(agent, "SpatVector"))
        if (!identical("points", geomtype(agent)))
          stop("crs can only take SpatialPoints* or SpatVector points geometry")
    }

    if (isTRUE(returnMatrix) ) {
      agent <- if (NCOL(agent)) {
        cbind(crds, as.data.frame(agent))
      } else {
        crds
      }
      agent <- as.matrix(agent)
    }
  }

  # move current coordinates to previous coordinates
  oldCrds <- crds[, xycolNames, drop = FALSE]

  if (is.matrix(agent)) {
    hasNames <- colnames(agent) %in% x1y1colNames
    otherCols <- setdiff(colnames(agent), xycolNames)
  } else {
    hasNames <- names(agent) %in% x1y1colNames
    otherCols <- setdiff(names(agent), xycolNames)
  }

  needRandomX1Y1 <- if (sum(hasNames) < 2 ) TRUE else FALSE

  origClass <- class(agent)
  if (needRandomX1Y1) {
    n <- NROW(agent)
    # agent <- sp::SpatialPointsDataFrame(agent, data = data.frame(
    #   x1 = runif(n, -180, 180), y1 = runif(n, -180, 180)
    # ))
    x1 <- runif(n, -180, 180)
    y1 <- runif(n, -180, 180)
    prevCoords <- cbind(x1 = x1, y1 = y1)
  } else {
    prevCoords <- if (is.matrix(agent)) {
      cbind(x1 = agent@.Data[, "x1", drop = TRUE], y1 = agent@.Data[,"y1", drop = TRUE])
    } else {
      cbind(x1 = agent$x1, y1 = agent$y1)
    }

  }
  if (inherits(agent, "SpatialPoints"))  {
    message("agent does not have columns named x1 and y1, which represent the 'previous' ",
            "locations. Assigning random values to those columns.")
    agent1 <- cbind(coords(agent), prevCoords)
    agent1 <- crw(agent1, extent = extent, stepLength = stepLength,
                  stddev = stddev, lonlat = lonlat, torus = torus)

    if (returnMatrix %in% FALSE)
      if (!inherits(agent1, origClass)) {
        if (grepl(pattern = "SpatialPoints", origClass)) {
          if ("SpatialPoints" %in% origClass) {
            df <- as.data.frame(agent1[, otherCols])
            agent1 <- sp::SpatialPointsDataFrame(agent1[, xycolNames], data = df)
          } else {
            coords(agent) <- agent1[, xycolNames]
            agent@data[, otherCols] <- agent1[, otherCols]
          }
        }
      }
    return(agent1)
  }
  if (is.null(lonlat) || !is.logical(lonlat)) {
    stop("you must provide a 'lonlat' argument (TRUE/FALSE)")
  }

  n <- NROW(agent)

  agentHeading <- heading(cbind(x = prevCoords[, "x1", drop = FALSE],
                                y = prevCoords[, "y1", drop = FALSE]),
                          crds)
  rndDir <- rnorm(n, agentHeading, stddev)
  rndDir[rndDir > 180] <- rndDir[rndDir > 180] - 360
  rndDir[rndDir <= 180 & rndDir < (-180)] <- 360 + rndDir[rndDir <= 180 & rndDir < (-180)]

  if (needRandomX1Y1) {
    agent <- cbind(crds[, xycolNames, drop = FALSE],
                   x1 = oldCrds@.Data[, "x", drop = TRUE], y1 = oldCrds@.Data[, "y", drop = TRUE])
  } else {
    agent[, x1y1colNames] <- oldCrds
  }
  # update current coordinates to be those after the move
  newCoords <- cbind(
    x = crds[, 1] + sin(rad2(rndDir)) * stepLength,
    y = crds[, 2] + cos(rad2(rndDir)) * stepLength
  )

  # for Spatial -- this was used: agent@coords <-
  coords(agent) <- newCoords

  if (returnMatrix %in% FALSE)
    if ("SpatVector" %in% origClass) {

      if (!inherits(agent, "SpatVector"))
        agent <- terra::vect(agent[, xycolNames], atts = agent[, x1y1colNames])
    }
  if (torus) {
    return(wrap(X = agent, bounds = extent, withHeading = TRUE))
  } else {
    return(agent)
  }
}
