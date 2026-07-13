utils::globalVariables(c(
  ".GRP", "distance", "dups", "from", "i.size", "ind", "indClDT", "initialPixels", "keep",
  "n", "newQuantity", "numNeighs", "numRetries", "origIndex", "pixels", "proportion",
  "quantityAdj", "quantityAdj2", "state", "size", "tooBigByNCells", "V1"
))

################################################################################
#' Simulate a contagious spread process on a landscape, with `data.table` internals
#'
#' This can be used to simulate fires, seed dispersal, calculation of iterative,
#' concentric, symmetric (currently) landscape values and many other things.
#' Essentially, it starts from a collection of cells (`start`, called "events")
#' and spreads to neighbours, according to the `directions`
#' and `spreadProb` with modifications due to other arguments.
#' **NOTE:** [spread()] is similar, but sometimes slightly faster, but less
#' robust, and more difficult to use iteratively.
#'
#' There are 2 main underlying algorithms for active cells to "spread" to
#' nearby cells (adjacent cells): `spreadProb` and `neighProb`.
#' Using `spreadProb`, every "active" pixel will assess all
#' neighbours (either 4 or 8, depending on  `directions`), and will "activate"
#' whichever neighbours successfully pass independent calls to
#' `runif(1,0,1) < spreadProb`.
#' The algorithm will iterate again and again, each time starting from the newly
#' "activated" cells. Several built-in decisions are as follows:
#'
#' 1. no active cell can activate a cell that was already activated by
#'    the same event (i.e., "it won't go backwards");
#' 2. If `allowOverlap` is `FALSE`, then the previous rule will also apply,
#'    regardless of which "event" caused the pixels to be previously active.
#'
#' This function can be interrupted before all active cells are exhausted if
#' the `iterations` value is reached before there are no more active
#' cells to `spread2` into. The interrupted output (a `data.table`) can be passed
#' subsequently as an input to this same function (as `start`).
#' This is intended to be used for situations where external events happen during
#' a `spread2` event, or where one or more arguments to the `spread2` function
#' change before a `spread2` event is completed.
#' For example, if it is desired that the `spreadProb` change before a
#' `spread2` event is completed because, for example, a fire is spreading, and a
#' new set of conditions arise due to a change in weather.
#'
#' `asymmetry` here is slightly different than in the `spread` function,
#' so that it can deal with a `RasterLayer` of `asymmetryAngle`.
#' Here, the `spreadProb` values of a given set of neighbours around each active pixel
#' are adjusted to create `adjustedSpreadProb` which is calculated maintain the
#' following two qualities: \deqn{mean(spreadProb) = mean(ajustedSpreadProb)} and
#' \deqn{max(spreadProb)/min(spreadProb) = asymmetry} along the axis of
#' `asymmetryAngle`. NOTE: this means that the 8 neighbours around an active
#' cell may not fulfill the preceeding equality if `asymmetryAngle` is not
#' exactly one of the 8 angles of the 8 neighbours. This means that
#' \deqn{max(spreadProb)/min(spreadProb)} will generally be less than
#' `asymmetry`, for the 8 neighbours. The exact adjustment to the spreadProb
#' is calculated with:
#' \deqn{angleQuality <- (cos(angles - rad2(asymmetryAngle))+1)/2}
#' which is multiplied to get an angle-adjusted spreadProb:
#' \deqn{spreadProbAdj <- actualSpreadProb * angleQuality}
#' which is then rescaled:
#' \deqn{adjustedSpreadProb = (spreadProbAdj - min(spreadProbAdj)) * par2 + par1},
#' where `par1` and `par2` are parameters calculated internally to make the 2 conditions above true.
#'
#' @section Breaking out of `spread2` events:
#'
#' There are 3 ways for the `spread2` to "stop" spreading.
#' Here, each "event" is defined as all cells that are spawned from each unique
#' `start` location.
#' The ways outlined below are all acting at all times, i.e., they are not
#' mutually exclusive.
#' Therefore, it is the user's responsibility to make sure the different rules
#' are interacting with each other correctly.
#'
#' \tabular{ll}{
#'   `spreadProb` \tab Probabilistically, if `spreadProb` is low enough,
#'                          active spreading events will stop.
#'                          In practice, this number generally should be below 0.3
#'                          to actually see an event stop.\cr
#'   `maxSize` \tab This is the number of cells that are "successfully" turned
#'                       on during a spreading event. `spreadProb` will still
#'                       be active, so, it is possible that the end size of each event
#'                       is smaller than `maxSize`, but they will not be greater
#'                       than `maxSize`\cr
#'   `exactSize` \tab This is the number of cells that are "successfully" turned
#'                       on during a spreading event. This will override an event that
#'                       stops probabilistically via `spreadProb`, but forcing
#'                       its last set of active cells to try again to find neighbours.
#'                       It will try `maxRetriesPerID` times per event, before giving up.
#'                       During those `maxRetriesPerID` times, it will try to "jump" up to
#'                       4 cells outwards from each of the active cells, every 5 retries.\cr
#'   `iterations` \tab This is a hard cap on the number of internal iterations to
#'                          complete before returning the current state of the system
#'                          as a `data.table`.\cr
#' }
#'
#' @param landscape Required. A `RasterLayer` object. This defines the possible
#'                  locations for spreading events to start and `spread2` into. Required.
#'
#' @param start Required. Either a vector of pixel numbers to initiate spreading, or a
#'              `data.table` that is the output of a previous `spread2`.
#'              If a vector, they should be cell indices (pixels) on the `landscape`.
#'              If user has x and y coordinates, these can be converted with
#'              [`cellFromXY()`][terra::cellFromXY].
#'
#' @param spreadProb  Numeric of length 1 or length `ncell(landscape)` or
#'                    a `RasterLayer` that is the identical dimensions as
#'                    `landscape`.
#'                    If numeric of length 1, then this is the global (absolute)
#'                    probability of spreading into each cell from a neighbour.
#'                    If a numeric of length `ncell(landscape)` or a raster,
#'                    then this must be the cell-specific (absolute)
#'                    probability of a "receiving" potential cell. Default is `0.23`.
#'                    If relative probabilities are required, use `spreadProbRel`.
#'                    If used together, then the relative probabilities will be
#'                    re-scaled so that the mean relative probability of potential
#'                    neighbours is equal to the mean of `spreadProb` of
#'                    the potential neighbours.
#'
#' @param persistProb Numeric of length 1 or `RasterLayer`.
#'                    If numeric of length 1, then this is the global (absolute)
#'                    probability of cell continuing to burn per time step.
#'                    If a raster, then this must be the cell-specific (absolute)
#'                    probability of a fire persisting.
#'                    Default is `NA`, which is the same as 0, i.e. a cell only burns
#'                    for one time step.
#'
#' @param spreadProbRel Optional `RasterLayer` indicating a surface of relative
#'                      probabilities useful when using `neighProbs` (which
#'                      provides a mechanism for selecting a specific number of
#'                      cells at each iteration).
#'                      This indicates the relative probabilities for the selection
#'                      of successful neighbours.
#'                      `spreadProb` will still be evaluated *after*
#'                      the relative probabilities and `neighProbs` has been
#'                      evaluated, i.e., potential cells will be identified, then
#'                      some could be rejected via `spreadProb`.
#'                      If absolute `spreadProb` is not desired,
#'                      *be sure to set* `spreadProb = 1`.
#'                      Ignored if `neighProbs` is not provided.
#'
#' @param asRaster Logical, length 1. If `TRUE`, the function will return a `Raster`
#'                 where raster non NA values indicate the cells that were "active", and the
#'                 value is the initial starting pixel.
#'
#' @param maxSize  Numeric. Maximum number of cells for a single or all events to be `spread2`.
#'                 Recycled to match `start` length, if it is not as long as `start`.
#'                 This will be overridden if `exactSize` also provided.
#'                 See section on 'Breaking out of `spread2` events'.
#'
#' @param exactSize Numeric vector, length 1 or `length(start)`.
#'                  Similar to `maxSize`, but these will be the exact
#'                  final sizes of the events.  i.e., the `spread2` events
#'                  will continue until they are `floor(exactSize)`.
#'                  This will override `maxSize` if both provided.
#'                  See Details.
#'
#' @param directions    The number adjacent cells in which to look;
#'                      default is 8 (Queen case). Can only be 4 or 8.
#'
#' @param iterations    Number of iterations to `spread2`.
#'                      Leaving this `NULL` allows the `spread2` to continue
#'                      until stops spreading itself (i.e., exhausts itself).
#'
#' @param returnDistances Logical. Should the function include a column with the
#'                      individual cell distances from the locus where that event
#'                      started. Default is FALSE. See Details.
#'
#' @param returnDirections Logical. Should the function include a column with the
#'                      individual directions (in radians) from the locus where
#'                      that event started. Default is FALSE.
#'
#' @param returnFrom Logical. Should the function return a column with the
#'                      source, i.e, the lag 1 "from" pixel, for each iteration.
#'
#' @param circle  Logical. If TRUE, then outward `spread2` will be by equidistant rings,
#'                rather than solely by adjacent cells (via `directions` arg.).
#'                Default is `FALSE`.
#'                Using `circle = TRUE` can be dramatically slower for large problems.
#'                Note, this will likely create unexpected results if `spreadProb < 1`.
#'
#' @param skipChecks Logical. If TRUE, the argument checking (i.e., assertions) will be
#'              skipped. This should likely only be used once it is clear that the function
#'              arguments are well understood and function speed is of the primary importance.
#'              This is likely most useful in repeated iteration cases i.e., if this call
#'              is using the previous output from this same function.
#'
#' @param neighProbs An optional numeric vector, whose sum is 1.
#'                   It indicates the probabilities that an individual spread iteration
#'                   will spread to `1, 2, ..., length(neighProbs)` neighbours, respectively.
#'                   If this is used (i.e., something other than `NA`), `circle` and
#'                   `returnDistances` will not work currently.
#' @param maxRetriesPerID Only active if `exactSize` is used. This is the number of attempts
#'                        that will be made per event ID, before abandoning, therefore completing
#'                        the `spread2` for that event with a size that is smaller than
#'                        `exactSize`. Default 10 times.
#'
#' @param asymmetry     A numeric or `RasterLayer` indicating the ratio of the
#'                      asymmetry to be used. i.e., 1 is no asymmetry; 2 means that the
#'                      angles in the direction of the `asymmetryAngle` are 2x the
#'                      `spreadProb`
#'                      of the angles opposite tot he `asymmetryAngle`  Default is
#'                      NA, indicating no asymmetry. See details. This is still experimental.
#'                      Use with caution.
#'
#' @param asymmetryAngle A numeric or `RasterLayer` indicating the angle in degrees
#'                      (0 is "up", as in North on a map),
#'                      that describes which way the `asymmetry` is.
#' @param allowOverlap `numeric` (`logical` will work for backwards compatibility).
#'                     See details.  Default is 0, i.e., no overlapping.
#'
#' @param oneNeighbourOnly Logical. Default is `FALSE`. If `TRUE`, then this
#'                         spread algorithm will allow exactly one neighbour to be
#'                         spread to (not fewer or more). This could be used, e.g.,
#'                         for an animal moving. If this is `TRUE`, then `allowOverlap`
#'                         will be set to `2` if it is `0` or `1`.
#'
#' @param plot.it If `TRUE`, then plot the raster at every iteration,
#'                so one can watch the `spread2` event grow.
#'
#' @details
#'
#' If `maxSize` or `exactSize` are used, then spreading will continue and stop
#' before or at `maxSize` or at `exactSize`, respectively.
#' If `iterations` is specified, then the function will end, and the returned `data.table`
#' may (if `maxSize`) or will (if `exactSize`) have at least one active
#' cell per event that did not already achieve `maxSize` or `exactSize`.
#' This will be very useful to build new, customized higher-level wrapper functions that
#' iteratively call `spread2`.
#'
#' @note
#' `exactSize` may not be achieved if there aren't enough cells in the map.
#' Also, `exactSize` may not be achieved because the active cells are "stuck",
#' i.e., they have no inactivated cells to move to; or the `spreadProb` is low.
#' In the latter two cases, the algorithm will retry again, but it will only
#' retry from the last iteration's active cells.
#' The algorithm will only retry 10 times before quitting.
#' Currently, there will also be an attempt to "jump" up to four cells away from
#' the active cells to try to continue spreading.
#'
#' A common way to use this function is to build wrappers around this, followed
#' by iterative calls in a `while` loop. See example.
#'
#' @section Building custom spreading events:
#'
#' This function can be used iteratively, with relatively little overhead compared to using
#' it non-iteratively. In general, this function can be called with arguments set as user
#' needs, and with specifying e.g., `iterations = 1`. This means that the function will spread
#' outwards 1 iteration, then stop. The returned object will be a `data.table` or
#' `RasterLayer` that can be passed immediately back as the start argument into a subsequent
#' call to `spread2`. This means that every argument can be updated at each iteration.
#'
#' When using this function iteratively, there are several things to keep in mind.
#' The output will likely be sorted differently than the input (i.e., the
#' order of start, if a vector, may not be the same order as that returned).
#' This means that when passing the same object back into the next iteration of the
#' function call, `maxSize` or `exactSize` may not be in the same order.
#' To get the same order, the easiest thing to do is sort the initial `start`
#' objects by their pixel location, increasing.
#' Then, of course, sorting any vectorized arguments (e.g., `maxSize`) accordingly.
#'
#' **NOTE**: the `data.table` or `RasterLayer` should not be altered
#' when passed back into `spread2`.
#'
#' @section `allowOverlap`:
#' If `1` (or `TRUE`),
#'  then individual events can overlap with one another, i.e., allow
#'  overlap between events. If `2` (or `NA`), then each pixel
#'  is essentially independent, allowing overlap between and within
#'  events. This likely requires a user to intervene as it is possible
#'  to spread back onto itself. If `3` (did not exist previously),
#'  individual events can overlap, and there can be overlap within an
#'  event, but only within an iteration, i.e., once an iteration is
#'  finished, and a pixel was activated, then the spreading will not
#'  return onto these pixels. If `0` (or `FALSE`), then once a
#'  pixel is activated, it cannot be re-activated, within or between event.
#'  This allows events to not interfere with one another i.e.,
#'  they do not interact (this is slower than if
#'  `allowOverlap = FALSE`). Default is 0. In the case of 2 or 3,
#'  this would be, perhaps, useful for dispersal of,
#'  say, insect swarms.
#'
#' @return
#' Either a `data.table` (`asRaster=FALSE`) or a `RasterLayer`
#' (`asRaster=TRUE`, the default).
#' The `data.table` will have one attribute named `spreadState`, which
#' is a list containing a `data.table` of current cluster-level information
#' about the spread events.
#' If `asRaster=TRUE`, then the `data.table` (with its `spreadState`
#' attribute) will be attached to the `Raster` as an attribute named `pixel` as it
#' provides pixel-level information about the spread events.
#'
#' The `RasterLayer` represents every cell in which a successful `spread2` event occurred.
#' For the case of, say, a fire this would represent every cell that burned.
#' If `allowOverlap` is `TRUE`, the return will always be a `data.table`.
#'
#' If `asRaster` is `FALSE`, then this function returns a
#' `data.table` with 3 (or 4 if `returnFrom` is `TRUE`) columns:
#'
#' \tabular{ll}{
#'   `initialPixels` \tab the initial cell number of that particular
#'                            `spread2` event.\cr
#'   `pixels` \tab The cell indices of cells that have
#'                        been touched by the `spread2` algorithm.\cr
#'   `state` \tab a logical indicating whether the cell is active (i.e.,
#'                        could still be a source for spreading) or not (no
#'                        spreading will occur from these cells).\cr
#'   `from` \tab The pixel indices that were the immediately preceding
#'                    "source" for each `pixels`, i.e., the lag 1 pixels.
#'                    Only returned if `returnFrom` is `TRUE` \cr
#' }
#'
#' The attribute saved with the name `"spreadState"` (e.g., `attr(output, "spreadState")`)
#' includes a `data.table` with columns:
#' \tabular{ll}{
#'   `id` \tab An arbitrary code, from 1 to `length(start)` for each "event".\cr
#'   `initialPixels` \tab the initial cell number of that particular
#'                            `spread2` event.\cr
#'   `numRetries` \tab The number of re-starts the event did because it got
#'                          stuck (normally only because `exactSize` was used
#'                          and was not achieved.\cr
#'   `maxSize` \tab The number of pixels that were provided as inputs via
#'                      `maxSize` or `exactSize`.\cr
#'   `size` \tab The current size, in pixels, of each event.\cr
#' }
#' and several other objects that provide significant speed ups in iterative calls to
#' `spread2`. If the user runs `spread2` iteratively, there will likely be significant
#' speed gains if the `data.table` passed in to `start` should have the attribute
#' attached, or re-attached if it was lost, e.g., via
#' `setattr(outInput, "spreadState", attr(out, "spreadState"))`, where `out` is the
#' returned `data.table` from the previous call to `spread2`, and `outInput` is
#' the modified `data.table`. Currently, the modified `data.table` **must** have the
#' same order as `out`.
#'
#' @author Eliot McIntire and Steve Cumming
#' @export
#' @importFrom checkmate assert assertClass assertMultiClass assertNumeric
#' @importFrom checkmate checkClass checkDataTable checkLogical checkNumeric checkScalarNA
#' @importFrom checkmate qassert checkMultiClass
#' @importFrom data.table := alloc.col as.data.table copy data.table is.data.table
#' @importFrom data.table rbindlist set setattr setcolorder setkeyv setnames uniqueN
#' @importFrom fpCompare %<=% %>>%
#' @importFrom terra ncell res ncol distance
#' @importFrom stats runif
#'
#' @seealso [spread()] for a different implementation of the same algorithm.
#' `spread` is less robust but it is often slightly faster.
#'
#' @example inst/examples/example_spread2.R
#'
spread2 <- function(landscape, start = ncell(landscape) / 2 - ncol(landscape) / 2,
                    spreadProb = 0.23, persistProb = NA_real_, asRaster = TRUE,
                    maxSize, exactSize, directions = 8L, iterations = 1e6L,
                    returnDistances = FALSE, returnDirections = FALSE,
                    returnFrom = FALSE, maxRetriesPerID = 10,
                    spreadProbRel = NA_real_, plot.it = FALSE, circle = FALSE,
                    asymmetry = NA_real_, asymmetryAngle = NA_real_,
                    allowOverlap = 0, neighProbs = NA_real_, oneNeighbourOnly = FALSE,
                    skipChecks = FALSE) {

  #### assertions ###############
  checkmate::assertMultiClass(landscape, c("Raster", "SpatRaster"))
  fmatch2 <- if (requireNamespace("fastmatch", quietly = TRUE)) fastmatch::fmatch else base::match
  landscapeOrigClass <- is(landscape)
  ncells <- ncell(landscape)
  numCols <- ncol(landscape)
  anyNAneighProbs <- any(is.na(neighProbs))
  if (!skipChecks) {
    assert(
      checkNumeric(start, min.len = 0, max.len = ncells, lower = 1, upper = ncells),
      checkMultiClass(start, c("Raster", "SpatRaster")),
      checkDataTable(start))

    qassert(neighProbs, "n[0,1]")
    assertNumeric(sum(neighProbs), lower = 1, upper = 1)

    # if (!inherits(spreadProb, "Raster") && !inherits(spreadProb, "SpatRaster")) {
    assert(# this is "or"
      checkNumeric(spreadProb, 0, 1, min.len = 1, max.len = ncell(landscape)),
      checkNumeric(spreadProb, 0, 1, min.len = 1, max.len = 1),
      checkClass(spreadProb, "Raster"),
      checkClass(spreadProb, "SpatRaster")
    )
    # }

    if (is(spreadProb, "Raster") || is(spreadProb, "SpatRaster")) {
      if (!terra::inMemory(spreadProb)) {
        warning("spreadProb is a raster layer stored on disk. This may cause spread2 to be",
                " very slow. We suggest extracting the values to a numeric vector first, ",
                "then passing this to spreadProb")
      }
    }
    assert(
      checkNumeric(persistProb, 0, 1, min.len = 1, max.len = 1),
      checkMultiClass(persistProb,  c("RasterLayer", "SpatRaster"))
    )
    assert(
      checkMultiClass(spreadProbRel, c("RasterLayer", "SpatRaster")),
      checkScalarNA(spreadProbRel) ## needs to be checked second; will fail if SpatRaster
    )
    assert(
      checkNumeric(asymmetry, 0, Inf, min.len = 1, max.len = 1),
      checkMultiClass(asymmetry, c("RasterLayer", "SpatRaster"))
    )
    assert(
      checkNumeric(asymmetryAngle, 0, 360, min.len = 1, max.len = 1),
      checkMultiClass(asymmetryAngle,  c("RasterLayer", "SpatRaster"))
    )
    qassert(directions, "N1[4,8]")
    qassert(iterations, "N1[0,Inf]")
    qassert(circle, "B")

    if (!missing(maxSize)) {
      if (is.data.table(start)) {
        n <- uniqueN(start, by = "initialPixels")
        assert(
          checkNumeric(maxSize, min.len = 1, max.len = 1),
          checkNumeric(maxSize, min.len = n, max.len = n)
        )
      } else {
        assert(
          checkNumeric(maxSize, min.len = 1, max.len = 1),
          checkNumeric(maxSize, min.len = NROW(start), max.len = NROW(start))
        )
      }
    }
    if (!missing(exactSize)) {
      if (is.data.table(start)) {
        n <- uniqueN(start, by = "initialPixels")
        assert(
          checkNumeric(exactSize, min.len = 1, max.len = 1),
          checkNumeric(exactSize, min.len = n, max.len = n)
        )
      } else {
        assert(
          checkNumeric(exactSize, min.len = 1, max.len = 1),
          checkNumeric(exactSize, min.len = NROW(start), max.len = NROW(start))
        )
      }
    }
  }
  ##### End assertions

  # Step 0 - set up objects -- main ones: dt, clusterDT -- get them from attributes
  ## on start or initiate them
  smallRaster <- ncells < 4e7 # should use bit vector (RAM)
  canUseAvailable <- !(isTRUE(allowOverlap > 0) | is.na(allowOverlap))
  if (missing(maxSize)) {
    maxSize <- NA_real_
  }

  if (missing(exactSize)) {
    exactSize <- NA_real_
  } else {
    maxSize <- exactSize
  }

  # returnDistances = TRUE and circle = TRUE both require distance calculations
  needDistance <- returnDistances | circle | returnDirections
  usingAsymmetry <- !is.na(asymmetry)
  asymmetryAngleNeedSubset <- (inherits(asymmetryAngle, "Raster") ||
                                 inherits(asymmetryAngle, "SpatRaster")) &&
    NROW(asymmetryAngle) != 1 # length was previously used, but has different meaning for SpatRaster & Raster

  # This means that if an event can not spread any more, it will try 10 times, incl. 2 jumps
  # maxRetriesPerID <- 10

  if (!is.numeric(start) && !is.data.table(start)) {
    if (is(start, "Raster") || is(start, "SpatRaster")) {
      start <- attr(start, "pixel")
    } else {
      stop("Start must be either a vector of pixels, a data.table from",
           "previous spread2 or a Raster from a previous spread2")
    }
  }

  if (!is.data.table(start)) {
    # A "new" entry into spread2 -- need to set up stuff
    if (canUseAvailable) {
      notAvailable <- if (requireNamespace("bit", quietly = TRUE)) {
        bit::bit(ncells)
      } else {
        logical(ncells)
      }
      notAvailable[start] <- TRUE
    }

    start <- as.integer(start)

    whActive <- seq_along(start)
    whInactive <- integer()
    if (any(duplicated(start)))
      stop("start has duplicates; duplicates are not currently allowed; ",
           "if that behaviour is desired, perhaps run this function multiple ",
           "times with duplicates separated? ")
    dt <- data.table(initialPixels = start)

    if (returnFrom) {
      set(dt, NULL, "from", NA_integer_)
    }
    set(dt, NULL, "pixels", start)
    set(dt, NULL, "state", "activeSource")

    clusterDT <- data.table(id = whActive, initialPixels = start,
                            numRetries = 0L, size = as.integer(iterations > 0))

    if (!anyNA(maxSize)) {
      set(clusterDT, NULL, "maxSize", maxSize)

      # de-activate ones that are 1 cell
      set(dt, which(clusterDT$maxSize == 1), "state", "inactive")
    }
    if (!anyNA(exactSize)) {
      set(clusterDT, NULL, "exactSize", TRUE)
    }

    setkeyv(clusterDT, "initialPixels")
    if (needDistance) set(dt, NULL, "distance", 0) # it is zero distance to self
    if (usingAsymmetry) {
      set(dt, NULL, "effectiveDistance", 0) # it is zero distance to self
      set(dt, NULL, "distClass", 0) # it is zero distance to self
    }
    totalIterations <- 0
  } else {
    # a "return" entry into spread2
    dt <- start
    if (!is.null(attr(start, "spreadState"))) {
      ## as.data.table is necessary to use `set` to add new columns.
      clusterDT <- as.data.table(attr(start, "spreadState")$clusterDT)

      ## make sure maxSize column exists when maxSize argument is passed.
      if (!anyNA(maxSize) && is.null(clusterDT$maxSize)) {
        message("maxSize provided, but not present in attr(start, 'spreadState')$maxSize. ",
                "Using the maxSize provided: ", maxSize)
        set(clusterDT, NULL, "maxSize", maxSize)
      }

      if (!key(clusterDT) == "initialPixels")
        # should have key if it came directly from output of spread2
        setkeyv(clusterDT, "initialPixels")
      if (!anyNA(maxSize)) {
        if (any(maxSize != clusterDT$maxSize)) {
          sizeType <- if (!anyNA(exactSize)) "exactSize" else "maxSize"
          message(
            sizeType, " provided. ",
            "It does not match with size attr(start, 'spreadState')$maxSize. ",
            "Using the new ", sizeType, " provided. Perhaps sorted differently?",
            "Try sorting initial call to spread2 so that pixel number of start ",
            "cells is strictly increasing")
          clusterDT$maxSize <- maxSize
        }
      }
      if (any(colnames(clusterDT) == "maxSize")) maxSize <- clusterDT$maxSize
      whActive <- attr(start, "spreadState")$whActive
      whInactive <- attr(start, "spreadState")$whInactive
      totalIterations <- attr(start, "spreadState")$totalIterations
      if (canUseAvailable) {
        notAvailable <- attr(start, "spreadState")$notAvailable
      }
    } else {
      # case where user has deleted the attributes
      whActive <- which(start$state == "activeSource")
      whInactive <- which(start$state == "inactive")
      canUseAvailable <- FALSE # not worth it if it has to be remade each time
      totalIterations <- if (needDistance) max(start$distance) else 0
      unIP <- unique(dt$initialPixels)
      clusterDT <- data.table(id = seq_along(unIP), initialPixels = unIP, numRetries = 0L)
      if (!anyNA(maxSize)) {
        set(clusterDT, NULL, "maxSize", maxSize)
        if (!anyNA(exactSize)) {
          set(clusterDT, NULL, "exactSize", TRUE)
        }
        set(clusterDT, NULL, "size", dt[, .N, by = "initialPixels"]$N)
        setkeyv(clusterDT, "initialPixels")
      }
    }
  }

  whTooSmall <- integer()
  dtPotentialColNames <- c("id", "from", "to", "state", "distance"[needDistance],
                           "effectiveDistance"[usingAsymmetry])

  if (isTRUE(oneNeighbourOnly)) {
    if (allowOverlap %in% 0:1) {
      message("oneNeighbourOnly is TRUE; allowOverlap was ", allowOverlap, " which is not allowed; ",
              "setting allowOverlap to 2")
      allowOverlap <- 2
    }
  }
  # start at iteration 0, note: totalIterations is also maintained,
  # which persists during iterative calls to spread2
  its <- 0

  # Main loop -- continue if active and still below iterations & none is too
  # small (and doesn't have any active cells)
  while (length(whTooSmall) || (length(whActive) && its < iterations)) {
    # Step 1
    # Get neighbours, either via adj (default) or cir (jumping if stuck)
    if (length(whTooSmall) > 0) {
      # cir
      ## get slightly further neighbours
      dtRetry <- dt[whTooSmall]
      set(dtRetry, NULL, "state", NULL)
      whNeedJump <- which(((clusterDT$numRetries + 1) %% 10) == 0)
      if (length(whNeedJump)) {
        # jump every 10, starting at 20
        resCur <- res(landscape)[1]
        dtRetryJump <- dtRetry[clusterDT[whNeedJump], nomatch = 0]
        fromPixels <- dtRetryJump$pixels
        dtPotential <- lapply(seq_along(fromPixels), function(fp) {
          cbind(id = fp, cir(landscape, loci = fromPixels[fp],
                             includeBehavior = "excludePixels",
                             minRadius = resCur,
                             maxRadius = 20 * resCur)[, "indices"]) # 20 pixels
        }) |>
          do.call(what = rbind)

        dtPotential <- matrix(as.integer(dtPotential), ncol = 2)
        colnames(dtPotential) <- c("id", "to")
        dtPotentialJump <- cbind(from = fromPixels[dtPotential[, "id"]],
                                 to = dtPotential[, "to"],
                                 id = dtRetryJump$initialPixels[dtPotential[, "id"]])
        dtRetry <- dtRetry[!clusterDT[whNeedJump]] # remove jumped neighbours
      }
      ## get adjacent neighbours
      dtPotential <- adj(
        directions = directions,
        numCell = ncells,
        numCol = numCols,
        id = dtRetry$initialPixels,
        cells = dtRetry$pixels, cutoff.for.data.table = 5e2,
        returnDT = TRUE
      )

      if (exists("dtPotentialJump")) {
        if (is.data.table(dtPotential)) {
          dtPotential <- rbindlist(list(dtPotential, as.data.table(dtPotentialJump)))
        } else {
          dtPotential <- rbind(dtPotential, dtPotentialJump)
        }
        rm("dtPotentialJump")
      }

      set(dt, whActive, "state", "holding") # take them out of commission for this iteration
      set(dt, whTooSmall, "state", "activeSource")
      whTooSmall <- integer()
    } else {
      # adj
      ## Spread to immediate neighbours
      dtPotential <- adj(
        numCell = ncells,
        numCol = numCols,
        directions = directions,
        id = dt$initialPixels[whActive],
        cells = dt$pixels[whActive], cutoff.for.data.table = 5e2,
        returnDT = TRUE
      )

      # only iterate if it is not a Retry situation
      its <- its + 1
      totalIterations <- totalIterations + 1
    }

    # Step 2 - Randomize order

    # randomize row order so duplicates are not always in same place
    i <- sample.int(NROW(dtPotential))
    if (!is.data.table(dtPotential)) {
      dtPotential <- as.data.table(dtPotential)
    }
    for (x in colnames(dtPotential)) set(dtPotential, NULL, x, dtPotential[[x]][i])

    # Step 3 -- if required -- calculate distances, if required ... attach to dt
    if (needDistance) {
      fromPts <- xyFromCell(landscape, dtPotential$id)
      toPts <- xyFromCell(landscape, dtPotential$to)
      dists <- terra::distance(fromPts, toPts, pairwise=TRUE, lonlat = FALSE)
      if (isTRUE(returnDirections))
        dirs <- .pointDirection(fromPts, toPts)
      if (usingAsymmetry) {
        actualAsymmetry <- if (length(asymmetry) == 1) {
          asymmetry
        } else {
          asymmetry[dtPotential$to]
        }
        actualAsymmetryAngle <- if (!asymmetryAngleNeedSubset) {
          asymmetryAngle
        } else {
          asymmetryAngle[dtPotential$to]
        }

        angleQualities <- angleQuality(from = dtPotential$id, to = dtPotential$to,
                                       landscape, actualAsymmetryAngle)
        naAQ <- is.na(angleQualities[, "angleQuality"])
        angleQualities[naAQ, "angleQuality"] <- 1
        # convert dists to effective distance
        effDists <- dists * ((2 - angleQualities[, "angleQuality"]) / 2 *
                               (actualAsymmetry - 1) + 1)

        # For asymmetry, we also may want to know what proportion of the outward spreading
        #  event will hit each pixel, not just the effectiveDistance
        lociHere <- if (is.numeric(start)) start else
          attributes(dt)$spreadState$clusterDT$initialPixels
        # pureCircle <- cir(landscape,
        #                   loci = lociHere,
        #                   allowOverlap = TRUE, allowDuplicates = FALSE,
        #                   maxRadius = totalIterations,
        #                   minRadius = totalIterations - 0.999999,
        #                   returnIndices = TRUE,
        #                   returnDistances = TRUE,
        #                   includeBehavior = "excludePixels")


        # This is a very fast version with allowOverlap = TRUE, allowDuplicates = FALSE,
        #   returnIndices = TRUE, returnDistances = TRUE, and includeBehavior = "excludePixels"
        pureCircle <- .cirSpecialQuick(landscape,
                                       loci = lociHere,
                                       maxRadius = totalIterations,
                                       minRadius = totalIterations - 0.999999)
        pureCircle <- cbind(pureCircle[, c("id", "indices", "dists"), drop = FALSE],
                            distClass = ceiling(pureCircle[, "dists"]))
        colnames(pureCircle)[2] <- c("to")
        theoreticalAngleQualities <- angleQuality(pureCircle[, "id", drop = FALSE],
                                                  pureCircle[, "to", drop = FALSE],
                                                  landscape,
                                                  actualAsymmetryAngle = actualAsymmetryAngle)
        naAQ <- is.na(theoreticalAngleQualities[, "angleQuality"])
        theoreticalAngleQualities[naAQ, "angleQuality"] <- 1
        # convert dists to effective distance
        effDists1 <- pureCircle[, "dists"] *
          ((2 - theoreticalAngleQualities[, "angleQuality"]) / 2 * (actualAsymmetry - 1) + 1)

        pc <- pureCircle[, "dists"] / effDists1
        pureCircle <- cbind(pureCircle, proportion = pc)
        pureCircle <- as.data.table(pureCircle)
        pureCircle[, proportion := proportion / sum(proportion), by = "id"]
        set(pureCircle, NULL, "dists", NULL)
        setkeyv(pureCircle, c("id", "to"))
        pureCirclePrev <- attr(dt, "spreadState")$pureCircle
        if (!is.null(pureCirclePrev)) {
          pureCircle <- rbindlist(list(pureCircle, pureCirclePrev),
                                  use.names = FALSE, fill = FALSE)
          #pureCircle <- unique(pureCircle)
        }
      }

      if (circle) {
        if (usingAsymmetry) {
          distKeepers <- effDists %<=% totalIterations & effDists %>>%
            (totalIterations - 1)
          dtPotentialAsymmetry <- dtPotential[!distKeepers]
          if (sum(distKeepers) == 0) {
            # all failed
            set(dt, NULL, "state", "successful")
          } else {
            unDTPotAssym <- unique(dtPotentialAsymmetry$from)
            if (length(unDTPotAssym) == length(unique(dt$pixel))) {
              set(dt, NULL, "state", "successful")
            } else {
              dt[pixels %in% unDTPotAssym, state := "successful"]
            }
          }
        } else {
          distKeepers <- dists %<=% (totalIterations * res(landscape)[1]) &
            dists %>>% ((totalIterations - 1) * res(landscape)[1])
        }

        dtPotentialAllNeighs <- copy(dtPotential)
        setkeyv(dtPotentialAllNeighs, "from")
        dtPotential <- dtPotential[distKeepers]
        dists <- dists[distKeepers]
        if (isTRUE(returnDirections))
          dirs <- dirs[distKeepers, , drop = FALSE]
      }

      set(dtPotential, NULL, "distance", dists)
      if (isTRUE(returnDirections))
        set(dtPotential, NULL, "direction", dirs[, "angles"])

      if (usingAsymmetry) {
        set(dtPotential, NULL, "effectiveDistance", effDists[distKeepers])
        if (circle) {
          dtPotential <- dtPotential[pureCircle, nomatch = 0, on = c("id", "to")][
            , proportion := proportion / .N, by = c("id", "to")]
        }
      }
    }

    # Step 4 -- assign "successful" to all dtPotentials --
    set(dtPotential, NULL, "state", "successful")

    # Step 5 -- optional -- Algorithm neighProbs - uses a specific number of neighbours
    if (!anyNAneighProbs) {
      # numNeighs algorithm
      numNeighsByPixel <- unique(dtPotential, by = c("id", "from"))
      if (is.list(neighProbs)) {
        if (NROW(numNeighsByPixel) != length(neighProbs)) {
          neighProbs1 <- neighProbs[match(numNeighsByPixel$from,
                                          start[state == "activeSource"]$pixels)]
        } else {
          neighProbs1 <- neighProbs
        }
        set(numNeighsByPixel, NULL, "numNeighs", unlist(lapply(
          neighProbs1, function(np) {
            sample.int(size = 1, n = length(np), replace = TRUE, prob = np)
          }))
        )
      } else {
        set(numNeighsByPixel, NULL, "numNeighs",
            sample.int(size = NROW(numNeighsByPixel), n = length(neighProbs),
                       replace = TRUE, prob = neighProbs))
      }
      setkeyv(numNeighsByPixel, c("id", "from"))

      # remove duplicates within dtPotential
      dupsWithinDtPotential <- duplicatedInt(dtPotential$to)
      successCells <- dtPotential$to[!dupsWithinDtPotential] # remove the dupsWithinDtPotential
      potentialNotAvailable <- notAvailable[successCells]
      whNoDupsCurItAndAll <- seq_along(dtPotential$to)[!dupsWithinDtPotential][
        !potentialNotAvailable]
      dtPotential <- dtPotential[whNoDupsCurItAndAll]
      setkeyv(dtPotential, c("id", "from")) # sort so it is the same as numNeighsByPixel

      if (NROW(dtPotential)) {
        if (is(spreadProbRel, "RasterLayer") || is(spreadProbRel, "SpatRaster")) {
          set(dtPotential, NULL, "spreadProbRel", spreadProbRel[][dtPotential$to])
        } else {
          set(dtPotential, NULL, "spreadProbRel", 1)
        }
        spreadProbNA <- is.na(dtPotential$spreadProbRel) # This is where a mask enters
        if (any(spreadProbNA)) {
          dtPotential <- dtPotential[!spreadProbNA]
          # code below is a possible replacement for previous line -- faster for small problems
          # colnamesDtPot <- colnames(dtPotential)
          # ll <-  lapply(colnamesDtPot, function(x) dtPotential[[x]][!spreadProbNA])
          # names(ll) <- colnamesDtPot
          # dtPotential <- as.data.table(ll)
        }
        # might be zero length because of spreadProb NAs
        if (NROW(dtPotential)) {
          # If it is a corner or has had pixels removed bc of duplicates,
          # it may not have enough neighbours
          set(numNeighsByPixel, NULL, c("to", "state"), NULL)
          dt1 <- dtPotential[numNeighsByPixel, nomatch = 0]
          dtPotential <- dt1[dt1[, .I[sample.int(.N, size = min(.N, numNeighs), prob = spreadProbRel)], by = c("id", "from")]$V1]

          if (FALSE) { # old algorithm, replaced by 3 lines above May 30 2019, Eliot -- appears to be a bug below
            #   the by = "from" should be c("id", "from") -- should sample 1 or more from each fire event, from each front line
              numNeighsByPixel <- numNeighsByPixel[dtPotential[, .N, by = c("id", "from")]]
              if (any(numNeighsByPixel$numNeighs > numNeighsByPixel$N))
                set(numNeighsByPixel, NULL, "numNeighs",
                    pmin(numNeighsByPixel$N, numNeighsByPixel$numNeighs, na.rm = TRUE))
              dtPotential <- dtPotential[numNeighsByPixel[dtPotential][
              , .I[sample.int(length(numNeighs), size = numNeighs, prob = spreadProbRel)],
              by = "from"]$V1]
          }
        }
        set(dtPotential, NULL, "spreadProbRel", NULL)
      }
    } # end of neighProbs -- should now have only dtPotentials that match number neighbours req'd

    # Step 6 -- spreadProb implementation - uses an absolute probability for
    # each potential neighbour
    # Extract spreadProb for the current set of potentials
    if (length(spreadProb) == 1 && !inherits(spreadProb, "SpatRaster")) {
      actualSpreadProb <- rep(spreadProb, NROW(dtPotential))
    } else {
      actualSpreadProb <- as.vector(spreadProb)[dtPotential$to]
      # remove NA values that may come from a spreadProb raster
      NAaSP <- !is.na(actualSpreadProb)
      if (any(NAaSP)) {
        if (!all(NAaSP)) {
        dtPotential <- dtPotential[NAaSP, ]
        actualSpreadProb <- actualSpreadProb[NAaSP]
      }
      }
    }

    # Step 6a -- asymmetry -- this will modify spreadProb if it is not a circle
    #  -- circle asymmetry happens elsewhere
    # modify actualSpreadProb if there is asymmetry
    if (usingAsymmetry && !circle) {
      actualAsymmetry <- if (length(asymmetry) == 1) {
        asymmetry
      } else {
        asymmetry[dtPotential$to]
      }

      actualAsymmetryAngle <- if (asymmetryAngleNeedSubset) {
        asymmetryAngle[][dtPotential$to]
      } else {
        asymmetryAngle
      }

      angleQualities <- angleQuality(from = dtPotential$id, to = dtPotential$to,
                                     landscape, actualAsymmetryAngle)

      naAQ <- is.na(angleQualities[, "angleQuality"])
      angleQualities[naAQ, "angleQuality"] <- actualSpreadProb[naAQ]

      actualSpreadProb <- asymmetryAdjust(angleQualities, actualSpreadProb, actualAsymmetry)
    }

    # Step 7 <- calculate spread success based on actualSpreadProb
    if (isTRUE(oneNeighbourOnly)) {
      set(dtPotential, NULL, "actualSpreadProb", actualSpreadProb)
      randoms <- runifC(length(unique(dtPotential$from)))
      dtPotential[, keep := {
        cumProb <- cumsum(actualSpreadProb) / sum(actualSpreadProb)
        draw <- randoms[.GRP]
        .I[min(which(draw <= cumProb))]},
        by = "from"]
      spreadProbSuccess <- rep(FALSE, NROW(dtPotential))
      spreadProbSuccess[dtPotential$keep] <- TRUE
      set(dtPotential, NULL, c("keep", "actualSpreadProb"), NULL)

    } else {
      randVars <- runifC(NROW(dtPotential))
      spreadProbSuccess <- randVars <= actualSpreadProb
    }

    # Step 8 - Remove duplicates & bind dt and dtPotential
    if (anyNAneighProbs) {
      if (isTRUE(allowOverlap > 0) || is.na(allowOverlap) || !canUseAvailable) {
        ## overlapping allowed
        dtPotential <- dtPotential[spreadProbSuccess]
        dtNROW <- NROW(dt)
        dt <- rbindlistDtDtpot(dt, dtPotential, returnFrom, needDistance, dtPotentialColNames)

        ## this is to prevent overlap within an event...
        ## in some cases, overlap within event is desired, so skip this block
        if (!is.na(allowOverlap) && (any(allowOverlap %in% c(1, 3) ) || isTRUE(allowOverlap))) {
          if (identical(allowOverlap, 1) || isTRUE(allowOverlap)) {
            dt[, `:=`(dups = duplicatedInt(pixels)), by = "initialPixels"]
            # } else {
            #   # dt[, dups := {
            #   #   successes <- state == "successful"
            #   #   c(rep(FALSE, length.out = sum(!successes)),
            #   #     pixels[successes] %in% pixels[!successes])
            #   # },
            #   # by = "initialPixels"]
            #
            #   set(dt, NULL, "successes", dt$state == "successful")
            #   dt[, dups := {
            #     c(rep(FALSE, length.out = sum(!successes)),
            #       pixels[successes] %in% pixels[!successes])
            #   },
            #   by = "initialPixels"]
            #   if (any(dt$dups)) browser()
            #
            #   set(dt, NULL, "successes", NULL)
            # }
            dupes <- dt$dups
            set(dt, NULL, "dups", NULL)
            dt <- dt[!dupes]
          }
        }

        ## remove all the duplicated ones from dtPotential
        dtPotential <- dt[-seq_len(dtNROW)]
      } else {
        # no overlapping allowed

        # This block is instead of a dt[!duplicated(pixels)] which becomes very
        # slow on large problems
        successCells <- dtPotential$to[spreadProbSuccess]
        dupsWithinDtPotential <- duplicatedInt(successCells)

        #successCells <- na.omit(successCells[!dupsWithinDtPotential]) # remove the dupsWithinDtPotential
        successCells <- successCells[!dupsWithinDtPotential] # remove the dupsWithinDtPotential
        potentialNotAvailable <- notAvailable[successCells]

        # 3 reasons why potentials are not selected
        whSuccNoDupsCurItAndAll <- seq_along(spreadProbSuccess)[spreadProbSuccess][
          !dupsWithinDtPotential][!potentialNotAvailable]

        # next line is a fix with data.table 1.11.4 or so, can't pass length 0 vector?
        if (length(successCells[!potentialNotAvailable]) > 0)
          notAvailable[successCells[!potentialNotAvailable]] <- TRUE
        dtPotential <- dtPotential[whSuccNoDupsCurItAndAll]

        dt <- rbindlistDtDtpot(dt, dtPotential, returnFrom, needDistance, dtPotentialColNames)

        if (circle) {
          if (usingAsymmetry) {
            saturated <- dtPotentialAllNeighs[, sum(to %in% dt$pixels) == directions,
                                              by = from][V1 == TRUE]$from
          }
        }
      }
    } else {
      # neighProbs -- duplication checking already happened, but
      dtPotential <- dtPotential[spreadProbSuccess]
      dt <- rbindlistDtDtpot(dt, dtPotential, returnFrom, needDistance, dtPotentialColNames)
      if (NROW(dtPotential)) notAvailable[dtPotential$pixels] <- TRUE
    }

    # Step 9 -- Size issues: i.e., if too big (remove extras) or too small (make sure keeps going)
    if (!anyNA(maxSize) || !(anyNA(exactSize))) {
      # Too big first
      setkeyv(dt, "initialPixels") # must sort because maxSize is sorted
      setkeyv(dtPotential, "initialPixels")
      dtPotClusterDT <- dtPotential[, list(size = as.integer(.N)), by = "initialPixels"]
      clusterDT[dtPotClusterDT, size := size + i.size]

      # This next line is a work around for a problem that doesn't make sense:
      # See: https://stackoverflow.com/q/29615181/1380598
      alloc.col(clusterDT, 7)
      set(clusterDT, NULL, "tooBigByNCells", clusterDT$size - as.integer(clusterDT$maxSize))

      currentSizetooBigByNCells <- clusterDT[tooBigByNCells > 0]
      if (NROW(currentSizetooBigByNCells) > 0) {
        # sort them so join works between dt1 and currentSizetooBigByNCells
        setkeyv(currentSizetooBigByNCells, "initialPixels")
        set(dt, NULL, "origIndex", seq_len(NROW(dt)))
        dt1 <- dt[state == "successful"]
        dt1b <- dt1[currentSizetooBigByNCells] # attach tooBigByNCells
        dt1a <- dt1b[, list(omit = origIndex[sample.int(.N, tooBigByNCells[1])]), by = "initialPixels"]

        dt <- dt[-dt1a$omit][, list(initialPixels, pixels, state)]
        dt[dt1a, state := "inactive"]

        clusterDT[currentSizetooBigByNCells[, list(initialPixels)], size := size - tooBigByNCells]
      }

      # Too small second
      if (!(anyNA(exactSize))) {
        # push those that are too small into "tooSmall"
        currentSizeTooSmall <- clusterDT[tooBigByNCells < 0, "initialPixels"]
        # dtOrig <- copy(dt)
        # csts <- copy(currentSizeTooSmall)
        #dt <- copy(dtOrig)
        #currentSizeTooSmall <- copy(csts)
        if (NROW(currentSizeTooSmall) > 0) {
          # successful means will become activeSource next iteration,
          # so they don't need any special treatment
          currentSizeTooSmall <- currentSizeTooSmall[
            !dt[dt$state %in% c("successful", "holding"), nomatch = 0]
          ]

        }
        # if the ones that are too small are unsuccessful, make them "tooSmall"
        set(dt, NULL, "ind", seq_len(NROW(dt)))
        whTooSmall <- dt[!(dt$state %in% c("successful", "inactive"))][
          currentSizeTooSmall, nomatch = 0]$ind
        set(dt, NULL, "ind", NULL)

        if (length(whTooSmall)) {
          # add index column -- like doing a 'which( )'
          set(clusterDT, NULL, "indClDT", seq_len(NROW(clusterDT)))
          whNeedRetryClusterDT <- clusterDT[dt[whTooSmall]]$indClDT
          set(clusterDT, NULL, "indClDT", NULL)
          tooManyRetries <- clusterDT[whNeedRetryClusterDT, numRetries > maxRetriesPerID]
          if (sum(tooManyRetries) > 0) {
            whNeedRetryClusterDT <- whNeedRetryClusterDT[!tooManyRetries]
            whTooSmall <- whTooSmall[!tooManyRetries]
          }
          set(dt, whTooSmall, "state", "tooSmall")
          set(clusterDT, whNeedRetryClusterDT, "numRetries",
              clusterDT$numRetries[whNeedRetryClusterDT] + 1L)
        }
      }
      set(clusterDT, NULL, "tooBigByNCells", NULL)
    } # end size-based assessments

    # Step 10 - Change states of cells
    if (usingAsymmetry) {
      if (!(isTRUE(allowOverlap > 0) || is.na(allowOverlap))) {
        if (circle) {
          if (length(saturated)) {
            set(dt, which(dt$pixels %in% saturated), "state", "activeSource")
          }
        }
      }
    }

    # Step 10a - Persistence: starting fire pixels (activeSource) continue burning
    # with a persistence probability, becoming "successful" and then
    # "activeSources" in Step 10b
    # at the moment, this is skipped if persistence is left = 0 to avoid
    # breaking some tests

    ## Extract persistenceProb for the current set of source pixels
    if (length(persistProb) == 1 && (!is(persistProb, "Raster") && !is(persistProb, "SpatRaster"))) {
      if (is.na(persistProb)) {
        actualPersistProb <- NULL
      } else {
        actualPersistProb <- rep(persistProb, sum(dt$state == "activeSource"))
      }
    } else {
      actualPersistProb <- persistProb[][dt[state == "activeSource", initialPixels]]
    }

    ## "activeSource" fires become "successful" depending on prob of persistence
    if (!is.null(actualPersistProb)) {
      startFires <- which(dt$state == "activeSource")
      persistingFires <- runifC(length(startFires)) <= actualPersistProb
      dt[startFires[persistingFires], state := "successful"]
    }

    # Step 10b convert previous states to new states
    notInactive <- dt$state != "inactive" # currently activeSource, successful, or holding
    whNotInactive <- which(notInactive)
    activeStates <- dt$state[whNotInactive]
    whActive <- whNotInactive[activeStates == "successful" | activeStates == "holding"]
    whInactive <- whNotInactive[activeStates == "activeSource"]

    #   activeSource ==> inactive
    #   holding ==> activeSource
    #   successful ==> activeSource
    #   tooSmall ==> tooSmall
    set(dt, whNotInactive, "state",
        c("inactive", "activeSource", "activeSource", "tooSmall")[
          fmatch2(activeStates, c("activeSource", "holding", "successful", "tooSmall"))])

    # Step 11 - plot it if necessary
    if (plot.it) {
      newPlot <- FALSE
      if (totalIterations == 1) {
        newPlot <- TRUE
      }
      if (newPlot || !(exists("spread2Ras", inherits = FALSE))) {
        if (any(landscapeOrigClass == "Raster"))
          spread2Ras <- raster::raster(landscape)
        else
          spread2Ras <- terra::rast(landscape)
      }
      if (returnDistances) {
        spread2Ras[dt$pixels] <- dt$distance
        newPlot <- TRUE # need to rescale legend each time
      } else {
        set(dt, NULL, "order", seq_along(dt$initialPixels))
        setkeyv(dt, "initialPixels")
        spread2Ras[dt$pixels] <- dt[clusterDT]$id # get id column from clusterDT
        setkeyv(dt, "order")
        set(dt, NULL, "order", NULL)
      }
      terra::plot(spread2Ras, add = !newPlot)
    }
  } # end of main loop

  # Step 12 -- Add attributes
  attrList <- list(clusterDT = clusterDT,
                   whActive = whActive,
                   whInactive = whInactive,
                   totalIterations = totalIterations)
  if (canUseAvailable) {
    attrList <- append(attrList, list(notAvailable = notAvailable))
  }

  if (usingAsymmetry) {
    if (exists("pureCircle", inherits = FALSE))
      attrList <- append(attrList, list(pureCircle = pureCircle))
  }
  setattr(dt, "spreadState", attrList)

  # Step 13 -- return either raster or data.table
  if (asRaster) {
    if (any(landscapeOrigClass == "Raster"))
      ras <- raster::raster(landscape)
    else
      ras <- terra::rast(landscape)
    # ras <- raster(landscape)
    # inside unit tests, this raster gives warnings if it is only NAs
    suppressWarnings(ras[dt$pixels] <- clusterDT[dt]$id)
    setattr(ras, "pixel", dt)
    return(ras)
  }

  return(dt)
}

#' Internal helpers
#'
#' Not for users.
#' A function to `setnames` and `rbindlist` that is used in `spread2`.
#'
#' @param dt a `data.table` object
#' @param dtPotential a `data.table` object
#' @param returnFrom logical
#' @param needDistance logical
#' @param dtPotentialColNames character vector.
#'
#' @keywords internal
#' @rdname spread2-internals
rbindlistDtDtpot <- function(dt, dtPotential, returnFrom, needDistance, dtPotentialColNames) {
  # distance column is second last, but needs to be last
  # to merge with dt, need: from, to, state in that order
  reorderColsWDistance(needDistance, dtPotential, dtPotentialColNames)

  if (!returnFrom) {
    set(dtPotential, NULL, "from", dtPotential$id)
    set(dtPotential, NULL, "id", NULL)
    setnames(dtPotential, old = c("from", "to"), new = c("initialPixels", "pixels"))
  } else {
    setnames(dtPotential, old = c("id", "to"), new = c("initialPixels", "pixels"))
  }

  # convert state of all those still left, move potentialPixels into pixels column
  if (NROW(dtPotential)) {
    # need fill = TRUE if user has passed extra columns
    dt <- rbindlist(list(dt, dtPotential), fill = TRUE)
  }

  return(dt)
}

#' Internal helpers for `spread2`
#'
#' @keywords internal
#' @rdname spread2-internals
reorderColsWDistance <- function(needDistance, dtPotential, dtPotentialColNames) {
  if (needDistance)
    setcolorder(dtPotential,
                neworder = c(
                  dtPotentialColNames[(dtPotentialColNames %in% colnames(dtPotential))],
                  colnames(dtPotential)[!(colnames(dtPotential) %in% dtPotentialColNames)]
                )
    )
}

#' @param from vector of cell locations which are the "from" or starting cells
#' @param to vector of same length as `from` which are the "to" or receiving cells
#' @param landscape `RasterLayer` passed from `spread2`.
#' @param actualAsymmetryAngle Angle in degrees, either a vector length 1 or vector
#'                             `NROW(dtPotential)`.
#'
#' @keywords internal
#' @rdname spread2-internals
angleQuality <- function(from, to, landscape, actualAsymmetryAngle) {
  from1 <- cbind(id = from, xyFromCell(landscape, cell = as.vector(from)))
  to1 <- cbind(id = from, xyFromCell(landscape, cell = as.vector(to)))
  d <- .pointDirection(from = from1, to = to1)

  angleQuality <- cbind(angleQuality = (cos(d[, "angles"] - rad2(actualAsymmetryAngle)) + 1), d)
  angleQuality
}

#' @param angleQualities Matrix. The output from `angleQuality`
#' @param quantity Variable of interest to adjust, e.g., `spreadProb`
#' @param actualAsymmetry Asymmetry intensity. Derived from `asymmetry` arg in `spread2`
#'
#' @keywords internal
#' @rdname spread2-internals
asymmetryAdjust <- function(angleQualities, quantity, actualAsymmetry) {
  if (sum(angleQualities[, "angleQuality"]) %==% 0) {
    # the case where there is no difference in the angles, and they are all zero
    return(quantity)
  } else {
    dd <- data.table(angleQualities, quantity)
    dd[, quantityAdj := quantity * angleQualities[, "angleQuality"]]
    dd[, quantityAdj2 := quantityAdj / (mean(quantityAdj) / mean(quantity)), by = "id"]

    dd[, newQuantity := {
      minQuantity <- 0
      maxQuantity <- max(2 * quantity)
      aaMinus1 <- actualAsymmetry - 1
      par2 <- aaMinus1 * sum(quantityAdj) /
        (length(quantityAdj) * (maxQuantity - minQuantity) +
           aaMinus1 * sum(quantityAdj - minQuantity))
      par1 <- par2 / aaMinus1 * (maxQuantity - minQuantity)
      (quantityAdj2 - minQuantity) * par2 + par1
    }, by = "id"]
  }
  dd$newQuantity
}
