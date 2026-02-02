#'
#' @importFrom graphics rect
#'
#' @describeIn averageSP DTW barycenter averaging of snow profiles (low level worker function)
#' @param Avg  the initial average snow profile: either a [sarp.snowprofile::snowprofile] object or an index to an initial average profile in SPx
#' @param resamplingRate Resampling rate for a regular depth grid among the profiles
#' @param maxiter maximum number of iterations
#' @param plotChanges specify whether and how you want to plot the dba process: either `FALSE`, 'TRUE` == `'iterations'`, or `'averages+last'`
#' @param verbose print similarities between old and new average in between iterations?
#'
#'
#' @examples
#'
#' ## EXAMPLES OF dbaSP
#' ## either rescale profiles beforehand...
#' if (FALSE) {  # don't run in package check to save time
#'   SPx <- reScaleSampleSPx(SPgroup)$set          # rescale profiles
#'   SPx <- snowprofileSet(lapply(SPx, labelPWL))  # label PWLs
#'   DBA <- dbaSP(SPx, 5, plotChanges = TRUE)      # average profiles
#' }
#'
#' ## or use unscaled snow heights:
#' if (FALSE) {  # don't run in package check to save time
#'   SPx <- snowprofileSet(lapply(SPgroup, labelPWL))  # label PWLs
#'   DBA <- dbaSP(SPx, 5, plotChanges = TRUE)          # average profiles
#' }
#' @export
dbaSP <- function(SPx, Avg, sm = summary(SPx),
                  resamplingRate = 0.5, proportionPWL = 0.3,
                  maxiter = 10, breakAtSim = 0.99, breakAfter = 1,
                  plotChanges = FALSE, verbose = TRUE,
                  tz = "auto", ...) {


#   ____________________________________________________________________________________________________________________
#   Assertions & initial declarations                                                                               ####
  if (!is.snowprofileSet(SPx)) stop("SPx must be a snowprofileSet")
  if (any(c("rescale2refHS", "resamplingRate") %in% names(list(...)))) stop("You are not allowed to provide 'rescale2refHS' to 'dbaSP' or 'averageSP'. Refer to ?dbaSP for more details!")
  if (!all(sapply(SPx, function(sp) "layerOfInterest" %in% names(sp$layers)))) stop("Each profile layers object needs to contain labeled weak layers/layers of interest, see documentation!")
  nSP <- length(SPx)
  avgs <- list()
  sim <- double(length = maxiter)

  if (tz == "auto") {
    tzfac <- factor(sapply(SPx, function(p) attr(p$datetime, "tzone")))
    tz <- levels(tzfac)[median(as.numeric(tzfac))]
    if (tz %in% c("NULL", "NA", NA)) tz <- ""
  }

  ## set simType for calculation of sim between iterative avgs
  dots <- list(...)
  if ("simType" %in% names(dots)) simType <- dots$simType
  else simType <- formals(dtwSP)$simType

  ## resampling and setting first average profile
  hs_max <- max(sm$hs, na.rm = TRUE)
  if (hs_max < resamplingRate) stop("The maximum snow height in SPx is smaller than the resamplingRate.")
  DFgrid <- seq(resamplingRate, hs_max, resamplingRate)
  rowz <- length(DFgrid)
  SPx <- snowprofileSet(lapply(SPx, resampleSP, h = resamplingRate))

  if (is.snowprofile(Avg)) {
    Avg <- resampleSP(Avg, resamplingRate)
  } else if (is.numeric(Avg)) {
    iAvg <- Avg
    Avg <- SPx[[Avg]]
  } else {
    stop("Avg needs to be a snow profile object or an index of SPx")
  }

  ## define subfunction for computing individual alignments and
  ## returning data.frame of height == DFgrid, and columns hardness & gtype
  getWarpedLayers <- function(sp, Avg) {

    ## use matrices for easy allocation/subsetting; one matrix per data type
    DFchar <- matrix(as.character(NA), nrow = rowz, ncol = 4, dimnames = list(seq(rowz), c("station_id", "gtype", "ddate", "bdate")))
    DFnum_colnames <- c("height", "AvgLayerIndex", "thickness", "hardness", "gsize", "density", "temperature", "tsa", "rta",
                        "queryLayerIndex", "queryLayerHeight", "queryLayerDepth", "sim", "layerOfInterest", "p_unstable", "slab_rhogs")
    DFnum <- matrix(as.double(NA), nrow = rowz, ncol = length(DFnum_colnames),
                    dimnames = list(seq(rowz), DFnum_colnames))

    alignment <- tryCatch({dtwSP(sp, Avg, rescale2refHS = FALSE, resamplingRate = NA, ...)},
                          error = function(err) {return(data.frame(DFnum, DFchar))})

    warpedLayers <- alignment$queryWarped$layers

    k_insert_to <- which(DFgrid %in% warpedLayers$height)
    k_insert_from <- which(warpedLayers$height %in% DFgrid)
    DFnum[, "height"] <- DFgrid
    DFnum[k_insert_to, "AvgLayerIndex"] <- k_insert_to
    DFnum[k_insert_to, "thickness"] <- warpedLayers$thickness[k_insert_from]
    DFnum[k_insert_to, "hardness"] <- warpedLayers$hardness[k_insert_from]
    DFnum[k_insert_to, "queryLayerIndex"] <- warpedLayers$queryLayerIndex[k_insert_from]
    DFnum[k_insert_to, "queryLayerHeight"] <- alignment$query$layers$height[DFnum[k_insert_to, "queryLayerIndex"]]  # k_insert_to on the RHS is correct here!
    DFnum[k_insert_to, "queryLayerDepth"] <- alignment$query$layers$depth[DFnum[k_insert_to, "queryLayerIndex"]]  # k_insert_to on the RHS is correct here!
    DFnum[k_insert_to, "sim"] <- rep(alignment$sim, times = length(k_insert_to))
    DFnum[k_insert_to, "layerOfInterest"] <- warpedLayers$layerOfInterest[k_insert_from]

    DFchar[k_insert_to, "station_id"] <- rep(as.character(sp$station_id), times = length(k_insert_to))
    DFchar[k_insert_to, "gtype"] <- as.character(warpedLayers$gtype)[k_insert_from]
    if ("ddate" %in% names(warpedLayers)) DFchar[k_insert_to, "ddate"] <- as.character(warpedLayers$ddate[k_insert_from])
    if ("bdate" %in% names(warpedLayers)) DFchar[k_insert_to, "bdate"] <- as.character(warpedLayers$bdate[k_insert_from])
    if ("gsize" %in% names(warpedLayers)) DFnum[k_insert_to, "gsize"] <- warpedLayers$gsize[k_insert_from]
    if ("density" %in% names(warpedLayers)) DFnum[k_insert_to, "density"] <- warpedLayers$density[k_insert_from]
    if ("temperature" %in% names(warpedLayers)) DFnum[k_insert_to, "temperature"] <- warpedLayers$temperature[k_insert_from]
    if ("tsa" %in% names(warpedLayers)) DFnum[k_insert_to, "tsa"] <- warpedLayers$tsa[k_insert_from]
    if ("rta" %in% names(warpedLayers)) DFnum[k_insert_to, "rta"] <- warpedLayers$rta[k_insert_from]
    if ("p_unstable" %in% names(warpedLayers)) DFnum[k_insert_to, "p_unstable"] <- warpedLayers$p_unstable[k_insert_from]
    if ("slab_rhogs" %in% names(warpedLayers)) DFnum[k_insert_to, "slab_rhogs"] <- warpedLayers$slab_rhogs[k_insert_from]

    return(data.frame(DFnum, DFchar))
  }


  ## define subfunction for averaging over layer DF
  averageOverLayers <- function(lyr, DF) {
    ## following grain types exist within ensemble of layers at given height 'lyr'
    grains <- DF[DF$height == lyr, "gtype"]
    if (all(is.na(grains))) return(matrix(as.character(NA), 1, length(cnames)))
    ## prevent individual profiles that exceed median HS to increase the average's HS:
    if (length(grains) < nSP/2) return(matrix(as.character(NA), 1, length(cnames)))

    ## compute frequencies of gtypes and assign most prevalent gtype
    grainTable <- table(grains, useNA = "ifany")
    grainTable <- grainTable[order(grainTable, decreasing = TRUE)]
    gtype_mode <- names(which.max(grainTable))  # i.e., most prevalent grain type

    ## special rule to prioritize classified weak layers
    if (isTRUE(sum(DF[DF$height == lyr, "layerOfInterest"], na.rm = TRUE)/length(grains) >= proportionPWL)) {
      ## the proportion of (externally) tagged WLs is greater than the user-defined threshold proportionPWL
      ## --> (1) all classified weak layers will contribute to the averaged layer properties
      DFsubset <- DF[(DF$height == lyr) & (DF$layerOfInterest == TRUE), ]

      ## --> (2) the new gtype_mode is set to the gtype mode of the weak layer grain types
      wl_grainTable <- table(DFsubset$gtype, useNA = "ifany")
      wl_grainTable <- wl_grainTable[order(wl_grainTable, decreasing = TRUE)]
      gtype_mode <- names(which.max(wl_grainTable))  # i.e., most prevalent WL grain type

    } else {
      ## non-WL case: only layers of the gtype_mode will contribute to the averaged layer properties
      DFsubset <- DF[(DF$height == lyr) & (DF$gtype %in% gtype_mode), ]
    }

    ## compute aggregated layer properties from matched gtypes
    ## and put it into a one-row matrix
    matrix(c(
      as.character(round(median(as.double(DFsubset$queryLayerHeight), na.rm = TRUE), digits = 2)),# medianPredominantHeight
      as.character(round(median(as.double(DFsubset$queryLayerDepth), na.rm = TRUE), digits = 2)),# medianPredominantDepth
      gtype_mode,                                                                            # gtype
      as.character(round(nrow(DFsubset) / nSP, digits = 2)),                                 # distribution
      as.character(round(median(as.double(DFsubset$hardness), na.rm = TRUE), digits = 2)),   # hardness
      as.character(median(as.POSIXct(DFsubset$ddate, tz = tz), na.rm = TRUE)),               # ddate
      as.character(median(as.POSIXct(DFsubset$bdate, tz = tz), na.rm = TRUE)),               # bdate
      as.character(round(median(as.double(DFsubset$gsize), na.rm = TRUE), digits = 2)),      # gsize
      as.character(round(median(as.double(DFsubset$density), na.rm = TRUE), digits = 2)),    # density
      as.character(round(median(as.double(DFsubset$temperature), na.rm = TRUE), digits = 2)),# temperature
      as.character(round(median(as.double(DFsubset$tsa), na.rm = TRUE), digits = 2)),        # tsa
      as.character(round(median(as.double(DFsubset$rta), na.rm = TRUE), digits = 2)),        # rta
      as.character(round(median(as.double(DFsubset$p_unstable), na.rm = TRUE), digits = 2)), # p_unstable
      as.character(round(median(as.double(DFsubset$slab_rhogs), na.rm = TRUE), digits = 2)), # slab_rhogs
      as.character(round(length(which(DFsubset$p_unstable >= 0.77))/nrow(DFsubset), digits = 2)),  # ppu
      as.character(round(length(which(DF[DF$height == lyr, "p_unstable"] >= 0.77))/nrow(DF[DF$height == lyr, ]), digits = 2))  # ppu_all
    ), nrow = 1)

  }

#   ____________________________________________________________________________________________________________________
#   DBA iterations                                                                                                  ####
  i <- 1
  while (i <= maxiter) {

##  ....................................................................................................................
##  (1) compute alignments                                                                                          ####
    ## align every sp in SPx to Avg
    DFlist <- lapply(SPx, getWarpedLayers, Avg = Avg)
    ## name lists by SPx ID
    names(DFlist) <- seq(length(DFlist))

    ## combine into long data.frame
    DF <- data.table::rbindlist(DFlist, idcol = TRUE)
    DF <- DF[!is.na(DF[, "gtype"]) | !is.na(DF[, "hardness"]), ]  # discard rows of NA ifany
    DF[, ".id"] <- as.numeric(DF[, ".id"])

##  ....................................................................................................................
##  (2) update Avg                                                                                                  ####
    ## get the grain type frequencies for each layer &
    ## compute median over layer properties that correspond to the most prevalent grain type
    cnames <- c("medianPredominantHeight", "medianPredominantDepth", "gtype", "distribution", "hardness",
                "ddate", "bdate", "gsize", "density", "temperature", "tsa", "rta", "p_unstable", "slab_rhogs", "ppu", "ppu_all")
    avgLyrs <- lapply(DFgrid, averageOverLayers, DF = DF)
    avgLyrs <- as.data.frame(do.call("rbind", avgLyrs))
    colnames(avgLyrs) <- cnames

    ## manage NA layers
    drop <- which(is.na(avgLyrs$gtype) & is.na(avgLyrs$hardness))
    if (length(drop) != 0) {
      ## raise error in case of no single layer
      if (nrow(avgLyrs[-drop, ]) == 0) stop("New average profile contains no single layer")
      ## don't drop highest of basal NA layers
      search4ones <- diff(c(0, drop))
      if (search4ones[1] == 1) {
        oneTooHigh <- which(search4ones != 1)
        if (length(oneTooHigh) == 0) drop <- drop[-length(search4ones)]
        else drop <- drop[-(oneTooHigh-1)]
      }
    }

    ## compute old and new Avg of this iteration
    if (plotChanges == "averages+last") avgs[[i]] <- Avg
    Avg_old <- Avg

    if (length(drop) > 0) {
      Avg <- suppressWarnings(snowprofile(type = "aggregate", layers = snowprofileLayers(height = DFgrid[-drop], gtype = avgLyrs$gtype[-drop], hardness = as.double(avgLyrs$hardness[-drop]),
                                                                        ddate = as.POSIXct(avgLyrs$ddate[-drop], tz = tz), bdate = as.POSIXct(avgLyrs$bdate[-drop], tz = tz),
                                                                        gsize = as.double(avgLyrs$gsize[-drop]), density = as.double(avgLyrs$density[-drop]), temperature = as.double(avgLyrs$temperature[-drop]),
                                                                        tsa = as.double(avgLyrs$tsa[-drop]), rta = as.double(avgLyrs$rta[-drop]),
                                                                        p_unstable = as.double(avgLyrs$p_unstable[-drop]), slab_rhogs = as.double(avgLyrs$slab_rhogs[-drop]),
                                                                        ppu = as.double(avgLyrs$ppu[-drop]), ppu_all = as.double(avgLyrs$ppu_all[-drop]),
                                                                        distribution = as.double(avgLyrs$distribution[-drop]), medianPredominantHeight = as.double(avgLyrs$medianPredominantHeight[-drop]),
                                                                        medianPredominantDepth = as.double(avgLyrs$medianPredominantDepth[-drop])))
      )  # suppressWarnings due to un-defined layer property 'distribution'
    } else {
      Avg <- suppressWarnings(snowprofile(type = "aggregate", layers = snowprofileLayers(height = DFgrid, gtype = avgLyrs$gtype, hardness = as.double(avgLyrs$hardness),
                                                                                         ddate = as.POSIXct(avgLyrs$ddate, tz = tz), bdate = as.POSIXct(avgLyrs$bdate, tz = tz),
                                                                                         gsize = as.double(avgLyrs$gsize), density = as.double(avgLyrs$density), temperature = as.double(avgLyrs$temperature),
                                                                                         tsa = as.double(avgLyrs$tsa), rta = as.double(avgLyrs$rta),
                                                                                         p_unstable = as.double(avgLyrs$p_unstable), slab_rhogs = as.double(avgLyrs$slab_rhogs),
                                                                                         ppu = as.double(avgLyrs$ppu), ppu_all = as.double(avgLyrs$ppu_all),
                                                                                         distribution = as.double(avgLyrs$distribution), medianPredominantHeight = as.double(avgLyrs$medianPredominantHeight),
                                                                                         medianPredominantDepth = as.double(avgLyrs$medianPredominantDepth)))
      )
    }


##  ............................................................................
##  print iteration summary                                                 ####

    sim[i] <- simSP(Avg, Avg_old, type = simType)
    if (verbose) print(paste0("Iteration ", i, ": Similarity between old and new Avg = ", sim[i]))

    if (!identical(plotChanges, FALSE)) {
      ## pre-compute required plotting data
      ## get rectangle coordinates:
      xx <- DF[, ".id"]
      y2 <- DF[, "height"]
      y1 <- DF[, "height"] - DF[, "thickness"]
      ## colors
      cols <- getColoursGrainType(DF[, "gtype"])
      ## old settings
      par_old <- par()

      if (plotChanges == "iterations" || isTRUE(plotChanges)) {
        zones <- matrix(c(1, 2, 3, 4), nrow = 1)
        layout(zones, widths = c(1/5, 2/5, 1/5, 1/5))

        ## FIRST panel:
        plot(Avg_old, main = "Avg in", ymax = max(y2))

        ## SECOND panel: plot side by side:
        ## create plot
        plot((1:nSP)-0.5, rep(NA, nSP),
                              t = "n", xlim = c(0, max(xx)) + 0.5, ylim = c(0, max(y2)),
                              main = paste0("Iteration ", i),
                              xaxt = "n", xlab = "", ylab = "", las = 1)

        ## Draw profiles by creating individual layer rectangles
        rect(xleft = xx - 0.5, ybottom = y1, xright = xx + 0.5, ytop = y2, col = cols, border = NA)

        ## THIRD panel: plot hardness scatterplot:
        plot(jitter(DF$hardness, 1, 0), DF$height, axes = FALSE, pch = 3, cex = 0.3, xlab = "Hardness", ylab = "", xlim = c(1, 5), ylim = c(0, max(y2)))
        axis(1, at = 1:5, labels = c('F', '4F', '1F', 'P', 'K'))

        ## FOURTH panel
        plot(Avg, main = "Avg out", ymax = max(y2))

        ## restore settings
        par(mfrow = par_old$mfrow)
      }
    }  # END IF plotChanges


    ## increment index or break loop
    # if (isTRUE(all.equal(sim, 1))) break()  # break if sim is exactly 1
    if (i >= breakAfter && isTRUE(sum(sim[seq(i-breakAfter+1, i)] > breakAtSim) == breakAfter)) break()
    else i <- i + 1

  }  # END of DBA iterations: `while`


  ## plotChanges after iterations have finished
  if (plotChanges == "averages+last") {
    par(mfrow = c(1, 2), mar = c(5, 4, 4, 0) + 0.1)

    ## FIRST panel: averages
    avgs[[length(avgs) + 1]] <- Avg
    avgs <- snowprofileSet(avgs)
    plot(avgs, SortMethod = "unsorted", xticklabels = c(as.character(seq(i)), "result"),
         main = "Average profiles", xlab = "Iteration #",
         hardnessResidual = 0, ylim = c(0, max(y2)))

    ## SECOND panel: last iteration
    ## create plot
    par(mar = c(5, 4, 4, 2) + 0.1)
    plot((1:nSP)-0.5, rep(NA, nSP),
                          t = "n", xlim = c(0, max(xx)) + 0.5, ylim = c(0, max(y2)),
                          main = paste0("Warped set at last iteration (", i, ")"),
                          xlab = "", ylab = "", las = 1)

    ## Draw profiles by creating individual layer rectangles
    rect(xleft = xx - 0.5, ybottom = y1, xright = xx + 0.5, ytop = y2, col = cols, border = NA)

    ## restore settings
    par(mfrow = par_old$mfrow)
  }

  ## append matrix with indices that allow for backtracking of layer properties:
  Avg$backtrackingTable <- as.data.frame(DF[DF$height <= Avg$hs, c("height", "AvgLayerIndex", ".id", "queryLayerIndex", "queryLayerHeight")])
  colnames(Avg$backtrackingTable) <- c("height", "layerID", "queryID", "queryLayerID", "queryLayerHeight")

  ## append vector of sim between Avg and each sp
  sim2group <- DF[!is.na(DF$sim) & !duplicated(DF[, c("station_id", "sim")]), c("sim", "station_id")]
  Avg$sim2group <- sim2group$sim
  names(Avg$sim2group) <- sim2group$station_id
  ## append (r)mse which can be derived from sim2group
  Avg$mse <- mean((1 - Avg$sim2group)**2)
  Avg$rmse <- sqrt(Avg$mse)

  ## inherits class label
  OUT <- list(set = SPx, avg = Avg)
  class(OUT) <- append("avgSP", class(OUT))

  return(OUT)
}
