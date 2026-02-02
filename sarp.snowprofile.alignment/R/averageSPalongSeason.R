#' Compute a seasonal timeseries of an average snowprofile
#'
#' This routine computes the seasonal timeseries of the average snow profile for a given region/set of profiles. The total snow height of the
#' seasonal average profile closely follows the *median snow height* represented by the group of profiles each day. Also the
#' new snow amounts represent the *median new snow amounts* within the group (i.e., PP and DF grains). The routine maintains
#' temporal consistency by using the previous day average profile as initial condition to derive the next day's. This creates the need for re-scaling
#' the layer thicknesses each day to account for snow settlement and melting. Two different re-scaling approaches have been implemented,
#' which both aim to re-scale the old snow part of the column (i.e., the snow which was on the ground already at the previous day).
#' See parameter description for more details. Also note, that the routine can be started at any day of the season by providing
#' an average profile from the previous day. The routine modifies several parameters, which are passed on to [dtwSP]. These
#' parameters differ from the defaults specified in [dtwSP], which are held very generic, whereas the application in this function
#' is much more specific to certain requirements and algorithm behavior. For more details, refer to the reference paper.
#'
#' Computing the seasonal average profile for an entire season and about 100 grid points (with a max of 150 cm snow depth) takes roughly 60 mins.
#'
#' @importFrom stats filter
#'
#' @param SPx a [sarp.snowprofile::snowprofileSet] that contains all profiles from the region to be averaged at all days of the season for which you want to compute the average profile.
#' Identically to [dbaSP], weak layers need to be labeled prior to this function call, see [dbaSP] and [sarp.snowprofile::labelPWL]. Note that only daily sampling is
#' allowed at this point (i.e., one profile per grid point per day).
#' @param sm a summary of `SPx` containing meta-data
#' @param AvgDayBefore an average [sarp.snowprofile::snowprofile] from the previous day. This is only necessary if you want to resume the computation
#' mid season.
#' @param DateEnd an end date character string (`"YYYY-MM-DD"`) if you only want to compute the timeseries up to a certain point
#' in time. Defaults to the future-most date contained in the meta-data object `sm`.
#' @param keep.profiles Do you want to keep the (resampled) individual snow profiles from `SPx` in your return object? **Note**
#' that this must be `TRUE` if you plan to [backtrackLayers] to derive any kind of summary statistics for the averaged layers.
#' See Notes below, and examples of how to conveniently [backtrackLayers].
#' @param progressbar display a progress bar during computation?
#' @param dailyRescaling choose between two settlement rescaling approaches. `settleEntireOldSnow` re-scales the entire old snow
#' column so that the average snow height represents the median snow height from the profile set. `settleTopOldSnow` (the default)
#' re-scales the upper part of the old snow column to achieve the same goal. While the former mostly leads to buried layers being
#' settled to too deep snow depths, the default approach aims to leave those buried layers unchanged, which are located at depths
#' that represent the median depths of their aligned layers.
#' @inheritParams averageSP
#' @param top.down a [dtwSP] parameter, which needs to be set to `FALSE` to ensure correct growing of the snowpack during snowfall.
#' @param checkGlobalAlignment a [dtwSP] parameter, which needs to be set to `FALSE` analogous to `top.down`
#' @param prefLayerWeights a [dtwSP] parameter. Might be best to set this to `NA`, but can potentially be set to
#' `layerWeightingMat(FALSE)` *in case of* averaging a very large geographic region with temporal lags between weather events.
#' @param dims a [dtwSP] parameter, which is modified to include deposition date alignments per default
#' @param weights a [dtwSP] parameter that sets the according weights to the `dims` specified above.
#' @param ... any other parameters passed on to [dbaSP] and then [dtwSP].
#'
#' @return A list of class `avgSP_timeseries` containing the fields `$avgs` with a [sarp.snowprofile::snowprofileSet] of the average profiles at each day.
#' If `keep.profiles == TRUE` a field `$sets` with the according profiles informing the average profile at each day (which can be
#' used to [backtrackLayers] to compute summary statistics of the averaged layers). And two fields `$call` and `$meta`. The
#' latter contains several useful meta-information such as `...$date`, `...$hs`, `...$hs_median`, `...$thicknessPPDF_median`, or `...$rmse`, which gauges
#' the representativity of the average profile (the closer to `0`, the better; the closer to `1`, the worse).
#'
#' @note
#'  - If you don't provide an AvgDayBefore, it will be computed with [averageSP] and *default* parameters
#'  (dots won't be passed to initializing the first average profile)!
#'  - Even though [backtrackLayers] allows for backtracking layers based on height, it is not recommended to try and
#'  backtrack layers if `keep.profiles = FALSE`, since profiles that can't be aligned to the average profile (`$avgs[[i]]`)
#'  are being discarded from the profile set at that day (`$sets[[i]]`), which changes `queryID`s in the backtrackingTable.
#'  Conclusion: If you want to backtrack layers from the seasonal average profile, you *must* `keep.profiles = TRUE`. See examples!
#'
#' @author fherla
#' @references Herla, F., Haegeli, P., and Mair, P. (2022). A data exploration tool for averaging and accessing large data sets of snow stratigraphy profiles useful for avalanche forecasting,
#' The Cryosphere, 16(8), 3149–3162, https://doi.org/10.5194/tc-16-3149-2022
#'
#' @seealso [dbaSP], [averageSP], [sarp.snowprofile::labelPWL]
#' @examples
#'
#' run_the_examples <- FALSE  # exclude long-running examples
#' if (run_the_examples) {
#'
#' ## compute average timeseries for simplistic example data set 'SPspacetime'
#' ## first: label weak layers (you can choose your own rules and thresholds!)
#' SPspacetime <- snowprofileSet(lapply(SPspacetime, function(sp) {
#'  labelPWL(sp, pwl_gtype = c("SH", "DH", "FC", "FCxr"), threshold_RTA = 0.8)
#' }))  # label weak layers in each profile of the profile set 'SPspacetime'
#'
#' ## second: average along several days
#' avgTS <- averageSPalongSeason(SPspacetime)
#'
#' ## explore resulting object
#' names(avgTS)
#'
#' # timeseries figure
#' plot(avgTS$avgs, main = "average time series")
#' # add line representing median snow height
#' lines(avgTS$meta$date, avgTS$meta$hs_median)
#' # add line representing median new snow amounts
#' lines(avgTS$meta$date, avgTS$meta$hs - avgTS$meta$thicknessPPDF_median, lty = 'dashed')
#'
#' # individual profile sets from one day
#' plot(avgTS$sets[[1]], SortMethod = "hs", main = "individual profiles from first day")
#'
#'
#' ## backtrack individual layers of the average profile...
#' individualLayers <- backtrackLayers(avgProfile = avgTS$avgs[[1]],
#'                       profileSet = avgTS$sets[[1]],
#'                       layer = findPWL(avgTS$avgs[[1]], pwl_gtype = c("SH", "DH"),
#'                                       pwl_date = "2018-10-17", threshold_RTA = 0.8))
#' ## ... to retrieve summary statistics or distributions, e.g. stability distribution
#' hist(individualLayers[[1]]$rta)
#' hist(individualLayers[[1]]$depth)
#'
#' ## see the Vignette about averaging profiles for more examples!
#'
#'
#' }
#'
#' @export
averageSPalongSeason <- function(SPx,
                                 sm = summary(SPx),
                                 AvgDayBefore = NULL,
                                 DateEnd = max(sm$date),
                                 keep.profiles = TRUE,
                                 progressbar = requireNamespace("progress", quietly = TRUE),
                                 dailyRescaling = c("settleTopOldSnow", "settleEntireOldSnow")[1],
                                 proportionPWL = 0.3, breakAtSim = 0.9, breakAfter = 2, verbose = FALSE, resamplingRate = 0.5,
                                 top.down = FALSE, checkGlobalAlignment = FALSE, prefLayerWeights = NA,
                                 dims = c("gtype", "hardness", "ddate"), weights = c(0.375, 0.125, 0.5),
                                 ...) {

  ##--- Initializations ----
  if (!is.snowprofileSet(SPx)) stop("SPx needs to be a snowprofileSet containing several locations at multiple days of the season")
  nSP <- nrow(sm)
  if (!all(sapply(SPx[1:min(10, nSP)], function(sp) "layerOfInterest" %in% names(sp$layers)))) stop("Each profile layers object needs to contain labeled weak layers/layers of interest, see documentation!")
  #                   ^-- only test first few profiles to save time, dbaSP enforces this check for each profile at a later point anyways

  medianHSatDate <- sapply(unique(sm$date), function(d) median(sm$hs[sm$date == d]))
  names(medianHSatDate) <- unique(sm$date)
  ## initial average profile
  if (is.null(AvgDayBefore)) {
    day <- min(unique(sm$date)[which(medianHSatDate > 2*resamplingRate)])
    initialize <- TRUE
  } else if (is.snowprofile(AvgDayBefore)) {
    if (is.na(AvgDayBefore$date)) stop("AvgDayBefore needs to contain a meaningful date field (of class Date)!")
    if (!inherits(AvgDayBefore$date, "Date")) stop("AvgDayBefore$date is not of class Date!")
    day <- AvgDayBefore$date
    initialize <- FALSE
  } else {
    stop("AvgDayBefore needs to be a snowprofile or a NULL value")
  }

  ## days along season
  days <- sort(unique(sm$date[sm$date >= day & sm$date <= DateEnd]))
  if (length(days) == 0) {
    if (day > DateEnd) {
      stop("DateEnd is earlier than AvgDayBefore$date!")
    } else {
      stop("There seem to be no dates in your provided data set that satisfy your time window!")
    }
  }
  ## ensure profile sampling not more often than daily
  if (!all(sapply(sm$station_id, function(stid) {
    length(sm$date[sm$station_id == stid]) == length(unique(sm$date[sm$station_id == stid]))
  }))) stop("Profiles are sampled more often than once per day! Only daily sampling supported.")
  ## check if profiles exist for each of the days and
  ## (1) discard missing days, (2) discard days with medianHS below resamplingRate
  days <- unique(c(day, days[sapply(days, function(dy) length(sm$date[sm$date == dy]) > 0 & medianHSatDate[unique(sm$date) == dy] > 2*resamplingRate)]))
  if (initialize) {
    day <- days[1]
  } else {
    day <- days[2]
  }


  ## initialize object for average time series
  RES <- vector("list", length(days))
  names(RES) <- days

  ## initialize progressbar:
  if (progressbar) {
    pb <- progress::progress_bar$new(
      format = " [:bar] :percent in :elapsed | eta: :eta",
      total = length(days), clear = FALSE, width= 60)
  }

  ## subfunction for initializing average profile:
  initializeAvg <- function(day) {
    iavg <- averageSP(SPx[sm$date == day], sm = sm[sm$date == day, ], progressbar = FALSE, resamplingRate = resamplingRate)
    iavg$avg$date <- day
    iavg$avg$reinitialized <- TRUE
    ## ensure continuity of ddates:
    if ("ddate" %in% names(iavg$avg$layers)) {
      while (any(diff(iavg$avg$layers$ddate) < 0)) {
        iavg$avg$layers$ddate[which(diff(iavg$avg$layers$ddate) < 0)] <- iavg$avg$layers$ddate[which(diff(iavg$avg$layers$ddate) < 0)+1]
      }
    }
    if (!keep.profiles) iavg$set <- NULL
    return(iavg)
  }

  ## subfunction for MovingAverage (for dailyRescaling == "settleTopOldSnow")
  ma <- function(x, n = 5){
    if (length(x) < n) n <- length(x)
    filter(x, rep(1 / n, n), sides = 2)
    }

  ##--- Compute averages ----
  if (initialize) {
    RES[[1]] <- tryCatch({initializeAvg(day)}, error = function(err) err)
    ## error handling: problems can happen during early season thin snowpack
    initialErrorDays <- 0
    while (inherits(RES[[1]], "error")) {
      initialErrorDays <- initialErrorDays + 1
      if (grepl("Can't find an average profile!", RES[[1]]$message)) {
        ## proceed to next day and try to find an average profile..
        days <- days[-1]
        day <- days[1]
        RES <- vector("list", length(RES)-1)
        names(RES) <- days
        RES[[1]] <- tryCatch({initializeAvg(day)}, error = function(err) err)
      } else {
        message(paste0("At day ", initialErrorDays, " after initialization:"))
        stop(RES[[1]]$message)
      }
      if (initialErrorDays > 59) stop(paste0("Can't find average profiles for the first ", initialErrorDays, " days after initialization?! Check your data set!"))
    }
  } else {
    RES[[1]] <- list(avg = AvgDayBefore, set = NULL)
  }
  if (progressbar) pb$tick()

  if (length(days) > 1) {
    for (i in seq(2, length(days))) {  # LOOP over each day
      ## compute average profile with dbaSP
      RES[[i]] <- tryCatch({
        ## previous average profile is more than one day back
        ## -> raise error that will provoke initializing a new average profile further down!
        if (as.numeric(days[i] - days[i-1]) > 1) stop("no single layer")
        ## call dbaSP
        tmp <- dbaSP(SPx[sm$date == days[i]], Avg = RES[[i-1]]$avg, sm = sm[sm$date == days[i], ], keep.profiles = TRUE,
                     proportionPWL = proportionPWL, breakAtSim = breakAtSim, breakAfter = breakAfter, verbose = verbose, resamplingRate = resamplingRate,
                     top.down = top.down, checkGlobalAlignment = checkGlobalAlignment, prefLayerWeights = prefLayerWeights,
                     dims = dims, weights = weights, ...)
        tmp$avg$date <- days[i]
        tmp$avg$reinitialized <- FALSE
        ## ensure continuity of ddates:
        if ("ddate" %in% names(tmp$avg$layers)) {
          while (any(diff(tmp$avg$layers$ddate) < 0)) {
            tmp$avg$layers$ddate[which(diff(tmp$avg$layers$ddate) < 0)] <- tmp$avg$layers$ddate[which(diff(tmp$avg$layers$ddate) < 0)+1]
          }
        }

        ## compute today's profile's deviation from medianHS
        dev4medianHS <- medianHSatDate[unique(sm$date) == days[i]] - tmp$avg$hs
        if (dev4medianHS < 0) {  # avg profile is higher than medianHS
          ## RESCALING OF PROFILE TO MATCH MEDIAN HS:
          ## HS difference between avg and avg at previous day
          hs_diff <- tmp$avg$hs - RES[[i-1]]$avg$hs
          if (dailyRescaling == "settleEntireOldSnow") {
            if (hs_diff > 0) {  # HS increased since last day
              ## rescale old snow part (i.e. height from day before) so that new HS equals median HS
              fac <- min((medianHSatDate[unique(sm$date) == days[i]] - hs_diff) / RES[[i-1]]$avg$hs, 1)  # max value 1 allowed!
              k_oldSnow <- which(tmp$avg$layers$height <= RES[[i-1]]$avg$hs)
              tmp$avg$layers$thickness[k_oldSnow] <- tmp$avg$layers$thickness[k_oldSnow] * fac
            } else {  # HS remained constant or decreased
              ## rescale entire profile to median HS, but don't allow factors > 1 for layer depth consistency reasons
              fac <- min((medianHSatDate[unique(sm$date) == days[i]]) / tmp$avg$hs, 1)  # max value 1 allowed!
              tmp$avg$layers$thickness <- tmp$avg$layers$thickness * fac
            }
            ## fix profile data:
            tmp$avg$layers$height <- cumsum(tmp$avg$layers$thickness)
            tmp$avg$layers$depth <- c(rev(cumsum(tmp$avg$layers$thickness))[-1], 0)
            newHS <- tail(tmp$avg$layers$height, n = 1)
            tmp$avg$maxObservedDepth <- tmp$avg$maxObservedDepth + (newHS - tmp$avg$hs)
            tmp$avg$hs <- newHS
            tmp$avg$backtrackingTable$height <- tmp$avg$layers$height[tmp$avg$backtrackingTable$layerID]

          } else if (dailyRescaling == "settleTopOldSnow") {
            ## find height of snow column that is consistently deeper than median layer heights (i.e., don't scale that even deeper!)
            if (hs_diff > 0) {  # HS increased since last day
              ## rescale old snow part (i.e. height from day before) so that new HS equals median HS
              k_oldSnow <- which(tmp$avg$layers$height <= RES[[i-1]]$avg$hs)
              ## smoothed ratio between medianPredominantHeight and actual avg height
              # layerwiseFactor <- ma(tmp$avg$layers$medianPredominantHeight[k_oldSnow] / tmp$avg$layers$height[k_oldSnow])
              layerwiseFactor <- ma(tmp$avg$layers$medianPredominantDepth[k_oldSnow] / tmp$avg$layers$depth[k_oldSnow])
            } else {  # HS remained constant or decreased
              ## smoothed ratio between medianPredominantHeight and actual avg height
              # layerwiseFactor <- ma(tmp$avg$layers$medianPredominantHeight / tmp$avg$layers$height)
              layerwiseFactor <- ma(tmp$avg$layers$medianPredominantDepth / tmp$avg$layers$depth)
            }
            nLF <- length(layerwiseFactor)
            notNALF <- which(!is.na(layerwiseFactor) & !is.infinite(layerwiseFactor))  # all indices from layerwiseFactor that are not NA
            if (length(notNALF) < 1) {
              layerwiseFactor[] <- 1  # if all layerwiseFactor is NA --> set all to 1
            } else {
              layerwiseFactor[seq(max(notNALF)+1, nLF)] <- layerwiseFactor[max(notNALF)]  # replace all upper end NAs with the last non-NA value
              layerwiseFactor[seq(1, min(notNALF)-1)] <- layerwiseFactor[min(notNALF)]  # replace all lower end NAs with the first non-NA value
            }
            if (all(layerwiseFactor >= 1) | all(layerwiseFactor < 1)) {
              k_scaleDeeper_start <- 1
            } else {
              largestIDXleq1 <- suppressWarnings(max(which(layerwiseFactor > 1)))
              if (is.infinite(largestIDXleq1)) largestIDXleq1 <- max(which(layerwiseFactor >= 1))
              k_scaleDeeper_start <- max(which(layerwiseFactor[1:largestIDXleq1] < 1), 1)
              #  note about previous line k_scaleDeeper_start:
              #  find the largest index of values smaller than 1, but first exclude all values smaller than 1 which are located at the end of the vector,
              #  because they lead to too few layers being rescaled, which requires negative scaling factors, which in turn breaks the snowprofileLayers object.
              #  Additionally, offer 1 as an alternative to max function to prevent infinite results.
            }
            k_scaleDeeper <- k_scaleDeeper_start:nLF  ## these layers will be scaled to lower heights

            ## compute static (scalar) fac(tor) that rescales to correct medianHS by altering k_scaleDeeper layers
            fac <- min(1 + (dev4medianHS/sum(tmp$avg$layers$thickness[k_scaleDeeper])), 1)  # max value 1 alowed!
            ## correct scaling factors that would break snowprofileLayers object (i.e., fac is too small):
            if (fac < 0.2) {
              correction_iteration_nr <- 1
              mod_layerwiseFactor <- layerwiseFactor[1:min(max(which(layerwiseFactor > 1)), length(layerwiseFactor))]
              while (fac < 0.2) {
                fac <- tryCatch({
                  sign_layerwiseFactor <- mod_layerwiseFactor - 1
                  ## with each iteration, include more layers into the rescaling so that fac grows larger
                  mod_layerwiseFactor <- suppressWarnings(mod_layerwiseFactor[1:max(which(sign(sign_layerwiseFactor) == sign(-1*sign_layerwiseFactor[length(sign_layerwiseFactor)])))])
                  k_scaleDeeper_start <- suppressWarnings(max(which(mod_layerwiseFactor < 1)))
                  k_scaleDeeper <- k_scaleDeeper_start:nLF
                  min(1 + (dev4medianHS/sum(tmp$avg$layers$thickness[k_scaleDeeper])), 1)
                }, error = function(err) err)

                correction_iteration_nr <- correction_iteration_nr + 1
                if (correction_iteration_nr > 11 | inherits(fac, "error")) {
                  warning(paste0("Can't correctly rescale to median snow height at day ", days[i],
                                 ". Make sure to compare the median snow height to the height of the time series."))
                  fac <- 0.2
                  break}}}  # while loop: scaling factor correction

            ## scaling
            tmp$avg$layers$thickness[k_scaleDeeper] <- tmp$avg$layers$thickness[k_scaleDeeper] * fac
            tmp$avg$layers$height <- cumsum(tmp$avg$layers$thickness)
            tmp$avg$layers$depth <- c(rev(cumsum(tmp$avg$layers$thickness))[-1], 0)

            ## fix profile data:
            newHS <- tail(tmp$avg$layers$height, n = 1)
            tmp$avg$maxObservedDepth <- tmp$avg$maxObservedDepth + (newHS - tmp$avg$hs)
            tmp$avg$hs <- newHS
            tmp$avg$backtrackingTable$height <- tmp$avg$layers$height[tmp$avg$backtrackingTable$layerID]

          } else {
            stop(paste0("'dailyRescaling' == ", paste0(dailyRescaling, collapse = " "), " is not supported"))
          }
        }  ## END if: settlement scaling

        if (!keep.profiles) tmp$set <- NULL
        tmp
      }, error = function(err) err)  #, warning = function(warn) warn)

      ## dbaSP potential error handling
      if (inherits(RES[[i]], "error")) {
        ## check for a specific error raised by dbaSP and re-initialize average profile in that case:
        if (grepl("no single layer", RES[[i]]$message)) {
          RES[[i]] <- initializeAvg(days[i])
        } else {  # different error --> stop computations!
          message(paste0("\n", RES[[i]]))
          message("\nReturning results prior to error. Resume computation by providing an AvgDayBefore to averageSPalongSeason.")
          RES <- RES[1:(i-1)]
          break
        }
      }

      if (progressbar) pb$tick()
    }  # END LOOP over each day
  }

  ##--- Format output ----
  if (!initialize){
    RES <- RES[2:length(RES)]
  }
  avgs <- lapply(RES, function(res) res$avg)
  avgs <- snowprofileSet(avgs)
  sets <- lapply(RES, function(res) res$set)
  if (all(is.null(sets[[1]]))) sets <- sets[-1]  # if the function was started with AvgDayBefore, the first set is NULL

  if (keep.profiles) {
    OUT <- list(avgs = avgs, sets = sets)
  } else {
    OUT <- list(avgs = avgs)
  }
  OUT$call <- match.call()
  OUT <- tryCatch({
    OUT$meta <- summary(avgs)[, c("date", "reinitialized", "rmse", "hs")]
    OUT$meta$hs_median <- sapply(OUT$meta$date, function(d) median(sm$hs[sm$date == d]))
    ## median thickness of new snow
    OUT$meta$thicknessPPDF_median <- NA
    names_sets <- names(sets)
    for (d in names_sets) {
      OUT$meta$thicknessPPDF_median[OUT$meta$date == d] <- median(sapply(sets[[which(names_sets == d)]], function(sp) {
        sum(sp$layers$thickness[findPWL(sp, pwl_gtype = c("PP", "DF"))])
        }))
    }
    OUT
  }, error = function(err) {
    message(err)
    message(paste0("An error occured shortly before the routine was finished, omitting return field $meta to provide you with the results anyway!"))
    OUT
  })

  ## inherits class label
  class(OUT) <- append("avgSP_timeseries", class(OUT))

  return(OUT)
}
