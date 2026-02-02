#' Average a group of snow profiles
#'
#' The functions [dbaSP] and [averageSP] implement Dynamic Time Warping Barycenter Averaging of snow profiles.
#' The convenient wrapper [averageSP] takes care of choosing several appropriate initial conditions and picking the optimal end result (by minimizing the mean squared error
#' between the average profile and the profile set). To pay appropriate attention to (thin) weak layers, weak layers need to be labeled in the profiles.
#' You can either do that manually before calling this routine to suit your personal needs, or you can provide specific properties (in `classifyPWLs`)
#' so that weak layers be labeled according to these properties by [sarp.snowprofile::labelPWL].
#' For more details, refer to the reference paper.
#'
#' Technical note: Since the layer characteristics of the average profile represent the median characteristics of the individual profiles, it can happen that ddates of the
#' averaged layers are not in a monotonic order. That is, of course non-physical, but we specifically decided not to override these values to highlight these slight inconsistencies
#' to users, so that they can decide how to deal with them. As a consequence, the function [sarp.snowprofile::deriveDatetag] does not work for these average profiles with ddate
#' inconsistencies, but throws an error. The suggested workaround for this issue is to apply that function to all individual profiles *before* computing the average profile. This
#' ensures that bdates or datetags are also included in the average profile.
#'
#' For developers: Including new variables into the averaging/dba routines can be done easily by following commit #9f9e6f9
#'
#'
#' @describeIn averageSP convenient wrapper function
#' @param SPx SPx a [sarp.snowprofile::snowprofileSet] object. Note that the profile layers need to contain a column
#' called `$layerOfInterest` which classifies weak layers. While [averageSP] will label weak layers automatically if not done by the user beforehand, [dbaSP] won't do that but fail instead!;
#' consider thinking about how you want to label weak layers, see Description, `classifyPWLs` below, and the references.
#' Also note, that if you wish to average the *rescaled* profile set, do so manually before calling this function (see examples).
#' @param n the number of initial conditions that will be used to run [dbaSP]; see also [chooseICavg].
#' @param sm a [summary] of `SPx` metadata
#' @param progressbar should a progressbar be displayed (the larger n, the more meaningful the progressbar)
#' @param progressbar_pretext a character string to be prepended to the progressbar (mainly used by higher level cluster function)
#' @param classifyPWLs an argument list for a function call to [sarp.snowprofile::findPWL] which returns relevant PWLs for identifying initial conditions. **Importantly**, these arguments will also be used
#' to label weak layers in the profiles, if these labels do not yet exist in the layers objects as column `$layerOfInterest`.
#' Check out the documentation of `findPWL` to familiarize yourself with your manifold options!
#' @param classifyCRs an argument list for a function call to [sarp.snowprofile::findPWL] which returns relevant crusts for identifying initial conditions.
#' @param proportionPWL decimal number that specifies the proportion required to average an ensemble of grain types as weak layer type.
#' A value of 0.3, for example, means that layers will get averaged to a PWL type if 30% of the layers are of PWL type.
#' Meaningful range is between `[0.1, 0.5]`. Values larger than 0.5 get set to 0.5.
#' @param breakAtSim stop iterations when [simSP] between the last average profiles is beyond that value. Can range between `[0, 1]`. Default values differ between [dbaSP] and [averageSP].
#' @param breakAfter integer specifying how many values of simSP need to be above `breakAtSim` to stop iterating. Default values differ between [dbaSP] and [averageSP].
#' @param tz timezone of profiles; necessary for assigning the correct timezone to the average profile's ddate/bdate. Either `'auto'` or a timezone known to `[as.POSIXct]`.
#' @param n_cores number of nodes to create for a cluster using the  parallel package to speed up calculations (default = NULL)
#' @param ... alignment configurations which are passed on to [dbaSP] and then further to [dtwSP]. Note, that you can't provide `rescale2refHS`, which is always set to FALSE. If you wish to rescale
#' the profiles, read the description of the `SPx` parameter and the examples.
#' @return A list of class `avgSP` that contains the fields
#'
#'   * `$avg`: the resulting average profile
#'   * `$set`: the corresponding resampled profiles of the group
#'   * `$call`: (only with `averageSP`) the function call
#'   * `$prelabeledPWLs`: (only with `averageSP`) boolean scalar whether PWLs (or any other layers of interest) were prelabeled before this routine (`TRUE`) or
#'   labeled by this routine with the defaults specified in `classifyPWLs` (`FALSE`)
#'
#' The profile layers of the average profile refer to the median properties of the predominant layers. For example, if you labeled all SH/DH layers as your 'layersOfInterest',
#' and you find a SH or DH layer in the average profile, then it means that the predominant grain type is SH/DH (i.e., more profiles than specified in `proportionPWL` have that layer)
#' and layer properties like hardness, p_unstable, etc refer to the median properties of these SH/DH layers. If you find a RG layer in your average profile, it means that most
#' profiles have that RG layer and the layer properties refer to the median properties of all these RG layers. There are two exceptions to this rule, one for `height`/`depth`, and one
#' for layer properties with the ending `_all`, such as `ppu_all`:
#'
#'  - `height` and `depth` provide the vertical grid of the average profile, and for algorithmic reasons, this grid is not always equal to the actual median height or depth
#'  of the predominant layers. To account for that, two layer columns exist called `medianPredominantHeight` and `medianPredominantDepth`.
#'  - Properties ending with `_all`: For example, while `ppu` refers to the proportion of profiles, whose *predominant* layers are unstable (i.e., `p_unstable` >= 0.77),
#'  `ppu_all` refers to the the proportion of profiles, whose layers are unstable while taking into account *all* individual layers matched to this average layer
#'  (i.e., despite grain type, etc).
#'  - Other layer properties specific to the average profile: `distribution` ranges between `[0, 1]` and specifies the proportion of profiles that contain the predominant layer described in the other properties.
#'
#' @author fherla
#' @references Herla, F., Haegeli, P., and Mair, P. (2022). A data exploration tool for averaging and accessing large data sets of snow stratigraphy profiles useful for avalanche forecasting,
#' The Cryosphere, 16(8), 3149–3162, https://doi.org/10.5194/tc-16-3149-2022
#'
#' @seealso [averageSPalongSeason]
#'
#' @examples
#' ## EXAMPLES OF averageSP
#' this_example_runs_about_10s <- TRUE
#' if (!this_example_runs_about_10s) {  # exclude from cran checks
#'
#' ## compute the average profile of the demo object 'SPgroup'
#' ## * by labeling SH/DH layers as weak layers,
#' ##   - choosing 3 initial conditions with an above average number of weak layers
#' ##   - in as many depth ranges as possible
#' ## * and neglecting crusts for initial conditions
#'
#'   avgList <- averageSP(SPgroup, n = 3,
#'                        classifyPWLs = list(pwl_gtype = c("SH", "DH")),
#'                        classifyCRs = NULL)
#'
#'   opar <- par(mfrow = c(1, 2))
#'   plot(avgList$avg, ymax = max(summary(avgList$set)$hs))
#'   plot(avgList$set, SortMethod = "unsorted", xticklabels = "originalIndices")
#'   par(opar)
#'
#'
#' ## compute the average profile of the demo object 'SPgroup'
#' ## * by labeling SH/DH/FC/FCxr layers with an RTA threshold of 0.65 as weak layers,
#' ## * otherwise as above
#'
#'   SPx <- computeRTA(SPgroup)
#'   avgList <- averageSP(SPx, n = 3,
#'                        classifyPWLs = list(pwl_gtype = c("SH", "DH", "FC", "FCxr"),
#'                                            threshold_RTA = 0.65),
#'                        classifyCRs = NULL)
#'
#'   opar <- par(mfrow = c(1, 2))
#'   plot(avgList$avg, ymax = max(summary(avgList$set)$hs))
#'   plot(avgList$set, SortMethod = "unsorted", xticklabels = "originalIndices")
#'   par(opar)
#'
#' ## compute the average profile of the other demo object 'SPgroup2', which
#' ## contains more stability indices, such as SK38 or p_unstable
#' ## * by labeling SH/DH/FC/FCxr layers that either
#' ##   - have an SK38 below 0.95, *or*
#' ##   - have a p_unstable above 0.77
#'
#'   SPx <- snowprofileSet(SPgroup2)
#'   avgList <- averageSP(SPx,
#'                        classifyPWLs = list(pwl_gtype = c("SH", "DH", "FC", "FCxr"),
#'                                            threshold_SK38 = 0.95, threshold_PU = 0.77))
#'
#'   opar <- par(mfrow = c(1, 2))
#'   plot(avgList$avg, ymax = max(summary(avgList$set)$hs))
#'   plot(avgList$set, SortMethod = "unsorted", xticklabels = "originalIndices")
#'   par(opar)
#'
#' }
#'
#'
#' @export
averageSP <- function(SPx, n = 5, sm = summary(SPx),
                      progressbar = requireNamespace("progress", quietly = TRUE),
                      progressbar_pretext = NULL,
                      classifyPWLs = list(pwl_gtype = c("SH", "DH")),
                      classifyCRs = list(pwl_gtype = c("MFcr", "IF", "IFsc", "IFrc")),
                      proportionPWL = 0.5,
                      breakAtSim = 0.9, breakAfter = 2, verbose = FALSE,
                      tz = "auto",
                      n_cores = NULL, ...) {

  if (!is.snowprofileSet(SPx)) stop("SPx must be a snowprofileSet")
  if (tz == "auto") {
    tzfac <- factor(sapply(SPx, function(p) attr(p$datetime, "tzone")))
    tz <- levels(tzfac)[median(as.numeric(tzfac))]
    if (tz %in% c("NULL", "NA", NA)) tz <- ""
  }

  ## if the layer objects from SPx do not contain the column '$layerOfInterest', label weak layers with the provided argument list
  prelabeledPWLs <- TRUE
  if (!all(sapply(SPx, function(sp) "layerOfInterest" %in% names(sp$layers)))) {
    message("Weak layers not pre-labeled for averageSP. Automatic labeling based on arguments in classifyPWLs.")
    SPx <- snowprofileSet(lapply(SPx, function(sp) do.call("labelPWL", c(quote(sp), classifyPWLs))))
    prelabeledPWLs <- FALSE
  }

  ## compute several meaningful initial condition profiles:
  IC_ids <- chooseICavg(SPx, n = n, classifyPWLs = classifyPWLs, classifyCRs = classifyCRs, sm = sm)
  n <- length(IC_ids)
  sapply(seq(n), function(i) {
    if (!is.snowprofile(SPx[[IC_ids[i]]])) stop(paste0("Problem with choosing initial conditions! (idx: ", IC_ids[i], ")"))
  })

  ## initialize progressbar:
  if (progressbar) {
    pb <- progress::progress_bar$new(
      format = paste0(progressbar_pretext, " [:bar] :percent in :elapsed | eta: :eta"),
      total = n, clear = FALSE, width= 60)
  }

  ## compute average for different IC
  ## IC gets rescaled to median hs though!
  ## Compute pairwise distance matrix using parallel computation if cluster object is provided
  if (!is.null(n_cores)) {

    ## Create cluster with dbaSP function loaded
    cluster <- parallel::makeCluster(n_cores)
    parallel::clusterExport(cluster, c('dbaSP', 'scaleSnowHeight'))

    ## Loop DBA calculation
    DBA <- parallel::parLapply(cluster, seq(n), function(i) {
      tryCatch({
        dbaSP(SPx,
              scaleSnowHeight(SPx[[IC_ids[i]]], height = median(sm$hs))$queryScaled,  # IC profile rescaled to median hs
              sm = sm,
              proportionPWL = proportionPWL,
              breakAtSim = breakAtSim, breakAfter = breakAfter,
              verbose = verbose, tz = tz, ...)
      },
      error = function(err) {
        warning(paste0("Error in averaging of profiles:\n ", err))
        return(NA)
      }, finally = {if (progressbar) pb$tick()})
    })
    parallel::stopCluster(cluster)

  ## Non-parallel calculations
  } else {
    DBA <- lapply(seq(n), function(i) {
      tryCatch({
        dbaSP(SPx,
              scaleSnowHeight(SPx[[IC_ids[i]]], height = median(sm$hs))$queryScaled,  # IC profile rescaled to median hs
              sm = sm,
              proportionPWL = proportionPWL,
              breakAtSim = breakAtSim, breakAfter = breakAfter,
              verbose = verbose, tz = tz, ...)
      },
      error = function(err) {
        warning(paste0("Error in averaging of profiles:\n ", err))
        return(NA)
      }, finally = {if (progressbar) pb$tick()})
    })
  }

  ## check whether there's a meaningful result:
  DBAmeaningful <- sapply(DBA, function(dba) ifelse(all(is.na(dba)), FALSE, TRUE))
  DBA <- DBA[DBAmeaningful]
  n <- length(DBA)
  if (n == 0) {
    if (median(sm$hs) < 2) stop("Can't find an average profile! Likely because your provided profiles are extremely shallow.")
    else stop("Can't find an average profile!")
  }

  ## compute MSE for each realization of average profile
  MSE <- sapply(seq(n), function(i) {
    DBA[[i]]$avg$mse
  })

  ## pick the average profile that has the smallest mse:
  ans <- DBA[[which.min(MSE)]]

  ## append other useful information:
  ans$call <- match.call()
  ans$prelabeledPWLs <- prelabeledPWLs

  return(ans)
}
