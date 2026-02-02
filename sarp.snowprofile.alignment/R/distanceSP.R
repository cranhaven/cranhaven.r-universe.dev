#' Compute pairwise distances between snow profiles
#'
#' Calculate the distance between all combinations of snowprofiles in a snowprofileSet by:
#'
#'  1. Matching their layers and aligning them (i.e., warp one profile onto the other one)
#'  2. Assessing the similarity of the aligned profiles based on avalanche hazard relevant characteristics
#'  3. Convert the similarity score into a distance value between `[0, 1]`
#'
#' @param SPx a [sarp.snowprofile::snowprofileSet] object (or a single snowprofile if SP2 is provided)
#' @param SP2 a [sarp.snowprofile::snowprofile] object if SPx is also a snowprofile and a single pairwise distance is to be computed
#' @param output type of output to return, either a class `dist` (default) or `matrix`
#' @param n_cores number of nodes to create for a cluster using the  parallel package to do distance matrix calculation in parallel (default is serial calculations)
#' @param symmetric only compute one of two alignments dtwSP(A, B) or dtwSP(B, A) rather than taking the min distance (when diminished accuracy is
#' favourable to speed up run times for large number of profiles)
#' @param fast_summary Option to compute distances from basic summary stats instead of layerwise comparisons
#' @param fast_summary_weights A named numeric vector with relative weights for each snowpack property. Must be in exact
#' order, but do not need to be normalized.  Use [clusterSPconfig]$args_fast for template. See details for summary stats that have been implemented.
#' @param progressbar Do you want to print a progress bar with recommended package 'progress'? (only works for n_core = NULL)
#' @param ... arguments passed to [dtwSP] and further to [simSP]. `simType` from [simSP] is an important choice.
#'
#' @return Either a dist or matrix object with pairwise distances (depending on `output` argument)
#'
#' @author shorton fherla
#'
#' @seealso [simSP], [medoidSP], [clusterSP]
#'
#' @details This procedure is useful for clustering and aggregating tasks, given a set of multiple profiles.
#'
#'  When computing the distance matrix this routine calls [simSP] for
#' *every possible pair* of profiles among the group. During that call the profile pair is aligned by [dtwSP]
#'  and the aligned pair is evaluated by [simSP].
#'
#' Note that the pairwise distance matrix is modified within the function call to represent a symmetric distance matrix.
#' That is, however, not originally the case, since `dtwSP(A, B) != dtwSP(B, A)`. The matrix is therefore made symmetric by
#' setting the similarity between the profiles A and B to `min({dtwSP(A, B), dtwSP(B, A)})`.
#'
#'  Note that the number of possible profile pairs grows exponentially with the number of profiles in the group (i.e.,
#'  O(n^2) calls, where n is the number of profiles in the group). Several option for improved performance include:
#'   - Using the `n_core` argument to activate thee `parallel` package. A suggestion value is the number of cores on your
#'     system minus one `n_cores = parallel::detectCores() - 1`.
#'   - Setting `symmetric = FALSE` will only calculate `dtwSP(A, B)` and therefore not make the matrix symmetric, but cut the number of alignments in half
#'   - Setting `fast_summary = TRUE` will compute similarities from basic summary stats instead of aligning layers with dynamic time warping.
#'
#'  When using `fast_summary = TRUE`, you can provide custom weights to change the relative importance of the following snowpack properties:
#'   - `w_hs`: total snow height
#'   - `w_hn24`: height of snow in past 24 h
#'   - `w_hn72`: height of snow in past 72 h
#'   - `w_slab`: average hand hardness of snow in past 72 h
#'   - `w_gtype`: total thickness of layers grouped into new snow (PP, DF), pwls (SH, FC, DH), bulk (RG, FCxr) and melt (MF, MFcr, IF)
#'   - `w_gtype_rel`: `w_gtype` scaled by HS
#'   - `w_new`: total thickness of PP/DF layers
#'   - `w_pwl`: do critical weak layers exist in the top/middle/bottom thirds of the profile
#'   - `w_crust`: do melt-freeze crusts exist in the top/middle/bottom thirds of the profile
#'   - `w_rta`: maximum rta in the top/middle/bottom thirds of the profile
#'  The number of stats computed depends on the `snowprofileLayer` properties available in the data.
#'
#'  @examples
#'
#' ## Simple serial calculation
#' distmat1 <- distanceSP(SPgroup2\[1:4\])
#'
#' ## Parallel calculation (uncomment)
#' #distmat2 <- distanceSP(SPgroup2\[1:4\], n_cores = parallel::detectCores() - 1)
#'
#' ## Fast summary method
#' distmat3 <- distanceSP(SPgroup2, fast_summary = T)
#' ## View the default weights, then recalculate the distances with adjusted weights
#' print(clusterSPconfig()$args_fast)
#' weights <- c(w_hs = 3, w_hn24 = 0, w_h3d = 2, w_slab = 0,
#'              w_gtype = 0, w_gtype_rel = 0, w_new = 0,
#'              w_pwl = 0, w_crust = 1, w_rta = 1)
#' distmat4 <- distanceSP(SPgroup2, fast_summary = T, fast_summary_weights = weights)
#'
#' @importFrom stats as.dist dist quantile weighted.mean
#' @importFrom utils combn
#' @export
#'
distanceSP <- function(SPx,
                       SP2 = NULL,
                       output = 'dist',
                       n_cores = NULL,
                       symmetric = TRUE,
                       fast_summary = FALSE,
                       fast_summary_weights = clusterSPconfig()$args_fast,
                       progressbar = requireNamespace('progress', quietly = TRUE),
                       ...) {

  ## Check input
  if (is.snowprofile(SPx) & is.snowprofile(SP2)) SPx <- snowprofileSet(list(SPx, SP2))
  stopifnot(is.list(SPx))
  sapply(SPx, function(x) if (!is.snowprofile(x)) stop('At least one element in snowprofileSet is not a snowprofile'))

  ## Are distances based on dtw alignments or summary stats?
  if (!fast_summary) {

    ## Create list of unique profile pairs to compare (instead of doing npro*npro calculations)
    profile_pair_ids <- as.data.frame(t(combn(1:length(SPx), 2)))
    profile_pairs <- lapply(1:nrow(profile_pair_ids), function(i) list(q = SPx[[profile_pair_ids[i, 1]]],
                                                                       r = SPx[[profile_pair_ids[i, 2]]]))

    ## Check number of dtwSP calls
    if (length(SPx) > 5){
      message(paste0('You are about to compute ', 2*length(profile_pairs), ' alignments for ', length(SPx),' profiles. Be patient...'))
      if (is.null(n_cores)) message(paste('Consider calculating alignments in parallel by setting',
                                          'n_cores argument to a number <= parallel::detectCores()'))
    }

    ## Serial calculation of pairwise distances
    if (is.null(n_cores)) {

      ## Initialize progressbar
      if (progressbar) {
        pb <- progress::progress_bar$new(
          format = ' [:bar] :percent in :elapsed | eta: :eta',
          total = length(profile_pairs), clear = FALSE, width= 60)
      }

      ## Calculate distances
      profile_pair_ids$dist <- sapply(profile_pairs, function(x)
        tryCatch({
          if (progressbar) pb$tick()
          pdist <- 1 - dtwSP(x$q, x$r, ...)$sim
          if (symmetric) {
            pdist2 <- 1 - dtwSP(x$r, x$q, ...)$sim
            pdist <- min(pdist, pdist2, na.rm = TRUE)
          }
          return(pdist)
        },
        error = function(err) {
          warning(paste0(paste(err), " (during alignment of ", x$q$station_id, ", ", x$r$station_id, ")"), immediate. = TRUE)
          return(NA)
        })
      )
    }

    ## Parallel calculation of pairwise distances
    else if (is.numeric(n_cores)) {

      ## Create parallel cluster with dtwSP function loaded
      cl <- parallel::makeCluster(n_cores)
      parallel::clusterExport(cl, 'dtwSP')

      ## Calculate distances
      profile_pair_ids$dist <- parallel::parSapply(cl, profile_pairs, function(x)
        tryCatch({
          pdist <- 1 - dtwSP(x$q, x$r, ...)$sim
          if (symmetric) {
            pdist2 <-1 - dtwSP(x$r, x$q, ...)$sim
            pdist <- min(pdist, pdist2, na.rm = TRUE)
          }
          return(pdist)
        },
        error = function(err) {
          warning(paste0(paste(err), " (during alignment of ", x$q$station_id, ", ", x$r$station_id, ")"), immediate. = TRUE)
          return(NA)
        })
      )

      ## Stop parallel cluster
      parallel::stopCluster(cl)
    } else {
      warning('n_cores must either be NULL or a number <= parallel::detectCores()')
    }

    ## Fill the pairwise distances into a symmetric matrix
    distmat <- matrix(0, nrow = length(SPx), ncol = length(SPx))
    distmat[as.matrix(profile_pair_ids[, c('V1', 'V2')])] <- profile_pair_ids$dist
    distmat[lower.tri(distmat)] <- t(distmat)[lower.tri(distmat)]
    diag(distmat) <- 0

  } else {

    ## Calculate summary statistics for each profile
    profileStats <- function(SP) {

      ## Initialize data.frame to store profile stats
      x <- data.frame(vstation = SP$station, hs = SP$hs,
                      hn24 = NA, h3d = NA, slab_hard = NA,
                      new = NA, bulk = NA, mf = NA, weak = NA,
                      new_rel = NA, bulk_rel = NA, mf_rel = NA, weak_rel = NA,
                      pwl_top = NA, pwl_mid = NA, pwl_bottom = NA,
                      crust_top = NA, crust_mid = NA, crust_bottom = NA,
                      rta_top = NA, rta_mid = NA, rta_bottom = NA)
      lyrs <- SP$layers

      ## New snow depths and hardness
      if ('ddate' %in% names(lyrs)) {
        age <- SP$date - as.Date(lyrs$ddate)
        x$hn24 <- ifelse(sum(age <= 1) > 0, lyrs$depth[min(which(age <= 1))], 0)
        x$h3d <- ifelse(sum(age <= 3) > 0, lyrs$depth[min(which(age <= 3))], 0)
        i3d <- ifelse(sum(age <= 3) > 0, min(which(age <= 3)), NA)
        if ('hardness' %in% names(lyrs)) {
          x$slab_hard <- ifelse(is.na(i3d), 0, weighted.mean(lyrs$hardness[i3d:nrow(lyrs)], lyrs$thickness[i3d:nrow(lyrs)]))
        }
      }
      if ('gtype' %in% names(lyrs)) {

        ## Grain type proportions
        x$weak <- sum(lyrs$thickness[lyrs$gtype %in% c('DH','SH','FC')])
        x$mf <- sum(lyrs$thickness[lyrs$gtype %in% c('MFcr','IF','IF')])
        x$new <- sum(lyrs$thickness[lyrs$gtype %in% c('PP','DF')])
        x$bulk <- sum(lyrs$thickness[lyrs$gtype %in% c('RG','FCxr')])
        x$weak_rel <- x$weak / SP$hs
        x$mf_rel <- x$mf / SP$hs
        x$new_rel <- x$new / SP$hs
        x$bulk_rel <- x$bulk / SP$hs

        ## PWLs or crusts at top/middle/bottom third of profile
        if ('p_unstable' %in% names(lyrs) || 'rta' %in% names(lyrs)) {
          lyrs$pwl <- labelPWL(SP,
                               pwl_gtype = ifelse('p_unstable' %in% names(lyrs), c('DH','SH','FC','FCxr'), c('DH','SH','FC','FCxr')),
                               threshold_PU = ifelse('p_unstable' %in% names(lyrs), 0.77, NA),
                               threshold_RTA = ifelse('rta' %in% names(lyrs), 0.65, NA))$layers$layerOfInterest
          lyrs$crust <- labelPWL(SP,
                                 pwl_gtype = ifelse('p_unstable' %in% names(lyrs), c('MFcr','IF'), c('MFcr','IF')),
                                 threshold_PU = ifelse('p_unstable' %in% names(lyrs), 0.77, NA),
                                 threshold_RTA = ifelse('rta' %in% names(lyrs), 0.65, NA))$layers$layerOfInterest
        } else if ('gsize' %in% names(lyrs)) {
          lyrs$pwl <- labelPWL(SP, pwl_gtype = c('DH','SH','FC','FCxr'), threshold_gsize = 1.5)$layers$layerOfInterest
        }
        if (!'pwl' %in% names(lyrs)) lyrs$crust <- lyrs$gtype %in% c('MFcr','IF')
        if (!'crust' %in% names(lyrs)) lyrs$crust <- lyrs$gtype %in% c('MFcr','IF')
        lyrs_ <- split(lyrs, cut(lyrs$height, unique(c(0, quantile(lyrs$height, probs = c(0.33, 0.66)), x$hs + 5))))
        lyrs_ <- rev(lyrs_[sapply(lyrs_, nrow) > 0])
        if (length(lyrs_) > 0) {
          x$pwl_top <- as.integer(any(lyrs_[[1]]$pwl))
          x$pwl_mid <- ifelse(length(lyrs_) > 1, as.integer(any(lyrs_[[2]]$pwl)), 0)
          x$pwl_bottom <- ifelse(length(lyrs_) > 2, as.integer(any(lyrs_[[3]]$pwl)), 0)
          x$crust_top <- as.integer(any(lyrs_[[1]]$crust))
          x$crust_mid <- ifelse(length(lyrs_) > 1, as.integer(any(lyrs_[[2]]$crust)), 0)
          x$crust_bottom <- ifelse(length(lyrs_) > 2, as.integer(any(lyrs_[[3]]$crust)), 0)
        }

        ## Relative threshold sums at top/middle/bottom third of profile
        if ('rta' %in% names(lyrs)) {
          lyrs_ <- split(lyrs, cut(lyrs$depth, c(0, 30, 90, Inf), right = FALSE))
          lyrs_ <- lyrs_[sapply(lyrs_, nrow) > 0]
          x$rta_top <- tryCatch(max(lyrs_$`[0,30)`$rta), warning = function(warn) 0)
          x$rta_mid <- tryCatch(max(lyrs_$`[30,90)`$rta), warning = function(warn) 0)
          x$rta_bottom <- tryCatch(max(lyrs_$`[90,Inf)`$rta), warning = function(warn) 0)
        }
      }
      return(x)
    }
    pstats <- do.call(rbind, lapply(SPx, profileStats))

    ## Normalize distances
    norm_dist <- function(x) {
      if (!all(is.na(x))) {
        if (max(x, na.rm = TRUE) > 0) x <- x / max(x, na.rm = TRUE)
      }
      return(x)
    }
    dist_hs <- norm_dist(dist(pstats$hs))
    dist_hn24 <- norm_dist(dist(pstats$hn24))
    dist_h3d <- norm_dist(dist(pstats$h3d))
    dist_slab <- norm_dist(dist(pstats$slab_hard))
    dist_gtype <- norm_dist(dist(pstats[, c('new','bulk','mf','weak')]))
    dist_gtype_rel <- norm_dist(dist(pstats[, c('new_rel','bulk_rel','mf_rel','weak_rel')]))
    dist_new <- norm_dist(dist(pstats$new))
    dist_pwl <- norm_dist(dist(pstats[, c('pwl_top','pwl_mid','pwl_bottom')]))
    dist_crust <- norm_dist(dist(pstats[, c('crust_top','crust_mid','crust_bottom')]))
    dist_rta <- norm_dist(dist(pstats[, c('rta_top','rta_mid','rta_bottom')]))

    ## Weighted average of distances
    mean_dist <- function(dist_list, weights) {
      weights <- weights / sum(weights)
      propertiesOfInterest <- which(weights != 0)
      dist_list <- dist_list[propertiesOfInterest]
      weights <- weights[propertiesOfInterest]
      dist_list <- lapply(seq_along(dist_list), function(i) weights[i] * dist_list[[i]])
      dist_reduced <- Reduce('+', dist_list)
      if (any(is.na(dist_reduced))) {
        problemProfiles <- unname(which(rowSums(as.matrix(dist_reduced), na.rm = TRUE) == 0))
        stop(paste0('Error with profile(s) # ', paste0(problemProfiles, collapse = ', '), '. Not all distances computed.'))
      }
      return(dist_reduced)
    }
    distmat <- mean_dist(list(dist_hs, dist_hn24, dist_h3d, dist_slab,
                              dist_gtype, dist_gtype_rel, dist_new,
                              dist_pwl, dist_crust, dist_rta),
                         fast_summary_weights[c('w_hs', 'w_hn24', 'w_h3d', 'w_slab',
                                                'w_gtype', 'w_gtype_rel', 'w_new',
                                                'w_pwl', 'w_crust', 'w_rta')])
  }

  ## Names rows and columns
  if (is.null(names(SPx))) {
    pnames <- seq(nrow(distmat))
  } else {
    pnames <- names(SPx)
  }

  ## Either return the results as class dist or as a matrix
  if (output == 'dist') {
    distmat <- as.dist(distmat)
    names(distmat) <- pnames
  }
  if (output == 'matrix') {
    distmat <- as.matrix(distmat)
    rownames(distmat) <- colnames(distmat) <- pnames
  }
  return(distmat)
}

