#' Get index of appropriate initial condition average profile
#'
#' To average a set of snow profiles, [dbaSP] requires a snow profile as initial condition (IC) to start the algorithm. To prevent persistent weak layers (PWLs) and crusts
#' from being averaged-out during the call to `dbaSP`, it is advised to start the algorithm with a best-guess IC. This best guess IC contains a large number of PWLs and crusts to ensure
#' that the most prevalent ones actually make their way into the final average profile. This function helps to choose meaningful IC profiles. See Details or (better) the source code
#' for how this function picks the profiles.
#'
#' This function first computes how many PWLs and how many crusts are in the profiles that have a close to median total snow height HS.
#' Each of these profile is then divided into several vertical levels (by [sarp.snowprofile::numberOfPWLsPerVerticalLevel]).
#' nPWL and nCR profiles are then randomly picked from the profiles that have PWLs or CR in most vertical levels and additionally have a rather large number of PWLs/CR overall.
#' The larger `n`, the more profiles with decreasing number of PWLs/CR in different levels are also returned. Note that this function is best applied
#' to large profile sets to obtain semi-random results. For small sets, the indices returned can actually be deterministic since the pool of relevant profiles is too small.
#'
#' @importFrom stats na.omit
#'
#' @param set a [sarp.snowprofile::snowprofileSet]
#' @param n number of profile indices to be picked (i.e., returned)
#' @param classifyPWLs an argument list for a function call to [sarp.snowprofile::findPWL] which returns relevant PWLs for identifying initial conditions
#' @param classifyCRs an argument list for a function call to [sarp.snowprofile::findPWL] which returns relevant CR(ust)s for identifying initial conditions
#' @param nPWL number of profile indices to be picked from profiles that have many PWLs in many different vertical levels; an analogous `nCR` will be the difference `n - nPWL`.
#' @param sm a (precomputed) summary of the `set`
#'
#' @return `n` number of indices that correspond to profiles in the `set`
#' @author fherla
#'
#' @seealso [sarp.snowprofile::findPWL], [averageSP]
#'
#' @examples
#' plot(SPgroup, SortMethod = "unsorted", TopDown = TRUE,
#'      xticklabels = "originalIndices", main = "entire profile set")
#' IC_ids_pwl <- chooseICavg(SPgroup, n = 4, nPWL = 4,
#'                           classifyPWLs = list(pwl_gtype = c("SH", "DH")),
#'                           classifyCRs = NULL)
#' plot(SPgroup[IC_ids_pwl], SortMethod = "unsorted", hardnessResidual = 0, TopDown = TRUE,
#'      xticklabels = IC_ids_pwl, main = "sample of profiles with rather many and distributed PWLs")
#'
#' @export
chooseICavg <- function(set, n, classifyPWLs, classifyCRs,
                        nPWL = round((2*n/3) + 0.001),
                        sm = summary(set)) {

  if (!is.snowprofileSet(set)) stop("'set' needs to be a snowprofileSet")
  if (n < nPWL) stop("'n' needs to be larger or equal to 'nPWL'")
  if (all(is.null(classifyCRs))) nPWL <- n
  nCR <- n - nPWL

  ## which profiles of the set have an appropriate hs to be an average profile?
  ## -> hs 25% around median hs (IQR)
  medianHS <- median(sm$hs)
  rangeHS <- 0.25
  ids2choose <- which(sm$hs > (1-rangeHS)*medianHS & sm$hs < (1+rangeHS)*medianHS)
  if (length(ids2choose) == 0) {  # no single profile within IQR of snow heights --> pick profiles centered around (i.e. greater and smaller than) median snow height
    offsetFromMedian <- medianHS - sm$hs
    ids2choose <- c(which.min(offsetFromMedian*NA**(offsetFromMedian < 0)),
                    which.max(offsetFromMedian*NA**(offsetFromMedian > 0)))
  }

  ## PWLs ###############
  ## count number of PWLs
  sm[ids2choose, "nPWL"] <- sapply(set[ids2choose], function(x) {
    length(do.call("findPWL", c(list(x = x), classifyPWLs)))
  })

  ## separate profile into several levels and count how many levels contain PWLs
  sm[ids2choose, "spreadPWL"] <- sapply(set[ids2choose], function(x) {
    tabPWL <- numberOfPWLsPerVerticalLevel(x, do.call("findPWL", c(list(x = x), classifyPWLs)))
    length(tabPWL[tabPWL != 0])
  })

  ## CRUSTS #############
  ## count number of crusts
  sm[ids2choose, "nCR"] <- sapply(set[ids2choose], function(x) {
    length(do.call("findPWL", c(list(x = x), classifyCRs)))
  })

  ## separate profile into several levels and count how many levels contain crusts
  sm[ids2choose, "spreadCR"] <- sapply(set[ids2choose], function(x) {
    tabCR <- numberOfPWLsPerVerticalLevel(x, do.call("findPWL", c(list(x = x), classifyCRs)))
    length(tabCR[tabCR != 0])
  })

  ## SAMPLING ############
  ## order an index vector according to 'spread' and 'nPWL'
  score_order_PWL <- order(sm$spreadPWL, sm$nPWL, decreasing = TRUE)
  score_order_CR <- order(sm$spreadCR, sm$nCR, decreasing = TRUE)

  ## retrieve indices of profiles with PWLs/CR in most vertical levels
  idx_max_spread_PWL <- which(sm$spreadPWL == max(sm$spreadPWL, na.rm = TRUE))
  idx_max_spread_CR <- which(sm$spreadCR == max(sm$spreadCR, na.rm = TRUE))
  n_max_spread_PWL <- length(idx_max_spread_PWL)
  n_max_spread_CR <- length(idx_max_spread_CR)

  ## among these profiles, which tend to have more PWLs/CR?
  n_median_nPWL <- length(which(sm$spreadPWL == max(sm$spreadPWL, na.rm = TRUE) &
                                  sm$nPWL >= median(sm$nPWL[idx_max_spread_PWL], na.rm = TRUE)))
  n_median_nCR <- length(which(sm$spreadCR == max(sm$spreadCR, na.rm = TRUE) &
                                 sm$nCR >= median(sm$nCR[idx_max_spread_CR], na.rm = TRUE)))

  ## depending on the requested n, ensure that sampling is done from the highest priority indices
  pick_from_first_PWL <- ifelse(nPWL > n_median_nPWL,
                                ifelse(nPWL > n_max_spread_PWL, nPWL, n_max_spread_PWL),
                                n_median_nPWL)
  pick_from_first_CR <- ifelse(nCR > n_median_nCR,
                               ifelse(nCR > n_max_spread_CR, nCR, n_max_spread_CR),
                               n_median_nCR)

  # and randomly pick profiles from the previously computed pool of highest priority indices
  IC_ids_PWL <- sample(score_order_PWL[1:pick_from_first_PWL], size = nPWL)
  IC_ids_CR <- sample(score_order_CR[1:pick_from_first_CR], size = nCR)

  return(unique(na.omit(c(IC_ids_PWL, IC_ids_CR))))
}
