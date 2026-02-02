#' Rescale and resample a snow profile list
#'
#' Rescale and resample all snow profiles provided in a list to an identical snow height and resampling rate.
#'
#' @param SPx list of `snowprofile` objects
#' @param resamplingRate resampling rate, units in centimeters
#' @param scHeight a function that calculates the resulting height from the profiles, default `median`
#' @param ... arguments passed on to the function provided in `scHeight`
#'
#' @return A list with the first entry `$set` storing the rescaled and resampled profile list, the second entry
#' `$maxHS` stores the maximum snow height found among the profiles
#'
#' @author fherla
#'
#' @examples
#'
#' ## let's take the 'SPgroup' object as profile list
#' SPrr <- reScaleSampleSPx(SPgroup)
#' print(paste0("max height before rescaling: ", SPrr$maxHS, " cm"))
#' print(paste0("rescaled height: ", SPrr$set[[1]]$hs, " cm"))
#' plot(SPrr$set, SortMethod = 'unsorted')
#'
#' @export
reScaleSampleSPx <- function(SPx, resamplingRate = 0.5, scHeight = median, ...) {

  ## ---- assertions ----
  if (!is.list(SPx)) stop("SPx needs to be a list")
  sapply(SPx, function(x) if (!is.snowprofile(x)) stop("At least one element in SPx is not a snowprofile"))

  ## ---- rescale and resample ----

  ## retrieve heights of individual profiles
  profileHeights <- as.double(unlist(lapply(SPx, function(x) x$hs)))
  if (any(is.na(profileHeights))) warning("There is at least one NA profile height (...$hs) in SPx!")

  ## calculate rescale height
  dots <- list(...)
  if (length(dots) == 0) rescaleHS <- scHeight(profileHeights)
  else rescaleHS <- scHeight(profileHeights, ...)
  maxHS <- max(profileHeights)

  ## first rescale, then resample
  SPxRR <- lapply(lapply(SPx, function(x) scaleSnowHeight(x, height = rescaleHS)$queryScaled), function(y) resampleSP(y, h = resamplingRate))

  ## ensure to return a 'snowprofileSet'
  SPxRR <- snowprofileSet(SPxRR)

  return(list(set = SPxRR, maxHS = maxHS))
}
