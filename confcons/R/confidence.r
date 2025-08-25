#' Confidence of the predictive distribution model
#'
#' Calculate the confidence in positive predictions within known presences (CPP,
#' \code{type = "positive"}) or confidence in predictions within known presences
#' (CP, \code{type = "neutral"}) based on the occurrence \code{observations},
#' the \code{predictions} of the probability of occurrence, and the two
#' \code{thresholds} distinguishing certain negatives/positives from uncertain
#' predictions.
#'
#' @param observations Either an integer or logical vector containing the binary
#'   observations where presences are encoded as \code{1}s/\code{TRUE}s and
#'   absences as \code{0}s/\code{FALSE}s.
#' @param predictions A numeric vector containing the predicted probabilities of
#'   occurrence typically within the \code{[0, 1]} interval.
#'   \code{length(predictions)} should be equal to \code{length(observations)}
#'   and the order of the elements should match.
#' @param thresholds A numeric vector of length two, typically calculated by
#'   \code{\link{thresholds}()}. The first element distinguishes certain
#'   negatives (certain absences) from uncertain predictions. The second element
#'   distinguishes certain positives (certain presences) from uncertain
#'   predictions. If missing, \code{confcons::thresholds(observations =
#'   observations, predictions = predictions)} is called, but see section 'Note'
#'   about why you should not use the default value.
#' @param type A character vector of length one containing the value "positive"
#'   (for calculating \emph{confidence in positive predictions} within known
#'   presences (CPP)) or "neutral" (for calculating \emph{confidence in
#'   predictions} within known presences (CP)). Defaults to "positive".
#' @return A numeric vector of length one. It is either NA_real_ or a positive
#'   number within the \code{[0, 1]} interval. Larger value indicates that the
#'   model is more confident.
#' @examples
#' set.seed(12345)
#'
#' # Using logical observations, default 'thresholds' and 'type' parameter:
#' observations_1000_logical <- c(rep(x = FALSE, times = 500),
#'                                rep(x = TRUE, times = 500))
#' predictions_1000 <- c(runif(n = 500, min = 0, max = 0.7),
#'                       runif(n = 500, min = 0.3, max = 1))
#' confidence(observations = observations_1000_logical,
#'            predictions = predictions_1000) # 0.561
#'
#' # Using integer observations, default 'thresholds' parameter,
#' # both 'positive' and 'neutral' confidence type:
#' observations_4000_integer <- c(rep(x = 0L, times = 3000),
#'                                rep(x = 1L, times = 1000))
#' predictions_4000 <- c(runif(n = 3000, min = 0, max = 0.8),
#'                       runif(n = 1000, min = 0.2, max = 0.9))
#' confidence(observations = observations_4000_integer,
#'            predictions = predictions_4000, type = "positive") # 0.691
#' confidence(observations = observations_4000_integer,
#'            predictions = predictions_4000, type = "neutral") # 0.778
#'
#' # Using some previously selected thresholds:
#' strict_thresholds <- c(0.1, 0.9)
#' permissive_thresholds <- c(0.4, 0.5)
#' percentile_thresholds <- quantile(x = predictions_4000[observations_4000_integer == 1],
#'                                   probs = c(0.1, 0.9)) # 10th and 90th percentile
#' confidence(observations = observations_4000_integer,
#'            predictions = predictions_4000,
#'            thresholds = strict_thresholds,
#'            type = "neutral") # 0
#' confidence(observations = observations_4000_integer,
#'            predictions = predictions_4000,
#'            thresholds = permissive_thresholds,
#'            type = "neutral") # 0.836
#' confidence(observations = observations_4000_integer,
#'            predictions = predictions_4000,
#'            thresholds = percentile_thresholds,
#'            type = "neutral") # 0.2
#'
#' # Real-life case
#' # (thresholds calculated from the whole dataset, confidence from the evaluation subset):
#' dataset <- data.frame(
#'   observations = observations_4000_integer,
#'   predictions = predictions_4000,
#'   evaluation_mask = c(rep(x = FALSE, times = 250),
#'                       rep(x = TRUE, times = 250),
#'                       rep(x = FALSE, times = 250),
#'                       rep(x = TRUE, times = 250))
#' )
#' thresholds_whole <- thresholds(observations = dataset$observations,
#'                                predictions = dataset$predictions)
#' (confidence_evaluation <- confidence(observations = dataset$observations[dataset$evaluation_mask],
#'                                      predictions = dataset$predictions[dataset$evaluation_mask],
#'                                      thresholds = thresholds_whole)) # 0.671
#'
#' # Wrong parameterization:
#' try(confidence(observations = observations_1000_logical,
#'                predictions = predictions_1000,
#'                type = "pos")) # error
#' try(confidence(observations = observations_1000_logical,
#'                predictions = predictions_1000,
#'                thresholds = c(0.2, NA_real_))) # warning
#' try(confidence(observations = observations_1000_logical,
#'                predictions = predictions_1000,
#'                thresholds = c(-0.4, 0.85))) # warning
#' try(confidence(observations = observations_1000_logical,
#'                predictions = predictions_1000,
#'                thresholds = c(0.6, 0.3))) # warning
#' try(confidence(observations = observations_1000_logical,
#'                predictions = predictions_4000)) # error
#' set.seed(12345)
#' observations_4000_numeric <- c(rep(x = 0, times = 3000),
#'                                rep(x = 1, times = 1000))
#' predictions_4000_strange <- c(runif(n = 3000, min = -0.3, max = 0.4),
#'                               runif(n = 1000, min = 0.6, max = 1.5))
#' try(confidence(observations = observations_4000_numeric,
#'                predictions = predictions_4000_strange,
#'                thresholds = c(0.2, 0.7))) # multiple warnings
#' mask_of_normal_predictions <- predictions_4000_strange >= 0 & predictions_4000_strange <= 1
#' confidence(observations = as.integer(observations_4000_numeric)[mask_of_normal_predictions],
#'            predictions = predictions_4000_strange[mask_of_normal_predictions],
#'            thresholds = c(0.2, 0.7)) # OK
#' @note Technically, confidence can be calculated for the training subset, the
#'   evaluation subset, or the whole dataset as well. Note, however, that there
#'   is not so much sense to calculate confidence in the training subset, except
#'   for using the result for \code{\link{consistency}} calculation. If you need
#'   only the confidence measure, calculate it on the evaluation subset using
#'   \code{\link{thresholds}} previously determined on the whole dataset (i.e.,
#'   do not use the default value of parameter \code{thresholds}). See the last
#'   example below and the vignette.
#' @seealso \code{\link{thresholds}} for calculating the two thresholds,
#'   \code{\link{consistency}} for calculating consistency
#' @export
confidence <- function(observations, predictions, thresholds = confcons::thresholds(observations = observations, predictions = predictions), type = "positive") {

	# Checking parameters
	if (missing(observations) | missing(predictions)) stop("Both parameter 'observations' and 'predictions' should be set.")
	if (is.logical(observations)) observations <- as.integer(observations)
	if (!is.integer(observations)) {
		warning("I found that parameter 'observations' is not an integer or logical vector. Coercion is done.")
		observations <- as.integer(observations)
	}
	if (!all(observations[is.finite(observations)] %in% 0:1)) stop("Parameter 'observations' should contain 0s (absences) and 1s (presences).")
	if (!is.numeric(predictions)) {
		warning("I found that parameter 'predictions' is not a numeric vector. Coercion is done.")
		predictions <- as.numeric(predictions)
	}
	if (any(predictions[is.finite(predictions)] < 0) | any(predictions[is.finite(predictions)] > 1)) warning("Strange predicted values found. Parameter 'predictions' preferably contains numbers falling within the [0,1] interval.")
	if (length(observations) != length(predictions)) stop("The length of parameters 'observations' and 'predictions' should be the same.")
	if (!is.numeric(thresholds)) {
		warning("I found that parameter 'thresholds' is not a numeric vector. Coercion is done.")
		thresholds <- as.numeric(thresholds)
	}
	if (length(thresholds) < 2) stop("Parameter 'thresholds' should be a vector of length two.")
	if (length(thresholds) > 2) warning(paste0("Parameter 'thresholds' has more elements (", as.character(length(thresholds)), ") then expected (2). Only the first two elements are used."))
	if (any(!is.finite(thresholds), thresholds < 0, thresholds > 1)) warning(paste0("Parameter 'thresholds' is expected to contain numbers falling within the [0, 1] interval, but found to contain ", format(x = round(x = thresholds[1], digits = 3), nsmall = 3), " and ", format(x = round(x = thresholds[2], digits = 3), nsmall = 3), "."))
	if (all(is.finite(thresholds)) & thresholds[1] >= thresholds[2]) warning(paste0("Parameter 'thresholds' is expected to contain two numbers increasing strictly monotonously, i.e. thresholds[1] < thresholds[2], but found to contain ", format(x = round(x = thresholds[1], digits = 3), nsmall = 3), " and ", format(x = round(x = thresholds[2], digits = 3), nsmall = 3), ", respectively."))
	if (!is.character(type)) stop("Parameter 'type' should be a character vector of length one.")
	if (length(type) < 1) stop("Parameter 'type' should be a vector of length one.")
	if (length(type) > 1) warning(paste0("Parameter 'type' has more elements (", as.character(length(type)), ") then expected (1). Only the first element is used."))
	if (!type[1] %in% c("positive", "neutral")) stop("Parameter 'type' must be 'positive' or 'neutral'.")

	# Calculation
	occurrence_mask <- observations == 1
	TP <- sum(as.integer(occurrence_mask & predictions > thresholds[2]), na.rm = TRUE)
	if (type[1] == "positive") {
		UP <- sum(as.integer(occurrence_mask & predictions > thresholds[1] & predictions <= thresholds[2]), na.rm = TRUE)
		return(if (TP + UP == 0) NA_real_ else TP / (TP + UP))
	} else {
		P <- sum(as.integer(occurrence_mask), na.rm = TRUE)
		FN <- sum(as.integer(occurrence_mask & predictions <= thresholds[1]), na.rm = TRUE)
		return(if (P == 0) NA_real_ else (TP + FN) / P)
	}

}
