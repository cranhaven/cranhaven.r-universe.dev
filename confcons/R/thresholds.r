#' Thresholds needed to create the extended confusion matrix
#'
#' Calculate the two thresholds distinguishing certain negatives/positives from
#' uncertain predictions. The thresholds are needed to create the extended
#' confusion matrix and are further used for confidence calculation.
#'
#' @param observations Either an integer or logical vector containing the binary
#'   observations where presences are encoded as \code{1}s/\code{TRUE}s and
#'   absences as \code{0}s/\code{FALSE}s.
#' @param predictions A numeric vector containing the predicted probabilities of
#'   occurrence typically within the \code{[0, 1]} interval.
#'   \code{length(predictions)} should be equal to \code{length(observations)}
#'   and the order of the elements should match. \code{predictions} is optional:
#'   needed and used only if \code{type} is 'mean' and ignored otherwise.
#' @param type A character vector of length one containing the value 'mean' (for
#'   calculating mean of the predictions within known presences and absences) or
#'   'information' (for calculating thresholds based on relative information
#'   gain) . Defaults to 'mean'.
#' @param range A numeric vector of length one containing a value from the
#'   \code{]0, 0.5]} interval. It is the parameter of the information-based
#'   method and is used only if \code{type} is 'information'. The larger the
#'   \code{range} is, the more predictions are treated as uncertain. Defaults to
#'   0.5.
#' @return A named numeric vector of length 2. The first element
#'   ('\code{threshold1}') is the mean of probabilities predicted to the absence
#'   locations distinguishing certain negatives (certain absences) from
#'   uncertain predictions. The second element ('\code{threshold2}') is the mean
#'   of probabilities predicted to the presence locations distinguishing certain
#'   positives (certain presences) from uncertain predictions. For a typical
#'   model better than the random guess, the first element is smaller than the
#'   second one. The returned value might contain \code{NaN}(s) if the number of
#'   observed presences and/or absences is 0.
#' @examples
#' set.seed(12345)
#'
#' # Using logical observations:
#' observations_1000_logical <- c(rep(x = FALSE, times = 500),
#'                                rep(x = TRUE, times = 500))
#' predictions_1000 <- c(runif(n = 500, min = 0, max = 0.7),
#'                       runif(n = 500, min = 0.3, max = 1))
#' thresholds(observations = observations_1000_logical,
#'            predictions = predictions_1000) # 0.370 0.650
#'
#' # Using integer observations:
#' observations_4000_integer <- c(rep(x = 0L, times = 3000),
#'                                rep(x = 1L, times = 1000))
#' predictions_4000 <- c(runif(n = 3000, min = 0, max = 0.8),
#'                       runif(n = 1000, min = 0.2, max = 0.9))
#' thresholds(observations = observations_4000_integer,
#'            predictions = predictions_4000) # 0.399 0.545
#'
#' # Wrong parameterization:
#' try(thresholds(observations = observations_1000_logical,
#'                predictions = predictions_4000)) # error
#' set.seed(12345)
#' observations_4000_numeric <- c(rep(x = 0, times = 3000),
#'                                rep(x = 1, times = 1000))
#' predictions_4000_strange <- c(runif(n = 3000, min = -0.3, max = 0.4),
#'                               runif(n = 1000, min = 0.6, max = 1.5))
#' try(thresholds(observations = observations_4000_numeric,
#'                predictions = predictions_4000_strange)) # multiple warnings
#' mask_of_normal_predictions <- predictions_4000_strange >= 0 & predictions_4000_strange <= 1
#' thresholds(observations = as.integer(observations_4000_numeric)[mask_of_normal_predictions],
#'            predictions = predictions_4000_strange[mask_of_normal_predictions]) # OK
#' @note \code{thresholds()} should be called using the whole dataset containing
#'   both training and evaluation locations.
#' @seealso \code{\link{confidence}} for calculating confidence,
#'   \code{\link{consistency}} for calculating consistency
#' @export
thresholds <- function(observations, predictions = NULL, type = "mean", range = 0.5) {

	# Checking parameters

	if (missing(observations)) stop("Parameter 'observations' should be set.")
	if (is.logical(observations)) observations <- as.integer(observations)
	if (!is.integer(observations)) {
		warning("I found that parameter 'observations' is not an integer or logical vector. Coercion is done.")
		observations <- as.integer(observations)
	}
	if (!all(observations[is.finite(observations)] %in% 0:1)) stop("Parameter 'observations' should contain 0s (absences) and 1s (presences).")
	if (!is.character(type)) stop("Parameter 'type' should be a character vector of length one.")
	if (length(type) < 1) stop("Parameter 'type' should be a vector of length one.")
	if (length(type) > 1) warning(paste0("Parameter 'type' has more elements (", as.character(length(type)), ") then expected (1). Only the first element is used."))
	if (!type[1] %in% c("mean", "information")) stop("Parameter 'type' must be 'mean' or 'information'.")
	if (type[1] == "information") {
		if (!is.numeric(range)) {
			warning("I found that parameter 'range' is not a numeric vector. Coercion is done.")
			range <- as.numeric(range)
		}
		if (length(range) < 1) stop("Parameter 'range' should be a vector of length one.")
		if (length(range) > 1) warning(paste0("Parameter 'range' has more elements (", as.character(length(range)), ") then expected (1). Only the first element is used."))
		if (is.na(range[1]) | range[1] <= 0 | range[1] > 0.5) stop(paste0("Parameter 'range' is expected to fall within the ]0, 0.5] interval, but found to be ", format(x = round(x = range, digits = 3), nsmall = 3), "."))
	}
	if (type[1] == "mean") {
		if (missing(predictions)) stop("Parameter 'predictions' should be set if parameter 'type' is 'mean'.")
		if (!is.numeric(predictions)) {
			warning("I found that parameter 'predictions' is not a numeric vector. Coercion is done.")
			predictions <- as.numeric(predictions)
		}
		if (any(predictions[is.finite(predictions)] < 0) | any(predictions[is.finite(predictions)] > 1)) warning("Strange predicted values found. Parameter 'predictions' preferably contains numbers falling within the [0,1] interval.")
		if (length(observations) != length(predictions)) stop("The length of the two parameters ('observations' and 'predictions') should be the same.")
	}

	# Calculation

	if (type[1] == "mean") {
		threshold1 <- mean(x = predictions[observations == 0], na.rm = TRUE)
		threshold2 <- mean(x = predictions[observations == 1], na.rm = TRUE)
	} else {
		prevalence <- sum(observations) / length(observations)
		resolution <- getOption("confcons_information_resolution")
		predictions <- seq(from = 0 + resolution, to = 1 - resolution, by = resolution)
		rel_information_gain <- predictions * log(predictions / prevalence) + (1 - predictions) * log((1 - predictions) / (1 - prevalence))
		rel_information_gain <- rel_information_gain / log(ifelse(test = predictions < prevalence, yes = 1 - prevalence, no = 1 / prevalence))
		threshold1 <- max(predictions[rel_information_gain < (-1) * range[1]])
		threshold2 <- max(predictions[rel_information_gain < range[1]])
	}
	return(c(threshold1 = threshold1, threshold2 = threshold2))

}
