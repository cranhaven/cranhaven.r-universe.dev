#' Goodness-of-fit, confidence and consistency measures
#'
#' Wrapper function for calculating the predictive distribution model's
#' \code{\link{confidence}}, \code{\link{consistency}}, and optionally some
#' well-known goodness-of-fit measures as well. The calculated measures are as
#' follows: \itemize{ \item confidence in predictions (CP) and confidence in
#' positive predictions (CPP) within known presences for the training and
#' evaluation subsets \item consistency of predictions (difference of CPs; DCP)
#' and positive predictions (difference of CPPs; DCPP) \item Area Under the ROC
#' Curve (AUC) - optional (see parameter \code{goodness}) \item maximum of the
#' True Skill Statistic (maxTSS) - optional (see parameter \code{goodness})}
#'
#' @param observations Either an integer or logical vector containing the binary
#'   observations where presences are encoded as \code{1}s/\code{TRUE}s and
#'   absences as \code{0}s/\code{FALSE}s.
#' @param predictions A numeric vector containing the predicted probabilities of
#'   occurrence typically within the \code{[0, 1]} interval.
#'   \code{length(predictions)} should be equal to \code{length(observations)}
#'   and the order of the elements should match.
#' @param evaluation_mask A logical vector (mask) of the evaluation subset. Its
#'   \code{i}th element indicates whether the  \code{i}th element of
#'   \code{observations} was used for evaluation (\code{TRUE}) or for training
#'   (\code{FALSE}). \code{length(evaluation_mask)} should be equal to
#'   \code{length(observations)} and the order of the elements should match,
#'   i.e. \code{observations[evaluation_mask]} were the evaluation subset and
#'   \code{observations[!evaluation_mask]} were the training subset.
#' @param goodness Logical vector of length one, defaults to \code{FALSE}.
#'   Indicates, whether goodness-of-fit measures (AUC and maxTSS) should be
#'   calculated. If set to \code{TRUE}, external package \pkg{ROCR} (Sing et al.
#'   2005) is needed for the calculation (see section 'Note').
#' @param df Logical vector of length one, defaults to \code{FALSE}. Indicates,
#'   whether the returned value should be a one-row \code{data.frame} that is
#'   \code{rbind()}able if \code{measures()} is called on multiple models in a
#'   \code{for} loop or a \code{lapply()}. See section 'Value' and 'Examples'
#'   for details.
#' @return A named numeric vector (if \code{df} is \code{FALSE}; the default) or
#'   a \code{data.frame} (if \code{df} is \code{TRUE}) of one row.
#'   \code{length()} of the vector or \code{ncol()} of the \code{data.frame} is
#'   6 (if \code{goodness} is \code{FALSE}; the default) or 8 (if
#'   \code{goodness} is \code{TRUE}). The name of the elements/columns are as
#'   follows: \describe{ \item{CP_train}{confidence in predictions within known
#'   presences (CP) for the training subset} \item{CP_eval}{confidence in
#'   predictions within known presences (CP) for the evaluation subset}
#'   \item{DCP}{consistency of predictions (difference of CPs)} \item{CPP_train}{confidence in
#'   positive predictions within known presences (CPP) for the training subset}
#'   \item{CPP_eval}{confidence in positive predictions within known presences
#'   (CPP) for the evaluation subset} \item{DCPP}{consistency of positive
#'   predictions (difference of CPPs)} \item{AUC}{Area Under the ROC Curve (Hanley and McNeil 1982;
#'   calculated by \code{\link[ROCR:performance]{ROCR::performance()}}). This
#'   element/column is available only if parameter '\code{goodness}' is set to
#'   \code{TRUE}. If package \pkg{ROCR} is not available but parameter
#'   '\code{goodness}' is set to \code{TRUE}, the value of AUC is
#'   \code{NA_real_} and a warning is raised.} \item{maxTSS}{Maximum of the True
#'   Skill Statistic (Allouche et al. 2006; calculated by
#'   \code{\link[ROCR:performance]{ROCR::performance()}}). This element/column
#'   is available only if parameter '\code{goodness}' is set to \code{TRUE}. If
#'   package \pkg{ROCR} is not available but parameter '\code{goodness}' is set
#'   to \code{TRUE}, the value of maxTSS is \code{NA_real_} and a warning is
#'   raised.} }
#' @examples
#' set.seed(12345)
#' dataset <- data.frame(
#' 	observations = c(rep(x = FALSE, times = 500),
#'                   rep(x = TRUE, times = 500)),
#' 	predictions_model1 = c(runif(n = 250, min = 0, max = 0.6),
#'                         runif(n = 250, min = 0.1, max = 0.7),
#'                         runif(n = 250, min = 0.4, max = 1),
#'                         runif(n = 250, min = 0.3, max = 0.9)),
#' 	predictions_model2 = c(runif(n = 250, min = 0.1, max = 0.55),
#'                         runif(n = 250, min = 0.15, max = 0.6),
#'                         runif(n = 250, min = 0.3, max = 0.9),
#'                         runif(n = 250, min = 0.25, max = 0.8)),
#' 	evaluation_mask = c(rep(x = FALSE, times = 250),
#' 	                    rep(x = TRUE, times = 250),
#' 	                    rep(x = FALSE, times = 250),
#' 	                    rep(x = TRUE, times = 250))
#' )
#'
#' # Default parameterization, return a vector without AUC and maxTSS:
#' conf_and_cons <- measures(observations = dataset$observations,
#'                           predictions = dataset$predictions_model1,
#'                           evaluation_mask = dataset$evaluation_mask)
#' print(conf_and_cons)
#' names(conf_and_cons)
#' conf_and_cons[c("CPP_eval", "DCPP")]
#'
#' # Calculate AUC and maxTSS as well if package ROCR is installed:
#' if (requireNamespace(package = "ROCR", quietly = TRUE)) {
#'   conf_and_cons_and_goodness <- measures(observations = dataset$observations,
#'                                          predictions = dataset$predictions_model1,
#'                                          evaluation_mask = dataset$evaluation_mask,
#'                                          goodness = TRUE)
#' }
#'
#' # Calculate the measures for multiple models in a for loop:
#' model_IDs <- as.character(1:2)
#' for (model_ID in model_IDs) {
#'   column_name <- paste0("predictions_model", model_ID)
#'   conf_and_cons <- measures(observations = dataset$observations,
#'                             predictions = dataset[, column_name, drop = TRUE],
#'                             evaluation_mask = dataset$evaluation_mask,
#'                             df = TRUE)
#'   if (model_ID == model_IDs[1]) {
#'     conf_and_cons_df <- conf_and_cons
#'   } else {
#'     conf_and_cons_df <- rbind(conf_and_cons_df, conf_and_cons)
#'   }
#' }
#' conf_and_cons_df
#'
#' # Calculate the measures for multiple models in a lapply():
#' conf_and_cons_list <- lapply(X = model_IDs,
#'                              FUN = function(model_ID) {
#'                                column_name <- paste0("predictions_model", model_ID)
#'                                measures(observations = dataset$observations,
#'                                         predictions = dataset[, column_name, drop = TRUE],
#'                                         evaluation_mask = dataset$evaluation_mask,
#'                                         df = TRUE)
#'                              })
#' conf_and_cons_df <- do.call(what = rbind,
#'                             args = conf_and_cons_list)
#' conf_and_cons_df
#' @note Since \pkg{confcons} is a light-weight, stand-alone packages, it does
#'   not import package \pkg{ROCR} (Sing et al. 2005), i.e. installing
#'   \pkg{confcons} does not mean installing \pkg{ROCR} automatically. If you
#'   need AUC and maxTSS (i.e., parameter '\code{goodness}' is set to
#'   \code{TRUE}), you should install \pkg{ROCR} or install \pkg{confcons} along
#'   with its dependencies (i.e., \code{devtools::install_github(repo =
#'   "bfakos/confcons", dependencies = TRUE)}).
#' @references \itemize{ \item Allouche O, Tsoar A, Kadmon R (2006): Assessing
#'   the accuracy of species distribution models: prevalence, kappa and the true
#'   skill statistic (TSS). Journal of Applied Ecology 43(6): 1223-1232.
#'   \doi{10.1111/j.1365-2664.2006.01214.x}. \item Hanley JA, McNeil BJ (1982):
#'   The meaning and use of the area under a receiver operating characteristic
#'   (ROC) curve. Radiology 143(1): 29-36.
#'   \doi{10.1148/radiology.143.1.7063747}. \item Sing T, Sander O, Beerenwinkel
#'   N, Lengauer T. (2005): ROCR: visualizing classifier performance in R.
#'   Bioinformatics 21(20): 3940-3941. \doi{10.1093/bioinformatics/bti623}. }
#' @seealso \code{\link{confidence}} for calculating confidence,
#'   \code{\link{consistency}} for calculating consistency,
#'   \code{\link[ROCR:performance]{ROCR::performance()}} for calculating AUC and
#'   TSS
#' @export
measures <- function(observations, predictions, evaluation_mask, goodness = FALSE, df = FALSE) {

	# Checking parameters
	if (missing(observations) | missing(predictions) | missing(evaluation_mask)) stop("Each of parameters 'observations', 'predictions' and 'evaluation_mask' should be set.")
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
	if (length(observations) != length(predictions)) stop("The length of parameters 'observations' and 'predictions' should be the same.")
	if (!is.logical(evaluation_mask)) {
		warning("I found that parameter 'evaluation_mask' is not a logical vector. Coercion is done.")
		evaluation_mask <- as.logical(evaluation_mask)
	}
	if (length(observations) != length(evaluation_mask)) stop("The length of parameters 'observations' and 'evaluation_mask' should be the same.")
	if (!is.logical(goodness)) {
		warning("I found that parameter 'goodness' is not a logical vector. Coercion is done.")
		goodness <- as.logical(goodness)
	}
	if (length(goodness) < 1) stop("Parameter 'goodness' should be a logical vector of length one.")
	if (length(goodness) > 1) warning(paste0("Parameter 'goodness' has more elements (", as.character(length(goodness)), ") then expected (1). Only the first element is used."))
	if (is.na(goodness[1])) stop("Parameter 'goodness' must not be NA.")
	if (!is.logical(df)) {
		warning("I found that parameter 'df' is not a logical vector. Coercion is done.")
		df <- as.logical(df)
	}
	if (length(df) < 1) stop("Parameter 'df' should be a logical vector of length one.")
	if (length(df) > 1) warning(paste0("Parameter 'df' has more elements (", as.character(length(df)), ") then expected (1). Only the first element is used."))
	if (is.na(df[1])) stop("Parameter 'df' must not be NA.")

	# Calculation
	goodness <- goodness[1]
	df <- df[1]
	if (goodness) {
		if (requireNamespace(package = "ROCR", quietly = TRUE)) {
			tpr <- ROCR::performance(prediction.obj = ROCR::prediction(predictions = predictions[evaluation_mask], labels = observations[evaluation_mask]), measure = "tpr")@y.values[[1]]
			tnr <- ROCR::performance(prediction.obj = ROCR::prediction(predictions = predictions[evaluation_mask], labels = observations[evaluation_mask]), measure = "tnr")@y.values[[1]]
			AUC_and_maxTSS <- c(
				ROCR::performance(prediction.obj = ROCR::prediction(predictions = predictions[evaluation_mask], labels = observations[evaluation_mask]), measure = "auc")@y.values[[1]], # AUC
				max(tpr + tnr - 1) # maxTSS
			)
		} else {
			warning("Althought parameter 'goodness' is set to TRUE, the required package, ROCR, is not available. See help(measures) for further details.")
			AUC_and_maxTSS <- rep(x = NA_real_, times = 2)
		}
	}
	thresholds_whole <- thresholds(observations = observations, predictions = predictions)
	measures <- c(
		c(
			CP_train <- confidence(observations = observations[!evaluation_mask], predictions = predictions[!evaluation_mask], thresholds = thresholds_whole, type = "neutral"),
			CP_eval <- confidence(observations = observations[evaluation_mask], predictions = predictions[evaluation_mask], thresholds = thresholds_whole, type = "neutral"),
			consistency(conf_train = CP_train, conf_eval = CP_eval),
			CPP_train <- confidence(observations = observations[!evaluation_mask], predictions = predictions[!evaluation_mask], thresholds = thresholds_whole, type = "positive"),
			CPP_eval <- confidence(observations = observations[evaluation_mask], predictions = predictions[evaluation_mask], thresholds = thresholds_whole, type = "positive"),
			consistency(conf_train = CPP_train, conf_eval = CPP_eval)
		), if (goodness) AUC_and_maxTSS else numeric(length = 0)
	)
	names <- c(c("CP_train", "CP_eval", "DCP", "CPP_train", "CPP_eval", "DCPP"), if (goodness) c("AUC", "maxTSS") else character(length = 0))
	if (df) {
		measures <- data.frame(matrix(data = measures, nrow = 1, ncol = length(measures), byrow = TRUE))
		colnames(measures) <- names
	} else {
		names(measures) <- names
	}
	return(measures)

}
