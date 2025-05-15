#' Parameters for resampling and training a dataset.
#'
#' The \code{ph_ctrl} function automatically generates a \code{trControl} object. This can be used in the \code{train}
#' function to automatically tune hyperparameters for every classification model in the ensemble.
#'
#' @param class A \code{factor} value for training data classes.
#' @param resample_method A \code{character} value for the resampling training method: "boot" (default), "cv", LOOCV", "repeatedcv".
#' @param number An \code{integer} value for the number of resampling iterations (25 default for boot) or folds (10 default for cross-validation).
#' @param repeats An \code{integer} value for the number of sets of folds for repeated cross-validation.
#' @param search A \code{character} value for the hyperparameter search strategy: "random" (default), "grid".
#' @param sampling A \code{character} value for the sampling strategy, sometimes used to fix class imbalances: \code{NULL} (default), "up", "down", "smote".
#' @returns A \code{trainControl} object for the \code{train} function.
#' @export
#' @examples
#' ## Import data.
#' data(ph_crocs)
#' ## Echo control object for train function.
#' ctrl <- ph_ctrl(ph_crocs$Species, resample_method = "boot")
ph_ctrl <- function(class, resample_method = "boot",
                    number = ifelse(grepl("cv", resample_method,
                                          ignore.case = TRUE), 10, 25),
                    repeats = ifelse(grepl("dcv$", resample_method,
                                           ignore.case = TRUE), 3, NA),
                    search = "random",
                    sampling = NULL)
{
    if (!is.factor(class)) { class <- as.factor(class) }
    if (!(resample_method %in% c("boot", "cv", "LOOCV", "repeatedcv")))
        stop("Resampling method does not exist.")
    if (!is.numeric(number))
        stop(paste("Number of resampling iterations or folds must be",
                   "numeric (an integer)."))
    if (!is.na(repeats))
        if (!is.numeric(repeats))
            stop("Number of repeats must be numeric (an integer).")
    if (!(search %in% c("random", "grid")))
        stop("Search strategy does not exist.")
    if (!is.null(sampling)) {
        if (!(sampling %in% c("up", "down", "smote")))
            stop("Sampling strategy does not exist.")
    } else {
        sampling = sampling
    }
    if (resample_method == "boot") {
        # trainControl echo for bootstrap resampling.
        ctrl <- caret::trainControl(method = resample_method,
                                    number = number,
                                    classProbs = TRUE,
                                    savePredictions = TRUE,
                                    sampling = sampling,
                                    search = search,
                                    summaryFunction = ifelse(length(levels(class)) > 2,
                                                      caret::multiClassSummary,
                                                      caret::twoClassSummary))
    } else {
        # trainControl echo for cross-validation resampling.
        ctrl <- caret::trainControl(method = resample_method,
                                    number = number,
                                    repeats = repeats,
                                    classProbs = TRUE,
                                    savePredictions = TRUE,
                                    sampling = sampling,
                                    search = search,
                                    summaryFunction = ifelse(length(levels(class)) > 2,
                                                      caret::multiClassSummary,
                                                      caret::twoClassSummary))

    }
    return(ctrl)
}
