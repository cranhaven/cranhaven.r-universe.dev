# Title     : TODO
# Objective : TODO
# Created by: ivan
# Created on: 8/4/21


#' Computes a classification from a pretrained discriminant
#'
#' This function uses a pretrained linear discriminant to classify a set of
#' test data. As output it returns a confusion matrix and optionally the raw
#' classification result.
#'
#' @param model Trained linear discriminant.
#'              see \code{\link{trainModel}}
#' @param test MultiWaveAnalysis class object to be used as test set.
#' @param labels Vector that determines the class to which each of the
#'             observations provided in the test set belongs.
#' @param returnClassification Allows to select if the raw result classification
#'        is returned.
#' @param ... Additional arguments
#'
#' @return * if returnClassification is false return a object of class
#'            confusionMatrix
#'  * if returnClassification is true, it returns a list containing an
#'  object of the confusionMatrix class and a vector with the
#'  classification result.
#'
#' @examples
#' \donttest{
#' load(system.file("extdata/ECGExample.rda",package = "TSEAL"))
#' # The dataset has the first 5 elements of class 1
#' # and the last 5 of class 2.
#' labels <- c(rep(1, 5), rep(2, 5))
#' MWA <- generateStepDiscrim(ECGExample, labels, "haar", maxvars = 5, features = c("var"))
#' aux <- extractSubset(MWA, c(1, 2, 9, 10))
#' MWATest <- aux[[1]]
#' MWATrain <- aux[[2]]
#' ldaDiscriminant <- trainModel(MWATrain, labels[3:8], "linear")
#' CM <- testModel(ldaDiscriminant, MWATest, labels[c(1, 2, 9, 10)])
#' }
#'
#' @seealso
#' \code{\link{testModel}}
#'
#' @importFrom checkmate anyMissing assertFlag
#' @export
testModel <- function(model,
                      test,
                      labels,
                      returnClassification = FALSE,
                      ...) {
    checkmate::anyMissing(c(model, test, labels))
    checkmate::assertFlag(returnClassification)
    stopifnot(is(model, "WaveModel"))
    if (test$Observations != length(labels)) {
        stop(
            "The number of observations in the test set does not correspond to the
         classes provided in the labels parameter."
        )
    }

    if (length(labels) < 2) {
        stop(
            "The minimun numer of observations is 2. If you want to classify only
         one observation use \"classify\" function"
        )
    }

    prediction <- classify(test, model)
    CM <- confusionMatrix(as.factor(prediction), as.factor(labels))

    if (returnClassification) {
        return(list("CM" = CM, "Clasification" = prediction[[1]]))
    }

    return(CM)
}

#' Leave-One-Out Cross Validation
#'
#' This function performs the Leave-One-Out Cross Validation (LOOCV) process
#' with different types of input parameters.
#'
#' @param data Starting data to generate the validation. It can be either the
#'        raw data, or a previously generated MultiWaveAnalysis object.
#' @param ... Additional arguments
#'
#' @return Not return value, used as generic function
#'
#' @seealso
#' * \code{\link{LOOCV.array}}
#' * \code{\link{LOOCV.MultiWaveAnalysis}}
#'
#' @importFrom checkmate anyMissing assertFlag
#' @export
LOOCV <- function(data, ...) {
    UseMethod("LOOCV")
}

#' Generates and validates a discriminant model generated directly from the
#' data.
#'
#' It generates and validates a discriminant model starting from the data.
#' First, a MultiWaveAnalysis object is obtained according to the selected
#' characteristics, filter and levels. Then, the most important features are
#' selected using a stepwise discriminant that allows to select a maximum number
#' of variables (maxvars) or a minimum enhancement step (VStep). Finally, the
#' model is trained using the subset of features and validated using
#' Leave-One-Out Cross Validation (LOOCV).
#'
#' @param data Sample from the population (dim x length x cases)
#' @param labels Labeled vector that classify the observations
#' @param f Selected filter for the MODWT (to see the available filters use the
#'        function \code{\link{availableFilters}}
#' @param method Selected method for the discriminant.
#'        Valid values "linear" "quadratic"
#' @param maxvars Maximum number of variables included by the StepDiscrim
#'        algorithm (Note that if you defined this, can not define VStep).
#'        Must be a positive integer greater than 0.
#' @param VStep Minimum value of V above which all other variables are
#'        considered irrelevant and therefore will not be included. (Note that
#'        if you defined this, can not defined maxvars). Must be a positive
#'        number and greater than 0. For more information see StepDiscrim
#'        documentation
#' @param lev Determines the number of decomposition levels for MODWT
#'        (by default the optimum is calculated using the "conservative"
#'        strategy). Must be a positive integer (including 0 to auto-select
#'        the level)
#' @param features A list of characteristics that will be used for the
#'        classification process. To see the available features see
#'        \code{\link{availableFeatures}}
#' @param returnClassification Allows to select if the raw result classification
#'        is returned.
#' @param nCores Determines the number of processes that will be used in the
#'        function, by default it uses all but one of the system cores. Must be
#'        a positive integer, where 0 corresponds to the default behavior.
#' @param ... Additional arguments
#'
#' @return * if returnClassification is false return a object of class
#'            confusionMatrix
#'  * if returnClassification is true, it returns a list containing an
#'    object of the confusionMatrix class and a vector with the
#'    classification result.
#'
#' @examples
#' \donttest{
#' load(system.file("extdata/ECGExample.rda",package = "TSEAL"))
#' labels <- c(rep(1, 5), rep(2, 5))
#' CM <- LOOCV(ECGExample, labels, "haar", "linear",
#'   maxvars = 5,
#'   features = c("Var"), returnClassification = FALSE
#' )
#' # or with VStep
#' CMV <- LOOCV(ECGExample, labels, "haar", "linear",
#'  VStep = 5,
#'  features = c("Var", "Cor"), returnClassification = FALSE
#' )
#' }
#' @seealso
#' * \code{\link{LOOCV}}
#' * \code{\link{LOOCV.MultiWaveAnalysis}}
#' * \code{\link{availableFilters}}
#' * \code{\link{availableFeatures}}
#'
#' @importFrom checkmate anyMissing
#' @export
LOOCV.array <-
    function(data,
             labels,
             f,
             method,
             maxvars,
             VStep,
             lev = 0,
             features = c("Var", "Cor", "IQR", "PE", "DM"),
             returnClassification = FALSE,
             nCores = 0,
             ...) {
        checkmate::anyMissing(c(data, labels, f, method))


        if (length(features) == 0) {
            stop(
                "At least one feature must be provided. To see the available filters
         use the availableFeatures()"
            )
        }

        if (length(dim(data)) != 3) {
            stop(
                "It seems that a dimension is missing, in case your series contains
         only one case, make sure that you have activated the option
         \"drop = FALSE\" as in the following example
         Series1 = Series2 [,,1, drop = FALSE]."
            )
        }

        checkmate::assertFlag(returnClassification)

        if (missing(maxvars) && missing(VStep)) {
            stop("maxvars o VStep must be defined")
        }

        if (!missing(maxvars) && !missing(VStep)) {
            stop("only maxvars or VStep can be defined.")
        }

        if (!missing(maxvars)) {
            maxvars <- asCount(maxvars, positive = TRUE)
        } else {
            if (!is.numeric(VStep) || length(VStep) != 1 || VStep <= 0) {
                stop("The argument \"VStep\" must be a number greater than 0")
            }
        }

        series <- data

        f <- tolower(f)
        method <- tolower(method)
        features <- tolower(features)

        MWA <-
            generateStepDiscrim(series, labels, f, maxvars, VStep, lev, features,
                                nCores)
        return(LOOCV(MWA, labels, method, returnClassification))
    }

#' LOOCV
#'
#' Performs a leave-one-cross-validation (LOOCV) method on a MultiWaveAnalysis
#' object. It is advisable to have selected a subset of all features
#' (\code{\link{StepDiscrim}},\code{\link{StepDiscrimV}})
#'
#' @param data MultiWaveAnalysis object obtained with MultiWaveAnalysis function and
#'        preferably obtained a subset of its characteristics
#'        (\code{\link{StepDiscrim}}, \code{\link{StepDiscrimV}})
#' @param labels Labeled vector that classify the observations.
#' @param method Selected method for discrimination. Valid options
#'        "linear" "quadratic"
#' @param returnClassification Allows to select if the raw result classification
#'       is returned.
#' @param ... Additional arguments
#'
#' @return * if returnClassification is false return a object of class
#'            confusionMatrix
#'  * if returnClassification is true, it returns a list containing an
#'    object of the confusionMatrix class and a vector with the
#'    classification result.
#'
#' @examples
#' \donttest{
#' load(system.file("extdata/ECGExample.rda",package = "TSEAL"))
#' MWA <- MultiWaveAnalysis(ECGExample, "haar", features = c("var"))
#' MWADiscrim <- StepDiscrim(MWA, c(rep(1, 5), rep(2, 5)), 5,
#'                           features = c("var"))
#' CM <- LOOCV(MWADiscrim, c(rep(1, 5), rep(2, 5)), "linear")
#' }
#'
#' @seealso
#' * \code{\link{LOOCV}}
#' * \code{\link{LOOCV.array}}
#' * \code{\link{StepDiscrim}}
#' * \code{\link{StepDiscrimV}}
#'
#' @export
#'
#' @importFrom caret confusionMatrix
LOOCV.MultiWaveAnalysis <-
    function(data,
             labels,
             method,
             returnClassification = FALSE,
             ...) {
        if (missing(data)) {
            stop("The argument \"data\" must be provided")
        }
        if (missing(labels)) {
            stop("The argument \"labels\" must be provided")
        }
        if (missing(method)) {
            stop("The argument \"method\" must be provided")
        }
        if (is.numeric(returnClassification)) {
            stop("The argument \"returnClassification\" must be a logical value")
        }

        if (length(labels) != data$Observations) {
            stop("The \"labels\" length mismatches with the observations of \"MWA\"")
        }

        stopifnot(is(data, "MultiWaveAnalysis"))

        MWA <- data

        method <- tolower(method)

        n <- MWA$Observations
        class <- vector("numeric", n)
        for (i in seq_len(n)) {
            aux <- extractSubset(MWA, c(i))
            MWATest <- aux[[1]]
            MWATrain <- aux[[2]]
            labelsT <- labels[-i]
            model <- trainModel(MWATrain, labelsT, method)
            class[i] <- classify(MWATest, model)
        }
        CM <- confusionMatrix(as.factor(class), as.factor(labels))

        if (returnClassification) {
            return(list("CM" = CM, "classification" = class))
        }

        return(CM)
    }

#' K-Fold Cross Validation (KFCV)
#'
#' This function performs the K-Fold Cross Validation (KFCV) process
#' with different types of input parameters.
#'
#' @param data Starting data to generate the validation. It can be either the
#'        raw data, or a previously generated MultiWaveAnalysis object.
#' @param ... Additional arguments
#'
#' @return Not return value, used as generic function
#'
#' @seealso
#' * \code{\link{KFCV.array}}
#' * \code{\link{KFCV.MultiWaveAnalysis}}
#' @export
KFCV <- function(data, ...) {
    UseMethod("KFCV")
}


#' Generates and validates a discriminant model generated directly from the
#' data.
#'
#' It generates and validates a discriminant model starting from the data. First
#' , a MultiWaveAnalysis object is obtained according to the selected characteristics
#' ,filter and levels. Then, the most important features are selected using a
#' stepwise discriminant that allows to select a maximum number of variables
#' (maxvars) or a minimum enhancement step (VStep). Finally, the model is
#' trained using the subset of features and validated using
#' K-Fold Cross Validation (KFCV).
#'
#' @param data Sample from the population (dim x length x cases)
#' @param labels Labeled vector that classify the observations
#' @param f Selected filter for the MODWT (to see the available filters use the
#'        function \code{\link{availableFilters}}
#' @param method Selected method for the discriminant.
#'        Valid values "linear" "quadratic"
#' @param maxvars Maximum number of variables included by the StepDiscrim
#'        algorithm (Note that if you defined this, can not define VStep).
#'        Must be a positive integer greater than 0.
#' @param VStep Minimum value of V above which all other variables are
#'        considered irrelevant and therefore will not be included. (Note that
#'        if you defined this, can not defined maxvars). Must be a positive
#'        number and greater than 0. For more information see StepDiscrim
#'        documentation
#' @param k The number of folds in KFCV. Must be a positive integer lower or
#'        equal than the number of observations
#' @param lev Determines the number of decomposition levels for MODWT
#'        (by default the optimum is calculated using the "conservative"
#'        strategy). Must be a positive integer (including 0 to auto-select
#'        the level)
#' @param features A list of characteristics that will be used for the
#'        classification process. To see the available features see
#'        \code{\link{availableFeatures}}
#' @param returnClassification Allows to select if the raw result classification
#'        is returned.
#' @param nCores Determines the number of processes that will be used in the
#'        function, by default it uses all but one of the system cores. Must be
#'        a positive integer, where 0 corresponds to the default behavior.
#' @param ... Additional arguments
#'
#' @return * if returnClassification is false return a object of class
#'            confusionMatrix
#'  * if returnClassification is true, it returns a list containing an
#'    object of the confusionMatrix class and a vector with the
#'    classification result.
#'
#' @examples
#' \donttest{
#' load(system.file("extdata/ECGExample.rda",package = "TSEAL"))
#' labels <- c(rep(1, 5), rep(2, 5))
#' CM <- KFCV(ECGExample, labels, "haar", "linear",
#'   maxvars = 5,
#'   features = c("Var"), returnClassification = FALSE
#' )
#' # or with VStep
#' CMV <- KFCV(ECGExample, labels, "haar", "linear",
#'  k = 5,
#'  VStep = 5,
#'  features = c("Var"), returnClassification = FALSE
#' )
#' }
#' @seealso
#' * \code{\link{LOOCV}}
#' * \code{\link{LOOCV.MultiWaveAnalysis}}
#' * \code{\link{availableFilters}}
#' * \code{\link{availableFeatures}}
#' @export
KFCV.array <-
    function(data,
             labels,
             f,
             method,
             maxvars,
             VStep,
             k = 5L,
             lev = 0L,
             features = c("Var", "Cor", "IQR", "PE", "DM"),
             returnClassification = FALSE,
             nCores = 0,
             ...) {
        checkmate::anyMissing(c(data, labels, f, method, features))

        if (length(features) == 0) {
            stop(
                "At least one feature must be provided. To see the available filters
         use the availableFeatures()"
            )
        }

        if (length(dim(data)) != 3) {
            stop(
                "It seems that a dimension is missing, in case your series contains
         only one case, make sure that you have activated the option
         \"drop = FALSE\" as in the following example
         Series1 = Series2 [,,1, drop = FALSE]."
            )
        }


        if (is.numeric(returnClassification)) {
            stop("The argument \"returnClassification\" must be a logical value")
        }

        if (missing(maxvars) && missing(VStep)) {
            stop("maxvars o VStep must be defined")
        }

        if (!missing(maxvars) && !missing(VStep)) {
            stop("only maxvars or VStep can be defined.")
        }

        if (!missing(maxvars)) {
            if (!is.numeric(maxvars) || length(maxvars) != 1 || maxvars <= 0) {
                stop("The argument \"maxvars\" must be an integer greater than 0")
            }
        } else {
            if (!is.numeric(VStep) || length(VStep) != 1 || VStep <= 0) {
                stop("The argument \"VStep\" must be a number greater than 0")
            }
        }

        k <- checkmate::asCount(k, positive = TRUE)
        lev <- checkmate::asCount(lev)
        nCores <- checkmate::asCount(nCores)


        series <- data

        f <- tolower(f)
        method <- tolower(method)
        features <- tolower(features)

        MWA <-
            generateStepDiscrim(series, labels, f, maxvars, VStep, lev, features,
                                nCores)
        return(KFCV(MWA, labels, method, k, returnClassification))
    }

#' KFCV
#'
#' Performs k-fold cross-validation where groups are chosen randomly.
#' In case the value k is not divisor of the number of observations the last
#' group will have nobs mod k observations.
#'
#' @param data MultiWaveAnalysis (MWA) object obtained with MultiWaveAnalysis and
#'        preferably obtained a subset of its characteristics
#'        (\code{\link{StepDiscrim}},\code{\link{StepDiscrimV}})
#' @param labels labeled vector that classify the observations.
#' @param method Selected method for discrimination. Valid options
#'       "linear" "quadratic"
#' @param k the number of folds in KFCV. Must be a positive integer and lower or
#'        equal than the number of observations
#' @param returnClassification Allows to select if the raw result classification
#'        is returned.
#' @param ... Additional arguments
#'
#' @return * if returnClassification is false return a object of class
#'            confusionMatrix
#'  * if returnClassification is true, it returns a list containing an
#'    object of the confusionMatrix class and a vector with the
#'    classification result.
#' @examples
#' \donttest{
#' load(system.file("extdata/ECGExample.rda",package = "TSEAL"))
#' MWA <- MultiWaveAnalysis(ECGExample, "haar", features = c("var"))
#' MWADiscrim <- StepDiscrim(MWA, c(rep(1, 5), rep(2, 5)), 5,
#'               features = c("var"))
#' CM <- KFCV(MWADiscrim, c(rep(1, 5), rep(2, 5)), "linear", 5,
#'   returnClassification = FALSE
#' )
#' }
#'
#' @importFrom checkmate anyMissing assertFlag
#'
#' @export
KFCV.MultiWaveAnalysis <- function(data,
                              labels,
                              method,
                              k = 5L,
                              returnClassification = FALSE,
                              ...) {
    checkmate::anyMissing(c(data, labels, method))
    checkmate::assertFlag(returnClassification)

    if (length(labels) != data$Observations) {
        stop("The \"labels\" length mismatches with the observations of \"MWA\"")
    }

    k <- asCount(k)

    MWA <- data

    nobs <- MWA$Observations

    if (k > nobs) {
        k <- nobs
        warning(
            "The number of groups is greater than the number of
                  observations. The value of k:",
            k,
            "will be used"
        )
    }

    method <- tolower(method)

    reorderMWA <- MWA
    cols <- sample(nobs)

    for (feature in names(reorderMWA$Features)) {
        if (!all(is.na(reorderMWA$Features[feature]))) {
            reorderMWA$Features[[feature]] <-
                reorderMWA$Features[[feature]][, cols, drop = FALSE]
        }
    }

    reorderGrps <- labels[cols]
    class <- vector("numeric", nobs)
    inc <- floor(nobs / k)
    n <- 1
    m <- inc
    while (m >= n) {
        aux <- extractSubset(reorderMWA, n:m)
        MWATest <- aux[[1]]
        MWATrain <- aux[[2]]
        labelsT <- reorderGrps[-c(n:m)]
        model <- trainModel(MWATrain, labelsT, method)
        class[n:m] <- classify(MWATest, model)
        n <- n + inc
        m <- m + inc
        m <- min(m, nobs)
    }

    CM <- confusionMatrix(as.factor(class), as.factor(reorderGrps))

    if (returnClassification) {
        return(list("CM" = CM, "classification" = class))
    }

    return(CM)
}

#' Generate a Discriminant Model
#'
#' This function allows training of a discriminant model using different inputs
#'
#' @param data Starting data to generate a discriminator (linear or quadratic).
#'        This starting data can be either the raw data, or a MultiWaveAnalysis
#'        object generated earlier.
#' @param ... Additional arguments
#'
#' @return A trained discriminant model
#'
#' @seealso
#' * \code{\link{trainModel.array}}
#' * \code{\link{trainModel.MultiWaveAnalysis}}
#'
#' @export
#'
trainModel <- function(data, ...) {
    UseMethod("trainModel")
}

#' Generates a discriminant model from training data.
#'
#' It generates a discriminant model starting from the training data, which must
#' be provided in 2 groups depending on their classification. The method first
#' obtains the variances and correlations using MODWT, the f filter is applied
#' with a number of levels lev. Then a subset of all the generated features
#' will be obtained by means of a stepwise discriminant, which can be driven
#' by a maximum number of features or by a minimum metric to be met. Finally,
#' the selected discriminant model is trained with the subset obtained.
#'
#' @param data Sample from the population (dim x length x cases)
#' @param labels Labeled vector that classify the observations
#' @param f Selected filter for the MODWT (to see the available filters use the
#'        function availableFilters)
#' @param method Selected method for the discriminant. Valid values
#'        "linear" "quadratic"
#' @param maxvars Maximum number of variables included by the StepDiscrim
#'        algorithm (Note that if you defined this, can not define VStep). Must
#'        be a positive integer greater than 0.
#' @param VStep Minimum value of V above which all other variables are
#'        considered irrelevant and therefore will not be included. (Note that
#'        if you defined this, can not defined maxvars).Must be a positive
#'        number greater than 0. For more information see StepDiscrim
#'        documentation
#' @param lev Determines the number of decomposition levels for MODWT
#'        (by default the optimum is calculated). Must be a positive integer
#' @param features A list of characteristics that will be used for the
#'        classification process. To see the available features
#'        see \code{\link{availableFeatures}}
#' @param nCores Determines the number of processes that will be used in the
#'        function, by default it uses all but one of the system cores. Must be
#'        a positive integer, where 0 corresponds to the default behavior.
#' @param ... Additional arguments
#'
#' @return A discriminant model object (lda or qda)
#'
#' @examples
#' \donttest{
#' load(system.file("extdata/ECGExample.rda",package = "TSEAL"))
#' # The dataset has the first 5 elements of class 1 and the last 5 of class 2.
#' labels <- c(rep(1, 5), rep(2, 5))
#' model <- trainModel(ECGExample, labels, "d6", "linear",
#'   maxvars = 5, features = c("Var")
#' )
#' # or using VStep
#' modelV <- trainModel(ECGExample, labels, "d6", "linear",
#'     VStep = 14.5, features = c("Var")
#' )
#' }
#' @export
#' @seealso
#' * \code{\link{StepDiscrim}}
#' * \code{\link{StepDiscrimV}}
#' * \code{\link{trainModel}}
#' @md
trainModel.array <-
    function(data,
             labels,
             f,
             method,
             maxvars,
             VStep,
             lev = 0,
             features = c("Var", "Cor", "IQR", "PE", "DM"),
             nCores = 0,
             ...) {
        if (missing(data)) {
            stop("The argument \"series\" must be provided")
        }
        if (missing(labels)) {
            stop("The argument \"labels\" must be provided")
        }
        if (missing(f)) {
            stop(
                "The argument \"f\" (filter) must be provided. To see the avaiable
         filters use availableFilters()"
            )
        }

        if (missing(method)) {
            stop(
                "The argument \"method\" must be defined. The available  methods are
         \"linear\" and \"quadratic\""
            )
        }

        if (length(features) == 0) {
            stop(
                "At least one feature must be provided. To see the available filters
         use the availableFeatures()"
            )
        }

        if (length(dim(data)) != 3) {
            stop(
                "It seems that a dimension is missing, in case your series contains
         only one case, make sure that you have activated the option
         \"drop = FALSE\" as in the following example
         Series1 = Series2 [,,1, drop = FALSE]."
            )
        }

        if (length(labels)) {
            if (missing(maxvars) && missing(VStep)) {
                stop("maxvars o VStep must be defined")
            }
        }
        if (!missing(maxvars) && !missing(VStep)) {
            stop("only maxvars or VStep can be defined.")
        }

        if (!missing(maxvars)) {
            if (!is.numeric(maxvars) || length(maxvars) != 1 || maxvars <= 0) {
                stop("The argument \"maxvars\" must be an integer greater than 0")
            }
        } else {
            if (!is.numeric(VStep) || length(VStep) != 1 || VStep <= 0) {
                stop("The argument \"VStep\" must be a number greater than 0")
            }
        }

        series <- data

        f <- tolower(f)
        method <- tolower(method)
        features <- tolower(features)

        MWA <-
            generateStepDiscrim(series, labels, f, maxvars, VStep, lev,
                                features, nCores)
        return(trainModel(MWA, labels, method))
    }
#' Generates a discriminant model from an already generated "MultiWaveAnalysis".
#'
#' @param data A MultiWaveAnalysis object obtained with MultiWaveAnalysis function
#' @param labels Labeled vector that classify the observations.
#' @param method Selected method for discrimination. Valid options are
#'        "linear" and "quadratic"
#' @param ... Additional arguments
#'
#' @return A discriminant model based on selected method. It can be an object of
#'         the class lda or qda.
#'
#' @examples
#' \donttest{
#' load(system.file("extdata/ECGExample.rda",package = "TSEAL"))
#' MWA <- MultiWaveAnalysis(ECGExample, "d6", features = c("Var"))
#' MWADiscrim <- StepDiscrim(MWA, c(rep(1, 5), rep(2, 5)), 5,
#'   features = c("Var")
#' )
#' model <- trainModel(MWADiscrim, c(rep(1, 5), rep(2, 5)), "linear")
#' }
#'
#' @seealso
#' * \code{\link{MultiWaveAnalysis}}
#' * \code{\link{StepDiscrim}}
#' * \code{\link{StepDiscrimV}}
#' @export
#'
#' @importFrom MASS lda qda
trainModel.MultiWaveAnalysis <- function(data, labels, method, ...) {
    MWA <- data
    if (missing(MWA)) {
        stop("The argument \"MWA\" must be provided")
    }
    if (missing(labels)) {
        stop("The argument \"labels\" must be provided")
    }

    if (missing(method)) {
        stop(
            "The argument \"method\" must be defined. The available  methods are
         \"linear\" and \"quadratic\""
        )
    }

    if (length(labels) != data$Observations) {
        stop("The \"labels\" length mismatches with the observations of \"MWA\"")
    }

    if (length(method) > 1) {
        stop("The argument must be a single string
         (\"linear\" or \"quadratic\") not a list")
    }

    stopifnot(is(MWA, "MultiWaveAnalysis"))

    method <- tolower(method)

    values <- values(MWA)
    if (method == "linear") {
        model <- lda(t(values), labels)
    } else if (method == "quadratic") {
        model <- qda(t(values), labels)
    } else {
        stop("Method", as.character(method), "not supported")
    }
    features <- c()
    selected <- list()
    for (feature in names(MWA$StepSelection)) {
        if (!all(is.na(MWA$StepSelection[[feature]]))) {
            features <- c(features,feature)
            selected[[feature]] <- MWA$StepSelection[[feature]]
        }
    }


    x <- list(Model = model, Features = features, Selected = selected,
              NLevels = MWA$NLevels, Filter = MWA$Filter)
    attr(x, "class") <- "WaveModel"
    return(x)
}

#' Classifies observations based on a pretrained model.
#'
#' This function allows to classify observations based on a pretrained model
#' that could have been obtained in several ways (such as using the train model
#' function). T
#'
#' @param data The data to be classified. This  data can be either the raw data
#'             , or a MultiWaveAnalysis object generated earlier.
#' @param ... Additional arguments
#'
#' @return A factor with predicted class of each observation
#'
#' @seealso
#' * \code{\link{trainModel}}
#' * \code{\link{classify.array}}
#' * \code{\link{classify.MultiWaveAnalysis}}
#'
#'
#' @export
classify <- function(data, ...){
    UseMethod("classify")
}

#' Classifies observations based on a pretrained model.
#'
#' This function allows to classify observations based on a pretrained model
#' that could have been obtained in several ways (such as using the train model
#' function).
#'
#' @param data Data to be classified by the model. Remember that it must be an
#'        object of type MultiWaveAnalysis. Note that it should have the same
#'        variables selected as those used to generate the model.
#' @param model pretrained discriminant model (lda or qda)
#' @param ... Additional arguments
#'
#' @return A factor with predicted class of each observation
#'
#' @examples
#' \donttest{
#' load(system.file("extdata/ECGExample.rda",package = "TSEAL"))
#' # We simulate that the second series has been obtained after
#' Series1 <- ECGExample[, , 1:9]
#' Series2 <- ECGExample[, , 10, drop = FALSE]
#'
#' # Training a discriminant model
#' MWA <- MultiWaveAnalysis(Series1, "haar", features = c("var"))
#' MWADiscrim <- StepDiscrim(MWA, c(rep(1, 5), rep(2, 4)), maxvars = 5,
#'                           features = c("var"))
#' model <- trainModel(MWADiscrim, c(rep(1, 5), rep(2, 4)), "linear")
#'
#' # Using the discriminant trained on new data
#' MWA2 <- MultiWaveAnalysis(Series2, "haar", features = c("var"))
#' MWA2Discrim <- SameDiscrim(MWA2, MWADiscrim)
#' prediction <- classify(MWA2Discrim, model)
#' }
#'
#' @seealso
#' * \code{\link{trainModel}}
#'
#' @export
#'
#' @importFrom stats predict
#' @importFrom magrittr %>%
classify.MultiWaveAnalysis <- function(data, model, ...) {
    if (missing(model)) {
        stop("The argument \"model\" must be provided")
    }
    stopifnot(is(data, "MultiWaveAnalysis"))
    stopifnot(is(model, "WaveModel"))

    values <- values(data)
    return(predict(model$Model, t(values))[[1]])
}

#' Classifies observations based on a pretrained model.
#'
#' This function allows to classify observations based on a pretrained model
#' that could have been obtained in several ways (such as using the train model
#' function).
#'
#' @param data Sample from the population (dim x length x cases)
#' @param model pretrained discriminant model (lda or qda)
#' @param ... Additional arguments
#'
#' @return A factor with predicted class of each observation
#'
#' @examples
#' \donttest{
#' load(system.file("extdata/ECGExample.rda",package = "TSEAL"))
#' # We simulate that the second series has been obtained after
#' Series1 <- ECGExample[, , 1:9]
#' Series2 <- ECGExample[, , 10, drop = FALSE]
#'
#' # Training a discriminant model
#' MWA <- MultiWaveAnalysis(Series1, "haar", features = c("var"))
#' MWADiscrim <- StepDiscrim(MWA, c(rep(1, 5), rep(2, 4)), maxvars = 5,
#'                           features = c("var"))
#' model <- trainModel(MWADiscrim, c(rep(1, 5), rep(2, 4)), "linear")
#'
#' # Using the discriminant trained on new data
#' prediction <- classify(Series2, model)
#' }
#'
#' @seealso
#' * \code{\link{trainModel}}
#'
#' @export
#'
#' @importFrom stats predict
#' @importFrom magrittr %>%
classify.array <- function(data, model, ...) {

    checkmate::anyMissing(c(data, model))

    if (length(dim(data)) != 3) {
        stop(
            "It seems that a dimension is missing, in case your series contains
         only one case, make sure that you have activated the option
         \"drop = FALSE\" as in the following example
         Series1 = Series2 [,,1, drop = FALSE]."
        )
    }

    MWA <- MultiWaveAnalysis(data, model$Filter, model$NLevels, model$Features);

    MWADiscrim <- list(
        Features = list(
            Var = NA,
            Cor = NA,
            IQR = NA,
            DM = NA,
            PE = NA
        ),
        StepSelection = list(
            Var = NA,
            Cor = NA,
            IQR = NA,
            DM = NA,
            PE = NA
        ),
        Observations = MWA$Observations,
        NLevels = MWA$NLevels,
        Filter = MWA$Filter
        )

    attr(MWADiscrim, "class") <- "MultiWaveAnalysis"

    for (feature in names(model$Selected)) {
        selection <- model$Selected[[feature]]
        MWADiscrim$Features[[feature]] <-
                MWA$Features[[feature]][selection, ,
                                        drop = FALSE]
        MWADiscrim$StepSelection[[feature]] <- selection
    }

    return (classify.MultiWaveAnalysis(MWADiscrim,model))
}
