# Title     : TODO
# Objective : TODO
# Created by: ivan
# Created on: 8/4/21

D3toD2 <- function(i, j, k, nRows, nCols, nPages) {
    if (missing(i) && missing(j)) {
        return(list(k, list(0, nRows * nCols)))
    }

    if (missing(i)) {
        return(k, list)
    }

    if (missing(j)) {
        return(list(k, list((i - 1) * nCols + 1, (i) * nCols)))
    }

    return(list(k, list(i * nCols + j, i * nCols + j)))
}

#' Generate StepDiscrim from raw data
#'
#' This function allows to obtain in a single step the complete MultiWaveAnalysis
#' and the selection of the most discriminating variables of the MultiWaveAnalysis.
#'
#' @param series Sample from the population (dim x length x cases)
#' @param labels Labeled vector that classify the observations
#' @param f Selected filter for the MODWT (to see the available filters use the
#'        function \code{\link{availableFilters}}
#' @param maxvars Maximum number of variables included by the StepDiscrim
#'        algorithm (Note that if you defined this, can not define VStep). Must
#'        be a positive integer
#' @param VStep Minimum value of V above which all other variables are
#'        considered irrelevant and therefore will not be included. (Note that
#'        if you defined this, can not defined maxvars).Must be a positive
#'        number. For more information see StepDiscrim documentation.
#' @param lev Determines the number of decomposition levels for MODWT
#'        (by default the optimum is calculated). Must be a positive integer,
#'        where 0 corresponds to the default behavior.
#' @param features A list of characteristics that will be used for the
#'        classification process. To see the available features see
#'        \code{\link{availableFeatures}}
#' @param nCores Determines the number of processes that will be used in the
#'        function, by default it uses all but one of the system cores.
#'        Must be a positive integer, where 0 corresponds to the default
#'        behavior
#'
#' @return A MultiWaveAnalysis with the most discriminant variables based on the
#'         features indicated.
#'
#' @seealso
#' * \code{\link{MultiWaveAnalysis}}
#' * \code{\link{StepDiscrim}}
#' * \code{\link{StepDiscrimV}}
#'
#' @examples
#' \donttest{
#' load(system.file("extdata/ECGExample.rda",package = "TSEAL"))
#' # The dataset has the first 5 elements of class 1
#' # and the last 5 of class 2.
#' labels <- c(rep(1, 5), rep(2, 5))
#' MWADiscrim <- generateStepDiscrim(ECGExample, labels, "haar",
#'   features = c("Var"), maxvars = 5
#' )
#' # or using the VStep option
#' MWADiscrim <- generateStepDiscrim(ECGExample, labels, "haar",
#'  features = c("Var", "Cor"), VStep = 0.7
#' )
#' }
#' @export
generateStepDiscrim <-
    function(series,
             labels,
             f,
             maxvars,
             VStep,
             lev = 0,
             features = c("Var", "Cor", "IQR", "PE", "DM"),
             nCores = 0) {
        if (missing(series)) {
            stop("The argument \"series\" must be provided.")
        }
        if (missing(f)) {
            stop(
                "The argument \"f\" (filter) must be provided.
                       To see available filter use availableFilters()"
            )
        }
        if (missing(labels)) {
            stop("The argument \"labels\" must be provided.")
        }

        if (length(dim(series)) != 3) {
            stop(
                "It seems that a dimension is missing, in case your series contains
         only one case, make sure that you have activated the option
         \"drop = FALSE\" as in the following example
         Series1 = Series2 [,,1, drop = FALSE]."
            )
        }

        if (length(labels) != dim(series)[3]) {
            stop("The number of observations in the data and those provided in labels do
         not match.")
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

        f <- tolower(f)

        MWA <- MultiWaveAnalysis(series, f, lev, features, nCores)
        if (!missing(maxvars)) {
            MWA <- StepDiscrim(MWA, labels, maxvars, features, nCores)
        } else {
            MWA <- StepDiscrimV(MWA, labels, VStep, features, nCores)
        }

        return(MWA)
    }

#' testFilters
#'
#' This function performs a test with a series of filters defined by the user,
#' for the maximum number of variables determined. This function can be used to
#' compare the performance of different filters with a different number of
#' variables to be considered and the differences between a linear and a
#' quadratic discriminant.
#'
#' @param series Samples from the population (dim x length x cases)
#' @param labels Labeled vector that classify the observations.
#' @param maxvars maximum number of variables included by the StepDiscrim
#'        algorithm. Must be grater than 0 and, in normal cases, lesser than 100
#' @param filters Vector indicating the filters to be tested. To see the
#'        available filters use the function \code{\link{availableFilters}}
#' @param features A list of characteristics that will be used for the
#'        classification process. To see the available features see
#'        \code{\link{availableFeatures}}
#' @param lev Wavelet decomposition level, by default is selected using the
#'        "conservative" strategy. See \code{\link{chooseLevel}} function.
#'
#' @return A list that each element contains:
#'   * CM: confusion matrix with a particular configuration using LOOCV
#'   * Classification: a vector with the raw classification result. "1" if the
#'                     observation belongs to the population 1 and "2" if
#'                    belongs to the population 2.
#'   * NVars: the total numbers of variables have been taken into account in the
#'            classification process
#'   * Method: type of classifier used.
#'   * Filter: filter used in the MultiWave analysis process
#'   * Features: vector containing the features taken into account
#'
#' @examples
#' \donttest{
#' load(system.file("extdata/ECGExample.rda",package = "TSEAL"))
#' # The dataset has the first 5 elements of class 1
#' # and the last 5 of class 2.
#' labels <- c(rep(1, 5), rep(2, 5))
#' result <- testFilters(ECGExample, labels, features=c("var","cor"),
#'           filters= c("haar","d4"), maxvars = 3)
#' }
#'
#' @export
#'
#' @seealso
#' * \code{\link{LOOCV}}
#' * \code{\link{MultiWaveAnalysis}}
#' * \code{\link{StepDiscrim}}
#' * \code{\link{availableFilters}}
#' * \code{\link{availableFeatures}}
#'
#' @md
testFilters <- function(series,
                        labels,
                        maxvars,
                        filters = c("haar", "d4", "d6", "d8", "la8"),
                        features = c("Var", "Cor", "IQR", "PE", "DM"),
                        lev = 0) {
    anyMissing(c(series, labels, maxvars))
    if (length(filters) == 0) {
        stop(
            "At least one filter must be provided. To see the available filters use
          availableFilters()"
        )
    }

    if (length(features) == 0) {
        stop(
            "At least one feature must be provided. To see the available filters
         use availableFeatures()"
        )
    }

    if (length(dim(series)) != 3) {
        stop(
            "It seems that a dimension is missing, in case your series contains
         only one case, make sure that you have activated the option
         \"drop = FALSE\" as in the following example
         Series1 = Series2 [,,1, drop = FALSE]."
        )
    }

    if (length(labels) != dim(series)[3]) {
        stop("The number of observations in the data and those provided in labels do
         not match.")
    }

    data <- list()
    nFeatures <- length(features)

    for (f in filters) {
        MWA <- MultiWaveAnalysis(series, f)
        for (i in seq_len(nFeatures)) {
            comFeatures <- combn(features, i)
            listFeatures <- split(comFeatures,
                                  rep(seq_len(ncol(
                                      comFeatures
                                  )), each = nrow(comFeatures)))
            for (cFeatures in listFeatures) {
                aux <- StepDiscrimRaw_(MWA, labels, maxvars, cFeatures)
                Tr <- aux[[1]]
                incl <- aux[[2]]
                maxVar <- min(maxvars, length(incl))
                for (v in seq(2, maxVar)) {
                    filterValues <- Tr[incl[seq_len(v)], ]
                    MWAAux <- list(
                        Features = list(
                            Var = filterValues,
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
                    attr(MWAAux, "class") <- "MultiWaveAnalysis"
                    aux <- LOOCV(MWAAux, labels, "linear", TRUE)
                    data <- append(data, list(
                        list(
                            CM = aux[[1]],
                            classification = aux[[2]],
                            NVars = v,
                            Method = "linear",
                            filter = f,
                            Features = cFeatures
                        )
                    ))
                    aux <- LOOCV(MWAAux, labels, "quadratic", TRUE)
                    data <- append(data, list(
                        list(
                            CM = aux[[1]],
                            classification = aux[[2]],
                            NVars = v,
                            Method = "quadratic",
                            filter = f,
                            Features = cFeatures
                        )
                    ))
                }
            }
        }
    }
    return(data)
}
