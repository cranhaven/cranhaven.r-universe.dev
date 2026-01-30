# Title     : TODO
# Objective : TODO
# Created by: ivan
# Created on: 8/4/21

#' Generate a MultiWave analysis
#'
#' Generates a multivariate analysis by calculating a series of features from
#' the result of applying MODWT to the input data.
#'
#' @param series Sample from the population (array of three dimensions
#'        \[dim, length, cases\]
#' @param f Selected wavelet filter for the analysis. To see the available
#'        filters use the function \code{\link{availableFilters}}
#' @param lev Wavelet decomposition level by default is selected using the
#'        "conservative" strategy. See \code{\link{chooseLevel}} function. Must
#'         be a positive integer (including 0 to auto-select the level)
#' @param features It allows to select the characteristics to be calculated for
#'        the analysis. To see the available features use the function
#'        \code{\link{availableFeatures}}
#' @param nCores Determines the number of processes that will be used in the
#'        function, by default it uses all but one of the system cores. Must be
#'        a positive integer, where 0 corresponds to the default behavior
#'
#' @return A multivariate analysis with the characteristics indicated in the
#'         parameter features. This is an object of class MultiWaveAnalysis with
#'         contains
#'         * Features: A list with the computed features
#'         * StepSelection: A selection with the most discriminant features
#'          \code{\link{StepDiscrim}}
#'         * Observations: Number of total observations
#'         * NLevels: Number of levels selected for the decomposition process
#'         * Filter: Filter used in the decomposition process
#'
#' @examples
#' \donttest{
#' load(system.file("extdata/ECGExample.rda",package = "TSEAL"))
#' MWA <- MultiWaveAnalysis(ECGExample,
#'   f = "haar", lev = 0,
#'   features = c("Var", "Cor"), nCores = 0
#' )
#' }
#'
#' @seealso
#' * \code{\link{availableFilters}}
#' * \code{\link{availableFeatures}}
#'
#' @export
#'
#' @importFrom bigmemory as.matrix GetMatrixSize
#' @importFrom waveslim modwt wave.variance wave.correlation wave.filter
#' @importFrom parallel makeCluster clusterExport clusterEvalQ detectCores
#' @importFrom pryr object_size
#' @importFrom stats sd
#' @importFrom utils combn
#' @importFrom checkmate anyMissing asCount
#' @importFrom parallelly availableCores
MultiWaveAnalysis <- function(series,
                              f,
                              lev = 0,
                              features = c("Var", "Cor", "IQR", "PE", "DM"),
                              nCores = 0) {
    checkmate::anyMissing(c(series, f))

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

    lev <- checkmate::asCount(lev)
    nCores <- checkmate::asCount(nCores)

    f <- tolower(f)
    features <- tolower(features)
    features <- unique(features)

    featuresTest <-
        unlist(lapply(features, function(x) {
            x %in% getAllFeatures()
        }))
    featuresTest <- which(featuresTest == FALSE)
    if (length(featuresTest) > 0) {
        erroneusFeatNames <- features[featuresTest]
        stop(
            "The features",
            erroneusFeatNames,
            "are not supported. Please use the availableFeatures() function
               to see the supported features",
        )
    }


    if (nCores == 0) {
        nCores <- parallelly::availableCores()
    }


    HVar <- "var" %in% features
    HCor <- "cor" %in% features
    HIQR <- "iqr" %in% features
    HPE <- "pe" %in% features
    HDM <- "dm" %in% features

    dim1 <- dim(series)

    nv1 <- dim1[1] # Time series Dimension
    nr1 <- dim1[2] # Time series Lenght
    nc1 <- dim1[3] # Sample Size

    nCores <- min(nCores, nc1)

    if (lev == 0) {
        lev <- chooseLevel("conservative", f, nr1)
    }

    if (lev > chooseLevel("supermax", f, nr1)) {
        nLev <- chooseLevel("supermax", f, nr1)
        warning(
            "The \"level\" provided is greater than the maximum level
                   supported for the selected filter and data. Using the level",
            as.character(nLev),
            "instead"
        )
        lev <- nLev
    }

    # set parallel enviorement
    c <- parallel::makeCluster(nCores)
    mgrinit(c)
    result <- tryCatch({
        # Standarizing the data
        mgrmakevar(c, "YSeries1", nc1, nv1 * nr1)

        for (i in seq_len(nc1)) {
            YSeries1[i, ] <- as.vector(t(series[, , i]))
        }


        clusterExport(c, c("nv1", "nr1", "nc1"), envir = environment())

        clusterExport(
            c,
            c(
                "D3toD2",
                "computeIQR",
                "computePermutationEntropy",
                "computeDMeasure"
            ),
            envir = loadNamespace("TSEAL")
        )

        clusterEvalQ(c, {
            ids <- getidxs(nv1)
            for (i in ids) {
                for (j in seq_len(nc1)) {
                    aux <- D3toD2(i, , j, nv1, nr1, nc1)
                    Vtemporal <-
                        YSeries1[aux[[1]], aux[[2]][[1]]:aux[[2]][[2]]]
                    YSeries1[aux[[1]], aux[[2]][[1]]:aux[[2]][[2]]] <-
                        (Vtemporal - mean(Vtemporal)) / sd(Vtemporal)
                }
            }
        })

        NVar <- nv1 * lev
        if (HCor || HDM) {
            NbK <- combn(seq_len(nv1), 2)
            NumberNbK <- dim(NbK)[2]
            NCor <- NumberNbK * lev
        } else {
            NCor <- 0
            NumberNbK <- 0
            NbK <- 0
        }


        clusterExport(
            c,
            c(
                "NumberNbK",
                "NbK",
                "lev",
                "f",
                "HVar",
                "HCor",
                "HIQR",
                "HPE",
                "HDM"
            ),
            envir = environment()
        )
        if (HVar) {
            mgrmakevar(c, "Var", NVar, nc1)
        } else {
            Var <- NA
        }
        if (HCor) {
            mgrmakevar(c, "Cor", NCor, nc1)
        } else {
            Cor <- NA
        }
        if (HIQR) {
            mgrmakevar(c, "IQR", NVar, nc1)
        } else {
            IQR <- NA
        }
        if (HPE) {
            mgrmakevar(c, "PE", NVar, nc1)
        } else {
            PE <- NA
        }
        if (HDM) {
            mgrmakevar(c, "DM", NCor, nc1)
        } else {
            DM <- NA
        }

        clusterEvalQ(c, {
            ids <- getidxs(nc1)
            for (i in ids) {
                WJ <- list()
                for (j in seq_len(nv1)) {
                    aux <- D3toD2(j, , i, nv1, nr1, nc1)
                    Vtemporal <-
                        YSeries1[aux[[1]], aux[[2]][[1]]:aux[[2]][[2]]]
                    aux.modwt <- waveslim::modwt(Vtemporal, f, lev)
                    WJ <- append(WJ, list(aux.modwt))

                    if (HVar) {
                        WVARAux <- waveslim::wave.variance(aux.modwt)
                        Var[((j - 1) * lev + 1):(j * lev), i] <-
                            WVARAux[seq_len(lev), 1]
                    }
                    if (HIQR) {
                        WIQRAux <- computeIQR(aux.modwt)
                        IQR[((j - 1) * lev + 1):(j * lev), i] <-
                            WIQRAux
                    }
                    if (HPE) {
                        WPEAux <- computePermutationEntropy(aux.modwt)
                        PE[((j - 1) * lev + 1):(j * lev), i] <-
                            WPEAux
                    }
                }

                if (HCor || HDM) {
                    for (k in seq_len(NumberNbK)) {
                        if (HCor) {
                            WCOR <- suppressWarnings(waveslim::wave.correlation
                                                     (WJ[[NbK[1, k]]], WJ[[NbK[2, k]]], nr1))
                            Cor[((k - 1) * lev + 1):(k * lev), i] <-
                                WCOR[seq_len(lev), 1]
                        }
                        if (HDM) {
                            WDM <- computeDMeasure(WJ[[NbK[1, k]]], WJ[[NbK[2, k]]])
                            DM[seq((k - 1) * lev + 1, k * lev), i] <-
                                WDM
                        }
                    }
                }
            }
        })
    },
    finally = stoprdsm(c))

    aVar <- as.matrix(Var)
    aCor <- as.matrix(Cor)
    aIQR <- as.matrix(IQR)
    aDM <- as.matrix(DM)
    aPE <- as.matrix(PE)

    if (nc1 == 1) {
        aVar <- matrix(aVar)
        aCor <- matrix(aCor)
        aIQR <- matrix(aIQR)
        aDM <- matrix(aDM)
        aPE <- matrix(aPE)
    }

    x <- list(
        Features = list(
            Var = aVar,
            Cor = aCor,
            IQR = aIQR,
            DM = aDM,
            PE = aPE
        ),
        StepSelection = list(
            Var = NA,
            Cor = NA,
            IQR = NA,
            DM = NA,
            PE = NA
        ),
        Observations = nc1,
        NLevels = lev,
        Filter = f
    )
    attr(x, "class") <- "MultiWaveAnalysis"
    return(x)
}

#' Select the DWT level of decomposition based on wavelet filter, data series
#'  length and a user choice
#'
#' @param choice Valid values:
#'  * "Conservative" : \eqn{J < log_2 ( N / (L - 1) + 1)}{J < log2(N / (L - 1) + 1)}
#'  * "Max" : \eqn{J \leq log_2(N)}{J <= log2(N) }
#'  * "Supermax" : \eqn{ J \leq log_2(1.5 * N)}{J <= log2(1.5 * N)}
#' @param filter Wavelet transform filter name. To see the available filters use
#'        the function \code{\link{availableFilters}}
#' @param N Number of observations. Must be a positive integer
#'
#' @return Number of level of decomposition based in selection criteria
#' @references Percival, D. B. and A. T. Walden (2000) Wavelet Methods for
#'   Time Series Analysis. Cambridge: Cambridge University Press.
#'
#' @examples
#' lev <- chooseLevel("conservative", "haar", 8)
#' @export
chooseLevel <- function(choice, filter, N) {
    anyMissing(c(choice, filter, N))
    N <- checkmate::asCount(N)

    choice <- tolower(choice)
    filter <- tolower(filter)

    L <- wave.filter(filter)[[1]]
    if (choice == "conservative") {
        J0 <- floor(log2((N / (L - 1)) - 1))
        return(J0 - 1)
    }
    if (choice == "max") {
        J0 <- floor(log2(N))
        return(J0 - 1)
    }
    if (choice == "supermax") {
        J0 <- floor(log2(1.5 * N))
        return(J0 - 1)
    }
    stop(
        "Selected choice",
        as.character(choice),
        "its not valid. The
                available options are \"Conservative\", \"max\" and
              \"supermax\""
    )
}

#' Extract observations from a MultiWaveAnalysis
#'
#' This function permits to extract certain observations from a MultiWaveAnalysis
#'
#' @param MWA MultiWaveAnalysis from which the desired observations will be extracted
#' @param indices Indices that will indicate which observations will be
#'        extracted
#'
#'
#' @return A list with two elements:
#'  * MWA: The MultiWaveAnalysis provided minus the extracted observations.
#'  * MWAExtracted: A new MultiWaveAnalysis with the extracted observations
#' @export
#'
#' @examples
#' load(system.file("extdata/ECGExample.rda",package = "TSEAL"))
#' MWA <- MultiWaveAnalysis(ECGExample, "haar", features = "Var")
#' aux <- extractSubset(MWA, c(1, 2, 3))
#' MWATrain <- aux[[1]]
#' MWATest <- aux[[2]]
#' @md
extractSubset <- function(MWA, indices) {
    if (missing(MWA)) {
        stop("The argument \"MWA\" must be provided.")
    }
    if (missing(indices)) {
        stop("The argument \"indices\" must be provided.")
    }

    n <- length(indices)

    MWA1 <- MWA
    MWA1$Observations <- n

    MWA2 <- MWA
    MWA2$Observations <- MWA2$Observations - n

    for (feature in names(MWA$Features)) {
        if (!(is.na(MWA$Features[[feature]][1]))) {
            MWA1$Features[[feature]] <-
                MWA1$Features[[feature]][, indices, drop = FALSE]

            MWA2$Features[[feature]] <-
                MWA2$Features[[feature]][, -indices, drop = FALSE]
        }
    }
    return(list(MWA1, MWA2))
}

#' availableFeatures
#'
#' Print the available features for the \code{\link{MultiWaveAnalysis}} and
#'  \code{\link{StepDiscrim}}
#'
#' @return A `data.frame` containing the name of the characteristics and their
#'  abbreviations for use in the code. For example, to use variances and
#'  correlations, the vector c("Var", "Cor") will be used.
#'
#' @examples
#' availableFeatures()
#'
#' @seealso
#' * \code{\link{MultiWaveAnalysis}}
#' * \code{\link{StepDiscrim}}
#' * \code{\link{StepDiscrimV}}
#'
#' @export
availableFeatures <- function() {
    names <- c(
        "Variance",
        "Correlation",
        "Interquartile range",
        "Permutation Entropy",
        "Hoefflin s D measure"
    )
    keys <- c("Var", "Cor", "IQR", "PE", "D")
    res <- data.frame(names, keys)
    return(res)
}

#' availableFilters
#'
#' Print the available filters for the wave analysis
#'
#' @return A `data.frame` containing all supported filters
#'
#' @examples
#' availableFilters()
#'
#' @seealso
#' * \code{\link{MultiWaveAnalysis}}
#'
#' @export
availableFilters <- function() {
    filters <- c(
        "harr",
        "d4",
        "mb4",
        "w4",
        "bs3",
        "fk4",
        "d6",
        "fk6",
        "d8",
        "fk8",
        "la8",
        "mb8",
        "bl14",
        "fk14",
        "d16",
        "la16",
        "mb16",
        "la20",
        "bl20",
        "fk22",
        "mb24"
    )
    return(data.frame(filter = filters))
}

#' @importFrom stats IQR
#' @noRd
computeIQR <- function(X) {
    aux <- head(X, -1)
    return(unlist(lapply(aux, function(x) {
        stats::IQR(x)
    })))
}

#' @importFrom utils head
#' @importFrom statcomp permutation_entropy ordinal_pattern_distribution
#' @noRd
computePermutationEntropy <- function(X) {
    aux <- head(X, -1)
    return(unlist(lapply(aux,
                         function(x) {
                             statcomp::permutation_entropy(statcomp::ordinal_pattern_distribution(x, 6))
                         })))
}

#' @importFrom utils head
#' @importFrom wdm wdm
#' @noRd
computeDMeasure <- function(X, Y) {
    X <- head(X, -1)
    Y <- head(Y, -1)
    return(mapply(function(x, y) {
        wdm::wdm(x, y, "hoeffding")
    }, X, Y))
}

#' @export
print.MultiWaveAnalysis <- function(x, ...) {
    summary(x)
}



#' @export
summary.MultiWaveAnalysis <- function(object, ...) {
    MWA <- object
    InitStr <- paste(
        "MultiWave Analysis Object:",
        "\n\tNumber of Observations: ",
        MWA$Observations,
        "\n\tNumber of decomposing levels: ",
        MWA$NLevels,
        "\n\tFilter used: ",
        MWA$Filter,
        sep = ""
    )


    FeaturesStr <- paste("\tStored Features per observation:")
    for (feature in names(MWA$Features)) {
        if (!all(is.na(MWA$Features[[feature]]))) {
            NFeature <- dim(MWA$Features[[feature]])[1]
            FeaturesStr <-
                paste(FeaturesStr,
                      paste("\n\t\t-", feature, " : ", NFeature))
        }
    }

    SelectionStr <- paste("")
    if (all(is.na(MWA$StepSelection))) {
        SelectionStr <-
            paste(
                "\tThis MultiWaveAnalysis object has not gone",
                "through the variable selection process."
            )
    } else {
        SelectionStr <-
            paste(
                "\tVariables selected by the selection process",
                "\n\t(Note that they refer to the index before being",
                "filtered):"
            )

        for (feature in names(MWA$StepSelection)) {
            if (!all(is.na(MWA$StepSelection[[feature]]))) {
                selected <- MWA$StepSelection[[feature]]
                SelectionStr <- paste(SelectionStr,
                                      paste(
                                          "\n\t\t-",
                                          feature,
                                          " : ",
                                          paste(selected, collapse = ", ")
                                      ))
            }
        }
    }

    cat(paste(InitStr, FeaturesStr, SelectionStr, sep = "\n"))
}

values <- function(MWA) {
    stopifnot(is(MWA, "MultiWaveAnalysis"))
    values <- matrix(0, nrow = 0, ncol = MWA$Observations)
    for (feature in MWA$Features) {
        if (!(is.na(feature[1]))) {
            values <- rbind(values, as.matrix(feature))
        }
    }
    return(values)
}

getAllFeatures <- function() {
    return(c("var", "cor", "iqr", "pe", "dm"))
}
