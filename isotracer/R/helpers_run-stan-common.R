### * None of the functions in this file is exported

### * encode_priors()

#' Prepare the prior encoding for stan data
#'
#' @param params_nm Output from params(nm, simplify = TRUE).
#' @param priors_nm Output from priors(nm) (must have the same parameter order
#'     as params_nm).
#'
#' @keywords internal
#' @noRd

encode_priors <- function(params_nm, priors_nm) {
    if (any(sapply(priors_nm$prior, is.null))) {
        stop("One or several parameters are missing a prior.\n",
             "You can list the current model priors with `priors(...)`.\n",
             "You can list the missing model priors with `missing_priors(...)`.")
    }
    d <- list()
    d[["nPriorConstant_code0"]] <- 0
    d[["nPriorUniform_code1"]] <- 0
    d[["nPriorHcauchy_code2"]] <- 0
    d[["nPriorBeta_code3"]] <- 0
    d[["nPriorTrNormal_code4"]] <- 0
    d[["nPriorExponential_code5"]] <- 0
    d[["nPriorGamma_code6"]] <- 0
    ### Prior types (used to map the correct prior in the stan model)
    priorTypes <- c("constant" = 0, "uniform" = 1, "hcauchy" = 2, "scaled_beta" = 3,
                    "trun_normal" = 4, "exponential" = 5, "gamma" = 6)
    ### Parameter priors
    d[["mappingParamPriorType"]] <- rep(NA, length(params_nm))
    d[["mappingParamPriorID"]] <- rep(NA, length(params_nm))
    prior_i <- c("constant" = 1, "uniform" = 1, "hcauchy" = 1,
                 "scaled_beta" = 1, "trun_normal" = 1, "exponential" = 1,
                 "gamma" = 1) # Counts for each prior type
    # Set some defaults
    for (priorDistParam in c("constantParams", "lowerParams", "upperParams", "hcauchyScaleParams",
                             "rawBetaAlpha", "rawBetaBeta", "betaScaleParams",
                             "trNormMeanParams", "trNormSdParams", "exponentialRateParams",
                             "gammaAlphaParams", "gammaBetaParams")) {
        # Set zero as default for all prior distribution parameters
        d[[priorDistParam]] <- rep(0, length(params_nm))
    }
    # Go through each param prior
    for (i in seq_along(params_nm)) {
        p <- priors_nm[["prior"]][[i]]
        stopifnot(p[["type"]] %in% names(priorTypes))
        d[["mappingParamPriorType"]][i] <- priorTypes[p[["type"]]]
        d[["mappingParamPriorID"]][i] <- prior_i[p[["type"]]]
        prior_i[p[["type"]]] <- prior_i[p[["type"]]] + 1
        if (p[["type"]] == "constant") {
            d[["constantParams"]][i] <- p[["parameters"]][["value"]]
            d[["nPriorConstant_code0"]] <- d[["nPriorConstant_code0"]] + 1
        }
        if (p[["type"]] == "uniform") {
            d[["lowerParams"]][i] <- p[["parameters"]]["min"]
            d[["upperParams"]][i] <- p[["parameters"]]["max"]
            d[["nPriorUniform_code1"]] <- d[["nPriorUniform_code1"]] + 1
        }
        if (p[["type"]] == "hcauchy") {
            d[["hcauchyScaleParams"]][i] <- p[["parameters"]]["scale"]
            d[["nPriorHcauchy_code2"]] <- d[["nPriorHcauchy_code2"]] + 1
        }
        if (p[["type"]] == "scaled_beta") {
            d[["rawBetaAlpha"]][i] <- p[["parameters"]]["alpha"]
            d[["rawBetaBeta"]][i] <- p[["parameters"]]["beta"]
            d[["betaScaleParams"]][i] <- p[["parameters"]]["scale"]
            d[["nPriorBeta_code3"]] <- d[["nPriorBeta_code3"]] + 1
        }
        if (p[["type"]] == "trun_normal") {
            d[["trNormMeanParams"]][i] <- p[["parameters"]][["mean"]]
            d[["trNormSdParams"]][i] <- p[["parameters"]][["sd"]]
            d[["nPriorTrNormal_code4"]] <- d[["nPriorTrNormal_code4"]] + 1
        }
        if (p[["type"]] == "exponential") {
            d[["exponentialRateParams"]][i] <- p[["parameters"]][["lambda"]]
            d[["nPriorExponential_code5"]] <- d[["nPriorExponential_code5"]] + 1
        }
        if (p[["type"]] == "gamma") {
            d[["gammaAlphaParams"]][i] <- p[["parameters"]][["alpha"]]
            d[["gammaBetaParams"]][i] <- p[["parameters"]][["beta"]]
            d[["nPriorGamma_code6"]] <- d[["nPriorGamma_code6"]] + 1
        }
    }
    # Count non-constant priors
    d[["nNonConstantPriors"]] <- length(params_nm) - d[["nPriorConstant_code0"]]
    # Return
    return(d)
}

### * encode_steady()

#' Encode steady compartments for stan data
#'
#' @param nm A \code{networkModel} object.
#' 
#' @keywords internal
#' @noRd

encode_steady <- function(nm) {
    d <- list()
    steady <- lapply(nm[["topology"]], function(x) {
        match(attr(x, "steadyState"), colnames(x))
    })
    d[["maxNsteady"]] <- max(sapply(steady, length))
    d[["nSteady"]] <- setNames(c(sapply(steady, length), 0), # Padded
                               nm = c(paste0("grp", seq_len(nrow(nm))), "padding"))
    d[["steadyIndices"]] <- array(0, dim = c(d[["maxNsteady"]], nrow(nm)),
                                  dimnames = list(seq_len(d[["maxNsteady"]]),
                                                  paste0("grp", seq_len(nrow(nm)))))
    for (i in seq_len(nrow(nm))) {
        d[["steadyIndices"]][seq_along(steady[[i]]),i] <- steady[[i]]
    }
    # Return
    return(d)
}

### * encode_split()

#' Encode split compartments for stan data
#'
#' @param nm A \code{networkModel} object.
#' @param allParams Parameters of the network model.
#'
#' @keywords internal
#' @noRd

encode_split <- function(nm, allParams) {
    d <- list()
    split <- lapply(nm[["topology"]], function(x) {
        match(attr(x, "split"), colnames(x))
    })
    d[["splitPresent"]] <- as.numeric(max(sapply(split, length)) > 0)
    nGroups <- nrow(nm)
    splitComps <- array(0, dim = c(length(comps(nm)[[1]]), nGroups),
                        dimnames = list(seq_along(comps(nm)[[1]]),
                                        paste0("grp", seq_len(nrow(nm)))))
    for (i in seq_along(split)) {
        splitComps[split[[i]], i] <- 1
    }
    d[["splitComps"]] <- splitComps
    # Parameter mapping
    piMapping <- array(0, dim = c(length(comps(nm)[[1]]), nGroups),
                       dimnames = list(seq_along(comps(nm)[[1]]),
                                       paste0("grp", seq_len(nrow(nm)))))
    for (g in seq_len(nGroups)) {
        compNames <- colnames(nm$topology[[g]])
        stopifnot(length(compNames) == length(comps(nm)[[1]]))
        for (k in seq_along(compNames)) {
            if (splitComps[k, g] > 0) {
                paramName <- paste0("portion.act_", compNames[k])
                paramGlobal <- nm$parameters[[g]]$in_model[nm$parameters[[g]]$in_replicate == paramName]
                stopifnot(length(paramGlobal) == 1)
                piMapping[k, g] <- match(paramGlobal, allParams)
            }
        }
    }
   d[["piMapping"]] <- piMapping
    # Return
    return(d)
}

### * encode_init()

#' Encode initial conditions for a network model
#'
#' For now this function assumes that each row has the same number of
#' compartments.
#'
#' @param nm A \code{networkModel} object.
#'
#' @keywords internal
#' @noRd

encode_init <- function(nm) {
    nComps <- sapply(comps(nm), length)
    stopifnot(all(nComps == nComps[1]))
    nGroups <- nrow(nm)
    d <- array(0, dim = c(nComps[1], 2, nGroups),
               dimnames = list(1:nComps[1], c("unmarked", "marked"),
                               c(paste0("grp", seq_len(nGroups)))))
    for (i in seq_len(nGroups)) {
        comps <- colnames(nm$topology[[i]])
        stopifnot(nrow(nm$initial[[i]]) == length(comps))
        stopifnot(setequal(nm$initial[[i]][["compartment"]], comps))
        comps <- nm$initial[[i]][match(comps, nm$initial[[i]][["compartment"]]), ]
        stopifnot(all(comps$compartment == colnames(nm$topology[[i]])))
        unmarked <- comps$size * (1 - comps$proportion)
        marked <- comps$size * comps$proportion
        d[,,i] <- cbind(unmarked, marked)
    }
    return(list(initialQuantities = d))
}

### * encode_distrib_families()

#' Encode the families for proportions and sizes distributions
#'
#' @param nm A \code{networkModel} object.
#'
#' @keywords internal
#' @noRd

encode_distrib_families <- function(nm) {
    o <- list()
    # Encode distribution family for proportions
    known_families <- c("gamma_cv" = 1, "normal_cv" = 2, "normal_sd" = 3,
                        "beta_phi" = 4)
    prop_family <- attr(nm, "prop_family")
    if (!prop_family %in% names(known_families)) {
        stop("Unknown distribution family for proportions. Got value: ",
             prop_family, "\n",
             "Allowed values are: ", names(known_families))
    }
    o[["propFamily"]] <- known_families[prop_family]
    # Encode distribution family for sizes
    size_known_families <- c("normal_cv" = 1, "normal_sd" = 2)
    size_family <- attr(nm, "size_family")
    if (!size_family %in% names(size_known_families)) {
        stop("Unknown distribution family for sizes. Got value: ",
             size_family, "\n",
             "Allowed values are: ", names(size_known_families))
    }
    o[["sizeFamily"]] <- size_known_families[size_family]
    return(o)
}

### * encode_upsilons()

#' Encode the uptake rates for stan data
#'
#' @param nm A \code{networkModel} object.
#' @param allParams Parameters of the network model.
#' 
#' @keywords internal
#' @noRd

encode_upsilons <- function(nm, allParams) {
    nGroups <- nrow(nm)
    upsilons <- nm_get_upsilons(nm, allParams)
    nUpsilons <- setNames(c(upsilons[["nUpsilons"]], 0), # Padding
                          nm = c(paste0("grp", seq_len(nGroups)), "padding"))
    maxNupsilons <- max(nUpsilons)
    upsilonMapping <- array(0, dim = c(maxNupsilons, 3, nGroups),
                            dimnames = list(1:maxNupsilons,
                                            c("from", "to", "param"),
                                            paste0("grp", seq_len(nGroups))))
    for (i in seq_len(nGroups)) {
        upsilonMapping[1:nUpsilons[i], 1:3, i] <- as.matrix(upsilons[["upsilons"]][[i]])
    }
    return(list(nUpsilons = nUpsilons,
                maxNupsilons = maxNupsilons,
                upsilonMapping = upsilonMapping))
}

### ** nm_get_upsilons()

#' Get upsilons for a network model
#' 
#' @param nm A \code{networkModel} object.
#' @param allParams Parameters of the network model.
#' 
#' @keywords internal
#' @noRd

nm_get_upsilons <- function(nm, allParams) {
    upsilons <- lapply(seq_len(nrow(nm)), function(i) {
        z <- nm_row_get_upsilons(nm[i, ], allParams = allParams)
        tibble::tibble(upsilons = list(z), nUpsilons = nrow(z))
    })
    return(dplyr::bind_rows(upsilons))
}

### ** nm_row_get_upsilons()

#' Get upsilons for one row of a network model
#' 
#' @param nm_row A row from a \code{networkModel} object.
#' @param allParams Parameters of the network model.
#' 
#' @keywords internal
#' @noRd

nm_row_get_upsilons <- function(nm_row, allParams) {
    nmRow <- nm_row
    stopifnot(nrow(nmRow) == 1)
    # Get data
    topo <- nmRow[["topology"]][[1]]
    mapping <- nmRow[["parameters"]][[1]][, c("in_replicate", "in_model")]
    mapping <- tibble::deframe(mapping)
    # Build output
    from <- vector()
    to <- vector()
    param <- vector()
    compartments <- colnames(topo)
    stopifnot(all(compartments == rownames(topo)))
    for (j in seq_len(ncol(topo))) {
        for (i in seq_len(nrow(topo))) {
            if (topo[i,j] == 1) {
                from <- c(from, j)
                to <- c(to, i)
                paramName <- paste0("upsilon_", compartments[j], "_to_",
                                    compartments[i])
                param <- c(param, match(mapping[paramName], allParams))
            }
        }
    }
    return(tibble::tibble(from = from, to = to, param = param))
}

### * encode_lambdas()

#' Encode the loss rates for stan data
#'
#' @param nm A \code{networkModel} object.
#' @param allParams Parameters of the network model.
#'
#' @keywords internal
#' @noRd

encode_lambdas <- function(nm, allParams) {
    nGroups <- nrow(nm)
    lambdas <- nm_get_lambdas(nm, allParams)
    nLambdas <- setNames(c(lambdas[["nLambdas"]], 0), # Padding
                         nm = c(paste0("grp", seq_len(nGroups)), "padding"))
    maxNlambdas <- max(nLambdas)
    lambdaMapping <- array(0, dim = c(maxNlambdas, 2, nGroups),
                           dimnames = list(1:maxNlambdas,
                                           c("from", "param"),
                                           paste0("grp", seq_len(nGroups))))
    for (i in seq_len(nGroups)) {
        lambdaMapping[1:nLambdas[i], 1:2, i] <- as.matrix(lambdas[["lambdas"]][[i]])
    }
    return(list(nLambdas = nLambdas,
                maxNlambdas = maxNlambdas,
                lambdaMapping = lambdaMapping))
}

### ** nm_get_lambdas()

#' Get lambdas for a network model
#' 
#' @param nm A \code{networkModel} object.
#' @param allParams Parameters of the network model.
#' 
#' @keywords internal
#' @noRd

nm_get_lambdas <- function(nm, allParams) {
    lambdas <- lapply(seq_len(nrow(nm)), function(i) {
        z <- nm_row_get_lambdas(nm[i, ], allParams = allParams)
        tibble::tibble(lambdas = list(z), nLambdas = nrow(z))
    })
    return(dplyr::bind_rows(lambdas))
}

### ** nm_row_get_lambdas()

#' Get lambdas for one row of a network model
#' 
#' @param nm_row A row from a \code{networkModel} object.
#' @param allParams Parameters of the network model.
#' 
#' @keywords internal
#' @noRd

nm_row_get_lambdas <- function(nm_row, allParams) {
    nmRow <- nm_row
    stopifnot(nrow(nmRow) == 1)
    # Get data
    topo <- nmRow[["topology"]][[1]]
    mapping <- nmRow[["parameters"]][[1]][, c("in_replicate", "in_model")]
    mapping <- tibble::deframe(mapping)
    # Build output
    from <- vector()
    param <- vector()
    compartments <- colnames(topo)
    stopifnot(all(compartments == rownames(topo)))
    for (j in seq_len(ncol(topo))) {
        paramName <- paste0("lambda_", compartments[j])
        if (paramName %in% names(mapping)) {
            from <- c(from, j)
            param <- c(param, match(mapping[paramName], allParams))
        }
    }
    return(tibble::tibble(from = from, param = param))
}
