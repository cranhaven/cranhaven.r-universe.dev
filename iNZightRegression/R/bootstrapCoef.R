bootstrapCoefs = function(fit, N = 1000) {
    ## list of N models
    models = bootstrapModels(fit, N)

    ## names/types of explanatory variables
    xVarterms = attr(fit$terms, "term.labels")
    xVarnames = xVarterms[ ! grepl(":", xVarterms)]
    xVartypes = attr(fit$terms, "dataClasses")[-1]

    ## Coefficients of original model
    fitCoefs = coef(fit)
    coefNames = names(fitCoefs)
    nCoefs = length(fitCoefs)

    ## a matrix to fill in using coefficients of N bootstrap models
    coefmatrix = matrix(0, nrow = N, ncol = nCoefs)

    ## Initialise counter for number of retained samples
    keptSamples = 0
    ## Debugging
    errors = character(0)

    for (i in 1:N) {
        ## Coefficients of current bootstrap model
        bootCoefs = coef(models[[i]])
        baselinePresent = TRUE
        newCoefs = bootCoefs

        ## Run through the factors in the model
        for (j in seq_along(xVarnames)) {
            if (xVartypes[j] == "factor") {
                facName = xVarnames[j]
                ## Full set of levels from the original model
                allLevels = levels(fit$model[,facName])
                ## Baseline of original model
                baseline = allLevels[1]
                ## Levels present in the current bootstrap model
                presentLevels = as.character(unique(models[[i]]$model[,facName]))
                nLevels = length(presentLevels)

                ## Is the original baseline present in the bootstrap model
                baselinePresent = baseline %in% presentLevels
                ## If not, store a message and skip to next model
                ## We can't use the model if baseline is missing
                if (!baselinePresent) {
                    errors = c(errors, paste("No baseline for factor",
                                             facName, "in sample", i))
                    break
                }

                ## If some non-baseline level(s) are missing...
                if (nLevels != length(levels(fit$model[,facName]))) {
                    ## Which levels are missing
                    missingLevels = (allLevels[-1])[! allLevels[-1] %in% presentLevels]
                    ## Add NA for coefficients of missing levels
                    newCoefs[paste(facName, missingLevels, sep = "")] = NA
                    ## Reorder into same order as original model
                    newCoefs = newCoefs[coefNames]
                    ## Store a message about the situation
                    errors = c(errors, paste("Sample", i, "ok but level(s)",
                                             paste(missingLevels, collapse = " & "),
                                             "of factor", facName, "missing"))
                }
            }
        }
        ## If the sample is OK, add model coefficients to matrix
        if (baselinePresent) {
            coefmatrix[i,] = newCoefs
            keptSamples = keptSamples + 1
        }
    }
    ## Trim off 'unused' rows
    if (keptSamples < N) coefmatrix = coefmatrix[1:keptSamples,]
    ## Covariance matrix of coefficients
    ## Need to remove any columns full of NAs. These correspond to factor level
    ## combinations that are not present in the original sample.
    naColumns = which(apply(coefmatrix, 2, function(col) all(is.na(col))))
    if (length(naColumns) > 0) {
        naNames = coefNames[naColumns]
        okNames = coefNames[-naColumns]
        covCoef = var(coefmatrix[,-naColumns], na.rm = TRUE)
        seCoefs = c(sqrt(diag(covCoef)), rep(NA, length(naNames)))
        names(seCoefs) = c(okNames, naNames)
        ## Put in same order as coefficients from original fit
        seCoefs = seCoefs[coefNames]
    } else {
        covCoef = var(coefmatrix, na.rm = TRUE)
        seCoefs = sqrt(diag(covCoef))
        names(seCoefs) = coefNames
        naNames = character()
    }

    list(fit = fit, seCoef = seCoefs, covCoef = covCoef, N = N,
         keptSamples = keptSamples, errors = errors, naNames = naNames)
}

bootstrapTTests = function(bootcoefs, N = 1000) {
    fit = bootcoefs$fit
    sf = summary(fit)
    if (length(bootcoefs$naNames) > 0) {
        sfcoef = rbind(sf$coefficients,
                       matrix(NA, nrow = length(bootcoefs$naNames),
                              ncol = ncol(sf$coefficients)))
        rownames(sfcoef) = c(rownames(sf$coefficients), bootcoefs$naNames)
        ## Re-order
        coefNames = names(coef(fit))
        sfcoef = sfcoef[coefNames,]
    } else {
        sfcoef = sf$coefficients
    }

    tValues = sfcoef[,1] / bootcoefs$seCoef
    pValues = 2 * pt(-abs(tValues), df = fit$df.residual)

    list(t = tValues, p = pValues)
}


bootstrapFTests = function(bootcoefs, N = 1000) {
    fit = bootcoefs$fit
    coefNames = names(coef(fit))
    ## Names of terms (different to variables) in the model
    xVarterms = attr(fit$terms, "term.labels")

    ## Names of variables in the model
    xVarnames = xVarterms[ ! grepl(":", xVarterms)]

    ## Types of terms in the model
    xVartypes = character(0)
    tmp = attr(fit$terms, "dataClasses")[-1]
    noninteractions = names(tmp)
    interactions = xVarterms[grepl(":", xVarterms)]
    xVartypes[noninteractions] = tmp
    xVartypes[interactions] = "interaction"
    ## Ordered factors can be treated as normal factors
    xVartypes = gsub("ordered", "factor", xVartypes)

    covCoef = bootcoefs$covCoef
    Fvals = Pvals = numeric(0)

    ## Records names of terms where F values can be calculated but are
    ## not meaningful.
    setToNA = character(0)

    ## Loop through the variables to find factors and calculate p values
    index = 2   # start at 2, since intercept is at 1
    for (v in xVarterms) {
        if ( ! xVartypes[v] %in% c("factor", "interaction")) {
            index = index + 1
            next
        }

        ## Calculate umber of coefficients we need to extract
        if (xVartypes[v] == "factor") {
            n = length(levels(fit$model[,v])) - 1
        } else {
            vars = unlist(strsplit(v, ":"))
            n = prod(sapply(vars, function(f) {
                                    if (xVartypes[f] != "factor") 1
                                    else length(levels(fit$model[,f])) - 1}))
        }
        coefBlock = coef(fit)[index + 0:(n-1)]
        if (any(is.na(coefBlock))) {
            Fvals[v] = NA
            Pvals[v] = NA
            ## NAs already removed from covariance matrix, so the index does not
            ## increase by n in this case
            index = index + sum(!is.na(coefBlock))
        } else {
            ## Pull out the relevant section of the coefficient covariance matrix
            covBlock = covCoef[index + 0:(n-1), index + 0:(n-1)]
            ## F TEST
            ## Numerator degrees of freedom
            numerDf = length(coefBlock)
            ## F statistic & P-value
            Fvals[v] = t(coefBlock) %*% solve(covBlock) %*% coefBlock / numerDf
            Pvals[v] = 1 - pf(Fvals[v], numerDf, fit$df.residual)
            index = index + n
        }

        ## If there are missing combinations of factor levels (hence NA interaction
        ## coefficients), F tests for main effects not meaningful. Keep a record of
        ## these terms so we can set to NA later.
        if (xVartypes[v] == "interaction" && is.na(Fvals[v]))
          setToNA = c(setToNA, vars)
    }

    ## Set unmeaningful F values to NA
    Fvals[setToNA] = NA
    Pvals[setToNA] = NA

    list(Fvals = Fvals, Pvals = Pvals)
}

reorderFactors = function(dataset, factorNames = "all") {
    if (factorNames == "all") {
        tmp = which(sapply(dataset, class) %in% c("factor", "ordered"))
        factorsToChange = names(dataset)[tmp]
    } else {
        factorsToChange = factorNames[factorNames %in% names(dataset)]
    }
    if (length(factorsToChange) == 0)
        stop("Invalid factor name(s) specified")
    for (f in factorsToChange) {
        newBaseline = names(which.max(table(dataset[,f])))[1]
        dataset[,f] = relevel(dataset[,f], newBaseline)
    }
    invisible(dataset)
}
