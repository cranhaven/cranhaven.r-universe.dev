# This may need to be converted to an S3 at some later date
# generate_bootstraps.lm, .glm, .svyglm, etc
generate_bootstraps <- function(x, env) {
    data <- eval(x$call$data, envir = env)
    bs.call <- x$call
    bs.call$data <- as.name("bs.data")
    bs.fits <- vector("list", 30L)
    for (i in seq_along(bs.fits)) {
        bs.data <- dplyr::slice_sample(data, n = nrow(data), replace = TRUE)
        bs.fits[[i]] <- eval(bs.call)
    }
    class(bs.fits) <- "inzboots"
    bs.fits
}


## THE FOLLOWING ARE DEPRECATED ... ?

bootstrapModels <- function(fit, nBootstraps = 30, env = parent.frame()) {
    if (isSurvey(fit)) {
        warning("Bootstrapping for survey glms is not yet ready.")
        return(NULL)
    }

    # Variables for adding bootstrap lowess lines
    # nr = nrow(fit$model)
    # modifiedCall <- modifyModelCall(fit)
    # bootstrapID <- replicate(nBootstraps, sample(1:nr, replace = TRUE))
    # listOfModels <- invisible(lapply(1:nBootstraps,
    #     function(i) {
    #         conv <- FALSE
    #         while (!conv) {
    #             bootstrapSample <- bootstrapData(fit, bootstrapID[, i])
    #             mod <- suppressWarnings(eval(modifiedCall))
    #             if (isGlm(fit)) {
    #                 if (mod$conv)
    #                     conv <- TRUE
    #             } else
    #             conv <- TRUE
    #         }
    #         mod
    #     }))

    bs.fits <- replicate(nBootstraps,
        {
            conv <- FALSE
            while (!conv) {
                bsfit <- suppressWarnings({
                    ## this is a little tricky.
                    ## - need to subset the FULL data set (using fit$model excludes missing values!)
                    ## - fit$call$data is the name of the data object, which needs to be evaluated FIRST
                    ## - and THEN the fit update is evaluate in the parent environment (where fit$call$data exists)
                    if (is.null(fit$call$data)) {
                        tmpdata <- fit$model
                    } else {
                        tmpdata <- eval(fit$call$data, envir = env)
                    }
                    call <- update(fit,
                        data = tmpdata,
                        subset = sample(nrow(tmpdata), replace = TRUE),
                        evaluate = FALSE
                    )
                    eval(call) # , envir = env)
                })
                conv <- !isGlm(fit) || bsfit$conv
            }
            bsfit
        },
        simplify = FALSE
    )

    invisible(bs.fits)
}

bootstrapData <- function(fit, id) {
    UseMethod("bootstrapData")
}

#' @export
bootstrapData.lm <- function(fit, id) {
    return(fit$model[id, ])
    ## Try just use the data set in the R session ...
    call <- fit$call
    callValues <- as.character(call)
    callNames <- names(call)
    dataName <- callValues[callNames == "data"]
    bsData <- eval(parse(text = dataName))[id, ]
    bsData
}

#' @export
bootstrapData.glm <- function(fit, id) {
    call <- fit$call
    callValues <- as.character(call)
    callNames <- names(call)
    dataName <- callValues[callNames == "data"]
    bsData <- eval(parse(text = dataName))

    if (grepl("/", colnames(fit$model)[1])) {
        # In this case, resample the number of successes
        mod <- fit$model
        response <- strsplit(names(mod), "/")[[1]]
        y <- bsData[, response[1]]
        n <- bsData[, response[2]]
        p <- ifelse(n == 0, 0, y / n)

        bsData[, response[1]] <- rbinom(nrow(bsData), n, p)
    } else {
        # otherwise, simply resample the data
        bsData <- bsData[id, ]
    }

    bsData
}

#' @export
bootstrapData.svyglm <- function(fit, id) {
    # To do: account for sample design when doing bootstrap resample.

    # Returns a bootstrapped survey design object

    # First, get the survey design name, then the dataset name:
    call <- fit$call
    callValues <- as.character(call)
    callNames <- names(call)
    designName <- callValues[callNames == "design"]

    des <- eval(parse(text = designName))
    descall <- des$call
    descallValues <- as.character(descall)
    descallNames <- names(descall)
    dataName <- descallValues[descallNames == "data"]

    # Rebuild design call with new data:
    bsData <- eval(parse(text = dataName))[id, ]

    o <- !descallNames %in% c("", "data")
    other <- paste(descallNames[o], " = ", descallValues[o], ", ",
        sep = "", collapse = ""
    )

    newcall <- paste("svydesign(", other, "data = bsData)", sep = "")
    bsDesign <- eval(parse(text = newcall))
}



modifyModelCall <- function(fit) {
    if (isSurvey(fit)) {
        modifiedCall <- update(fit, . ~ .,
            design = eval(parse(text = "bootstrapSample")),
            evaluate = FALSE
        )
    } else {
        modifiedCall <- update(fit, . ~ .,
            data = eval(parse(text = "bootstrapSample")),
            evaluate = FALSE
        )
    }

    modifiedCall
}


modelDataName <- function(fit) {
    call <- fit$call
    callValues <- as.character(call)
    callNames <- names(call)
    dataName <- callValues[callNames == "data"]
    dataName
}
