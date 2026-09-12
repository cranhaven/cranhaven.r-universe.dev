#' Constructor for S3 class `expmodel`
#'
#' @description This function is a constructor for S3 class `expmodel`,
#' which represents Exponential-based model.
#' It usually takes `data` and optionally `freq` as arguments and
#' also optionally `stepsize`
#' Members of interest in practice are `result` and `coeffs`, which maintain
#' the information of estimates and coefficients of polynomials, respectively.
#' @param data A nonnegative numeric vector of a data set to be estimated.
#' @param freq A frequency vector corresponding to the `data` vector.
#' The default value is `NULL`, which means all frequencies are one.
#' If supplied, the length of a vector should be same as `data` and
#' each element should be a nonnegative integer.
#' @param stepsize A numeric vector whose element is larger than 0 and smaller
#' than 1, and decreasing order. The default value is \code{c(0.5, 0.3)}.
#' If you encounter numerical difficulties, decreasing its values, for
#' example, to \code{c(0.4, 0.2)}, might help to estimate a model.
#' @return An object of Exponential-based model `expmodel`.
#' @examples
#' ## Create `expmodel` object from a data set `mixexpgamma$n200`.
#'  emodel <- expmodel(mixexpgamma$n200)
#' ## Create `expmodel` object from a data set `mixExpGammaHist$n800p` and
#' ## its frequencies `mixExpGammaHist$n800f`.
#'  emodel <- expmodel(mixExpGammaHist$n800p, mixExpGammaHist$n800f)
#' @seealso [summary.expmodel()] [plot.expmodel()] [estimate.expmodel()]
#' @export
expmodel <- function(data=data, freq=NULL, stepsize=c(0.5, 0.3)) {
    dname <- deparse(substitute(data))
    fname <- deparse(substitute(freq))
    if (!is.null(freq)) {
        dname <- paste0("(", dname, ", ", fname, ")")
    }
    if (!is.numeric(data)) {
        stop("'data' should be numeric.")
    }
    if (any(is.na(data))) {
        stop("'data' contains NAs.")
    }
    if (any(is.infinite(data))) {
        stop("'data' contains Infs.")
    }
    if (any(data < 0)) {
        stop("'data' should be nonnegative.")
    }

    if (!is.null(freq)) {
        if (length(data) != length(freq)) {
            stop("'data' and 'freq' must be same length.")
        }
        if (!is.numeric(freq)) {
            stop("'freq' should be numeric.")
        }
        if (any(is.na(freq))) {
            stop("'freq' contains NAs.")
        }
        if (any(is.infinite(freq))) {
            stop("'freq' contains Infs.")
        }
        if (any(freq < 0.0)) {
            stop("'freq' contains negative values.")
        }
    }

    if (!is.numeric(stepsize)) {
        stop("'stepsize' should be numeric.")
    }
    if (any(is.na(stepsize))) {
        stop("'stepsize' contains NAs.")
    }
    if (!all(stepsize > 0.0 & stepsize <= 0.98)) {
        stop("'stepsize' is not in (0.0, 0.98).")
    }

    mu0 <- histmean(data, freq)
    lmd0 <- 1.0 / mu0
    if (!is.null(freq)) {
        idx <- (freq != 0)
        data <- data[idx]
        freq <- freq[idx]
    }
    data_scaled <- data * lmd0

    if (is.null(freq)) {
        q_scaled <- stats::quantile(data_scaled)
        q_orig <- stats::quantile(data)
    } else {
        nlen <- length(freq)
        tlst <- vector(mode = "list", length = nlen)
        for (i in 1:nlen) {
            tlst[[i]] <- rep(data[i], freq[i])
        }
        data0 <- do.call(c, tlst)
        q_orig <- stats::quantile(data0)

        for (i in 1:nlen) {
            tlst[[i]] <- rep(data_scaled[i], freq[i])
        }
        data1 <- do.call(c, tlst)
        q_scaled <- stats::quantile(data1)
    }

    datalst <- list(data = data_scaled, data_orig = data,
        freq = freq, lmd = lmd0, dname = dname,
        q_orig = q_orig, q_scaled = q_scaled,
        result = NULL, coeffs = NULL, stepsize = stepsize)
    structure(datalst, class = "expmodel")
}


#' Summary of Exponential-based `expmodel` object.
#'
#' @description Summary of `expmodel` object, including a mean and quantiles.
#' If some estimation has done, also print out estimates, up to `nmax`
#' number of them.
#' @param object \code{expmodel} object.
#' @param nmax A number of estimates to show in the summary.
#' The default is 10.
#' @param estonly Show only the results of estimates. The default value
#' is `FALSE`.
#' @param ... Arguments to be passed to or from other methods.
#' @return None.
#' @method summary expmodel
#' @examples
#' ## Create expmodel object from a data set mixexpgamma$n200
#' emodel <- expmodel(mixexpgamma$n200)
#' ## Print a summary of an object
#' summary(emodel)
#' @seealso [expmodel()] [plot.expmodel()] [estimate.expmodel()]
#' @export
summary.expmodel <- function(object, nmax = 10, estonly=FALSE, ...) {
    if (length(nmax) != 1) {
        stop("'nmax' should be length 1.")
    }
    if (!is.numeric(nmax)) {
        stop("'nmax' should be numeric.")
    }
    if (any(is.na(nmax))) {
        stop("'nmax' contains NAs.")
    }
    if (nmax < 1) {
        stop("'nmax' should be larger than or equal to 1.")
    }

    if (!estonly) {
        ndata <- length(object$data)
        st1 <- datastats(object$data_orig, object$freq)
        cat("  SUMMARY\n")
        cat("Name: ")
        cat(object$dname, "\n")
        cat("The number of Data: ", ndata, "\n")
        cat("Mean   Std.\n")
        cat(st1[[1]], st1[[2]], "\n")
        cat("Quantile:\n")
        print(object$q_orig)
        cat("Quantile of Scaled Data:\n")
        print(object$q_scaled)
    }

    if (!is.null(object$result)) {
        cat("  ESTIMATION\n")
        cat("Name: ")
        cat(object$dname, "\n")
        ncol1 <- dim(object$result)[1]
        ncol1 <- min(ncol1, nmax)
        print(object$result[1:ncol1, c("deg", "lmd", "aic", "accuracy")])
    }
}


#' Plot a histogram and estimated densities/distributions of Exponential-based
#' model  object
#'
#' @description Plot the histogram and, if available, estimated densities
#' or cumulative distributions of `expmodel` object.
#' @param x \code{expmodel} object.
#' @param cum A logical scalar, whether or not it plots cumulative
#' histogram/distributions instead of plain histogram/densities.
#' Default value is `FALSE`.
#' @param nmax A maximum number of estimates to be plotted in the graph.
#'  The default value is 4.
#' @param graphs A vector of indices to be displayed in the graph.
#' These indices appear in the leftmost column of the table in
#' `summary.expmodel`.
#' The default value is `NULL`, and if it is not `NULL`, only the estimated
#' densities designated by `graphs` option appear, and `nmax` is ignored.
#' @param bins A number of bins of the histogram.
#' @param hist A logical scalar. If `TRUE`, display a histogram, otherwise not.
#' The default value is `TRUE`.
#' @param linesize A positive numeric scalar, which indicates the thickness of
#' lines. The default value is `1`.
#' @param ... Arguments to be passed to or from other methods.
#' @return A \code{ggplot2} object.
#' @examples
#' ## Create `expmodel` object from a data set mixexpgamma$n200
#' emodel <- expmodel(mixexpgamma$n200)
#' ## Plot it (histogram only)
#' plot(emodel)
#' @seealso [expmodel()] [summary.expmodel()] [func.expmodel()] [pdf_expmodel()]
#' [cdf_expmodel()]
#' @importFrom rlang .data
#' @method plot expmodel
#' @export
plot.expmodel <- function(x, cum=FALSE, nmax=4, graphs=NULL, bins=40,
        hist=TRUE, linesize=1, ...) {
    if (!is.numeric(nmax)) {
        stop("'nmax' should be numeric.")
    }
    if (any(is.na(nmax))) {
        stop("'nmax' contains NAs.")
    }
    if (nmax < 1) {
        stop("'nmax' should be larger than or equal to 1.")
    }
    if (!is.null(graphs)) {
        nEst <- 0
        if (!is.null(x$result)) {
            nEst <- dim(x$result)[1]
        }
        if (is.numeric(graphs)) {
            graphs <- as.integer(graphs)
            idx <- !is.nan(graphs) & graphs >= 1 & graphs <= nEst
            graphs <- graphs[idx]
            graphs <- unique(graphs)
        } else {
            stop("'graphs' should be 'NULL' or numeric")
        }
    }
    if (length(cum) > 1) {
        stop("'cum' should be a scalar logical.")
        if (!is.logical(cum)) {
            stop("'cum' should be 'TRUE' or 'FALSE'.")
        }
    }
    if (length(hist) > 1) {
        stop("'hist' should be a scalar logical.")
        if (!is.logical(hist)) {
            stop("'hist' should be 'TRUE' or 'FALSE'.")
        }
    }
    if (length(linesize) !=  1 || !is.numeric(linesize)) {
        stop("'linesize' should be a scalar numeric.")
    }

    max0 <- max(x$data_orig)
    breaks <- seq(0, max0, length.out = bins + 1)
    g <- ggplot2::ggplot()
    if (hist) {
        if (is.null(x$freq)) {
            if (!cum) {
                g <- g + ggplot2::geom_histogram(ggplot2::aes(
                        x = x$data_orig,
                        y = ggplot2::after_stat(.data$density)
                    ), breaks = breaks, fill = "white", color = "black")
            } else {
                dlen <- length(x$data_orig)
                g <- g + ggplot2::geom_histogram(ggplot2::aes(
                        x = x$data_orig,
                        y = cumsum(ggplot2::after_stat(.data$count)) / dlen
                    ), bins = bins, fill = "white", color = "black")
            }

        } else {
            freq_positive_idx <- x$freq > 0
            freq_positive <- x$freq[freq_positive_idx]
            nlen <- length(freq_positive)
            samples_positive <- x$data_orig[freq_positive_idx]
            v <- vector(mode = "list", length = nlen)
            for (i in 1:nlen) {
                v[[i]] <- rep(samples_positive[i], freq_positive[i])
            }
            cv <- do.call(c, v)
            if (!cum) {
                g <- g + ggplot2::geom_histogram(ggplot2::aes(
                        x = cv,
                        y = ggplot2::after_stat(.data$density)
                    ), breaks = breaks, fill = "white", color = "black")
            } else {
                g <- g + ggplot2::geom_histogram(ggplot2::aes(
                        x = cv,
                        y = cumsum(ggplot2::after_stat(.data$count)) /
                                length(cv)
                    ), bins = bins, fill = "white", color = "black")
            }
        }
    }

    if (nmax == 0 && is.null(graphs) && hist) {
        g <- g + ggplot2::xlab(x$dname) + ggplot2::ylab("Probability")
        return(g)
    }

    if (!is.null(x$result) && is.null(graphs)) {
        len_result <- dim(x$result)[1]
        n1 <- min(len_result, nmax)
        tbool <- !is.na(x$result$accuracy)
        tbool2 <- rep(FALSE, len_result)
        tbool2[1:n1] <- TRUE
        tbool <- tbool & tbool2
        ngraphs <- length(which(tbool))

        datarangetmp <- max(x$data_orig)
        datarangetmp01 <- datarangetmp * 0.2
        maxpt <- max(x$data_orig) + datarangetmp01
        nlen <- 81
        xv <- seq(0, maxpt, length.out = nlen)

        dlst <- vector(mode = "list", length = ngraphs)

        if (!cum) {
            for (k in 1:ngraphs) {
                dlst[[k]] <- pdf_expmodel(x$coeffs[[k]],
                        x$result[k, "lmd1"], xv)
            }
        } else {
            for (k in 1:ngraphs) {
                dlst[[k]] <- cdf_expmodel(x$coeffs[[k]],
                        x$result[k, "lmd1"], xv)
            }
        }

        yv1 <- do.call(c, dlst)
        xv1 <- rep(xv, ngraphs)
        dlst2 <- vector(mode = "list", length = ngraphs)
        for (k in 1:ngraphs) {
            s1 <- toString(x$result[k, "deg"])
            s2 <- toString(x$result[k, "lmd"])
            dlst2[[k]] <- paste("(", s1, ", ", s2, ")", sep = "")
        }
        dlst3 <- vector(mode = "list", length = ngraphs)
        for (k in 1:ngraphs) {
            dlst3[[k]] <- factor(rep(dlst2[[k]], nlen))
        }
        cv1 <- do.call(c, dlst3)
        df <- data.frame(x = xv1, y = yv1, params = cv1)

        g <- g + ggplot2::geom_line(data = df, ggplot2::aes(x = .data$x,
            y = .data$y, colour = .data$params), size = linesize)
    } else if (!is.null(x$result) && !is.null(graphs)) {
        ngraphs <- length(graphs)
        if (ngraphs < 1) {
            g <- g + ggplot2::xlab(x$dname) + ggplot2::ylab("Probability")
            return(g)
        }

        datarangetmp <- max(x$data_orig)
        datarangetmp01 <- datarangetmp * 0.2
        maxpt <- max(x$data_orig) + datarangetmp01
        nlen <- 81
        xv <- seq(0, maxpt, length.out = nlen)

        dlst <- vector(mode = "list", length = ngraphs)

        if (!cum) {
            for (k in 1:ngraphs) {
                dlst[[k]] <- pdf_expmodel(x$coeffs[[graphs[k]]],
                        x$result[graphs[k], "lmd1"], xv)
            }
        } else {
            for (k in 1:ngraphs) {
                dlst[[k]] <- cdf_expmodel(x$coeffs[[graphs[k]]],
                        x$result[graphs[k], "lmd1"], xv)
            }
        }

        yv1 <- do.call(c, dlst)
        xv1 <- rep(xv, ngraphs)
        dlst2 <- vector(mode = "list", length = ngraphs)
        for (k in 1:ngraphs) {
            s1 <- toString(x$result[graphs[k], "deg"])
            s2 <- toString(x$result[graphs[k], "lmd"])
            dlst2[[k]] <- paste("(", s1, ", ", s2, ")", sep = "")
        }
        dlst3 <- vector(mode = "list", length = ngraphs)
        for (k in 1:ngraphs) {
            dlst3[[k]] <- factor(rep(dlst2[[k]], nlen))
        }
        cv1 <- do.call(c, dlst3)
        df <- data.frame(x = xv1, y = yv1, params = cv1)

        g <- g + ggplot2::geom_line(data = df, ggplot2::aes(x = .data$x,
            y = .data$y, colour = .data$params), size = linesize)
    }

    g <- g + ggplot2::xlab(x$dname) + ggplot2::ylab("Probability")
    return(g)
}


#' Estimate Exponential-based model `expmodel`
#'
#' @description Estimates Exponential-based model `expmodel` among
#' parameter vectors, \code{deglist}, \code{lmdlist}.
#' Then it sorts the results by AIC.
#' @param model An object of \code{expmodel} class.
#' @param deglist A vector of degrees of polynomials. The element should be
#' positive integers.
#' @param lmdlist A vector of rate parameters of Exponential-based models.
#' The element should be larger than 0.
#' @param recompute If \code{TRUE}, recomputes the results for
#' better estimation and accuracy. Parameters whose accuracies had been already
#' attained sufficiently, namely around 1.0e-6, are not included in candidates
#' for recomputing.
#' @param stepsize A vector in descending order whose values are
#' between 0 and 1.
#' If a small step size is supplied, it can attain successful estimates,
#' but it might take more iterations.
#' @param verbose If \code{TRUE}, it shows the detailed message of SDP solver.
#' @param ... Arguments to be passed to or from other methods.
#' @return A \code{expmodel} object including the estimates.
#' Those estimates are stored in \code{model$result} with
#' \code{data.frame} format and \code{model$coeffs} in \code{list} format.
#' @examples
#' ## Create an expmodel object
#' emodel <- expmodel(mixexpgamma$n200)
#' ## Estimate a model with parameters
#' emodel <- estimate(emodel, deglist=c(4,5), lmdlist=c(0.5, 1, 2))
#' @seealso [expmodel()] [summary.expmodel()] [plot.expmodel()]
#' @method estimate expmodel
#' @export
estimate.expmodel <- function(model, deglist=deglist, lmdlist=lmdlist,
            recompute=FALSE, stepsize=NULL, verbose=FALSE, ...) {
    if (recompute && is.null(stepsize)) {
        stop("stepsize should be specified if 'recompute = TRUE'.")
    }
    if (is.null(stepsize)) {
        stepsize <- model$stepsize
    } else {
        if (!is.numeric(stepsize)) {
            stop("'stepsize' should be numeric.")
        }
        if (any(is.na(stepsize))) {
            stop("'stepsize' contains NAs.")
        }
        if (!all(stepsize > 0.0 & stepsize <= 0.98)) {
            stop("'stepsize' is not in (0.0, 0.98).")
        }
    }

    if (length(recompute) > 1) {
        stop("'recompute' should be a scalar logical.")
    }
    if (!is.logical(recompute)) {
        stop("'recompute' should be 'TRUE' or 'FALSE'.")
    }

    if (length(verbose) > 1) {
        stop("'verbose' should be a scalar logical.")
    }
    if (!is.logical(verbose)) {
        stop("'verbose' should be 'TRUE' or 'FALSE'.")
    }

    deglist <- as.integer(deglist)
    didx <- deglist >= 1 & deglist <= 30
    deglist <- deglist[didx]
    deglist <- sort(unique(deglist))
    lmdidx <- lmdlist > 0
    lmdlist <- lmdlist[lmdidx]
    lmdlist <- sort(unique(lmdlist))
    if (length(deglist) == 0) {
        stop("'deglist' is empty.")
    }
    if (length(lmdlist) == 0) {
        stop("'lmdlist' is empty.")
    }

    resultdf <- model$result
    estdf <- expand.grid(deg = deglist, lmd = lmdlist)
    idx <- order(estdf$deg, estdf$lmd)
    estdf <- estdf[idx, ]
    len_estdf <- dim(estdf)[1]
    lmd0 <- model$lmd

# Estimate first time.
    if (is.null(resultdf)) {
        rlst <- vector(mode = "list", length = len_estdf)
        coefflst <- vector(mode = "list", length = len_estdf)

        for (k in 1:len_estdf) {
            d <- estdf[k, "deg"]
            lmd <- estdf[k, "lmd"]
            message("(", d, ",", lmd, ")")
            est <- exp_est(as.integer(d), lmd, model$data, model$freq,
                        verbose, stepsize)
            est1 <- vector(mode = "numeric", length = 5)
            for (i in 1:4) {
                est1[i] <- est[[i]]
            }
            est1[5] <- lmd * lmd0
            if (!is.na(est[[4]])) {
                coeff1 <- polyaxb(est[[5]], 1.0, lmd0, 0.0)
                coefflst[[k]] <- coeff1
            } else {
                coefflst[[k]] <- NA
            }
            rlst[[k]] <- est1
        }

        rdf <- data.frame(do.call(rbind, rlst))
        colnames(rdf) <- c("deg", "lmd", "aic", "accuracy", "lmd1")
        ordidx <- order(rdf$aic)
        model$result <- rdf[ordidx, ]
        rownames(model$result) <- NULL
        model$coeffs <- coefflst[ordidx]

    } else if (!recompute) {
# Add new entries of estimations but do not update existing results
# even if they are NA or substandard accuracy.
        estdfidx <- rep(TRUE, len_estdf)
        for (k in 1:len_estdf) {
            d1 <- estdf[k, "deg"]
            lmd1 <- estdf[k, "lmd"]
            tbool <- resultdf$deg == d1 & nearlyequal(resultdf$lmd, lmd1)
            if (any(tbool)) {
                estdfidx[k] <- FALSE
            }
        }

        if (any(estdfidx)) {
            estdf <- estdf[estdfidx, ]
            len_estdf <- dim(estdf)[1]
            rlst <- vector(mode = "list", length = len_estdf)
            coefflst <- vector(mode = "list", length = len_estdf)

            for (k in 1:len_estdf) {
                d <- estdf[k, "deg"]
                lmd <- estdf[k, "lmd"]
                message("(", d, ",", lmd, ")")
                est <- exp_est(as.integer(d), lmd, model$data,
                        model$freq, verbose, stepsize)
                est1 <- vector(mode = "numeric", length = 5)
                for (i in 1:4) {
                    est1[i] <- est[[i]]
                }
                est1[5] <- lmd * lmd0
                if (!is.na(est[[4]])) {
                    coeff1 <- polyaxb(est[[5]], 1.0, lmd0, 0.0)
                    coefflst[[k]] <- coeff1
                } else {
                    coefflst[[k]] <- NA
                }
                rlst[[k]] <- est1
            }

            rdf <- data.frame(do.call(rbind, rlst))
            colnames(rdf) <- c("deg", "lmd", "aic", "accuracy", "lmd1")
            rdf <- rbind(resultdf, rdf)
            rlst2 <- c(model$coeffs, coefflst)
            ordidx <- order(rdf$aic)
            rdf <- rdf[ordidx, ]
            rownames(rdf) <- NULL
            model$result <- rdf
            model$coeffs <- rlst2[ordidx]
        }
    } else if (recompute) {
# Add estimations and update existing estimations if desired accuracy is not
# obtained.
        estdf0idx <- rep(FALSE, len_estdf)
        estdf1idx <- rep(FALSE, len_estdf)
        estdf2idx <- rep(FALSE, len_estdf)
        vecidx <- rep(0L, len_estdf)

        for (k in 1:len_estdf) {
            d1 <- estdf[k, "deg"]
            lmd1 <- estdf[k, "lmd"]
            tbool <- resultdf$deg == d1 & nearlyequal(resultdf$lmd, lmd1)

            if (any(tbool)) {
                l <- which(tbool)
                vecidx[k] <- l
                accuracyresult <- resultdf[l, "accuracy"]

                if (is.na(accuracyresult)) {
                    estdf1idx[k] <- TRUE
                } else if (accuracyresult > 1.0e-6) {
                    estdf2idx[k] <- TRUE
                }
            } else {
                estdf0idx[k] <- TRUE
            }
        }

        # New params
        if (any(estdf0idx)) {
            estdf0 <- estdf[estdf0idx, ]
            len_estdf0 <- dim(estdf0)[1]
            rlst0 <- vector(mode = "list", length = len_estdf0)
            coefflst <- vector(mode = "list", length = len_estdf0)
            for (k in 1:len_estdf0) {
                d <- estdf0[k, "deg"]
                lmd <- estdf0[k, "lmd"]
                message("(", d, ",", lmd, ")")
                est <- exp_est(as.integer(d), lmd, model$data,
                            model$freq, verbose, stepsize)
                est1 <- vector(mode = "numeric", length = 5)
                for (i in 1:4) {
                    est1[i] <- est[[i]]
                }
                est1[5] <- lmd * lmd0
                if (!is.na(est[[4]])) {
                    coeff1 <- polyaxb(est[[5]], 1.0, lmd0, 0.0)
                    coefflst[[k]] <- coeff1
                } else {
                    coefflst[[k]] <- NA
                }
                rlst0[[k]] <- est1
            }
        }

        # NA params
        if (any(estdf1idx)) {
            estdf1 <- estdf[estdf1idx, ]
            vecidx1 <- vecidx[estdf1idx]
            len_estdf1 <- dim(estdf1)[1]
            for (k in 1:len_estdf1) {
                d <- estdf1[k, "deg"]
                lmd <- estdf1[k, "lmd"]
                message("(", d, ",", lmd, ")")
                est <- exp_est(as.integer(d), lmd, model$data,
                            model$freq, verbose, stepsize)
                if (!is.na(est[[4]])) {
                    l <- vecidx1[k]
                    resultdf[l, "aic"] <- est[[3]]
                    resultdf[l, "accuracy"] <- est[[4]]
                    coeff1 <- polyaxb(est[[5]], 1.0, lmd0, 0.0)
                    model$coeffs[[l]] <- coeff1
                }
            }
        }

        # substandard accuracy params
        if (any(estdf2idx)) {
            estdf2 <- estdf[estdf2idx, ]
            vecidx2 <- vecidx[estdf2idx]
            len_estdf2 <- dim(estdf2)[1]
            for (k in 1:len_estdf2) {
                d <- estdf2[k, "deg"]
                lmd <- estdf2[k, "lmd"]
                message("(", d, ",", lmd, ")")
                est <- exp_est(as.integer(d), lmd, model$data,
                            model$freq, verbose, stepsize)
                if (!is.na(est[[4]])) {
                    l <- vecidx2[k]
                    if (est[[4]] < resultdf[l, "accuracy"]) {
                        resultdf[l, "aic"] <- est[[3]]
                        resultdf[l, "accuracy"] <- est[[4]]
                        coeff1 <- polyaxb(est[[5]], 1.0, lmd0, 0.0)
                        model$coeffs[[l]] <- coeff1
                    }
                }
            }
        }

        # Combine and sort Results
        # if new params exists
        if (any(estdf0idx)) {
            rdf0 <- data.frame(do.call(rbind, rlst0))
            colnames(rdf0) <- c("deg", "lmd", "aic", "accuracy", "lmd1")
            rdf <- rbind(resultdf, rdf0)

            coefflst1 <- c(model$coeffs, coefflst)
            ordidx <- order(rdf$aic)
            rdf <- rdf[ordidx, ]
            rownames(rdf) <- NULL
            model$result <- rdf
            model$coeffs <- coefflst1[ordidx]
        } else {
            ordidx <- order(resultdf$aic)
            model$result <- resultdf[ordidx, ]
            rownames(model$result) <- NULL
            model$coeffs <- model$coeffs[ordidx]
        }
    }

    return(model)
}


#' Return the evaluation of a vector with Exponential-based
#' model
#'
#' @description Evaluate an input vector `x` with Exponential-based model and
#' return its vector.
#' By default, it evaluate with the best model and its density, but
#' it can designate the model by index and also can evaluate with a cumulative
#' distribution.
#' @param model `expmodel` object.
#' @param x A numeric vector to be evaluated with a distribution.
#' @param cdf A logical scalar whether the evaluation is done with a cumulative
#' distribution or not. A default value is `FALSE`, which means that the
#' evaluation is done with a density.
#' @param n The index indicates the estimates. 1, by default, is the best
#' estimate, and 2 is the 2nd best, etc.
#' @param ... Arguments to be passed to or from other methods.
#' @return A numeric vector of the evaluatio of input vector `x` with a model.
#' @method func expmodel
#' @examples
#' ## Create an `expmodel` object
#' emodel <- expmodel(mixexpgamma$n200)
#' ## Estimate an model with parameters
#' emodel <- estimate(emodel, deglist=5, lmdlist=3.75)
#' ## A vector for input
#' x <- seq(0, 14, by=0.1)
#' ## Density function
#' y <- func(emodel, x)
#' ## Cumulative distribution
#' y <- func(emodel, x, cdf=TRUE)
#' @seealso [expmodel()] [summary.expmodel()] [plot.expmodel()]
#' [estimate.expmodel()] [pdf_expmodel()] [cdf_expmodel()]
#' @export
func.expmodel <- function(model, x, cdf=FALSE, n=1, ...) {
    if (is.null(model$result)) {
        stop("'model' has no estimates")
    }
    if (!is.numeric(x)) {
        stop("'x' should be numeric.")
    }
    if (any(is.na(x))) {
        stop("'x' contains NAs.")
    }
    if (any(is.infinite(x))) {
        stop("'x' contains Infs.")
    }

    if (length(cdf) > 1) {
        stop("'cdf' should be a scalar logical.")
    }
    if (!is.logical(cdf)) {
        stop("'cdf' should be logical")
    }

    if (!is.numeric(n)) {
        stop("'n' should be integer")
    }
    nest <- dim(model$result)[1]
    n <- as.integer(n)
    n <- n[1]
    if (n > nest || n <= 0) {
        stop("'n' is out of range")
    }

    if (!cdf) {
        z <- pdf_expmodel(model$coeffs[[n]], model$result[n, "lmd1"], x)
    } else {
        z <- cdf_expmodel(model$coeffs[[n]], model$result[n, "lmd1"], x)
    }
    return(z)
}


#' Estimate coefficients of a polynomial in Exponential-based Model
#'
#' @description Estimate coefficients of a polynomial in Exponential-based
#' model:
#' \deqn{\mathrm{poly}(x; \alpha) \mathrm{Exp}(x; \lambda)},
#' where \eqn{\alpha} is a coefficient vector, \eqn{\lambda} is a rate
#' parameter of an exponential distribution:
#' \deqn{\mathrm{Exp}(x; \lambda) := \lambda e^{-\lambda x}}.
#'
#' Using `data` and optionally its frequencies `freq`,
#' and a degree of a polynomial,
#' a rate parameter `lmd` of an exponential distribution,
#' it computes the coefficients of polynomial, along with
#' Akaike Information Criterion(AIC) and an accuracy information from
#' underlying SDP solver.
#' In general, the smaller the AIC is, the better the model is.
#' An `accuracy` around `1e-7` is a good indication for a computational
#' result of coefficients estimation.
#'
#' @param deg A degree of polynomial, which is positive even integer.
#' @param lmd A rate parameter of an exponential distribution, which
#' is positive.
#' @param data A numeric vector of a data set to be estimated.
#' @param freq A numeric vector of frequencies for a data set `data`.
#' The default value is `NULL`, which indicates that all frequencies are
#' equally one.
#' If `freq` is not `NULL`, then it should be the same length as `data`, and
#' all values should be positive integers.
#' @param verbose If `TRUE`, it shows a detail information about SDP solver.
#' @param stepvec It designates the stepsize for SDP solver.
#' If the problem is easy, i.e., the number of a data set are small and a degree
#' of a polynomial is small, then, for example, `0.9` might be ok.
#' If it looks difficult, then `c(0.5, 0.3)` might work.
#' @return A `list`  of `deg, lmd, aic, accuracy, coefficient vector`
#' @seealso [estimate.expmodel()]
#' @examples
#' rlst <- exp_est(3, 1.0, mixexpgamma$n200, NULL, FALSE, c(0.7, 0.4))
#' @export
exp_est <- function(deg, lmd, data, freq, verbose, stepvec)
    .Call(rsolve_ExpModel_, deg, lmd, data, freq, verbose, stepvec)


#' Probability density function of Exponential-based model
#'
#' @description A probability density function(PDF) of Exponential-based
#' model.
#' It is an underlying routine for `plot.expmodel` to compute the values
#' of PDF.
#' To access parameters and coefficients in an object `emodel`
#' of a class `expmodel`, use `emodel$result[k, "lmd1"]`,
#' `emodel$coeffs[[k]]` for some index `k`.
#' This index appears in the leftmost column of estimation table generated by
#' `summary(emodel)`.
#' @param coeff A coefficient vector in increasing order of degrees;
#' the first element is 0th degree, ..., and last element is the largest degree
#' of coefficients.
#' @param lmd A rate parameter of an exponential distribution, which is
#' positive.
#' @param x A numeric input vector.
#' @return A numeric vector of PDF of an exponential-based model.
#' @seealso [expmodel()] [summary.expmodel()] [estimate.expmodel()]
#' [func.expmodel()] [plot.expmodel()] [cdf_expmodel()]
#' @examples
#' ## Create an object of `expmodel`
#' emodel <- expmodel(mixexpgamma$n200)
#' ## Estimate with degree 4 and rate parameter 2.0
#' emodel <- estimate(emodel, 4, 2.0)
#' ## Input vector
#' x <- seq(0, 12, 0.1)
#' ## Output of PDF in above estimation
#' yv <- pdf_expmodel(emodel$coeffs[[1]], emodel$result[1, "lmd1"], x)
#' @export
pdf_expmodel <- function(coeff, lmd, x) .Call(reval_ExpModel_,
    coeff, lmd, x)


#' Cumulative distribution function of Expomemtial-based model
#'
#' @description A cumulative distribution function(CDF) of Exponential-based
#' model.
#' To access parameters and coefficients in an object `emodel`
#' of a class `expmodel`, use `emodel$result[k, "lmd1"]`,
#' `emodel$coeffs[[k]]` for some index `k`.
#' This index appears in the leftmost column of estimation table generated by
#' `summary(emodel)`.
#' @param coeff A coefficient vector in increasing order of degrees;
#' the first element is 0th degree, ..., and last element is the largest degree
#' of coefficients.
#' @param lmd A rate parameter, which is positive.
#' @param x A numeric vector of input.
#' @return A numeric vector of CDF of an exponential-based model.
#' @seealso [expmodel()] [summary.expmodel()] [estimate.expmodel()]
#' [func.expmodel()] [cdf_expmodel()]
#' @examples
#' ## Create an object of `expmodel`
#' emodel <- expmodel(mixexpgamma$n200)
#' ## Estimate with degree 4 and rate parameter 2.0
#' emodel <- estimate(emodel, 4, 2.0)
#' ## Input vector
#' x <- seq(0, 12, 0.1)
#' ## Output of PDF in above estimation
#' yv <- cdf_expmodel(emodel$coeffs[[1]], emodel$result[1, "lmd1"], x)
#' @export
cdf_expmodel <- function(coeff, lmd, x) .Call(rcdf_polyggamma_,
    coeff, 1.0, lmd, 1.0, x)
