#' Constructor for S3 class `gaussmodel`
#'
#' @description This function is a constructor for S3 class `gaussmodel`,
#' which represents Gaussian-based model.
#' It usually takes `data` and optionally `freq` as arguments and
#' also optionally `stepsize`.
#' Members of interest in practice are `result` and `coeffs`, which maintain
#' the information of estimates and coefficients of polynomials, respectively.
#' @param data A numeric vector of a data set to be estimated.
#' @param freq A frequency vector corresponding to the `data` vector.
#' The default value is `NULL`, which means all frequencies are one.
#' If supplied, the length of a vector should be same as `data` and
#' each element should be a nonnegative integer.
#' @param stepsize A numeric vector whose element is larger than 0 and smaller
#' than 1, and decreasing order. The default value is \code{c(0.5, 0.3)}.
#' If you encounter numerical difficulties, decreasing its values, for example,
#' to \code{c(0.4, 0.2)}, might help to estimate a model.
#' @return An object of Gaussian-based model `gaussmodel`.
#' @examples
#' ## Create `gaussmodel` object from a data set `mix2gauss$n200`.
#'  gmodel <- gaussmodel(mix2gauss$n200)
#' ## Create `gaussmodel` object from a data set `mix2gaussHist$n200p` and
#' ## its frequencies `mix2gaussHist$n200f`.
#'  gmodel <- gaussmodel(mix2gaussHist$n200p, mix2gaussHist$n200f)
#' @seealso [summary.gaussmodel()] [plot.gaussmodel()] [estimate.gaussmodel()]
#' @export
gaussmodel <- function(data=data, freq=NULL,
    stepsize=c(0.5, 0.3)) {
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

    stats0 <- datastats(data, freq)
    mu0 <- stats0[[1]]
    sig0 <- stats0[[2]]
    if (!is.null(freq)) {
        idx <- (freq != 0)
        data <- data[idx]
        freq <- freq[idx]
    }
    data_scaled <- (data - mu0) / sig0

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
        freq = freq, mu = mu0, sig = sig0, dname = dname,
        q_orig = q_orig, q_scaled = q_scaled,
        result = NULL, coeffs = NULL, coeffs_scaled = NULL,
        stepsize = stepsize)
    structure(datalst, class = "gaussmodel")
}


#' Summary of Gaussian-based model `gaussmodel` object
#'
#' @description Summary of `gaussmodel` object, including a mean and
#' a standard deviation and quantiles.
#' If some estimation has done, also print out estimates, up to `nmax`
#' number of them.
#' @param object \code{gaussmodel} object.
#' @param nmax A number of estimates to show in the summary.
#' The default is 10.
#' @param estonly Show only the results of estimates. The default value
#' is `FALSE`.
#' @param ... Arguments to be passed to or from other methods.
#' @return None.
#' @method summary gaussmodel
#' @examples
#' ## Create gaussmodel object from a data set mix2gauss$n200
#' gmodel <- gaussmodel(mix2gauss$n200)
#' ## Print a summary of an object
#' summary(gmodel)
#' @seealso [gaussmodel()] [plot.gaussmodel()] [estimate.gaussmodel()]
#' @export
summary.gaussmodel <- function(object, nmax=10, estonly=FALSE, ...) {
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
        if (estonly) {
            cat("  ESTIMATION\n")
            cat("Name: ")
            cat(object$dname, "\n")
        } else {
            cat("\n")
            cat("  ESTIMATION\n")
        }
        ncol1 <- dim(object$result)[1]
        ncol1 <- min(ncol1, nmax)
        print(object$result[1:ncol1, c("deg", "mu1", "sig1", "mu", "sig",
                "aic", "accuracy")])
    }
}


#' Plot a histogram and estimated densities/distributions of
#' Gaussian-based model object
#'
#' @description Plot the histogram and, if available, estimated densities
#' or cumulative distributions of `gaussmodel` object.
#' @param x \code{gaussmodel} object.
#' @param cum A logical scalar, whether or not it plots cumulative
#' histogram/distributions instead of plain histogram/densities.
#' Default value is `FALSE`.
#' @param nmax A maximum number of estimates to be plotted in the graph.
#'  The default value is 4.
#' @param graphs A vector of indices to be displayed in the graph.
#' These indices appear in the leftmost column of the table in
#' `estimate.gaussmodel`.
#' The default value is `NULL`, and if it is not `NULL`, only the estimated
#' densities designated by `graphs` option appear, and `nmax` is ignored.
#' @param bins A number of bins of the histogram.
#' @param hist A logical scalar. If `TRUE`, display a histogram, otherwise not.
#' The default value is `TRUE`.
#' @param scaling A logical scalar, which indicates whether or not it scales
#' means and standard deviations in `mulist` and `sdlist`.
#' The default value is `FALSE`.
#' @param linesize A positive numeric scalar, which indicates the thickness of
#' lines. The default value is `1`.
#' @param ... Arguments to be passed to or from other methods.
#' @return A \code{ggplot2} object.
#' @examples
#' ## Create `gaussmodel` object from a data set mix2gauss$n200
#' gmodel <- gaussmodel(mix2gauss$n200)
#' ## Plot it (histogram only)
#' plot(gmodel)
#' @seealso [gaussmodel()] [summary.gaussmodel()] [func.gaussmodel()]
#' [pdf_gaussmodel()] [cdf_gaussmodel()]
#' @importFrom rlang .data
#' @method plot gaussmodel
#' @export
plot.gaussmodel <- function(x, cum=FALSE, nmax=4, graphs=NULL, bins=40,
        hist=TRUE, scaling=FALSE, linesize=1, ...) {
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
            stop("'graphs' should be 'NULL' or numeric.")
        }
    }

    if (length(cum) > 1) {
        stop("'cum' should be a scalar logical.")
    }
    if (!is.logical(cum)) {
        stop("'cum' should be 'TRUE' or 'FALSE'.")
    }

    if (length(hist) > 1) {
        stop("'hist' should be a scalar logical.")
    }
    if (!is.logical(hist)) {
        stop("'hist' should be 'TRUE' or 'FALSE'.")
    }

    if (length(scaling) > 1) {
        stop("'scaling' should be a scalar logical.")
    }
    if (!is.logical(scaling)) {
        stop("'scaling' should be 'TRUE' or 'FALSE'.")
    }

    if (length(linesize) !=  1 || !is.numeric(linesize)) {
        stop("'linesize' should be a scalar numeric.")
    }


    g <- ggplot2::ggplot()
    if (hist) {
        if (!scaling) {
            xdata <- x$data_orig
        } else {
            xdata <- x$data
        }

        if (is.null(x$freq)) {
            if (!cum) {
                g <- g + ggplot2::geom_histogram(ggplot2::aes(
                        x = xdata,
                        y = ggplot2::after_stat(.data$density)
                    ), bins = bins, fill = "white", color = "black")
            } else {
                dlen <- length(xdata)
                g <- g + ggplot2::geom_histogram(ggplot2::aes(
                        x = xdata,
                        y = cumsum(ggplot2::after_stat(.data$count)) / dlen
                    ), bins = bins, fill = "white", color = "black")
            }

        } else {
            freq_positive_idx <- x$freq > 0
            freq_positive <- x$freq[freq_positive_idx]
            nlen <- length(freq_positive)
            data_positive <- xdata[freq_positive_idx]
            v <- vector(mode = "list", length = nlen)
            for (i in 1:nlen) {
                v[[i]] <- rep(data_positive[i], freq_positive[i])
            }
            data <- do.call(c, v)
            if (!cum) {
                g <- g + ggplot2::geom_histogram(ggplot2::aes(
                        x = data,
                        y = ggplot2::after_stat(.data$density)
                    ), bins = bins, fill = "white", color = "black")
            } else {
                g <- g + ggplot2::geom_histogram(ggplot2::aes(
                        x = data,
                        y = cumsum(ggplot2::after_stat(.data$count)) /
                                length(data)
                    ), bins = bins, fill = "white", color = "black")
            }
        }
    }

    if (nmax == 0 && is.null(graphs) && hist) {
        if (scaling) {
            xlabel <- paste0("Scaled: ", x$dname)
        } else {
            xlabel <- x$dname
        }
        g <- g + ggplot2::xlab(xlabel) + ggplot2::ylab("Probability")
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

        dlst <- vector(mode = "list", length = ngraphs)
        dlst2 <- vector(mode = "list", length = ngraphs)
        nlen <- 101

        if (!scaling) {
            datarangetmp <- max(x$data_orig) - min(x$data_orig)
            datarangetmp01 <- datarangetmp * 0.1
            minpt <- min(x$data_orig) - datarangetmp01
            maxpt <- max(x$data_orig) + datarangetmp01
            xv <- seq(minpt, maxpt, length.out = nlen)
            if (!cum) {
                for (k in 1:ngraphs) {
                    dlst[[k]] <- pdf_gaussmodel(x$coeffs[[k]],
                            x$result[k, "mu1"], x$result[k, "sig1"], xv)
                }
            } else {
                for (k in 1:ngraphs) {
                    dlst[[k]] <- cdf_gaussmodel(x$coeffs[[k]],
                            x$result[k, "mu1"], x$result[k, "sig1"], xv)
                }
            }

            for (k in 1:ngraphs) {
                s1 <- toString(x$result[k, "deg"])
                s2 <- sprintf("%.5f", x$result[k, "mu1"])
                s3 <- sprintf("%.5f", x$result[k, "sig1"])
                dlst2[[k]] <- paste("(", s1, ", ", s2, ", ", s3, ")", sep = "")
            }
        } else {
            datarangetmp <- max(x$data) - min(x$data)
            datarangetmp01 <- datarangetmp * 0.1
            minpt <- min(x$data) - datarangetmp01
            maxpt <- max(x$data) + datarangetmp01
            xv <- seq(minpt, maxpt, length.out = nlen)
            if (!cum) {
                for (k in 1:ngraphs) {
                    dlst[[k]] <- pdf_gaussmodel(x$coeffs_scaled[[k]],
                            x$result[k, "mu"], x$result[k, "sig"], xv)
                }
            } else {
                for (k in 1:ngraphs) {
                    dlst[[k]] <- cdf_gaussmodel(x$coeffs_scaled[[k]],
                            x$result[k, "mu"], x$result[k, "sig"], xv)
                }
            }

            for (k in 1:ngraphs) {
                s1 <- toString(x$result[k, "deg"])
                s2 <- sprintf("%.5f", x$result[k, "mu"])
                s3 <- sprintf("%.5f", x$result[k, "sig"])
                dlst2[[k]] <- paste("(", s1, ", ", s2, ", ", s3, ")", sep = "")
            }
        }

        yv1 <- do.call(c, dlst)
        xv1 <- rep(xv, ngraphs)

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

        dlst <- vector(mode = "list", length = ngraphs)
        dlst2 <- vector(mode = "list", length = ngraphs)
        nlen <- 101

        if (!scaling) {
            datarangetmp <- max(x$data_orig) - min(x$data_orig)
            datarangetmp01 <- datarangetmp * 0.1
            minpt <- min(x$data_orig) - datarangetmp01
            maxpt <- max(x$data_orig) + datarangetmp01
            xv <- seq(minpt, maxpt, length.out = nlen)
            if (!cum) {
                for (k in 1:ngraphs) {
                    dlst[[k]] <- pdf_gaussmodel(x$coeffs[[graphs[k]]],
                                    x$result[graphs[k], "mu1"],
                                    x$result[graphs[k], "sig1"],
                                    xv)
                }
            } else {
                for (k in 1:ngraphs) {
                    dlst[[k]] <- cdf_gaussmodel(x$coeffs[[graphs[k]]],
                                    x$result[graphs[k], "mu1"],
                                    x$result[graphs[k], "sig1"],
                                    xv)
                }
            }

            for (k in 1:ngraphs) {
                s1 <- toString(x$result[graphs[k], "deg"])
                s2 <- sprintf("%.5f", x$result[graphs[k], "mu1"])
                s3 <- sprintf("%.5f", x$result[graphs[k], "sig1"])
                dlst2[[k]] <- paste("(", s1, ", ", s2, ", ", s3, ")", sep = "")
            }
        } else {
            datarangetmp <- max(x$data) - min(x$data)
            datarangetmp01 <- datarangetmp * 0.1
            minpt <- min(x$data) - datarangetmp01
            maxpt <- max(x$data) + datarangetmp01
            xv <- seq(minpt, maxpt, length.out = nlen)
            if (!cum) {
                for (k in 1:ngraphs) {
                    dlst[[k]] <- pdf_gaussmodel(x$coeffs_scaled[[graphs[k]]],
                                    x$result[graphs[k], "mu"],
                                    x$result[graphs[k], "sig"],
                                    xv)
                }
            } else {
                for (k in 1:ngraphs) {
                    dlst[[k]] <- cdf_gaussmodel(x$coeffs_scaled[[graphs[k]]],
                                    x$result[graphs[k], "mu"],
                                    x$result[graphs[k], "sig"],
                                    xv)
                }
            }

            for (k in 1:ngraphs) {
                s1 <- toString(x$result[graphs[k], "deg"])
                s2 <- sprintf("%.5f", x$result[graphs[k], "mu"])
                s3 <- sprintf("%.5f", x$result[graphs[k], "sig"])
                dlst2[[k]] <- paste("(", s1, ", ", s2, ", ", s3, ")", sep = "")
            }
        }

        yv1 <- do.call(c, dlst)
        xv1 <- rep(xv, ngraphs)

        dlst3 <- vector(mode = "list", length = ngraphs)
        for (k in 1:ngraphs) {
            dlst3[[k]] <- factor(rep(dlst2[[k]], nlen))
        }
        cv1 <- do.call(c, dlst3)
        df <- data.frame(x = xv1, y = yv1, params = cv1)

        g <- g + ggplot2::geom_line(data = df, ggplot2::aes(x = .data$x,
                    y = .data$y, colour = .data$params), size = linesize)
    }

    if (scaling) {
        xlabel <- paste0("Scaled: ", x$dname)
    } else {
        xlabel <- x$dname
    }
    g <- g + ggplot2::xlab(xlabel) + ggplot2::ylab("Probability")
    return(g)
}


#' Estimate Gaussian-based model `gaussmodel`
#'
#' @description  Estimates Gaussian-based model `gaussmodel` among
#' parameter vectors, \code{deglist}, \code{mulist}, \code{sdlist}.
#' Then it sorts the results by AIC.
#' @param model An object of a \code{gaussmodel} class.
#' @param deglist A vector of degrees of polynomials. The element should be
#' positive even numbers.
#' @param mulist A vector of means for Gaussian-based models.
#' @param sdlist A vector of standard deviations for Gaussian-based models.
#' The element should be larger than 0.
#' @param scaling A logical scalar, which indicates whether or not it scales
#' means and standard deviations in `mulist` and `sdlist`.
#' The default value is `FALSE`.
#' @param recompute If \code{TRUE}, recomputes the results for better
#' estimation and accuracy. Parameters whose accuracies had been already
#' attained sufficiently, namely around 1.0e-6, are not included in candidates
#' for recomputing.
#' @param stepsize A vector in descending order whose values are
#' between 0 and 1.
#' If a small step size is supplied, it can attain successful estimates,
#' but it might take more iterations.
#' @param verbose If \code{TRUE}, it shows the detailed message of SDP solver.
#' @param ... Arguments to be passed to or from other methods.
#' @return A \code{gaussmodel} object including the estimates.
#' Those estimates are stored in \code{model$result} with
#' \code{data.frame} format and \code{model$coeffs} in \code{list} format.
#' @examples
#' ## Create an `gaussmodel` object
#' gmodel <- gaussmodel(mix2gauss$n200)
#' ## Estimate a model with parameters
#' gmodel <- estimate(gmodel, deglist=c(2, 4), mulist=c(0.0, 0.2),
#'              sdlist=c(0.75, 1.0))
#' @seealso [gaussmodel()] [summary.gaussmodel()] [plot.gaussmodel()]
#' @method estimate gaussmodel
#' @export
estimate.gaussmodel <- function(model, deglist=deglist, mulist=mulist,
            sdlist=sdlist, scaling=FALSE,
            recompute=FALSE, stepsize=NULL, verbose=FALSE,
            ...) {
    if (recompute && is.null(stepsize)) {
        stop("stepsize should be specified.")
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

    if (length(scaling) > 1) {
        stop("'scaling' should be a scalar logical.")
    }
    if (!is.logical(scaling)) {
        stop("'scaling' should be 'TRUE' or 'FALSE'.")
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
    didx <- deglist %% 2 == 0
    deglist <- deglist[didx]
    didx <- deglist >= 2 & deglist <= 30
    deglist <- deglist[didx]
    deglist <- sort(unique(deglist))
    mulist <- sort(unique(mulist))
    sdidx <- sdlist > 0
    sdlist <- sdlist[sdidx]
    sdlist <- sort(unique(sdlist))
    if (length(deglist) == 0) {
        stop("'deglist' is empty.")
    }
    if (length(mulist) == 0) {
        stop("'mulist' is empty.")
    }
    if (length(sdlist) == 0) {
        stop("'sdlist' is empty.")
    }

    resultdf <- model$result
    estdf <- expand.grid(deg = deglist, mu = mulist, sig = sdlist)
    idx <- order(estdf$deg, estdf$mu, estdf$sig)
    estdf <- estdf[idx, ]
    len_estdf <- dim(estdf)[1]
    mu0 <- model$mu
    sig0 <- model$sig

# Estimate in a first time.
    if (is.null(resultdf)) {
        rlst <- vector(mode = "list", length = len_estdf)
        coefflst <- vector(mode = "list", length = len_estdf)
        coefflst_scaled <- vector(mode = "list", length = len_estdf)

        if (scaling) {
            for (k in 1:len_estdf) {
                d <- estdf[k, "deg"]
                mu <- estdf[k, "mu"]
                sig <- estdf[k, "sig"]
                message("(", d, ",", mu, ",", sig, ")")
                est <- gauss_est(as.integer(d), mu, sig, model$data, model$freq,
                            verbose, stepsize)
                est1 <- vector(mode = "numeric", length = 7)
                for (i in 1:5) {
                    est1[i] <- est[[i]]
                }
                est1[6] <- mu0 + sig0 * mu
                est1[7] <- sig * sig0
                if (!is.na(est[[4]])) {
                    coefflst_scaled[[k]] <- est[[6]]
                    coeff1 <- polyaxb(est[[6]], 1.0, 1.0 / sig0, -mu0 / sig0)
                    coefflst[[k]] <- coeff1
                } else {
                    coefflst[[k]] <- NA
                    coefflst_scaled[[k]] <- NA
                }
                rlst[[k]] <- est1
            }

            rdf <- data.frame(do.call(rbind, rlst))
            colnames(rdf) <- c("deg", "mu", "sig", "aic", "accuracy", "mu1",
                                "sig1")
            ordidx <- order(rdf$aic)
            model$result <- rdf[ordidx, ]
            rownames(model$result) <- NULL
            model$coeffs <- coefflst[ordidx]
            model$coeffs_scaled <- coefflst_scaled[ordidx]
        } else {
            for (k in 1:len_estdf) {
                d <- estdf[k, "deg"]
                mu <- estdf[k, "mu"]
                sig <- estdf[k, "sig"]
                message("(", d, ",", mu, ",", sig, ")")
                mu_scaled <- (mu - mu0) / sig0
                sig_scaled <- sig / sig0
                est <- gauss_est(as.integer(d), mu_scaled, sig_scaled,
                            model$data, model$freq, verbose, stepsize)
                est1 <- vector(mode = "numeric", length = 7)
                for (i in 1:5) {
                        est1[i] <- est[[i]]
                }
                est1[6] <- mu
                est1[7] <- sig
                if (!is.na(est[[4]])) {
                    coefflst_scaled[[k]] <- est[[6]]
                    coeff1 <- polyaxb(est[[6]], 1.0, 1.0 / sig0, -mu0 / sig0)
                    coefflst[[k]] <- coeff1
                } else {
                    coefflst_scaled[[k]] <- NA
                    coefflst[[k]] <- NA
                }
                rlst[[k]] <- est1
            }

            rdf <- data.frame(do.call(rbind, rlst))
            colnames(rdf) <- c("deg", "mu", "sig", "aic", "accuracy", "mu1",
                                "sig1")
            ordidx <- order(rdf$aic)
            model$result <- rdf[ordidx, ]
            rownames(model$result) <- NULL
            model$coeffs <- coefflst[ordidx]
            model$coeffs_scaled <- coefflst_scaled[ordidx]
        }
    } else if (!recompute) {
# Add new entries of estimations but do not update existing results
# even if they are NA or substandard accuracy.
        estdfidx <- rep(TRUE, len_estdf)

        if (scaling) {
            for (k in 1:len_estdf) {
                d1 <- estdf[k, "deg"]
                mu1 <- estdf[k, "mu"]
                sig1 <- estdf[k, "sig"]
                tbool <- resultdf$deg == d1 &
                            nearlyequal(resultdf$mu, mu1) &
                            nearlyequal(resultdf$sig, sig1)
                if (any(tbool)) {
                    estdfidx[k] <- FALSE
                }
            }

            if (any(estdfidx)) {
                estdf <- estdf[estdfidx, ]
                len_estdf <- dim(estdf)[1]
                rlst <- vector(mode = "list", length = len_estdf)
                coefflst <- vector(mode = "list", length = len_estdf)
                coefflst_scaled <- vector(mode = "list", length = len_estdf)

                for (k in 1:len_estdf) {
                    d <- estdf[k, "deg"]
                    mu <- estdf[k, "mu"]
                    sig <- estdf[k, "sig"]
                    message("(", d, ",", mu, ",", sig, ")")
                    est <- gauss_est(as.integer(d), mu, sig, model$data,
                            model$freq, verbose, stepsize)
                    est1 <- vector(mode = "numeric", length = 7)
                    for (i in 1:5) {
                        est1[i] <- est[[i]]
                    }
                    est1[6] <- mu0 + sig0 * mu
                    est1[7] <- sig * sig0
                    if (!is.na(est[[4]])) {
                        coeff1 <- polyaxb(est[[6]], 1.0, 1.0 / sig0,
                                    -mu0 / sig0)
                        coefflst[[k]] <- coeff1
                        coefflst_scaled[[k]] <- est[[6]]
                    } else {
                        coefflst[[k]] <- NA
                        coefflst_scaled[[k]] <- NA
                    }
                    rlst[[k]] <- est1
                }

                rdf <- data.frame(do.call(rbind, rlst))
                colnames(rdf) <- c("deg", "mu", "sig", "aic", "accuracy", "mu1",
                                    "sig1")
                rdf <- rbind(resultdf, rdf)
                rlst2 <- c(model$coeffs, coefflst)
                rlst2_scaled <- c(model$coeffs_scaled, coefflst_scaled)
                ordidx <- order(rdf$aic)
                rdf <- rdf[ordidx, ]
                rownames(rdf) <- NULL
                model$result <- rdf
                model$coeffs <- rlst2[ordidx]
                model$coeffs_scaled <- rlst2_scaled[ordidx]
            }
        } else {
            for (k in 1:len_estdf) {
                d1 <- estdf[k, "deg"]
                mu1 <- estdf[k, "mu"]
                sig1 <- estdf[k, "sig"]
                tbool <- resultdf$deg == d1 &
                            nearlyequal(resultdf$mu1, mu1) &
                            nearlyequal(resultdf$sig1, sig1)
                if (any(tbool)) {
                    estdfidx[k] <- FALSE
                }
            }

            if (any(estdfidx)) {
                estdf <- estdf[estdfidx, ]
                len_estdf <- dim(estdf)[1]
                rlst <- vector(mode = "list", length = len_estdf)
                coefflst <- vector(mode = "list", length = len_estdf)
                coefflst_scaled <- vector(mode = "list", length = len_estdf)

                for (k in 1:len_estdf) {
                    d <- estdf[k, "deg"]
                    mu <- estdf[k, "mu"]
                    sig <- estdf[k, "sig"]
                    message("(", d, ",", mu, ",", sig, ")")
                    mu_scaled <- (mu - mu0) / sig0
                    sig_scaled <- sig / sig0
                    est <- gauss_est(as.integer(d), mu_scaled, sig_scaled,
                            model$data, model$freq, verbose, stepsize)
                    est1 <- vector(mode = "numeric", length = 7)
                    for (i in 1:5) {
                        est1[i] <- est[[i]]
                    }
                    est1[6] <- mu
                    est1[7] <- sig
                    if (!is.na(est[[4]])) {
                        coefflst_scaled[[k]] <- est[[6]]
                        coeff1 <- polyaxb(est[[6]], 1.0, 1.0 / sig0,
                                -mu0 / sig0)
                        coefflst[[k]] <- coeff1
                    } else {
                        coefflst_scaled[[k]] <- NA
                        coefflst[[k]] <- NA
                    }
                    rlst[[k]] <- est1
                }

                rdf <- data.frame(do.call(rbind, rlst))
                colnames(rdf) <- c("deg", "mu", "sig", "aic", "accuracy", "mu1",
                                    "sig1")
                rdf <- rbind(resultdf, rdf)
                rlst2 <- c(model$coeffs, coefflst)
                rlst2_scaled <- c(model$coeffs_scaled, coefflst_scaled)
                ordidx <- order(rdf$aic)
                rdf <- rdf[ordidx, ]
                rownames(rdf) <- NULL
                model$result <- rdf
                model$coeffs <- rlst2[ordidx]
                model$coeffs_scaled <- rlst2_scaled[ordidx]
            }
        }
    } else if (recompute) {
# Add estimations and update existing estimations if desired accuracy is not
# obtained.
        estdf0idx <- rep(FALSE, len_estdf)
        estdf1idx <- rep(FALSE, len_estdf)
        estdf2idx <- rep(FALSE, len_estdf)
        vecidx <- rep(0L, len_estdf)

        if (scaling) {
            for (k in 1:len_estdf) {
                d1 <- estdf[k, "deg"]
                mu1 <- estdf[k, "mu"]
                sig1 <- estdf[k, "sig"]
                tbool <- resultdf$deg == d1 &
                            nearlyequal(resultdf$mu, mu1) &
                            nearlyequal(resultdf$sig, sig1)

                if (any(tbool)) {
                # previously computed.
                    l <- which(tbool)
                    vecidx[k] <- l
                    accuracyresult <- resultdf[l, "accuracy"]

                    if (is.na(accuracyresult)) {
                    # accuracy is NA i.e., previous computations failed.
                        estdf1idx[k] <- TRUE
                    } else if (accuracyresult > 1.0e-6) {
                    # accuracy > 1.0e6, i.e., not enough accuracy
                    # in previous computations
                        estdf2idx[k] <- TRUE
                    }
                } else {
                # never computed.
                    estdf0idx[k] <- TRUE
                }
            }

            # New params
            if (any(estdf0idx)) {
                estdf0 <- estdf[estdf0idx, ]
                len_estdf0 <- dim(estdf0)[1]
                rlst0 <- vector(mode = "list", length = len_estdf0)
                coefflst <- vector(mode = "list", length = len_estdf0)
                coefflst_scaled <- vector(mode = "list", length = len_estdf0)

                for (k in 1:len_estdf0) {
                    d <- estdf0[k, "deg"]
                    mu <- estdf0[k, "mu"]
                    sig <- estdf0[k, "sig"]
                    message("(", d, ",", mu, ",", sig, ")")
                    est <- gauss_est(as.integer(d), mu, sig, model$data,
                                model$freq, verbose, stepsize)
                    est1 <- vector(mode = "numeric", length = 7)
                    for (i in 1:5) {
                        est1[i] <- est[[i]]
                    }
                    est1[6] <- mu0 + sig0 * mu
                    est1[7] <- sig * sig0
                    if (!is.na(est[[4]])) {
                        coeff1 <- polyaxb(est[[6]], 1.0, 1.0 / sig0,
                                    -mu0 / sig0)
                        coefflst[[k]] <- coeff1
                        coefflst_scaled[[k]] <- est[[6]]
                    } else {
                        coefflst[[k]] <- NA
                        coefflst_scaled[[k]] <- NA
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
                    mu <- estdf1[k, "mu"]
                    sig <- estdf1[k, "sig"]
                    message("(", d, ",", mu, ",", sig, ")")
                    est <- gauss_est(as.integer(d), mu, sig, model$data,
                                model$freq, verbose, stepsize)
                    if (!is.na(est[[5]])) {
                        l <- vecidx1[k]
                        resultdf[l, "aic"] <- est[[4]]
                        resultdf[l, "accuracy"] <- est[[5]]
                        coeff1 <- polyaxb(est[[6]], 1.0, 1.0 / sig0,
                                    -mu0 / sig0)
                        model$coeffs[[l]] <- coeff1
                        model$coeffs_scaled[[l]] <- est[[6]]
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
                    mu <- estdf2[k, "mu"]
                    sig <- estdf2[k, "sig"]
                    message("(", d, ",", mu, ",", sig, ")")
                    est <- gauss_est(as.integer(d), mu, sig, model$data,
                                model$freq, verbose, stepsize)
                    if (!is.na(est[[5]])) {
                        l <- vecidx2[k]
                        if (est[[5]] < resultdf[l, "accuracy"]) {
                            resultdf[l, "aic"] <- est[[4]]
                            resultdf[l, "accuracy"] <- est[[5]]
                            coeff1 <- polyaxb(est[[6]], 1.0, 1.0 / sig0,
                                    -mu0 / sig0)
                            model$coeffs[[l]] <- coeff1
                            model$coeffs_scaled[[l]] <- est[[6]]
                        }
                    }
                }
            }
        } else {
            for (k in 1:len_estdf) {
                d1 <- estdf[k, "deg"]
                mu1 <- estdf[k, "mu"]
                sig1 <- estdf[k, "sig"]
                tbool <- resultdf$deg == d1 &
                            nearlyequal(resultdf$mu1, mu1) &
                            nearlyequal(resultdf$sig1, sig1)

                if (any(tbool)) {
                # previously computed.
                    l <- which(tbool)
                    vecidx[k] <- l
                    accuracyresult <- resultdf[l, "accuracy"]

                    if (is.na(accuracyresult)) {
                    # accuracy is NA i.e., previous computations failed.
                        estdf1idx[k] <- TRUE
                    } else if (accuracyresult > 1.0e-6) {
                    # accuracy > 1.0e6, i.e., not enough accuracy
                    # in previous computations
                        estdf2idx[k] <- TRUE
                    }
                } else {
                # never computed.
                    estdf0idx[k] <- TRUE
                }
            }

            # New params
            if (any(estdf0idx)) {
                estdf0 <- estdf[estdf0idx, ]
                len_estdf0 <- dim(estdf0)[1]
                rlst0 <- vector(mode = "list", length = len_estdf0)
                coefflst <- vector(mode = "list", length = len_estdf0)
                coefflst_scaled <- vector(mode = "list", length = len_estdf0)

                for (k in 1:len_estdf0) {
                    d <- estdf0[k, "deg"]
                    mu <- estdf0[k, "mu"]
                    sig <- estdf0[k, "sig"]
                    message("(", d, ",", mu, ",", sig, ")")
                    mu_scaled <- (mu - mu0) / sig0
                    sig_scaled <- sig / sig0
                    est <- gauss_est(as.integer(d), mu_scaled, sig_scaled,
                                model$data, model$freq, verbose, stepsize)
                    est1 <- vector(mode = "numeric", length = 7)
                    for (i in 1:5) {
                        est1[i] <- est[[i]]
                    }
                    est1[6] <- mu
                    est1[7] <- sig
                    if (!is.na(est[[4]])) {
                        coeff1 <- polyaxb(est[[6]], 1.0, 1.0 / sig0,
                                    -mu0 / sig0)
                        coefflst[[k]] <- coeff1
                        coefflst_scaled[[k]] <- est[[6]]
                    } else {
                        coefflst[[k]] <- NA
                        coefflst_scaled[[k]] <- NA
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
                    mu <- estdf1[k, "mu"]
                    sig <- estdf1[k, "sig"]
                    message("(", d, ",", mu, ",", sig, ")")
                    mu_scaled <- (mu - mu0) / sig0
                    sig_scaled <- sig / sig0
                    est <- gauss_est(as.integer(d), mu_scaled, sig_scaled,
                                model$data, model$freq, verbose, stepsize)
                    if (!is.na(est[[5]])) {
                        l <- vecidx1[k]
                        resultdf[l, "aic"] <- est[[4]]
                        resultdf[l, "accuracy"] <- est[[5]]
                        coeff1 <- polyaxb(est[[6]], 1.0, 1.0 / sig0,
                                    -mu0 / sig0)
                        model$coeffs[[l]] <- coeff1
                        model$coeffs_scaled[[l]] <- est[[6]]
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
                    mu <- estdf2[k, "mu"]
                    sig <- estdf2[k, "sig"]
                    message("(", d, ",", mu, ",", sig, ")")
                    mu_scaled <- (mu - mu0) / sig0
                    sig_scaled <- sig / sig0
                    est <- gauss_est(as.integer(d), mu_scaled, sig_scaled,
                                model$data, model$freq, verbose, stepsize)
                    if (!is.na(est[[5]])) {
                        l <- vecidx2[k]
                        if (est[[5]] < resultdf[l, "accuracy"]) {
                            resultdf[l, "aic"] <- est[[4]]
                            resultdf[l, "accuracy"] <- est[[5]]
                            coeff1 <- polyaxb(est[[6]], 1.0, 1.0 / sig0,
                                    -mu0 / sig0)
                            model$coeffs[[l]] <- coeff1
                            model$coeffs_scaled[[l]] <- est[[6]]
                        }
                    }
                }
            }
        }
        # Combine and sort Results
        if (any(estdf0idx)) {
        # if new params exists
            rdf0 <- data.frame(do.call(rbind, rlst0))
            colnames(rdf0) <- c("deg", "mu", "sig", "aic", "accuracy", "coeff1",
                                "mu1", "sig1")
            rdf <- rbind(resultdf, rdf0)

            coefflst1 <- c(model$coeffs, coefflst)
            coefflst1_scaled <- c(model$coeffs_scaled, coefflst_scaled)
            ordidx <- order(rdf$aic)
            rdf <- rdf[ordidx, ]
            rownames(rdf) <- NULL
            model$result <- rdf
            model$coeffs <- coefflst1[ordidx]
            model$coeffs_scaled <- coefflst1_scaled[ordidx]
        } else {
            ordidx <- order(resultdf$aic)
            model$result <- resultdf[ordidx, ]
            rownames(model$result) <- NULL
            model$coeffs <- model$coeffs[ordidx]
            model$coeffs_scaled <- model$coeffs_scaled[ordidx]
        }
    }

    return(model)
}

#' Return the evaluation of a vector with Gaussian-based model
#'
#' @description Evaluate an input vector `x` with Gaussian-based model and
#' return its vector.
#' By default, it evaluate with the best model and its density, but
#' it can designate the model by index and also can evaluate with a cumulative
#' distribution.
#' @param model `gaussmodel` object.
#' @param x A numeric vector to be evaluated with a distribution.
#' @param cdf A logical scalar whether the evaluation is done with a cumulative
#' distribution or not. A default value is `FALSE`, which means that the
#' evaluation is done with a density.
#' @param n The index indicates the estimates. 1, by default, is the best
#' estimate, and 2 is the 2nd best, etc.
#' @param scaling A logical scalar, which indicates whether or not it scales
#' means and standard deviations in `mulist` and `sdlist`.
#' The default value is `FALSE`.
#' @param ... Arguments to be passed to or from other methods.
#' @return A numeric vector of the evaluatio of input vector `x` with a model.
#' @method func gaussmodel
#' @examples
#' ## Create an `gaussmodel` object
#' gmodel <- gaussmodel(mix2gauss$n200)
#' ## Estimate an model with parameters
#' gmodel <- estimate(gmodel, deglist=4, mulist=0.15, sdlist=0.73)
#' ## A vector for input
#' x <- seq(-4, 4, by=0.1)
#' ## Density function
#' y <- func(gmodel, x)
#' ## Cumulative distribution
#' y <- func(gmodel, x, cdf=TRUE)
#' @seealso [gaussmodel()] [summary.gaussmodel()] [plot.gaussmodel()]
#' [estimate.gaussmodel()] [pdf_gaussmodel()] [cdf_gaussmodel()]
#' @export
func.gaussmodel <- function(model, x, cdf=FALSE, n=1, scaling=FALSE, ...) {
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
        stop("'cdf' should be 'TRUE' or 'FALSE'.")
    }

    if (length(scaling) > 1) {
        stop("'scaling' should be a scalar logical.")

    }
    if (!is.logical(scaling)) {
        stop("'scaling' should be 'TRUE' or 'FALSE'.")
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

    if (!scaling) {
        if (!cdf) {
            z <- pdf_gaussmodel(model$coeffs[[n]], model$result[n, "mu1"],
                    model$result[n, "sig1"], x)
        } else {
            z <- cdf_gaussmodel(model$coeffs[[n]], model$result[n, "mu1"],
                    model$result[n, "sig1"], x)
        }
    } else {
        if (!cdf) {
            z <- pdf_gaussmodel(model$coeffs_scaled[[n]], model$result[n, "mu"],
                    model$result[n, "sig"], x)
        } else {
            z <- cdf_gaussmodel(model$coeffs_scaled[[n]], model$result[n, "mu"],
                    model$result[n, "sig"], x)
        }
    }
    return(z)
}


#' Estimate coefficients of a polynomial in Gaussian-based model
#'
#' @description Estimate coefficients of a polynomial in Gaussian-based model:
#' \deqn{\mathrm{poly}(x, \alpha) N(x; \mu, \sigma^2)},
#' where \eqn{\alpha} is a coefficient vector, \eqn{\mu} and \eqn{\sigma}
#' are a mean and a standard deviation of Gaussian distribution:
#' \deqn{N(x; \mu, \sigma^2) :=\frac{1}{\sigma \sqrt{2\pi}}
#'  \exp\left(-\frac{(x-\mu)^2}{2\sigma^2}\right) }
#'
#' Using `data` and optionally its frequencies `freq`,
#' and a degree of a polynomial,
#' a mean `mu` and a standard deviation `sig` of Gausian distribution,
#' it computes the coefficients of a polynomial, along with
#' Akaike Information Criterion(AIC) and an accuracy information from
#' an underlying SDP solver.
#' In general, the smaller the AIC is, the better the model is.
#' An `accuracy` around `1e-7` is a good indication for a computational
#' result of coefficients estimation.
#'
#' @param deg A degree of polynomial, which is positive even integer.
#' @param mu A mean of Gaussian distribution.
#' @param sig A standard deviation of Gaussian distribution, which is positive.
#' @param data A numeric vector of a data set to be estimated.
#' @param freq A numeric vector of frequencies for a data set `data`.
#' The default value is `NULL`, which indicates that all frequencies are
#' equally one.
#' If `freq` is not `NULL`, then it should be the same length as `data`, and
#' all values should be positive integers.
#' @param verbose If `TRUE`, it shows a detail information about SDP solver.
#' @param stepsize It designates the stepsize for SDP solver.
#' If the problem is easy, i.e., the number of a data set are small and a degree
#' of a polynomial is small, then, for example, `0.9` might be ok.
#' If it looks difficult, then `c(0.5, 0.3)` might work.
#' @return A `list` of `deg`, `mu`, `sig`, `aic`, `accuracy`,
#' `coefficient vector`.
#' @seealso [estimate.gaussmodel()]
#' @examples
#' rlst <- gauss_est(4, 0, 1, mix2gauss$n200, NULL, FALSE, c(0.7, 0.4))
#' @export
gauss_est <- function(deg, mu, sig, data, freq, verbose, stepsize)
    .Call(rsolve_GaussModel_, deg, mu, sig, data, freq, verbose, stepsize)


#' Probability density function of Gaussian-based model
#'
#' @description A probability density function(PDF) of a Gaussian model.
#' It is an underlying routine for `plot.gaussmodel` to compute the values
#' of PDF.
#' To access parameters and coefficients in an object `gmodel`
#' of a class `gaussmodel`, use `gmodel$result[k, "mu1"]`,
#' `gmodel$result[k, "sig1"]`, `gmodel$coeffs[[k]]` for some index `k`.
#' This index appears in the leftmost column of estimation table generated by
#' `summary(gmodel)`.
#' @param coeff A coefficient vector in increasing order of degrees;
#' the first element is 0th degree, ..., and last element is the largest degree
#' of coefficients.
#' @param mu A mean of Gaussian distribution.
#' @param sig A standard deviation of Gaussian distribution, which is positive.
#' @param x A numeric input vector.
#' @return A numeric vector of PDF of Gaussian-based distribution.
#' @seealso [gaussmodel()] [summary.gaussmodel()] [estimate.gaussmodel()]
#' [func.gaussmodel()] [plot.gaussmodel()] [cdf_gaussmodel()]
#' @examples
#' ## Create an object of `gaussmodel`
#' gmodel <- gaussmodel(mix2gauss$n200)
#' ## Estimate with a degree 6, a mean 0, and standard deviations 0.5
#' gmodel <- estimate(gmodel, 6, 0, 0.5)
#' ## Input vector
#' x <- seq(-3, 3, 0.1)
#' ## Output of PDF in above estimation
#' yv <- pdf_gaussmodel(gmodel$coeffs[[1]], gmodel$result[1, "mu1"],
#'  gmodel$result[1, "sig1"], x)
#' @export
pdf_gaussmodel <- function(coeff, mu, sig, x) .Call(reval_GaussModel_,
    coeff, mu, sig, x)


#' Cumulative distribution function of Gaussian-based model
#'
#' @description A cumulative distribution function(CDF) of Gaussian-based model.
#' To access parameters and coefficients in an object `gmodel`
#' of a class `gaussmodel`, use `gmodel$result[k, "mu1"]`,
#' `gmodel$result[k, "sig1"]`, `gmodel$coeffs[[k]]` for some index `k`.
#' This index appears in the leftmost column of estimation table generated by
#' `summary(gmodel)`.
#' @param coeff A coefficient vector in increasing order of degrees;
#' the first element is 0th degree, ..., and last element is the largest degree
#' of coefficients.
#' @param mu A mean of Gaussian distribution.
#' @param sig A standard deviation of Gaussian distribution, which is positive.
#' @param x A numeric input vector.
#' @return A numeric vector of CDF of Gaussian-based model.
#' @seealso [gaussmodel()] [summary.gaussmodel()] [estimate.gaussmodel()]
#' [func.gaussmodel()] [pdf_gaussmodel()]
#' @examples
#' ## Create an object of `gaussmodel`
#' gmodel <- gaussmodel(mix2gauss$n200)
#' ## Estimate with a degree 6, a mean 0, and standard deviations 0.5
#' gmodel <- estimate(gmodel, 6, 0, 0.5)
#' ## Input vector
#' x <- seq(-3, 3, 0.1)
#' ## Output of PDF in above estimation
#' yv <- cdf_gaussmodel(gmodel$coeffs[[1]], gmodel$result[1, "mu1"],
#'  gmodel$result[1, "sig1"], x)
#' @export
cdf_gaussmodel <- function(coeff, mu, sig, x) .Call(rcdf_polygauss_, coeff,
    mu, sig, x)
