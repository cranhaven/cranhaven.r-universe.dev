#' The iNZight summary improves upon the base R summary output for
#' fitted regression models. More information is provided and displayed
#' in a more intuitive format. This function both creates and returns a
#' summary object, as well as printing it.
#'
#' This summary function provides more information in the following ways:
#'
#' Factor headers are now given. The base level for a factor is also
#' listed with an estimate of 0. This is to make it clear what the base
#' level of a factor is, rather than attempting to work out by deduction
#' from what has already been printed.
#'
#' The p-value of a factor is now given; this is the output from
#' \code{\link[car]{Anova}}, which calculates the p-value based off of
#' Type III sums of squares, rather than sequentially as done by
#' \code{\link{anova}}.
#'
#' Each level of a factor is indented by 2 characters for its label and
#' its p-value to distinguish between a factor, and levels of a factor.
#'
#' The labels for each level of an interaction are now just the levels of
#' the factor (separated by a \code{.}), rather than being prepended with
#' the factor name also.
#'
#' @title Informative Summary Information for Regression Models
#'
#' @param x an object of class \code{"lm"}, \code{"glm"} or \code{"svyglm"},
#' usually the result of a call to the corresponding function.
#'
#' @param method one of either \code{"standard"} or \code{"bootstrap"}. If
#' \code{"bootstrap"}, then bootstrapped estimates and standard errors
#' are calculated; otherwise, uses the standard estimates.
#'
#' @param reorder.factors logical, if \code{TRUE}, and there are factors present in the model,
#' then the most common level of the factor is set to be the baseline.
#'
#' @param digits the number of significant digits to use when printing.
#'
#' @param symbolic.cor logical, if \code{TRUE}, print the correlations in a symbolic form
#' (see \code{\link{symnum}}), rather than as numbers.
#'
#' @param signif.stars logical, if \code{TRUE}, \sQuote{significance stars} are printed for
#' each coefficient.
#'
#' @param exclude a character vector of names of variables to be excluded from the
#' summary output (i.e., confounding variables).
#'
#' @param exponentiate.ci logical, if \code{TRUE}, the exponential of the
#' confidence intervals will be printed if appropriate (log/logit link or log
#' transformed response)
#'
#' @param ... further arguments passed to and from other methods.
#'
#' @return An object of class \code{summary.lm}, \code{summary.glm}, or
#' \code{summary.svyglm}.
#'
#' @author Simon Potter, Tom Elliott.
#'
#' @note   If any level is not observed in a factor, no p-values will be printed
#' on all factors. This is because we cannot calculate Type III sums of
#' squares when this is the case.
#'
#' The fitted model currently requires that the data are stored in a
#' dataframe, which is pointed at by the \code{data} argument to
#' \code{lm} (or equivalent).
#'
#' @seealso The model fitting functions \code{\link{lm}}, \code{\link{glm}}, and
#' \code{\link{summary}}.
#'
#' The 'survey' package: https://cran.r-project.org/package=survey.
#'
#' Function \code{\link{coef}} will extract the matrix of coefficients
#' with standard errors, t-statistics and p-values.
#'
#' To calculate p-values for factors, use \code{\link[car]{Anova}} with
#' type III sums of squares.
#'
#' @export
#'
#' @examples
#' m <- lm(Sepal.Length ~ ., data = iris)
#' iNZightSummary(m)
#'
#' # exclude confounding variables for which you don't
#' # need to know about their coefficients:
#' iNZightSummary(m, exclude = "Sepal.Width")
iNZightSummary <- function(x, method = "standard", reorder.factors = FALSE,
                           digits = max(3, getOption("digits") - 3),
                           symbolic.cor = x$symbolic.cor,
                           signif.stars = getOption("show.signif.stars"),
                           exclude = NULL,
                           exponentiate.ci = FALSE,
                           ...) {
    # method: 'standard' or 'bootstrap'
    # reorder.factors: TRUE - most common level as baseline
    # exclude: variables to be excluded from output (eg. confounders)
    # if (!is.null(exclude))
    #    exclude <- paste(exclude, collapse = '|')

    if (reorder.factors) {
        varsAreFactors <- which(sapply(x$model, class) %in%
            c("factor", "ordered"))
        if (length(varsAreFactors) > 0) {
            dataName <- modelDataName(x)
            old <- eval(parse(text = dataName))
            assign(dataName, reorderFactors(x$model))
            # newCall = modifyModelCall(x)
            # x = eval(newCall)
            x <- update(x)
            assign(dataName, old) # reset the original dataset
        }
    }

    ## If method bootstrap, get bootstrap inference
    if (method == "bootstrap") {
        bootCoefs <- bootstrapCoefs(x)
        T.info <- bootstrapTTests(bootCoefs)
        F.info <- bootstrapFTests(bootCoefs)

        if (bootCoefs$keptSamples / bootCoefs$N < .95) {
            lwd <- getOption("width")
            ind <- paste(rep(" ", floor(0.05 * lwd)), collapse = "")
            header <- paste(rep("=", lwd), collapse = "")
            parwrap <- function(txt, init = "", indent = "") {
                paste(strwrap(txt, prefix = indent, initial = init),
                    collapse = "\n"
                )
            }

            if (reorder.factors) {
                txt <-
                    "Not enough baseline cases in one or more factors even after reordering."
                cat(parwrap(paste(txt,
                    init = "Error: ",
                    indent = "       "
                )), "\n")
                txt <- "Use standard output instead."
                cat(parwrap(paste(txt,
                    init = "       ",
                    indent = "       "
                )), "\n")
                return(invisible())
            } else {
                txt <- "Not enough baseline cases in one or more factors."
                cat(parwrap(paste(txt,
                    init = "Warning: ",
                    indent = "         "
                )), "\n")
                txt <- "Trying again with factor levels reordered."
                cat(parwrap(paste(txt,
                    init = "         ",
                    indent = "         "
                )), "\n")
                iNZightSummary(x,
                    method = "bootstrap",
                    reorder.factors = TRUE,
                    digits = digits, symbolic.cor = symbolic.cor,
                    signif.stars = signif.stars
                )
                return(invisible())
            }
        }
    }

    x.lm <- x
    x.data <- x.lm$model
    x <- summary(x)
    surv <- ifelse(isSurvey(x.lm), "Survey ", "")
    genlin <- ifelse(isGlm(x.lm), "Generalised Linear ", "")
    baseline <- ""
    linkfun <- ""
    if (isGlm(x.lm) && x.lm$family$family == "binomial") {
        if (length(levels(x.lm$model[, 1]))) {
            baseline <- sprintf(
                " (baseline = %s)",
                levels(x.lm$model[, 1])[1]
            )
        }
        linkfun <- sprintf(
            "(using the %s link function)\n",
            x.lm$family$link
        )
    }

    cox <- ifelse(isCox(x.lm), "Cox Proportional Hazards ", "")

    cat(sprintf(
        "\n%s%s%sModel for: %s%s\n%s\n",
        surv, genlin, cox, gsub("survival::", "", attr(x.data, "names")[1]), baseline,
        linkfun
    ))

    if (isSurvey(x.lm)) {
        cat("Survey design:\n")
        print(x$survey.design$call)
        cat("\n")
    }

    if (isCox(x.lm)) {
        surv.resp <- names(x.lm$model)[1]
        cat("Survival parameters:\n")
        cat(sprintf("\tTime to follow-up: %s\n", gsub("(survival::)?Surv\\((.*), ?.*\\)", "\\2", surv.resp)))
        cat(sprintf("\tStatus indicator:  %s\n\n", gsub("(survival::)?Surv\\(.*, ?(.*)\\)", "\\2", surv.resp)))
    }

    # Print out a description of the confounding variables excluded from
    # output;
    if (!is.null(exclude)) {
        cat("The model has been adjusted for the",
            "following confounder(s):\n",
            sep = " "
        )
        cat("\t")
        cat(exclude, sep = ", ")
        cat("\n\n")
    }

    if (!isCox(x.lm)) {
        var.classes <- attr(x$terms, "dataClasses")[-1]
        var.labels <- attr(x$terms, "term.labels")
    } else {
        var.classes <- attr(x.lm$terms, "dataClasses")[-1]
        var.labels <- attr(x.lm$terms, "term.labels")
    }
    var.labels <- strsplit(var.labels, ":")
    resid <- ifelse(isGlm(x.lm), x$deviance.resid, ifelse(isCox(x.lm), x.lm$residuals, x$residuals))

    if (!isCox(x.lm)) {
        df <- x$df
        rdf <- df[2L]

        if (rdf > 5L) {
            nam <- c("Min", "1Q", "Median", "3Q", "Max")
            rq <- if (length(dim(resid)) == 2L) {
                structure(apply(t(resid), 1L, quantile),
                    dimnames = list(nam, dimnames(resid)[[2L]])
                )
            } else {
                zz <- zapsmall(quantile(resid), digits + 1)
                structure(zz, names = nam)
            }
        } else if (rdf > 0L) {
            print(resid, digits = digits, ...)
        } else { # rdf == 0 : perfect fit!
            cat(
                "ALL", df[1L],
                "residuals are 0: no residual degrees of freedom!\n"
            )
        }
    }

    if (length(x$aliased) == 0L && !isCox(x.lm)) {
        cat("\nNo Coefficients\n")
    } else {
        if (!isCox(x.lm)) {
            if (nsingular <- df[3L] - df[1L]) {
                cat("Coefficients: (", nsingular,
                    " not defined because of singularities)\n",
                    sep = ""
                )
            } else {
                cat("Coefficients:\n")
            }
        } else {
            cat("Coefficients:\n")
        }
        coefs <- x$coefficients
        if (!is.null(aliased <- x$aliased) && any(aliased)) {
            cn <- names(aliased)
            coefs <- matrix(NA, length(aliased), 4,
                dimnames = list(cn, colnames(coefs))
            )
            coefs[!aliased, ] <- x$coefficients
        }

        ### ------------------------------------------------------------ ###
        #                   iNZight changes start here                   #
        ### ------------------------------------------------------------ ###

        coefs.copy <- coefs
        if (isCox(x.lm)) {
            coefs.copy <- coefs.copy[, -2, drop = FALSE]
        }
        rowns <- rownames(coefs)
        varnames <- names(x.data)
        coefs.copy <- cbind(coefs.copy, as.matrix(confint(x.lm), ncol = 2L))

        if (!is.null(exclude)) {
            excl <- apply(sapply(
                exclude,
                function(x) grepl(x, rowns)
            ), 1, any)
            coefs.copy <- coefs.copy[!excl, , drop = FALSE]
        }
        na.line <- rep(NA, 4)
        i <- 1

        while (i <= nrow(coefs.copy)) {
            ## If the name has been modified, we know we're not dealing
            ## with a numeric variable, or it is crossed with some factor

            summary.row <- rownames(coefs.copy)[i]

            split.current.row <- strsplit(summary.row, ":")[[1]]
            nlines.to.add <- 1
            if (!summary.row %in% varnames) {
                # Need to test whether the var is indeed a factor
                # and that the factor contains the level we want
                for (j in 1:length(varnames)) {
                    current.var <- varnames[j]

                    # Need to account for the fact that there may be
                    # an interaction
                    # term being included, need to account for cases like:
                    # numeric*factor, factor*numeric, factor*factor,
                    # factor^3, etc

                    if (length(split.current.row) > 1) {
                        row.label <- ""
                        for (vl in 1:length(var.labels)) {
                            current.term <- var.labels[[vl]]
                            # Dealing with an interaction
                            if (length(current.term) > 1) {
                                row.cand <- substr(
                                    split.current.row, 1,
                                    nchar(current.term)
                                )
                                if (all(row.cand == current.term)) {
                                    row.label <- paste(current.term,
                                        collapse = ":"
                                    )
                                }
                            }
                        }

                        # Now that we have the row labels, try
                        # printing out all of the level labels

                        # Inserting the interaction title
                        if (row.label != rownames(coefs.copy)[i - 1]) {
                            if (method == "bootstrap") {
                                pvalue <- F.info$Pvals[row.label]
                            } else {
                                tmpaov <- tryCatch(car::Anova(x.lm, type = 3),
                                    error = function(e) NA
                                )
                                type3pval <-
                                    if (all(is.na(tmpaov))) {
                                        NA
                                    } else {
                                        tmpaov[
                                            which(rownames(tmpaov) == row.label),
                                            ifelse(isGlm(x.lm) || isCox(x.lm), 3, 4)
                                        ]
                                    }
                                pvalue <- type3pval
                            }

                            coefs.copy <-
                                insert.lines(
                                    row.label, i,
                                    c(rep(NA, 5), pvalue),
                                    coefs.copy
                                )
                        }

                        counter <- 1
                        cterms <- strsplit(row.label, ":")[[1]]

                        data.sub <- x.data[, cterms]
                        isf <- lapply(data.sub, class)
                        level.list <- list()
                        for (fs in 1:length(isf)) {
                            if (isf[fs] == "factor") {
                                ## Always omit base level ## ---> Why?
                                l <- levels(data.sub[, fs]) # [-1]
                            } else {
                                l <- cterms[fs]
                            }
                            level.list[[fs]] <- l
                        }
                        vars.to.parse <-
                            expand.grid(level.list,
                                stringsAsFactors = FALSE
                            )

                        for (l in 1:nrow(vars.to.parse)) {
                            row.data <- vars.to.parse[l, ]
                            int.effect.name <- paste(row.data,
                                collapse = "."
                            )
                            int.effect.name <- paste("  ", int.effect.name,
                                sep = ""
                            )
                            if (method == "bootstrap") {
                                rwname <- paste(paste(cterms, row.data,
                                    sep = ""
                                ), collapse = ":")
                                bsrow <- c(
                                    coefs.copy[i + counter, 1],
                                    bootCoefs$seCoef[rwname],
                                    T.info$t[rwname],
                                    T.info$p[rwname]
                                )
                                coefs.copy <-
                                    insert.lines(int.effect.name,
                                        i + counter, bsrow,
                                        coefs.copy,
                                        replace = TRUE
                                    )
                            } else {
                                coefs.copy <-
                                    insert.lines(int.effect.name,
                                        i + counter,
                                        coefs.copy[i + counter, ],
                                        coefs.copy,
                                        replace = TRUE
                                    )
                            }
                            counter <- counter + 1
                        }

                        nlines.to.add <- counter # Added rows plus var
                        break
                    } else {
                        # Just a factor:
                        summary.row.subs <-
                            substr(summary.row, 1, nchar(current.var))
                        row.var.level <-
                            substr(
                                summary.row, nchar(current.var) + 1,
                                nchar(summary.row)
                            )

                        # Is the rest of the string a level of the
                        # variable?
                        is.level.of.cvar <-
                            nchar(row.var.level) > 0 && row.var.level %in%
                                levels(x.data[, current.var])

                        # The case where we have a row with a
                        # substring matching an existing variable
                        # name, and there is a level present

                        if (current.var == summary.row.subs &&
                            is.level.of.cvar) {
                            levels.of.cvar <- levels(x.data[, current.var])
                            base.level <- levels.of.cvar[1]
                            nlines.to.add <- length(levels.of.cvar) + 1
                            new.names <-
                                c(current.var, paste("  ", levels.of.cvar,
                                    sep = ""
                                ))
                            for (k in (0:length(levels.of.cvar) + i)) {
                                name.k <- new.names[k - i + 1]
                                ## Var, need to get the p-val
                                if (k == i) {
                                    if (method == "bootstrap") {
                                        pvalue <- F.info$Pvals[summary.row]
                                    } else {
                                        tmpaov <-
                                            tryCatch(car::Anova(x.lm, type = 3),
                                                error = function(e) NA
                                            )
                                        type3pval <-
                                            if (all(is.na(tmpaov))) {
                                                NA
                                            } else {
                                                ## Anova() on glm has different dimensions:
                                                col.i <- ifelse(isGlm(x.lm), 3, ifelse(isCox(x.lm) && !("loglik" %in% colnames(tmpaov)), 3, 4))
                                                tmpaov[which(rownames(tmpaov) == name.k), col.i]
                                            }
                                        pvalue <- type3pval
                                    }
                                    coefs.copy <-
                                        insert.lines(
                                            name.k, k,
                                            c(rep(NA, 3), pvalue, rep(NA, 2)),
                                            coefs.copy
                                        )
                                }
                                ## Base level
                                if (k == (i + 1)) {
                                    coefs.copy <-
                                        insert.lines(
                                            name.k, k, rep(NA, 6),
                                            coefs.copy
                                        )
                                }

                                ## No longer dealing with var or base
                                if (k > (i + 1)) {
                                    if (method == "bootstrap") {
                                        original.name <-
                                            paste(current.var,
                                                substring(name.k, 3),
                                                sep = ""
                                            )
                                        bsCoefs <- c(
                                            coefs.copy[k, 1],
                                            bootCoefs$seCoef[original.name],
                                            T.info$t[original.name],
                                            T.info$p[original.name]
                                        )
                                        if (all(is.na(bsCoefs))) {
                                            bsCoefs <- rep(" ", 4)
                                        }
                                        coefs.copy <-
                                            insert.lines(name.k, k, bsCoefs,
                                                coefs.copy,
                                                replace = TRUE
                                            )
                                    } else {
                                        if (all(is.na(coefs.copy[k, ]))) {
                                            coefs.copy[k, ] <- rep(" ", 4)
                                        }
                                        coefs.copy <-
                                            insert.lines(name.k, k,
                                                coefs.copy[k, ],
                                                coefs.copy,
                                                replace = TRUE
                                            )
                                    }
                                }
                            }
                        }
                    }
                }
            }
            if (summary.row == "(Intercept)" || summary.row %in% varnames) {
                if (method == "bootstrap") {
                    bsrow <- c(
                        coefs.copy[i, 1],
                        bootCoefs$seCoef[summary.row],
                        T.info$t[summary.row],
                        T.info$p[summary.row]
                    )
                    coefs.copy <- insert.lines(summary.row, i, bsrow,
                        coefs.copy,
                        replace = TRUE
                    )
                }
            }
            i <- i + nlines.to.add
        }

        ## If the link is logit/log for GLM, or the response is log-transformed,
        ## add exponentiated coefficients to output
        log.resp <- grepl("^log\\(.*\\)", attr(x.lm$model, "names")[1])
        if (isGlm(x.lm) && x.lm$family$link %in% c("logit", "log") || log.resp || isCox(x.lm)) {
            coefs.copy <- cbind(
                coefs.copy[, 1, drop = FALSE],
                exp(coefs.copy[, 1]),
                coefs.copy[, 2:ncol(coefs.copy), drop = FALSE]
            )

            colnames(coefs.copy)[2] <- ifelse(
                isGlm(x.lm) && x.lm$family$link == "logit",
                "Odds Ratio",
                "Estimate (exp)"
            )

            if (exponentiate.ci) {
                ci.cols <- (ncol(coefs.copy) - 1):ncol(coefs.copy)
                coefs.copy[, ci.cols] <- exp(coefs.copy[, ci.cols, drop = FALSE])
                ci.label <- ifelse(
                    isGlm(x.lm) && x.lm$family$link == "logit",
                    "(OR)",
                    "(exp)"
                )

                colnames(coefs.copy)[ci.cols] <- paste(colnames(coefs.copy)[ci.cols], ci.label)
            }
        }

        iNZightPrintCoefmat(coefs.copy, digits = digits)

        ######
        # End iNZight changes
        ######
    }
    ##

    if (isGlm(x.lm)) {
        cat("\n(Dispersion parameter for ",
            x.lm$family$family, " family taken to be ",
            format(x$dispersion), ")\n\n",
            apply(
                cbind(
                    paste(format(c("Null", "Residual"),
                        justify = "right"
                    ), "deviance:"),
                    format(unlist(x[c("null.deviance", "deviance")]),
                        digits = max(5, digits + 1)
                    ), " on",
                    format(unlist(x[c("df.null", "df.residual")])),
                    " degrees of freedom\n"
                ),
                1L, paste,
                collapse = " "
            ),
            sep = ""
        )
        if (nzchar(mess <- naprint(x$na.action))) {
            cat("  (", mess, ")\n", sep = "")
        }
        if (!is.na(x$aic)) {
            cat("AIC: ", format(x$aic, digits = max(4, digits + 1)),
                "\n",
                sep = ""
            )
        }
        cat("\n", "Number of Fisher Scoring iterations: ", x$iter,
            "\n",
            sep = ""
        )
        correl <- x$correlation
        if (!is.null(correl)) {
            p <- NCOL(correl)
            if (p > 1) {
                cat("\nCorrelation of Coefficients:\n")
                if (is.logical(symbolic.cor) && symbolic.cor) {
                    print(symnum(correl, abbr.colnames = NULL))
                } else {
                    correl <- format(round(correl, 2),
                        nsmall = 2,
                        digits = digits
                    )
                    correl[!lower.tri(correl)] <- ""
                    print(correl[-1, -p, drop = FALSE], quote = FALSE)
                }
            }
        }
    } else if (!isCox(x.lm)) {
        cat(
            "\nResidual standard error:",
            format(signif(x$sigma, digits)), "on", rdf,
            "degrees of freedom\n"
        )
        if (nzchar(mess <- naprint(x$na.action))) {
            cat("  (", mess, ")\n",
                sep = ""
            )
        }
        if (!is.null(x$fstatistic)) {
            cat("Multiple R-squared:", formatC(x$r.squared, digits = digits))
            cat(",\tAdjusted R-squared:", formatC(x$adj.r.squared,
                digits = digits
            ), "\n")
        }
        correl <- x$correlation
        if (!is.null(correl)) {
            p <- NCOL(correl)
            if (p > 1L) {
                cat("\nCorrelation of Coefficients:\n")
                if (is.logical(symbolic.cor) && symbolic.cor) {
                    print(symnum(correl, abbr.colnames = NULL))
                } else {
                    correl <-
                        format(round(correl, 2),
                            nsmall = 2,
                            digits = digits
                        )
                    correl[!lower.tri(correl)] <- ""
                    print(correl[-1, -p, drop = FALSE], quote = FALSE)
                }
            }
        }
    } else if (isCox(x.lm)) {
        ## For Cox PH models, just print the last few lines of summary output
        other.stats <- utils::capture.output(x)
        s.len <- length(other.stats)

        other.stats <- other.stats[(s.len - 4):(s.len - 1)]
        cat("\n", other.stats, sep = "\n")
    }
    cat("\n")
    invisible(x)
}


iNZightPrintCoefmat <-
    function(x, digits = max(3, getOption("digits") - 2),
             signif.stars = getOption("show.signif.stars"),
             signif.legend = signif.stars,
             dig.tst = max(1, min(5, digits - 1)),
             cs.ind = 1:k, tst.ind = k + 1, zap.ind = integer(0L),
             P.values = NULL,
             has.Pvalue = nc >= 4 && any(substr(colnames(x), 1, 3) == "Pr("),
             eps.Pvalue = .Machine$double.eps,
             na.print = "NA", ...) {
        ## For printing ``coefficient matrices'' as they are in
        ## summary.xxx(.) where xxx in {lm, glm, aov, ..}. (Note:
        ## summary.aov(.) gives a class "anova").

        ## By Default Assume: x is a matrix-like numeric object.  ------
        ## with *last* column = P-values --iff-- P.values (== TRUE)
        ## columns {cs.ind}= numbers, such as coefficients & std.err
        ## [def.: 1L:k] columns {tst.ind}= test-statistics (as "z", "t",
        ## or "F") [def.: k+1]

        if (is.null(d <- dim(x)) || length(d) != 2L) {
            stop("'x' must be coefficient matrix/data frame")
        }
        nc <- d[2L]
        if (is.null(P.values)) {
            scp <- getOption("show.coef.Pvalues")
            if (!is.logical(scp) || is.na(scp)) {
                warning("option \"show.coef.Pvalues\" is invalid: assuming TRUE")
                scp <- TRUE
            }
            P.values <- has.Pvalue && scp
        } else if (P.values && !has.Pvalue) {
            stop("'P.values' is TRUE, but 'has.Pvalue' is not")
        }

        ## Renaming p-value column so that we're not using an explicit test stat
        pcol <- grep("Pr\\(", colnames(x))
        if (length(pcol) > 1) stop("Multiple p-value columns...?")
        colnames(x)[pcol] <- "p-value"

        if (has.Pvalue && !P.values) { # P values are there, but not wanted
            d <- dim(xm <- data.matrix(x[, -pcol, drop = FALSE]))
            nc <- nc - 1
            has.Pvalue <- FALSE
        } else {
            xm <- data.matrix(x)
        }

        k <- nc - has.Pvalue - (if (missing(tst.ind)) 1 else length(tst.ind))
        if (!missing(cs.ind) && length(cs.ind) > k) stop("wrong k / cs.ind")

        Cf <- array("", dim = d, dimnames = dimnames(xm))

        ok <- !(ina <- is.na(xm))
        ## zap before deciding any formats
        for (i in zap.ind) xm[, i] <- zapsmall(xm[, i], digits)
        if (length(cs.ind)) {
            acs <- abs(coef.se <- xm[, cs.ind, drop = FALSE])
            if (any(ia <- is.finite(acs))) {
                ## #{digits} BEFORE decimal point -- for min/max. value:
                digmin <- 1 + if (length(acs <- acs[ia & acs != 0])) {
                    floor(log10(range(acs[acs != 0], finite = TRUE)))
                } else {
                    0
                }
                Cf[, cs.ind] <- format(round(coef.se, max(1, digits - digmin)),
                    digits = digits
                )
            }
        }
        if (length(tst.ind)) {
            Cf[, tst.ind] <- format(round(xm[, tst.ind], digits = dig.tst),
                digits = digits
            )
        }
        if (any(r.ind <- !((1L:nc) %in%
            c(cs.ind, tst.ind, if (has.Pvalue) pcol)))) {
            for (i in which(r.ind)) Cf[, i] <- format(xm[, i], digits = digits)
        }
        ok[, tst.ind] <- FALSE
        okP <- if (has.Pvalue) ok[, -pcol] else ok
        ## we need to find out where Cf is zero.  We can't use as.numeric
        ## directly as OutDec could have been set.
        ## x0 <- (xm[okP]==0) != (as.numeric(Cf[okP])==0)
        x1 <- Cf[okP]
        dec <- getOption("OutDec")
        if (dec != ".") x1 <- chartr(dec, ".", x1)
        x0 <- (xm[okP] == 0) != (as.numeric(x1) == 0)
        if (length(not.both.0 <- which(x0 & !is.na(x0)))) {
            ## not.both.0==TRUE:  xm !=0, but Cf[] is: --> fix these:
            Cf[okP][not.both.0] <- format(xm[okP][not.both.0],
                digits = max(1, digits - 1)
            )
        }

        for (i in 1:nrow(Cf)) {
            curr.row <- Cf[i, ]
            ## Need to get numbers if possible
            curr.row <- suppressWarnings(as.numeric(curr.row))
            na.inds <- which(is.na(curr.row)) # No need for p-val
            curr.row.name <- rownames(Cf)[i]
            next.row.name <- if (i != nrow(Cf)) rownames(Cf)[i + 1] else ""
            prev.row.name <- if (i > 1) rownames(Cf)[i - 1] else ""

            if (i == nrow(Cf)) {
                curr.start.chars <- substr(curr.row.name, 1, 2)
                ## Only need to test whether this is a level of a factor.
                ## This cannot be a base level
                if (curr.start.chars == "  " && length(na.inds) > 0) {
                    Cf[i, na.inds] <- rep("-", length(na.inds))
                }
            }

            ## We need to test whether current row is a factor var label
            if (next.row.name != "") {
                curr.start.chars <- substr(curr.row.name, 1, 2)
                next.start.chars <- substr(next.row.name, 1, 2)
                prev.start.chars <- substr(prev.row.name, 1, 2)

                ## If there is indentation on the next row but not the
                ## current one
                if (curr.start.chars != "  " & next.start.chars == "  ") {
                    if (length(na.inds) > 0) {
                        Cf[i, na.inds] <- " "
                    }
                }

                ## If there is indentation in current line, assume both
                ## level of factor
                if (curr.start.chars == "  " && length(na.inds) > 0) {
                    if (prev.start.chars != "  ") { # is base level
                        Cf[i, na.inds] <- c("0", rep("-", length(na.inds) - 1))
                    } else { # is an error row
                        Cf[i, na.inds] <- rep("-", length(na.inds))
                    }
                }
            }
        }

        ## Need to fix ina because we have modified which are NA values
        ## due to factor levels
        ina <- is.na(suppressWarnings(as.numeric(Cf)) & !Cf %in% c("-", " "))
        if (any(ina)) Cf[ina] <- na.print

        if (P.values) {
            if (!is.logical(signif.stars) || is.na(signif.stars)) {
                warning("option \"show.signif.stars\" is invalid: assuming TRUE")
                signif.stars <- TRUE
            }
            if (any(okP <- ok[, pcol])) {
                orig.pv <- xm[, pcol] # keeping copy with names
                empty.lvl <- Cf[, 1] == "-" # Used for when we cannot use
                # a level
                lvl <- substr(names(orig.pv), 1, 2) == "  "
                pv <- as.vector(xm[, pcol]) # drop names
                okP[empty.lvl] <- TRUE
                pvals <- format.pval(c(pv[okP & !lvl], pv[okP & lvl]),
                    digits = dig.tst, eps = eps.Pvalue
                )
                reg.pvals <- pvals[1:sum(okP & !lvl)]
                lvl.pvals <- pvals[-(1:sum(okP & !lvl))]
                lvl.pvals[lvl.pvals == "NA"] <- "-" # when there is no
                # observation on this
                # lvl Need to indent
                # level values to the
                # right, consequently
                # regular p-values
                # must be indented to
                # the left
                reg.pvals <- paste(reg.pvals, "  ", sep = "")
                lvl.pvals <- paste("  ", lvl.pvals, sep = "")
                Cf[okP & !lvl, pcol] <- reg.pvals
                Cf[okP & lvl, pcol] <- lvl.pvals
                signif.stars <- signif.stars && any(pv[okP] < .1)
                if (signif.stars) {
                    Signif <- symnum(pv,
                        corr = FALSE, na = FALSE,
                        cutpoints = c(0, .001, .01, .05, .1, 1),
                        symbols = c("***", "**", "*", ".", " ")
                    )
                    # place stars directly after p-value
                    Cf <- cbind(
                        Cf[, 1:pcol, drop = FALSE],
                        format(Signif),
                        Cf[, -(1:pcol), drop = FALSE]
                    ) # format.ch: right=TRUE
                }
            } else {
                signif.stars <- FALSE
            }
        } else {
            signif.stars <- FALSE
        }
        print.default(Cf, quote = FALSE, right = TRUE, na.print = na.print, ...)
        if (signif.stars && signif.legend) {
            cat("---\nSignif. codes: ", attr(Signif, "legend"), "\n")
        }
        invisible(x)
    }

insert.lines <- function(name, line.num, linedata, mat, replace = FALSE) {
    nr <- nrow(mat)
    mat.copy <- mat
    mat[line.num, ] <- linedata
    mat <- mat[1:line.num, ]
    mat <- rbind(mat, mat.copy[line.num:nr, ])
    rns <- rownames(mat)
    ## Remove the line following the one we just added
    if (replace) {
        mat <- mat[-(line.num + 1), ]
        rownames(mat)[line.num] <- name
    } else {
        if (line.num == 1) {
            rownames(mat) <- c(name, rns[-1])
        } else {
            rownames(mat) <- c(
                rns[1:(line.num - 1)], name,
                rns[line.num:nr + 1]
            )
        }
    }
    mat
}
