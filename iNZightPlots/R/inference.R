inference <- function(object, ...) {
    UseMethod("inference")
}


#' @export
inference.inzdot <- function(object, des, bs, opts, class, width,
                             vn, hypothesis, survey.options, ...) {
    toplot <- object$toplot
    inf <- object$inference.info

    if (is.character(inf)) {
        return("Sample too small to get inference")
    }

    if (is.null(inf[[1]]$conf)) {
        warning("Please specify `inference.type = conf` to get inference information.")
        return("No inference information available. Sample size too small?")
    }

    is.survey <- !is.null(des)

    ci.width <- attr(inf, "ci.width")
    mat <- inf$mean$conf[, c("mean", "lower", "upper"), drop = FALSE]

    mat <- format(mat, digits = opts$signif)

    ## Remove NA's and replace with an empty space
    mat[grep("NA", mat)] <- ""

    ## Text formatting to return a character vector - each row of matrix
    mat <- rbind(c("Estimate", "Lower", "Upper"), mat)
    colnames(mat) <- NULL

    byFactor <- length(toplot) > 1

    if (byFactor) {
        mat <- cbind(c("", names(toplot)), mat)
    }
    rownames(mat) <- NULL

    mat <- matrix(
        apply(
            mat, 2,
            function(col) {
                format(col, justify = "right")
            }
        ),
        nrow = nrow(mat)
    )

    out <- apply(
        mat, 1,
        function(x) paste0("   ", paste(x, collapse = "   "))
    )

    plural <- ifelse(byFactor, "s", "")
    bsCI <- ifelse(bs, " Percentile Bootstrap", "")
    out <- c(
        paste0(
            ifelse(byFactor,
                ifelse(is.survey, "Population Means", "Group Means"),
                ifelse(is.survey, "Population Mean", "Mean")
            ),
            " with ", ci.width * 100, "%",
            bsCI,
            " Confidence Interval",
            plural
        ),
        "",
        out
    )

    if (bs) {
        if (is.survey) {
            return("Bootstrap inference not yet implemented for survey data.")
        }

        ## BOOTSTRAP MEDIAN
        mat <- inf$median$conf[, c("mean", "lower", "upper"), drop = FALSE]

        mat <- format(mat, digits = opts$signif)
        # mat <- matrix(
        #     apply(
        #         mat, 2,
        #         function(col) {
        #             format(col, digits = opts$signif)
        #         }
        #     ),
        #     nrow = nrow(mat)
        # )

        ## Remove NA's and replace with an empty space
        mat[grep("NA", mat)] <- ""

        ## Text formatting to return a character vector - each row of matrix
        mat <- rbind(c("Estimate", "Lower", "Upper"), mat)
        colnames(mat) <- NULL

        byFactor <- length(toplot) > 1

        if (byFactor) {
            mat <- cbind(c("", names(toplot)), mat)
        }
        rownames(mat) <- NULL

        mat <- matrix(
            apply(
                mat, 2,
                function(col) {
                    format(col, justify = "right")
                }
            ),
            nrow = nrow(mat)
        )

        mat <- apply(
            mat, 1,
            function(x) paste0("   ", paste(x, collapse = "   "))
        )

        out <- c(
            out,
            "",
            paste0(
                ifelse(byFactor, "Group Medians", "Median"),
                " with ", ci.width * 100, "%",
                bsCI,
                " Confidence Interval",
                plural
            ),
            "",
            mat
        )


        ## BOOTSTRAP INTERQUARTILE RANGE
        mat <- inf$iqr$conf[, c("mean", "lower", "upper"), drop = FALSE]

        mat <- format(mat, digits = opts$signif)
        # mat <- matrix(
        #     apply(
        #         mat, 2,
        #         function(col) {
        #             format(col, digits = opts$signif)
        #         }
        #     ),
        #     nrow = nrow(mat)
        # )

        ## Remove NA's and replace with an empty space
        mat[grep("NA", mat)] <- ""

        ## Text formatting to return a character vector - each row of matrix
        mat <- rbind(c("Estimate", "Lower", "Upper"), mat)
        colnames(mat) <- NULL

        byFactor <- length(toplot) > 1

        if (byFactor) {
            mat <- cbind(c("", names(toplot)), mat)
        }
        rownames(mat) <- NULL

        mat <- matrix(
            apply(
                mat, 2,
                function(col) {
                    format(col, justify = "right")
                }
            ),
            nrow = nrow(mat)
        )

        mat <- apply(
            mat, 1,
            function(x) paste0("   ", paste(x, collapse = "   "))
        )

        out <- c(
            out,
            "",
            paste0(
                ifelse(byFactor,
                    "Group Interquartile Ranges",
                    "Interquartile Range"
                ),
                " with ", ci.width * 100, "%",
                bsCI,
                " Confidence Interval",
                plural
            ),
            "",
            mat
        )
    }

    if (byFactor & !bs) {
        if (!is.null(hypothesis)) {
            if (length(toplot) == 2 && hypothesis$test %in% c("default", "t.test")) {
                if (is.survey) {
                    test.out <- try(
                        svyttest(
                            if (is.numeric(des$variables$x)) x ~ y else y ~ x,
                            des,
                            na.rm = TRUE
                        ),
                        silent = TRUE
                    )
                } else {
                    test.out <- t.test(toplot[[1]]$x, toplot[[2]]$x,
                        mu = hypothesis$value,
                        alternative = hypothesis$alternative,
                        var.equal = hypothesis$var.equal
                    )
                }

                if (!inherits(test.out, "try-error")) {
                    pval <- format_pval(test.out$p.value, opts)
                    out <- c(
                        out,
                        "",
                        ifelse(is.survey,
                            "Design-based Two Sample T-test",
                            paste0(
                                ifelse(hypothesis$var.equal, "", "Welch "),
                                "Two Sample t-test",
                                ifelse(hypothesis$var.equal,
                                    " assuming equal variance", ""
                                )
                            )
                        ),
                        "",
                        paste0(
                            "   t = ",
                            format(test.out$statistic, digits = 5),
                            ", df = ",
                            format(test.out$parameter, digits = 5),
                            ", p-value ",
                            ifelse(substr(pval, 1, 1) == "<", "", "= "),
                            pval
                        ),
                        "",
                        paste0(
                            "          Null Hypothesis: true difference in means is equal to ",
                            test.out$null.value
                        ),
                        paste0(
                            "   Alternative Hypothesis: true difference in means is ",
                            ifelse(test.out$alternative == "two.sided",
                                "not equal to",
                                paste0(test.out$alternative, " than")
                            ),
                            " ",
                            test.out$null.value
                        )
                    )

                    if (!is.survey && hypothesis$var.equal) {
                        ## F test for equal variance
                        ftest <- var.test(toplot[[1]]$x, toplot[[2]]$x)

                        svar <- sapply(toplot, function(d) var(d$x))
                        sn <- sapply(toplot, function(d) length(d$x))
                        var.pooled <- sum((sn - 1) * svar) / sum(sn - 1)
                        pval <- format_pval(ftest$p.value, opts)
                        out <- c(
                            out,
                            "",
                            paste0(
                                "          Pooled Variance: ",
                                format(var.pooled, digits = 5)
                            ),
                            "",
                            "F-test for equal variance [NOTE: very sensitive to non-normality]",
                            "",
                            paste0(
                                "   F = ",
                                format(ftest$statistic, digits = 5),
                                ", df = ",
                                paste(format(ftest$parameter, digits = 5),
                                    collapse = " and "
                                ),
                                ", p-value ",
                                ifelse(substr(pval, 1, 1) == "<", "", "= "),
                                pval
                            )
                        )
                    }
                }
            }
        }

        dat <- do.call(
            rbind,
            lapply(
                names(toplot),
                function(t) {
                    if (is.null(toplot[[t]]$x)) {
                        NULL
                    } else {
                        data.frame(
                            x = toplot[[t]]$x, y = t,
                            stringsAsFactors = TRUE
                        )
                    }
                }
            )
        )
        if (is.survey) {
            fmla <- if (is.numeric(des$variables$x)) x ~ y else y ~ x
            fit <- try(
                if (is.numeric(des$variables$x)) {
                    svyglm(x ~ y, des)
                } else {
                    svyglm(y ~ x, des)
                },
                silent = TRUE
            )
        } else {
            fit <- lm(x ~ y, data = dat)
        }

        if (!is.null(hypothesis) &&
            (length(toplot) > 2 | hypothesis$test == "anova")) {
            if (is.survey) {
                fstat <- regTermTest(
                    fit,
                    if (is.numeric(des$variables$x)) ~y else ~x
                    # hypothesis values here
                    # null = ...
                )
                Fval <- fstat$Ftest[1]
                Fdf <- c(fstat$df, fstat$ddf)
                fpval <- fstat$p[1]
                Fname <- sprintf(
                    "Wald test for %s (ANOVA equivalent for survey design)",
                    if (is.numeric(des$variables$x)) vn$y else vn$x
                )
            } else {
                fstat <- summary(fit)$fstatistic
                Fval <- fstat[1]
                Fdf <- fstat[2:3]
                fpval <- pf(fstat[1], fstat[2], fstat[3], lower.tail = FALSE)
                Fname <- "One-way Analysis of Variance (ANOVA F-test)"
            }
            fpval <- format_pval(fpval, opts, digits = 5)

            Ftest <- c(
                Fname,
                "",
                paste0(
                    "   F = ",
                    signif(Fval, 5),
                    ", df = ",
                    Fdf[1],
                    " and ",
                    Fdf[2],
                    ", p-value ",
                    ifelse(substr(fpval, 1, 1) == "<", "", "= "),
                    fpval
                )
            )
            out <- c(
                out,
                "",
                Ftest,
                "",
                "          Null Hypothesis: true group means are all equal",
                "   Alternative Hypothesis: true group means are not all equal"
            )
        }

        if (length(toplot) == 2) {
            ## Two sample t-test

            if (is.survey) {
                fmla <- if (is.numeric(des$variables$x)) x ~ y else y ~ x
                ttest <- try(
                    svyttest(fmla, design = des, na.rm = TRUE),
                    silent = TRUE
                )
                if (inherits(ttest, "try-error")) {
                    mat <- rbind(
                        c("Estimate", "Lower", "Upper"),
                        rep(NA, 3)
                    )
                } else {
                    ## the survey t-test is "level[2] - level[1]",
                    ## rather than "level[1] - level[2]"
                    ci <- confint(ttest, level = ci.width)
                    mat <- rbind(
                        c("Estimate", "Lower", "Upper"),
                        format(-c(ttest$estimate[[1]], ci[[2]], ci[[1]]), digits = opts$signif)
                    )
                    colnames(mat) <- NULL
                }
            } else {
                ttest <- t.test(toplot[[1]]$x, toplot[[2]]$x,
                    var.equal = if (is.null(hypothesis)) TRUE else hypothesis$var.equal,
                    conf.level = ci.width
                )
                mat <- rbind(
                    c("Estimate", "Lower", "Upper"),
                    format(
                        c(
                            diff(rev(ttest$estimate)),
                            ttest$conf.int[1],
                            ttest$conf.int[2]
                        ),
                        digits = opts$signif
                    )
                )
                colnames(mat) <- NULL
            }

            if (any(is.na(mat))) {
                out <- c(out, "")
            } else {
                mat <- cbind(
                    c("", paste0(names(toplot)[1], " - ", names(toplot)[2])),
                    mat
                )

                mat <- matrix(
                    apply(
                        mat, 2,
                        function(col) {
                            format(col, justify = "right")
                        }
                    ),
                    nrow = nrow(mat)
                )

                mat <- apply(
                    mat, 1,
                    function(x) paste0("   ", paste(x, collapse = "   "))
                )

                out <- c(
                    out,
                    "",
                    sprintf(
                        "Difference in %s means with %s%s Confidence Interval",
                        ifelse(is.survey, "population", "group"),
                        ci.width * 100,
                        "%"
                    ),
                    "",
                    mat
                )
            }
        } else if (length(toplot) > 2) {
            ## For x ~ factor, we also include an F test, and multiple comparisons
            ## (estimates, pvalues, and confidence intervals).

            LEVELS <- levels(dat$y)

            if (is.survey) {
                mc <- summary(
                    emmeans::emmeans(fit,
                        if (is.numeric(des$variables$x)) pairwise ~ y else pairwise ~ x,
                        data = dat,
                        infer = c(TRUE, TRUE)
                    ),
                    level = ci.width
                )

                mc <- mc$contrasts
                rownames(mc) <- mc[, 1]
                mc <- mc[, c("estimate", "lower.CL", "upper.CL", "p.value")]
            } else {
                mc <- try(
                    s20x::multipleComp(fit, conf.level = ci.width),
                    silent = TRUE
                )
            }

            if (!inherits(mc, "try-error")) {
                mat <- matrix(
                    apply(
                        mc, 2,
                        function(col) {
                            format(col, digits = opts$signif, justify = "right")
                        }
                    ),
                    nrow = nrow(mc)
                )
                mat[, 4] <- format_pval(as.numeric(mat[, 4]), opts)
                mat[grep("NA", mat)] <- ""

                rnames <- lapply(strsplit(rownames(mc), " - "), trimws)
                rnames <- do.call(rbind, rnames)
                rnames[, 1] <- format(rnames[, 1], justify = "right")
                rnames[, 2] <- format(rnames[, 2], justify = "left")
                rnames <- apply(rnames, 1, paste, collapse = " - ")
                mat <- cbind(format(rnames, justify = "left"), mat)

                mat <- rbind(c("", "Estimate", "Lower", "Upper", "P-value"), mat)
                mat <- matrix(
                    apply(
                        mat, 2,
                        function(col) {
                            format(col, justify = "right")
                        }
                    ),
                    nrow = nrow(mat)
                )

                mat <- apply(
                    mat, 1,
                    function(x) paste(x, collapse = "   ")
                )
                mat <- c(
                    mat[1],
                    paste(rep("-", nchar(mat[1])), collapse = ""),
                    mat[-1]
                )

                # add a new line each time the LHS changes
                rl <- (length(LEVELS) - 1L):1L
                rl <- cumsum(rl) + 2L
                mat[rl] <- paste0(mat[rl], "\n")

                out <- c(
                    out,
                    "",
                    sprintf(
                        "Pairwise differences in %sgroup means with %s%s Confidence Intervals and P-values",
                        ifelse(is.survey, "population ", ""),
                        ci.width * 100,
                        "%"
                    ),
                    "(The CIs and P-values have been adjusted for multiple comparisons)",
                    "",
                    paste0(" ", mat),
                    "",
                    "          Null Hypothesis: true difference in group means is zero",
                    "   Alternative Hypothesis: true difference in group means is not zero"
                )
            } else {
                # make standard table ...
                out <- c(
                    out,
                    "",
                    "Unable to compute confidence intervals and p-values."
                )
            }
        }
    } else if (!is.null(hypothesis) & !bs) {
        ## hypothesis testing - one sample
        if (is.survey) {
            des <- eval(parse(text = "update(des, .h0 = x - hypothesis$value)"))
            test.out <- svyttest(.h0 ~ 1, des, na.rm = TRUE)
        } else {
            test.out <- t.test(toplot$all$x,
                alternative = hypothesis$alternative,
                mu = hypothesis$value
            )
        }
        pval <- format_pval(test.out$p.value, opts)
        out <- c(
            out,
            "",
            sprintf(
                "%sOne Sample t-test",
                ifelse(is.survey, "Design-based ", "")
            ),
            "",
            paste0(
                "   t = ",
                format(test.out$statistic, digits = 5),
                ", df = ",
                test.out$parameter,
                ", p-value ",
                ifelse(substr(pval, 1, 1) == "<", "", "= "),
                pval
            ),
            "",
            paste0(
                "          Null Hypothesis: true mean is equal to ",
                hypothesis$value
            ),
            paste0(
                "   Alternative Hypothesis: true mean is ",
                ifelse(test.out$alternative == "two.sided",
                    "not equal to",
                    paste0(test.out$alternative, " than")
                ),
                " ",
                hypothesis$value
            )
        )
    }

    out
}

formatTriMat <- function(mat, names, digits = 3) {
    ## Formats a (lower) triangular matrix nicely for display:

    mat[!lower.tri(mat)] <- NA
    mat <- mat[-1, , drop = FALSE]

    mat <- format(mat, digits = digits)
    # mat <- matrix(
    #     apply(
    #         mat, 2,
    #         function(col) {
    #             format(col, digits = digits)
    #         }
    #     ),
    #     nrow = nrow(mat)
    # )

    mat[grep("NA", mat)] <- ""
    mat[grep("NaN", mat)] <- ""

    mat <- cbind(
        c("", names[-1]),
        rbind(names, mat)
    )
    mat <- mat[, -ncol(mat)]

    mat <- matrix(
        apply(
            mat, 2,
            function(col) {
                format(col, justify = "right")
            }
        ),
        nrow = nrow(mat)
    )

    mat
}

formatMat <- function(mat, digits = 4) {
    dn <- dimnames(mat)

    mat <- apply(mat, 1, function(x) suppressWarnings(as.numeric(x)))
    ## If the matrix has a single column, apply returns a vector rather than a matrix
    if (is.matrix(mat)) {
        mat <- t(mat)
    } else {
        mat <- matrix(mat, ncol = 1)
    }

    mat <- format(mat, digits = digits)
    # mat <- matrix(
    #     apply(
    #         mat, 2,
    #         function(col) {
    #             format(col, digits = digits)
    #         }
    #     ),
    #     nrow = nrow(mat)
    # )
    mat[grep("NA", mat)] <- ""
    mat[grep("NaN", mat)] <- ""

    mat <- cbind(
        c("", dn[[1]]),
        rbind(dn[[2]], mat)
    )

    mat <- matrix(
        apply(
            mat, 2,
            function(col) {
                format(col, justify = "right")
            }
        ),
        nrow = nrow(mat)
    )

    mat
}


#' @export
inference.inzhist <- function(object, des, bs, opts, class, width, vn, hypothesis,
                              survey.options, ...) {
    inference.inzdot(
        object, des, bs, opts, class, width, vn, hypothesis,
        survey.options, ...
    )
}



#' @export
inference.inzbar <- function(object, des, bs, opts, nb, vn, hypothesis,
                             survey.options, epi.out = FALSE, ...) {
    phat <- object$phat
    inf <- object$inference.info
    is.survey <- !is.null(des)
    ci.width <- attr(inf, "ci.width")

    if (!"conf" %in% names(inf)) {
        stop("Please specify `inference.type = conf` to get inference information.")
    }

    if (is.null(inf$conf)) {
        return("Unable to obtain inference information.")
    }

    if (bs & sum(object$tab) < 10) {
        return("Not enough data to perform bootstraps.")
    }

    twoway <- nrow(phat) > 1
    alpha <- 1 - (1 - ci.width) / 2

    if (!is.null(hypothesis) && !bs) {
        HypOut <- switch(hypothesis$test,
            "proportion" = {
                if (twoway) {
                    HypOut <- NULL
                } else if (is.survey) {
                    if (ncol(object$tab) == 2) {
                        pr <- survey::svymean(~x, des, na.rm = TRUE)
                        phat <- coef(pr)[[1]]
                        p <- hypothesis$value
                        se <- SE(pr)[[1]]
                        Z <- (phat - p) / se
                        prtest <- list(
                            statistic = Z,
                            p.value = switch(hypothesis$alternative,
                                "two.sided" = 2 * pnorm(abs(Z), lower.tail = FALSE),
                                "less" = pnorm(Z, lower.tail = TRUE),
                                "greater" = pnorm(Z, lower.tail = FALSE)
                            )
                        )

                        HypOut <- c(
                            "One-sample test of a proportion",
                            "",
                            sprintf(
                                "   Z-score = %s, p-value %s%s",
                                format(signif(prtest$statistic, 5)),
                                ifelse(prtest$p.value < 2.2e-16, "", "= "),
                                format_pval(prtest$p.value, opts, digits = 5)
                            ),
                            "",
                            sprintf(
                                "          Null Hypothesis: %s",
                                sprintf(
                                    "true proportion of %s = %s is %s",
                                    vn$x, colnames(object$tab)[1],
                                    hypothesis$value
                                )
                            ),
                            sprintf(
                                "   Alternative Hypothesis: %s",
                                sprintf(
                                    "true proportion of %s = %s is %s %s",
                                    vn$x, colnames(object$tab)[1],
                                    ifelse(hypothesis$alternative == "two.sided",
                                        "not equal to",
                                        paste(hypothesis$alternative, "than")
                                    ),
                                    hypothesis$value
                                )
                            ),
                            ""
                        )
                    } else {
                        HypOut <- NULL
                        ## Use anova.glm
                        # for fit0 <- quasipoisson(y~1)
                        # vs  fit1 <- quasipoisson(y~group)

                        # then Rao-Scott likelihood comparison
                    }
                } else {
                    if (ncol(object$tab) == 2) {
                        if (hypothesis$use.exact) {
                            prtest <- binom.test(object$tab,
                                p = hypothesis$value,
                                alternative = hypothesis$alternative
                            )
                        } else {
                            n <- sum(object$tab)
                            phat <- object$tab[1] / n
                            p <- hypothesis$value
                            Z <- (phat - p) / sqrt(p * (1 - p) / n)
                            prtest <- list(
                                statistic = Z,
                                p.value = switch(hypothesis$alternative,
                                    "two.sided" = 2 * pnorm(abs(Z), lower.tail = FALSE),
                                    "less" = pnorm(Z, lower.tail = TRUE),
                                    "greater" = pnorm(Z, lower.tail = FALSE)
                                )
                            )
                        }

                        HypOut <- c(
                            sprintf(
                                "%s",
                                ifelse(hypothesis$use.exact,
                                    "Exact binomial test",
                                    "One-sample test of a proportion"
                                )
                            ),
                            "",
                            sprintf(
                                "   %s, p-value %s%s",
                                ifelse(hypothesis$use.exact,
                                    sprintf(
                                        "Number of successes = %s, number of trials = %s",
                                        prtest$statistic, prtest$parameter
                                    ),
                                    sprintf(
                                        "Z-score = %s",
                                        format(signif(prtest$statistic, 5))
                                    )
                                ),
                                ifelse(prtest$p.value < 2.2e-16, "", "= "),
                                format_pval(prtest$p.value, opts, digits = 5)
                            ),
                            "",
                            sprintf(
                                "          Null Hypothesis: %s",
                                sprintf(
                                    "true proportion of %s = %s is %s",
                                    vn$x, colnames(object$tab)[1],
                                    hypothesis$value
                                )
                            ),
                            sprintf(
                                "   Alternative Hypothesis: %s",
                                sprintf(
                                    "true proportion of %s = %s is %s %s",
                                    vn$x, colnames(object$tab)[1],
                                    ifelse(hypothesis$alternative == "two.sided",
                                        "not equal to",
                                        paste(hypothesis$alternative, "than")
                                    ),
                                    hypothesis$value
                                )
                            ),
                            ""
                        )
                    } else {
                        HypOut <- NULL
                    }
                }
                HypOut
            },
            "chi2" = ,
            "default" = {
                chi2sim <- NULL
                if (is.survey) {
                    chi2 <- try(svychisq(~ y + x, des, na.rm = TRUE), TRUE)
                } else {
                    chi2 <- suppressWarnings(chisq.test(object$tab))
                    # from experimenting, tables bigger than this
                    # start taking too long to simulate the p-value
                    if (prod(dim(object$tab)) > 2000) {
                        hypothesis$simulated.p.value <- FALSE
                    }
                    if (any(chi2$expected < 5) || hypothesis$simulated.p.value) {
                        chi2 <- suppressWarnings(
                            chisq.test(object$tab, correct = FALSE)
                        )
                        chi2sim <- suppressWarnings(
                            chisq.test(object$tab, simulate.p.value = TRUE)
                        )
                    }
                }

                if (inherits(chi2, "try-error")) {
                    HypOut <- NULL
                } else {
                    piece1 <- ifelse(twoway,
                        sprintf("distribution of %s does not depend on %s", vn$x, vn$y),
                        "true proportions in each category are equal"
                    )
                    piece2 <- ifelse(twoway,
                        sprintf("distribution of %s changes with %s", vn$x, vn$y),
                        "true proportions in each category are not equal"
                    )

                    chi2out <- ""
                    simpval <- ""
                    if (!is.null(chi2sim) && any(chi2$expected < 5)) {
                        chi2out <- " (since some expected counts < 5)"
                    }
                    if (!is.null(chi2sim)) {
                        simpval <- sprintf(
                            "\n   Simulated p-value%s %s%s",
                            chi2out,
                            ifelse(chi2sim$p.value < 2.2e-16, "", "= "),
                            format_pval(chi2sim$p.value, opts, digits = 5)
                        )
                    }

                    HypOut <- c(
                        sprintf(
                            "Chi-square test for equal %s",
                            ifelse(twoway, "distributions", "proportions")
                        ),
                        "",
                        paste0(
                            "   X^2 = ",
                            format(signif(chi2$statistic, 5)),
                            ", ",
                            "df = ",
                            paste(
                                sprintf("%.4g", chi2$parameter),
                                collapse = " and "
                            ),
                            ", ",
                            "p-value ",
                            ifelse(chi2$p.value < 2.2e-16, "", "= "),
                            format_pval(chi2$p.value, opts, digits = 5),
                            simpval
                        ),
                        "",
                        sprintf("          Null Hypothesis: %s", piece1),
                        sprintf("   Alternative Hypothesis: %s", piece2),
                        ""
                    )
                }
                HypOut
            }
        )
    } else {
        HypOut <- NULL
    }

    if (twoway) {
        mat <- inf$conf$estimate
        dn <- dimnames(object$tab)

        mat <- format(mat, digits = opts$signif)
        # mat <- matrix(
        #     apply(
        #         mat, 2,
        #         function(col) {
        #             format(col, digits = opts$signif)
        #         }
        #     ),
        #     nrow = nrow(mat)
        # )

        ## Remove NA's and replace with an empty space
        mat[grep("NA", mat)] <- ""
        mat[grep("NaN", mat)] <- ""

        ## Text formatting to return a character vector - each row of matrix
        mat <- rbind(dn[[2]], mat)
        colnames(mat) <- NULL

        mat <- cbind(
            c("", dn[[1]]),
            mat,
            c("Row sums", rep(1, nrow(phat)))
        )
        rownames(mat) <- NULL

        mat <- matrix(
            apply(
                mat, 2,
                function(col) {
                    format(col, justify = "right")
                }
            ),
            nrow = nrow(mat)
        )

        out <- apply(
            mat, 1,
            function(x) paste0("   ", paste(x, collapse = "   "))
        )
        out <- c("Estimated Proportions", "", out)

        cis <- inf$conf
        cis <- rbind(cis$lower, cis$upper)
        cis <- cis[rep(1:nrow(phat), each = 2) + c(0, nrow(phat)), ]

        cis <- format(cis, digits = opts$signif)
        # cis <- matrix(
        #     apply(
        #         cis, 2,
        #         function(col) {
        #             format(col, digits = opts$signif)
        #         }
        #     ),
        #     nrow = nrow(cis)
        # )
        cis[grep("NA", cis)] <- ""
        cis[grep("NaN", cis)] <- ""

        cis <- rbind(dn[[2]], cis)
        colnames(cis) <- NULL

        cis <- cbind(
            c("", rbind(dn[[1]], "")),
            cis
        )
        colnames(cis) <- NULL

        cis <- matrix(
            apply(
                cis, 2,
                function(col) {
                    format(col, justify = "right")
                }
            ),
            nrow = nrow(cis)
        )

        bsCI <- ifelse(bs, " Percentile Bootstrap", "")
        out <- c(
            out,
            "",
            paste0(100 * ci.width, "%", bsCI, " Confidence Intervals"),
            "",
            apply(
                cis, 1,
                function(x) paste0("   ", paste(x, collapse = "   "))
            )
        )

        out <- c(
            out,
            "",
            HypOut,
            "",
            paste0(
                "Comparing differences in ",
                vn$x,
                "-distribution proportions between ",
                vn$y,
                " groups"
            ),
            "(Note: CIs are not adjusted for multiple comparisons)"
        )

        if (bs) {
            tab <- object$tab
            dat <- data.frame(
                x = rep(colnames(tab), times = colSums(tab)),
                y = rep(rep(rownames(tab), times = ncol(tab)), tab),
                stringsAsFactors = TRUE
            )

            b <- boot(dat,
                function(d, f) {
                    tt <- t(table(d[f, "x"], d[f, "y"]))
                    n1 <- nrow(tt)
                    n2 <- ncol(tt)
                    nn <- rowSums(tt)
                    pp <- sweep(tt, 1L, nn, "/")

                    cm <- utils::combn(n1, 2L)
                    t(apply(
                        cm, 2L,
                        function(x) {
                            pp[x[1], ] - pp[x[2], ]
                        }
                    ))
                },
                R = nb
            )
            bmat <- apply(
                b$t, 2L,
                function(x) {
                    c(mean(x), quantile(x, probs = c(1 - alpha, alpha)))
                }
            )
            b_mean <- matrix(bmat[1L, ], ncol = choose(nrow(tab), 2L), byrow = TRUE)
            b_low <- matrix(bmat[2L, ], ncol = choose(nrow(tab), 2L), byrow = TRUE)
            b_upp <- matrix(bmat[3L, ], ncol = choose(nrow(tab), 2L), byrow = TRUE)
        }

        ## TODO: work out correct value of M for adjusting alpha
        alpha_m <- (length(dn$x) - 1L) * choose(length(dn$y), 2L)
        alpha_m <- 2L
        alpha_adjusted <- 1 - (1 - ci.width) / alpha_m

        for (j in 1:ncol(phat)) {
            p <- phat[, j]
            n <- length(p)
            lev <- dn[[2]][j]
            sum <- rowSums(object$tab)

            if (bs) {
                diffs <- data.frame(
                    t(utils::combn(rownames(tab), 2L)),
                    b_mean[j, ],
                    b_low[j, ],
                    b_upp[j, ]
                )
            } else if (is.survey) {
                des$variables$x0 <- ifelse(des$variables$x == lev, 1L, 0L)
                fit <- survey::svyglm(x0 ~ y - 1L, design = des)
                diffs <- freq2way.survey(fit, dn[[1]], alpha_adjusted)
            } else {
                diffs <- freq2way(p, sum, alpha_adjusted)
            }

            rnames <- cbind(diffs[, 1], "-", diffs[, 2])
            rnames[, 1] <- format(rnames[, 1], justify = "right")
            rnames[, 3] <- format(rnames[, 3], justify = "left")
            rnames <- apply(rnames, 1, paste, collapse = " ")

            mat <- matrix(
                apply(
                    diffs[3:5], 2,
                    function(col) {
                        format(col, digits = opts$signif, justify = "right")
                    }
                ),
                nrow = nrow(diffs)
            )
            mat[grep("NA", mat)] <- ""

            mat <- cbind(format(rnames, justify = "left"), mat)

            mat <- rbind(c("", "Estimate", "Lower", "Upper"), mat)
            mat <- matrix(
                apply(
                    mat, 2,
                    function(col) {
                        format(col, justify = "right")
                    }
                ),
                nrow = nrow(mat)
            )

            mat <- apply(
                mat, 1,
                function(x) paste(x, collapse = "   ")
            )
            mat <- c(
                mat[1],
                paste(rep("-", nchar(mat[1])), collapse = ""),
                mat[-1]
            )

            out <- c(
                out,
                "",
                paste0(
                    " # Pairwise differences in proportions of ",
                    vn$x,
                    " = ",
                    lev
                ),
                "",
                paste("  ", mat),
                ""
            )
        }

        ##### EPI CALCS #####
        if (epi.out && ncol(object$tab) == 2) {
            or.mat <- vapply(
                2:nrow(object$tab),
                function(i) {
                    calculate_or(object$tab[c(1, i), ])
                },
                FUN.VALUE = numeric(4)
            )

            or.method <- "fisher"
            or.method.full <- c(
                "midp" = "median-unbiased estimation",
                "fisher" = "conditional maximum likelihood",
                "small" = "small sample adjustment"
            )

            hypo.mat <- matrix(
                c(
                    "Null Hypothesis:", "true odds ratio is equal to 1",
                    "Alternative Hypothesis:", "true odds ratio is not equal to 1"
                ),
                byrow = TRUE,
                ncol = 2
            )
            hypo.mat <- apply(hypo.mat, 2, function(x) format(x, justify = "right"))
            hypo.mat <- apply(hypo.mat, MARGIN = 1, paste0, collapse = " ")
            hypo.mat <- paste0("   ", hypo.mat)

            out <- c(
                out,
                "",
                "",
                sprintf("### Odds Ratio estimates for %s = %s", vn$x, dn[[2]][2]),
                sprintf("  (baseline: %s = %s)", vn$y, dn[[1]][1]),
                sprintf("  Using conditional maximum likelihood estimation"),
                "",
                "  For each line:",
                hypo.mat,
                "",
                epi.format(or.mat, "OR", names = rownames(object$tab))
            )

            #### RISK RATIO ####

            rr.mat <- vapply(
                2:nrow(object$tab),
                function(i) {
                    calculate_rr(object$tab[c(1, i), ])
                },
                FUN.VALUE = numeric(4)
            )

            out <- c(
                out,
                "",
                "",
                sprintf("### Risk Ratio estimates for %s = %s", vn$x, dn[[2]][2]),
                sprintf("  (baseline: %s = %s)", vn$y, dn[[1]][1]),
                "",
                epi.format(rr.mat, "RR", names = rownames(object$tab))
            )

            #### RISK DIFF ####

            rd.mat <- vapply(
                2:nrow(object$tab),
                function(i) {
                    calculate_rd(object$tab[c(1, i), ])
                },
                FUN.VALUE = numeric(4)
            )

            out <- c(
                out,
                "",
                "",
                sprintf("### Risk Difference estimates for %s = %s", vn$x, dn[[2]][2]),
                sprintf("  (baseline: %s = %s)", vn$y, dn[[1]][1]),
                "",
                epi.format(rd.mat, "RD", names = rownames(object$tab), 0)
            )
        }
        ##### END CALCS #####
    } else { ## one-way table
        mat <- t(rbind(inf$conf$estimate, inf$conf$lower, inf$conf$upper))

        mat <- format(mat, digits = opts$signif)
        # mat <- matrix(
        #     apply(
        #         mat, 2,
        #         function(col) {
        #             format(col, digits = opts$signif)
        #         }
        #     ),
        #     nrow = nrow(mat)
        # )

        ## Remove NA's and replace with an empty space
        mat[grep("NA", mat)] <- ""
        mat[grep("NaN", mat)] <- ""

        ## Text formatting to return a character vector - each row of matrix
        mat <- rbind(c("Estimate", "Lower", "Upper"), mat)
        colnames(mat) <- NULL
        LEVELS <- colnames(object$tab)
        mat <- cbind(c("", LEVELS), mat)
        rownames(mat) <- NULL

        mat <- matrix(
            apply(
                mat, 2,
                function(col) {
                    format(col, justify = "right")
                }
            ),
            nrow = nrow(mat)
        )

        out <- apply(
            mat, 1,
            function(x) paste0("   ", paste(x, collapse = "   "))
        )

        bsCI <- ifelse(bs, " Percentile Bootstrap", "")
        out <- c(
            paste0(
                sprintf(
                    "Estimated %sProportions with %s%s",
                    ifelse(is.survey, "Population ", ""),
                    ci.width * 100, "%"
                ),
                bsCI,
                " Confidence Interval"
            ),
            "",
            out
        )

        if (bs) {
            ## This is about the only place we do bootstrapping within the inference function, as no such
            ## method is applicable to the plots themselves.
            ## All other inferences are generated by the plotting function, which are then displayed on the plot
            ## and hence match the output from this function.

            dat <- data.frame(
                x = rep(colnames(object$tab), times = object$tab),
                stringsAsFactors = TRUE
            )
            b <- boot(dat,
                function(d, f) {
                    tt <- table(d[f, ])
                    ni <- length(tt)
                    nn <- sum(tt)
                    pp <- tt / nn

                    d <- c()
                    for (ii in 2:ni) {
                        for (jj in 1:(ii - 1)) {
                            d <- c(d, pp[ii] - pp[jj])
                        }
                    }

                    d
                },
                R = nb
            )

            diffs <- apply(
                b$t, 2L,
                function(x) {
                    c(mean(x), quantile(x, probs = c(1 - alpha, alpha)))
                }
            )
            diffs <- data.frame(
                t(utils::combn(colnames(object$tab), 2L)),
                t(diffs)
            )
        } else {
            if (is.survey) {
                diffs <- freq1way.survey(des, ci.width)
            } else {
                diffs <- freq1way.edited(object$tab, conf.level = ci.width)
            }
        }

        rnames <- cbind(diffs[, 1], "-", diffs[, 2])
        rnames[, 1] <- format(rnames[, 1], justify = "right")
        rnames[, 3] <- format(rnames[, 3], justify = "left")
        rnames <- apply(rnames, 1, paste, collapse = " ")

        mat <- matrix(
            apply(
                diffs[3:5], 2,
                function(col) {
                    format(col, digits = opts$signif, justify = "right")
                }
            ),
            nrow = nrow(diffs)
        )
        mat[grep("NA", mat)] <- ""

        mat <- cbind(format(rnames, justify = "left"), mat)

        mat <- rbind(c("", "Estimate", "Lower", "Upper"), mat)
        mat <- matrix(
            apply(
                mat, 2,
                function(col) {
                    format(col, justify = "right")
                }
            ),
            nrow = nrow(mat)
        )

        mat <- apply(
            mat, 1,
            function(x) paste(x, collapse = "   ")
        )
        mat <- c(
            mat[1],
            paste(rep("-", nchar(mat[1])), collapse = ""),
            mat[-1]
        )

        # add a new line each time the LHS changes
        rl <- (length(LEVELS) - 1L):1L
        rl <- cumsum(rl) + 2L
        mat[rl] <- paste0(mat[rl], "\n")

        out <- c(
            out,
            "",
            HypOut,
            "",
            sprintf(
                "### Difference in %sproportions of %s",
                ifelse(is.survey, "population ", ""),
                vn$x
            ),
            sprintf(
                "    with %s%s %sConfidence Intervals%s",
                ci.width * 100,
                "%",
                ifelse(bs, "Percentile Bootstrap ", ""),
                ifelse(bs, "", " (adjusted for multiple comparisons)")
            ),
            "",
            paste0(" ", mat)
        )
    }

    out
}

freq1way.edited <- function(tbl, conf.level = 0.95) {
    ## Before freq1way is called should output the variable name in the table
    level.names <- colnames(tbl)
    n <- sum(tbl)
    ncats <- length(tbl)
    ncatsC2 <- choose(ncats, 2)

    if (is.null(level.names)) level.names <- 1:ncats

    conf.pc <- conf.level * 100
    phat <- tbl / sum(tbl)

    qval.adjusted <- abs(qnorm((1 - conf.level) / (2 * ncatsC2)))

    tempw <- NA_real_

    lvlc <- utils::combn(level.names, 2L)
    comp_results <- data.frame(
        a = lvlc[1L, ],
        b = lvlc[2L, ],
        estimate = numeric(ncatsC2),
        lower = numeric(ncatsC2),
        upper = numeric(ncatsC2)
    )

    for (i in seq_len(ncatsC2)) {
        a <- comp_results[i, "a"]
        b <- comp_results[i, "b"]
        comp_results[i, "estimate"] <- phat[, a] - phat[, b]
        ci <- phat[, a] - phat[, b] +
            qval.adjusted * c(-1, 1) *
                sqrt(((phat[, a] + phat[, b]) - ((phat[, a] - phat[, b])^2)) / n)
        comp_results[i, "lower"] <- ci[1]
        comp_results[i, "upper"] <- ci[2]
    }

    comp_results
}

freq1way.survey <- function(des, conf.level = 0.95) {
    phat <- svymean(~x, des)
    lvls <- levels(des$variables$x)
    Nc <- length(lvls)
    cmb <- utils::combn(Nc, 2L)
    contrast_mat <- apply(cmb, 2L,
        function(x) {
            z <- rep(0L, Nc)
            z[x[1]] <- 1L
            z[x[2]] <- -1L
            z
        },
        simplify = FALSE
    )
    names(contrast_mat) <- apply(
        cmb, 2L,
        function(i) paste(lvls[i], collapse = " - ")
    )

    ctr <- svycontrast(phat, contrast_mat)

    alpha <- 1 - conf.level
    alpha_m <- alpha / length(contrast_mat)

    data.frame(
        lvls[cmb[1, ]],
        lvls[cmb[2, ]],
        coef(ctr),
        confint(ctr, level = 1 - alpha_m * 2)
    )
}

freq2way <- function(p, n, alpha) {
    cn <- utils::combn(names(p), 2L)
    data.frame(
        t(cn),
        t(apply(
            cn, 2L,
            function(x) {
                c(
                    p[x[1]] - p[x[2]],
                    pDiffCI(p[x[1]], p[x[2]], n[x[1]], n[x[2]], qnorm(alpha))
                )
            }
        ))
    )
}

freq2way.survey <- function(fit, lvls, alpha) {
    Nc <- length(lvls)
    cmb <- utils::combn(Nc, 2L)
    contrast_mat <- apply(cmb, 2L,
        function(x) {
            z <- rep(0L, Nc)
            z[x[1]] <- 1L
            z[x[2]] <- -1L
            z
        },
        simplify = FALSE
    )
    names(contrast_mat) <- apply(
        cmb, 2L,
        function(i) paste(lvls[i], collapse = " - ")
    )
    ctr <- try(svycontrast(fit, contrast_mat), silent = TRUE)
    if (inherits(ctr, "try-error")) {
        return(
            data.frame(
                lvls[cmb[1, ]],
                lvls[cmb[2, ]],
                sapply(contrast_mat, function(x) sum(coef(fit) * x)),
                rep(NA, length(contrast_mat)),
                rep(NA, length(contrast_mat))
            )
        )
    }
    data.frame(
        lvls[cmb[1, ]],
        lvls[cmb[2, ]],
        coef(ctr),
        confint(ctr, level = 1 - alpha * 2)
    )
}


pDiffCI <- function(p1, p2, n1, n2, z = 1.96) {
    p <- p1 - p2
    se <- sqrt(p1 * (1 - p1) / n1 + p2 * (1 - p2) / n2)

    p + c(-1, 1) * z * se
}

#' @export
inference.inzscatter <- function(object, des, bs, opts, nb, vn, survey.options, ...) {
    d <- data.frame(
        x = object$x,
        y = object$y,
        stringsAsFactors = TRUE
    )
    trend <- object$trend

    if (is.null(trend)) {
        return("Please specify a trend line to obtain inference information.")
    }

    if (bs & nrow(d) < 10) {
        return("Not enough observations to perform bootstrap simulation.")
    }

    is.survey <- !is.null(des)
    ci.width <- object$ci.width

    ## Ensure the order is linear/quad/cubic
    allT <- c("linear", "quadratic", "cubic")
    tr <- (1:3)[allT %in% trend]

    out <- character()

    alpha <- 1 - (1 - ci.width) / 2

    for (t in tr) {
        if (nrow(d) <= t + 1) {
            out <- c(
                out,
                "",
                paste0(
                    "Not enough observations (n = ",
                    nrow(d),
                    ") to fit ",
                    switch(t,
                        "Linear",
                        "Quadratic",
                        "Cubic"
                    ),
                    " trend"
                )
            )
            break
        } else {
            if (bs) {
                if (is.survey) {
                    return("Bootstrap inference not yet implemented for survey data.")
                }

                b <- boot(d,
                    function(dat, f) {
                        fit <- switch(t,
                            lm(y ~ x, data = dat[f, ]),
                            lm(y ~ x + I(x^2), data = dat[f, ]),
                            lm(y ~ x + I(x^2) + I(x^3), data = dat[f, ])
                        )
                        c(
                            coef(fit),
                            if (t == 1) cor(d[f, "x"], d[f, "y"])
                        )
                    },
                    R = object$n.boot
                )
                mat <- cbind(
                    sprintf("%.5g", colMeans(b$t, na.rm = TRUE)),
                    sprintf("%.5g", apply(b$t, 2, quantile, probs = 1 - alpha, na.rm = TRUE)),
                    sprintf("%.5g", apply(b$t, 2, quantile, probs = alpha, na.rm = TRUE))
                )
            } else {
                if (is.survey) {
                    fit <- switch(t,
                        svyglm(y ~ x, des),
                        svyglm(y ~ x + I(x^2), des),
                        svyglm(y ~ x + I(x^2) + I(x^3), des)
                    )
                } else {
                    fit <- switch(t,
                        lm(y ~ x, data = d),
                        lm(y ~ x + I(x^2), data = d),
                        lm(y ~ x + I(x^2) + I(x^3), data = d)
                    )
                }

                cc <- summary(fit)$coef
                ci <- confint(fit, level = ci.width)

                mat <- cbind(
                    sprintf("%.5g", cc[, 1]),
                    sprintf("%.5g", ci[, 1]),
                    sprintf("%.5g", ci[, 2]),
                    format_pval(cc[, 4], opts, digits = opts$signif)
                )
            }

            if (bs & t == 1) {
                covMat <- mat[3, ]
                mat <- mat[1:2, ]
            }


            mat <- rbind(
                c("Estimate", "Lower", "Upper", if (!bs) "p-value"),
                mat
            )

            rn <- paste0(vn$x, c("", "^2", "^3"))
            mat <- cbind(c("", "Intercept", rn[1:t]), mat)
            if (bs & t == 1) {
                mat <- rbind(mat, "", c("correlation", covMat))
            }
            mat <- apply(
                mat, 2,
                function(x) format(x, justify = "right")
            )

            out <- c(
                out,
                "",
                paste0(
                    switch(t,
                        "Linear",
                        "Quadratic",
                        "Cubic"
                    ),
                    " Trend Coefficients with ", ci.width * 100, "% ",
                    ifelse(bs, "Percentile Bootstrap ", ""),
                    "Confidence Intervals"
                ),
                "",
                apply(
                    mat, 1,
                    function(x) paste0("   ", paste(x, collapse = "   "))
                )
            )

            ## add a 'key' to the end of the output
            if (t == max(tr) & !bs) {
                out <- c(
                    out,
                    "",
                    "",
                    "   p-values for the null hypothesis of no association, H0: beta = 0"
                )
            }
        }
    }

    out
}

#' @export
inference.inzgrid <- function(object, bs, opts, nboot, vn, survey.options, ...) {
    inference.inzscatter(object, bs, opts, nboot, vn, survey.options, ...)
}

#' @export
inference.inzhex <- function(object, bs, opts, nboot, vn, survey.options, ...) {
    inference.inzscatter(object, bs, opts, nboot, vn, survey.options, ...)
}
