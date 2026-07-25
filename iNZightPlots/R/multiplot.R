#' @importFrom rlang .data
multiplot <- function(df, args) {
    check_suggested_packages()

    # what type of plot?
    d <- df$data

    # do things for Y here, once enabled:
    if ("y" %in% names(d)) {
        stop("multi y plot not yet supported")
    }

    if (tibble::is_tibble(d$x) && ncol(d$x) > 1L) {
        # plotting multiple x-variables
        vtypes <- sapply(d$x, iNZightTools::vartype)

        if (any(vtypes != "cat") && isTRUE(getOption("inzight.auto.remove.noncatvars"))) {
            warning(
                sprintf(
                    "The following non-categorical variables were removed:\n%s",
                    paste0("    - ", names(d$x)[vtypes != "cat"], collapse = "\n")
                )
            )
            df$data$x <- df$data$x[, vtypes == "cat", drop = FALSE]
            d <- df$data
            vtypes <- sapply(d$x, iNZightTools::vartype)
        }

        if (all(vtypes == "cat")) {
            # unique values ?

            return(multiplot_cat(df, args))
        }

        if (all(vtypes == "num")) {
            return(multiplot_num(df, args))
        }
    }

    stop(
        sprintf(
            "Please remove the following non-categorical variables:\n%s",
            paste0("    - ", names(d$x)[vtypes != "cat"], collapse = "\n")
        )
    )

    "Not supported"
}

multiplot_cat <- function(df, args) {
    xvars <- names(df$data$x) <- paste("x", names(df$data$x), sep = "_")
    d <- tidyr::unnest(df$data, seq_len(ncol(df$data)))

    olvls <- levels(d[[1]])

    var_table_groups <- sapply(
        d,
        function(x) {
            t <- attr(x, "table", exact = TRUE)
            if (is.null(t)) {
                return("")
            }
            t
        }
    )

    ## need to remove labels ...
    levels <- sapply(names(d), function(x) expss::var_lab(d[[x]]) %||% x)
    d <- tibble::as_tibble(lapply(d, expss::unlab))

    facet <- ""
    if ("g1" %in% names(d)) facet <- "g1"

    if (!is.null(args$full_cases) && args$full_cases) {
        d <- tidyr::drop_na(d)
    }

    # mutate X's
    d <- tidyr::pivot_longer(d,
        cols = xvars,
        names_to = "x",
        values_to = "value"
    )

    if (all(var_table_groups != "")) {
        d <- dplyr::mutate(d, y = var_table_groups[.data$x])
    }

    levels <- setNames(stringr::str_replace(levels, "^x_", ""), xvars)
    d <- dplyr::mutate(d, x = factor(.data$x, levels = xvars, labels = levels[xvars]))

    var_y <- if ("y" %in% names(d) && length(unique(d$y)) > 1) rlang::sym("y") else NULL

    if (!is.null(var_y)) {
        newx <- tapply(seq_len(nrow(d)), d$y, function(i) {
            di <- d[i, ]
            xlv <- levels[xvars]
            xlv <- xlv[xlv %in% unique(as.character(di$x))]
            di$x <- factor(di$x, levels = as.character(xlv))
            xvar <- setNames(
                iNZightMR::substrsplit(levels(di$x), " "),
                c("name", "levels")
            )
            levels(di$x) <- xvar$levels
            di$x <- as.character(di$x)

            list(d = di, name = xvar$name, levels = xvar$levels)
        })

        # combine levels
        xlvls <- unique(do.call(c, lapply(newx, function(x) x$levels)))
        d <- do.call(rbind, lapply(newx, function(x) x$d))
        d$x <- factor(d$x, levels = xlvls)

        if (!is.null(args$x_groups)) {
            d <- dplyr::mutate(d,
                x = forcats::fct_collapse(.data$x, !!!args$x_groups)
            )
        }

        xnames <- as.character(sapply(newx, function(x) x$name))
        xnamesr <- sapply(xnames, function(z) paste(rev(strsplit(z, "")[[1]]), collapse = ""))
        if (any(xnamesr == "")) {
            xvar <- list(
                name = "",
                levels = xnamesr
            )
        } else {
            xvar <- setNames(iNZightMR::substrsplit(xnamesr, " "), c("name", "levels"))
            xvar$name <- paste(rev(strsplit(xvar$name, "")[[1]]), collapse = "")
            xvar$levels <- xlvls
        }
    } else {
        d$x <- factor(d$x, levels = levels[xvars])
        xvar <- setNames(
            iNZightMR::substrsplit(levels(d$x), " "),
            c("name", "levels")
        )
    }

    if (is.null(args$keep_missing)) args$keep_missing <- FALSE

    if (args$keep_missing) {
        d <- dplyr::mutate(d, value = ifelse(is.na(.data$value), "Missing", .data$value))
    } else {
        d <- dplyr::filter(d, !is.na(.data$value))
    }
    lvls <- unique(c(olvls, unique(as.character(d$value))))
    if ("Missing" %in% lvls) lvls <- c(lvls[lvls != "Missing"], "Missing")
    d$value <- factor(d$value, levels = lvls)

    if (!is.null(args$outcome_value)) {
        args$outcome_value_options <- lvls
        olvls <- paste(lvls[lvls != args$outcome_value], collapse = " | ")
        d$value <- ifelse(d$value == args$outcome_value,
            args$outcome_value,
            olvls
        )
        d$value <- factor(d$value, c(args$outcome_value, olvls))
    }

    if (facet == "g1") {
        glevels <- levels(d$g1)
        d <- dplyr::mutate(d, g1 = ifelse(is.na(.data$g1), "missing", as.character(.data$g1)))
        if ("missing" %in% unique(d$g1)) {
            d$g1 <- factor(d$g1, c(glevels, "missing"))
        } else {
            d$g1 <- factor(d$g1, glevels)
        }

        if (!is.null(df$glevels) && !is.null(df$glevels$g1.level)) {
            g1level <- df$glevels$g1.level
            if (is.numeric(g1level)) g1level <- glevels[g1level]
            d <- dplyr::filter(d, .data$g1 == g1level)
            d <- dplyr::mutate(d, g1 = droplevels(.data$g1))
        }

        d <- dplyr::group_by(d, .data$x, .data$value, .data$g1, .drop = FALSE)
        d <- dplyr::tally(d)
        d <- dplyr::group_by(d, .data$x, .data$g1, .drop = FALSE)
    } else {
        d <- dplyr::group_by(d, .data$x, !!var_y, .data$value, .drop = FALSE)
        d <- dplyr::tally(d)
        d <- dplyr::group_by(d, .data$x, !!var_y, .drop = FALSE)
    }
    d <- dplyr::reframe(d,
        value = .data$value,
        n = .data$n,
        p = .data$n / sum(.data$n) * 100
    )

    title <- xlab <- ""
    if (is.null(var_y) && xvar$name != "" && all(xvar$levels != "")) {
        newlvls <- setNames(levels(d$x), stringr::str_wrap(xvar$levels, width = 30))
        d$x <- forcats::fct_recode(d$x, !!!newlvls)
        title <- xvar$name
    }
    if (!is.null(args$main) && args$main != "") title <- args$main

    plottype <- args$plottype
    if (is.null(plottype) || plottype %in% c("default", "gg_multi")) {
        # binary answers?
        if (length(levels(d$value)) == 2L) {
            plottype <- "gg_multi_binary"
        } else {
            plottype <- "gg_multi_stack"
        }
    }
    if (plottype == "gg_multi_col" && !is.null(var_y)) plottype <- "gg_multi_stack"

    ylab <- "Percentage (%)"

    p <- switch(plottype,
        "gg_multi_binary" = {
            if (length(levels(d$value)) != 2L) stop("Invalid plot type")
            xlvls <- levels(d$value)
            xlevel <- if ("yes" %in% tolower(xlvls)) xlvls[tolower(xlvls) == "yes"] else unique(xlvls)[1]
            d <- dplyr::filter(d, .data$value == !!xlevel)

            if (!is.null(args$ordered) && args$ordered %in% c("desc", "asc")) {
                d$x <- factor(d$x, levels = unique(d$x)[order(d$p, decreasing = args$ordered == "desc")])
            }

            ylab <- sprintf("%s of responses = '%s'", ylab, xlevel)

            if (!is.null(var_y)) {
                ggplot2::ggplot(d, ggplot2::aes(.data$x, .data$p, fill = !!var_y)) +
                    ggplot2::geom_bar(stat = "identity", position = "dodge")
            } else {
                ggplot2::ggplot(d, ggplot2::aes(.data$x, .data$p)) +
                    ggplot2::geom_bar(stat = "identity", width = 0.5)
            }
        },
        "gg_multi_stack" = {
            if (!is.null(var_y)) {
                # return(d)
                ggplot2::ggplot(
                    d,
                    ggplot2::aes(.data$y, .data$p, fill = .data$value)
                ) +
                    ggplot2::geom_bar(
                        stat = "identity",
                        position = ggplot2::position_stack(reverse = TRUE)
                    ) +
                    ggplot2::facet_grid(x ~ .)
            } else {
                ggplot2::ggplot(
                    d,
                    ggplot2::aes(.data$x, .data$p, fill = .data$value)
                ) +
                    ggplot2::geom_bar(
                        stat = "identity",
                        position = ggplot2::position_stack(reverse = TRUE)
                    )
            }
        },
        "gg_multi_col" = {
            ggplot2::ggplot(
                d,
                ggplot2::aes(.data$value, .data$p, fill = .data$x)
            ) +
                ggplot2::geom_bar(stat = "identity", position = "dodge")
        }
    )

    subtitle <- ""

    if (facet == "g1") {
        p <- p + ggplot2::facet_wrap(~g1)
        subtitle <- sprintf(
            "Faceted by %s",
            df$labels$g1 %||% df$varnames$g1
        )
    }

    p <- p + ggplot2::theme(legend.position = "bottom")

    if (!is.null(args$gg_theme)) {
        if (is.character(args$gg_theme)) {
            if (args$gg_theme != "_default") {
                ptheme <- try(
                    eval(parse(text = sprintf("ggplot2::theme_%s", args$gg_theme))),
                    silent = TRUE
                )
                if (!inherits(ptheme, "try-error")) {
                    p <- p + ptheme()
                } else {
                    ptheme <- try(
                        eval(parse(text = sprintf("ggthemes::theme_%s", args$gg_theme))),
                        silent = TRUE
                    )
                    if (!inherits(ptheme, "try-error")) {
                        p <- p + ptheme()
                    }
                }
            }
        } else {
            p <- p + do.call(ggplot2::theme, args$gg_theme)
        }
    }

    p <- p +
        ggplot2::xlab(xlab) +
        ggplot2::ylab(ylab) +
        ggplot2::ylim(0, 100) +
        ggplot2::labs(fill = "") +
        ggplot2::ggtitle(title, subtitle = subtitle)

    if (!is.null(args$rotation) && args$rotation) {
        if (plottype %in% c("gg_multi_column")) {
            p <- p + ggplot2::coord_flip()
        }
    } else if (plottype %in% c("gg_multi_binary", "gg_multi_stack")) {
        if (plottype == "gg_multi_stack" && !is.null(var_y)) {
            p <- p + ggplot2::coord_flip() +
                ggplot2::scale_x_discrete(limits = rev(levels(d$y)))
        } else {
            p <- p + ggplot2::coord_flip() +
                ggplot2::scale_x_discrete(limits = rev(levels(d$x)))
        }
    }

    if (is.null(args$plot) || isTRUE(args$plot)) {
        dev.hold()
        tryCatch(
            print(p),
            finally = dev.flush()
        )
    }

    attr(p, "plottype") <- plottype
    attr(p, "varnames") <- list(
        x = strsplit(as.character(df$varnames["x"]), " + ", fixed = TRUE)[[1]]
    )
    attr(p, "xlevels") <- levels(d$value)
    attr(p, "data") <- d
    attr(p, "labels") <- list(
        title = title,
        subtitle = subtitle,
        xlab = xlab,
        ylab = ylab
    )
    attr(p, "n") <- nrow(df$data)
    attr(p, "args") <- args

    class(p) <- c(plottype, "inzmulti_gg", class(p))

    invisible(p)
}

multiplot_num <- function(df, args) {

}

check_suggested_packages <- function() {
    pkgs <- c("ggplot2", "ggthemes", "dplyr", "tibble", "tidyr")
    inst <- sapply(pkgs, requireNamespace, quietly = TRUE)
    if (any(!inst)) {
        stop(
            sprintf(
                "Please install suggested packages:\n%s\n\nOr use:\n\n%s",
                paste("-", pkgs[!inst], collapse = "\n"),
                "install.packages('iNZightPlots', dependencies = TRUE)"
            )
        )
    }
}


summary.inzmulti_gg <- function(object, ...) {
    d <- attr(object, "data", exact = TRUE)
    d
}

summary.gg_multi_binary <- function(object, html = FALSE, ...) {
    args <- modifyList(inzpar(), attr(object, "args"))
    varnames <- attr(object, "varnames", exact = TRUE)
    labels <- attr(object, "labels", exact = TRUE)
    d <- attr(object, "data", exact = TRUE)
    response_val <- as.character(d$value[1])

    if ("y" %in% colnames(d)) {
        # special handling of this case ...
        # TODO: if g1 specified, will need to do some fancy subsetting...

        # else, just usual:
        smry <- tidyr::pivot_wider(d,
            names_from = !!rlang::sym("y"),
            values_from = c(!!rlang::sym("n"), !!rlang::sym("p"))
        )
        smry <- dplyr::select(smry, -!!rlang::sym("value"))
        cn <- colnames(smry)[-1]
        cn <- sapply(cn, function(x) {
            paste0(paste(rev(stringr::str_split(x, "_", n = 2L)[[1]]), collapse = " ("), ")")
        })
        smry <- setNames(smry, c("", cn))
        smry <- smry[, c(1, order(cn) + 1L)]
        colnames(smry) <- gsub("(p)", "(%)", colnames(smry), fixed = TRUE)
        pcols <- grepl("(%)", colnames(smry), fixed = TRUE)
        digits <- ifelse(pcols, args$round_percent, 0)
    } else if ("g1" %in% colnames(d)) {
        smry <- tidyr::pivot_wider(d,
            names_from = !!rlang::sym("g1"),
            values_from = c(!!rlang::sym("n"), !!rlang::sym("p"))
        )
        smry <- dplyr::select(smry, -!!rlang::sym("value"))
        cn <- colnames(smry)[-1]
        cn <- sapply(cn, function(x) {
            paste0(paste(rev(strsplit(x, "_")[[1]]), collapse = " ("), ")")
        })
        smry <- setNames(smry, c("", cn))
        smry <- smry[, c(1, order(cn) + 1L)]
        colnames(smry) <- gsub("(p)", "(%)", colnames(smry), fixed = TRUE)
        pcols <- grepl("(%)", colnames(smry), fixed = TRUE)
        digits <- ifelse(pcols, args$round_percent, 0)
    } else {
        lvls <- c(levels(d$x), "Total")
        d <- tibble::add_row(
            dplyr::ungroup(d),
            x = "Total",
            n = attr(object, "n", exact = TRUE)
        )
        d$x <- factor(d$x, levels = lvls)
        smry <- setNames(
            dplyr::select(
                d,
                !!rlang::sym("x"),
                !!rlang::sym("n"),
                !!rlang::sym("p")
            ),
            c(" ", "N", "%")
        )
        digits <- c(0, args$round_percent, 0)
    }

    smry <- smry[order(smry[[1]]), ]
    smry[[1]] <- stringr::str_replace(as.character(smry[[1]]), "\n", " ")

    if (!requireNamespace("knitr", quietly = TRUE)) {
        return(smry)
    }

    if (is.null(labels$title) || labels$title == "") {
        labels$title <- sprintf(
            "Counts and percentages of responses = '%s'",
            response_val
        )
    }

    opt <- options(knitr.kable.NA = "-")
    on.exit(options(opt))

    if (!html) {
        return(
            knitr::kable(smry,
                format = "simple",
                caption = labels$title,
                digits = digits
            )
        )
    }

    if (is.null(args$kable_styling)) {
        args$kable_styling <- list(bootstrap_options = NULL)
    }

    if (!requireNamespace("kableExtra", quietly = TRUE)) {
        res <- knitr::kable(smry,
            format = "html",
            caption = labels$title,
            digits = digits
        )
        tf <- tempfile(fileext = ".html")
        writeLines(res, tf)
        browseURL(tf)
        return(invisible(tf))
    }

    tbl_groups <- NULL
    if (sum(c("y", "g1") %in% names(d)) == 1L) {
        # fancy header
        cn <- colnames(smry)[-1]
        cnl <- strsplit(cn, " (", fixed = TRUE)
        cn_group <- sapply(cnl, function(x) x[1])
        cn_head <- gsub(")", "", sapply(cnl, function(x) x[2]), fixed = TRUE)
        colnames(smry)[-1] <- cn_head

        cn_group <- unique(cn_group)
        tbl_groups <- c(" ", setNames(rep(2, length(cn_group)), cn_group))
    }

    res <- knitr::kable(smry,
        format = "html",
        caption = labels$title,
        digits = digits
    )

    tbl <- kableExtra::kable_classic(res, full_width = FALSE)

    if (!is.null(tbl_groups)) {
        tbl <- kableExtra::add_header_above(tbl, tbl_groups)
    }

    if (as.character(d$x[nrow(d)]) == "Total") {
        tbl <- kableExtra::row_spec(tbl, nrow(smry),
            extra_css = "border-top: solid 1px gray;"
        )
    }

    tbl
}

summary.gg_multi_col <- function(object, html = FALSE, ...) {
    args <- modifyList(inzpar(), attr(object, "args"))
    varnames <- attr(object, "varnames", exact = TRUE)
    labels <- attr(object, "labels", exact = TRUE)
    d <- attr(object, "data", exact = TRUE)

    if ("y" %in% colnames(d)) {
        # special handling of this case ...
        # TODO: if g1 specified, will need to do some fancy subsetting...

        # else, just usual:
        smry <- tidyr::pivot_wider(d,
            names_from = !!rlang::sym("y"),
            values_from = c(!!rlang::sym("n"), !!rlang::sym("p"))
        )
        smry$x <- ifelse(smry$value == levels(smry$value)[1],
            as.character(smry$x), ""
        )
        cn <- colnames(smry)[-(1:2)]
        cn <- sapply(cn, function(x) {
            paste0(paste(rev(stringr::str_split(x, "_", n = 2L)[[1]]), collapse = " ("), ")")
        })
        smry <- setNames(smry, c("", "", cn))
        smry <- smry[, c(1:2, order(cn) + 2L)]
        colnames(smry) <- gsub("(p)", "(%)", colnames(smry), fixed = TRUE)
        pcols <- grepl("(%)", colnames(smry), fixed = TRUE)
        digits <- ifelse(pcols, args$round_percent, 0)
    } else if ("g1" %in% colnames(d)) {
        smry <- tidyr::pivot_wider(d,
            names_from = !!rlang::sym("g1"),
            values_from = c(!!rlang::sym("n"), !!rlang::sym("p"))
        )
        smry$x <- ifelse(smry$value == levels(smry$value)[1],
            as.character(smry$x), ""
        )
        cn <- colnames(smry)[-(1:2)]
        cn <- sapply(cn, function(x) {
            paste0(paste(rev(stringr::str_split(x, "_", n = 2L)[[1]]), collapse = " ("), ")")
        })
        smry <- setNames(smry, c("", "", cn))
        smry <- smry[, c(1:2, order(cn) + 2L)]
        colnames(smry) <- gsub("(p)", "(%)", colnames(smry), fixed = TRUE)
        pcols <- grepl("(%)", colnames(smry), fixed = TRUE)
        digits <- ifelse(pcols, args$round_percent, 0)
    } else {
        d$x <- ifelse(d$value == levels(d$value)[1],
            stringr::str_replace(as.character(d$x), "\n", " "), ""
        )
        smry <- setNames(
            dplyr::select(
                d,
                !!rlang::sym("x"),
                !!rlang::sym("value"),
                !!rlang::sym("n"),
                !!rlang::sym("p")
            ),
            c("", "", "N", "%")
        )
        digits <- c(0, args$round_percent, 0)
    }

    if (!requireNamespace("knitr", quietly = TRUE)) {
        return(smry)
    }

    opt <- options(knitr.kable.NA = "-")
    on.exit(options(opt))

    if (!html) {
        return(
            knitr::kable(smry,
                format = "simple",
                caption = labels$title,
                digits = digits
            )
        )
    }

    if (is.null(args$kable_styling)) {
        args$kable_styling <- list(bootstrap_options = NULL)
    }

    if (!requireNamespace("kableExtra", quietly = TRUE)) {
        res <- knitr::kable(smry,
            format = "html", ,
            caption = labels$title,
            digits = digits
        )
        tf <- tempfile(fileext = ".html")
        writeLines(res, tf)
        browseURL(tf)
        return(invisible(tf))
    }

    row_groups <- smry[[1]]
    row_gn <- length(unique(smry[[2]]))
    row_gi <- which(row_groups != "")
    row_group_names <- row_groups[row_gi]
    row_groups <- setNames(
        rep(row_gn, length(unique(row_group_names))),
        gsub("\n", " ", row_group_names, fixed = TRUE)
    )
    smry <- smry[, -1]

    tbl_groups <- NULL
    if (sum(c("y", "g1") %in% names(d)) == 1L) {
        # fancy header
        cn <- colnames(smry)[-1]
        cnl <- strsplit(cn, " (", fixed = TRUE)
        cn_group <- sapply(cnl, function(x) x[1])
        cn_head <- gsub(")", "", sapply(cnl, function(x) x[2]), fixed = TRUE)
        colnames(smry)[-1] <- cn_head

        cn_group <- unique(cn_group)
        tbl_groups <- c(" ", setNames(rep(2, length(cn_group)), cn_group))
    }

    res <- knitr::kable(smry,
        format = "html",
        caption = labels$title,
        digits = digits
    )

    tbl <- kableExtra::kable_classic(res, full_width = FALSE)

    tbl <- kableExtra::pack_rows(tbl, index = row_groups)

    if (!is.null(tbl_groups)) {
        tbl <- kableExtra::add_header_above(tbl, tbl_groups)
    }

    tbl
}

summary.gg_multi_stack <- summary.gg_multi_col
