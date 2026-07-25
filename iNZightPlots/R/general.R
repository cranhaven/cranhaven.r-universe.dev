rescale <- function(x) {
    r <- 3.75 * (x - min(x, na.rm = TRUE)) / diff(range(x, na.rm = TRUE)) + 0.25
    ## r <- (x - min(x, na.rm = TRUE)) / diff(range(x, na.rm = TRUE))
    ## r <- x / max(x, na.rm = TRUE)
    ## r[is.na(x)] <- NA
    r
}

Darken <- function(x = "#FFFFFF", v = 0.6) {
    if (x %in% colours()) {
        x <- rgb(convertColor(t(col2rgb(x)), "sRGB", "Apple RGB"))
    }

    x <- gsub('#', '', x)
    if (nchar(x) == 3)
        x <- paste(rep(strsplit(x, '')[[1]], each = 2), collapse = '')

    ## catch any transparency added to the colours
    if (nchar(x) == 8) {
        alpha <- substr(x, 7, 8)
        x <- substr(x, 1, 6)
    } else {
        alpha <- ""
    }

    if (nchar(x) != 6)
        stop("Not a valid hexadecimal code!")

    # Now start the function!

    r <- substr(x, 1, 2)
    g <- substr(x, 3, 4)
    b <- substr(x, 5, 6)

    dark <- strtoi(c(r, g, b), base = 16) * v / 255

    paste0(rgb(dark[1], dark[2], dark[3]), alpha)
}

darken <- Vectorize(Darken)  # allow it to work on a vector of Xs


Shade <- function(x, light, method = c("relative", "absolute")) {
    method <- match.arg(method)

    if (x %in% colours()) {
        x <- rgb(convertColor(t(col2rgb(x)), "sRGB", "Apple RGB"))
    }

    x <- gsub('#', '', x)
    if (nchar(x) == 3)
        x <- paste(rep(strsplit(x, '')[[1]], each = 2), collapse = '')

    ## catch any transparency added to the colours
    if (nchar(x) == 8) {
        alpha <- substr(x, 7, 8)
        x <- substr(x, 1, 6)
    } else {
        alpha <- ""
    }

    if (nchar(x) != 6)
        stop("Not a valid hexadecimal code!")

    rgb <- c(substr(x, 1, 2),
             substr(x, 3, 4),
             substr(x, 5, 6))
    rgb <- strtoi(rgb, base = 16)

    if (method == "relative") {
        if (light > 1 | light < -1) stop("light must be in [-1, 1]")

        if (light < 0) {
            rgb <- (1 + light) * rgb
        } else {
            rgb <- (1 - light) * rgb + light * 255
        }
    } else {
        rgb <- pmax(0, pmin(255, rgb + light))
    }

    rgb <- rgb / 255
    paste0(rgb(rgb[1], rgb[2], rgb[3]), alpha)
}

shade <- Vectorize(Shade)


#' Convert a numeric variable in to a factor with four levels.
#'
#' @title Convert to Factor
#' @param x a numeric vector
#' @return a factor variable
#' @author Tom Elliott
#' @export
#' @examples
#' f <- convert.to.factor(runif(100, 0, 10))
#' levels(f)
convert.to.factor <- function(x) {
    lbls <- expss::var_lab(x)
    if (is_cat(x)) {
        # to simplify coding elsewhere, allow convert to factor to simply return
        # the supplied x vector if it is already a factor.
        x.fact <- x
    } else if (is_dt(x)) {
        x.quantiles <- scales::pretty_breaks(4)(x)
        labs <- names(x.quantiles)
        labs <- paste(labs[-length(labs)], labs[-1], sep = " to ")
        x.fact <- cut(x, x.quantiles, labs)
    } else {

        ## converts a
        if (length(unique(x)) < 5)
            x.fact <- factor(x)
        else {
            x.quantiles <- round((quantile(x, na.rm = TRUE)), 0)
            x.fact <- try(
                cut(x,
                    c(
                        -Inf,
                        ifelse(unique(x.quantiles[2:4]) == 3,
                            x.quantiles[2:4],
                            unique(x.quantiles[2:4])
                        ),
                        Inf
                    )
                )
            )

            if (inherits(x.fact, "try-error")) {
                eps <- .Machine$double.eps
                x.quantiles <- round((quantile(x, na.rm = TRUE)), 2) + eps * (0:10)
                x.fact <- cut(x,
                    c(
                        -Inf,
                        ifelse(unique(x.quantiles[2:4]) == 3,
                            x.quantiles[2:4],
                            unique(x.quantiles[2:4])
                        ),
                        Inf
                    )
                )
            }

            if ( x.quantiles[2] == x.quantiles[3] &&
                 x.quantiles[3] == x.quantiles[4] ) {
                levels(x.fact) <- c(
                    paste(c("[", x.quantiles[1], " - ", x.quantiles[2], "]"),
                        collapse = ""
                    ),
                    paste(c("(", x.quantiles[2], " - ", x.quantiles[5], "]"),
                        collapse = ""
                    )
                )
            } else if (x.quantiles[2] == x.quantiles[3]) {
                levels(x.fact) <- c(
                    paste(c("[", x.quantiles[1], " - ", x.quantiles[2], "]"),
                        collapse = ""
                    ),
                    paste(c("(", x.quantiles[2], " - ", x.quantiles[4], "]"),
                        collapse = ""
                    ),
                    paste(c("(", x.quantiles[4], " - ", x.quantiles[5], "]"),
                        collapse = ""
                    )
                )
            } else if (x.quantiles[3] == x.quantiles[4]) {
                levels(x.fact) <- c(
                    paste(c("[", x.quantiles[1], " - ", x.quantiles[2], "]"),
                        collapse = ""
                    ),
                    paste(c("(", x.quantiles[2], " - ", x.quantiles[3], "]"),
                        collapse = ""
                    ),
                    paste(c("(", x.quantiles[3], " - ", x.quantiles[5], "]"),
                        collapse = ""
                    )
                )
            } else {
                levels(x.fact) <- c(
                    paste(c("[", x.quantiles[1], " - ", x.quantiles[2], "]"),
                        collapse = ""
                    ),
                    paste(c("(", x.quantiles[2], " - ", x.quantiles[3], "]"),
                        collapse = ""
                    ),
                    paste(c("(", x.quantiles[3], " - ", x.quantiles[4], "]"),
                        collapse = ""
                    ),
                    paste(c("(", x.quantiles[4], " - ", x.quantiles[5], "]"),
                        collapse = ""
                    )
                )
            }
        }
    }

    # Remove any empty levels -_-
    x <- factor(x.fact)
    if (!is.null(lbls)) expss::set_var_lab(x, lbls)
    x
}


nullPlot <- function(opts, xattr) {
    # simply draw nothing!
    out <- list(xlim = c(-Inf, Inf), ylim = c(-Inf, Inf))
    class(out) <- "inznull"
    out
}
plot.inznull <- function(...) {
    return(invisible(NULL))
}


colourPoints <- function(x, col.args, opts = inzpar()) {
    if (is.null(x))
        return(opts$col.pt)
    xclass <- ifelse(is.numeric(x), "numeric", "factor")
    switch(xclass,
        "numeric" = {
            xr <- col.args$n.range
            xm <- xr[1]
            xsc <- as.integer(199 * ((x - xm) / diff(xr)) + 1)
            ifelse(is.na(x), col.args$missing, col.args$n.cols[xsc])
        },
        "factor" = {
            x <- as.character(x)
            x[is.na(x)] <- "missing"
            col.args$f.cols[x]
        }
    )
}

#' Construct plot call from settings list
#' @param settings a list of plot settings, similar to `inzpar()`
#' @param vartypes a list of variables types (numeric, factor)
#' @param data a data set to pass to the call
#' @param design a survey design (can be NULL)
#' @param what the type of call to produce
#' @return a plot/summary/inference call
#' @md
#' @importFrom iNZightTools "%notin%"
construct_call <- function(settings, vartypes,
                           data = quote(.dataset),
                           design = quote(.design),
                           what = c("plot", "summary", "inference")) {
    if (is.null(settings$x)) {
        settings <- list(data = data)
        call <- capture.output(dput(settings))
        call <- gsub("^list", "getPlotSummary", call)
        call <- gsub(".DROP = ", "", call)

        return(parse(text = paste(call, collapse = "\n")))
    }

    what <- match.arg(what)

    ## remove names:
    rem_names <- c("pch")
    for (n in rem_names) {
        names(settings[[n]]) <- NULL
    }

    ## remove options
    rmv_args <- c("locate.settings")
    settings <- settings[names(settings) %notin% rmv_args]

    # go through settings and compare to default settings
    default_args <- formals(iNZightPlot)
    inz_args <- inzpar()
    gg_args <- gg_defaults
    if (what %in% c("summary", "inference")) {
        smry_args <- formals(getPlotSummary)
        smry_args <- smry_args[names(smry_args) %notin% names(default_args)]
        default_args <- c(default_args, smry_args)
    }
    defaults <- c(default_args, inz_args, gg_args)

    lapply(names(settings),
        function(s_name) {
            is_same <- identical(
                settings[[s_name]],
                defaults[[s_name]],
                ignore.bytecode = TRUE,
                ignore.environment = TRUE
            )
            if (is_same) settings[[s_name]] <<- NULL
        }
    )
    ## set the data
    settings$data <- data
    if (!is.null(design)) {
        settings$data <- NULL
        settings$design <- design
    }

    ## order of list
    name_order <- c(names(default_args),  names(inz_args))
    name_order <- name_order[name_order %in% names(settings)]

    ## missing args
    missing <-
        names(settings) %notin% name_order &
        names(settings) %notin% c("data_name")
    if (any(missing)) {
        name_miss <- names(settings)[missing]
        name_order <- c(name_order, name_miss)
    }

    settings <- settings[name_order]

    # formula
    if (!is.null(settings$y) || !is.null(settings$g1) || !is.null(settings$g2)) {
        fmla <- as.character(settings$x)
        if (!is.null(settings$y)) {
            fmla <- paste(fmla, as.character((settings$y)), sep = " ~ ")
        } else {
            fmla <- paste("~", fmla)
        }
        if (!is.null(settings$g1) || !is.null(settings$g2)) {
            if (is.null(settings$g1)) {
                if (settings$g2.level == "_ALL") {
                    gfm <- NULL
                    settings$g2.level <- NULL
                } else {
                    gfm <- as.character(settings$g2)
                    settings$g1.level <- settings$g2.level
                    settings$g2.level <- NULL
                }
            } else if (is.null(settings$g2) || settings$g2.level == "_ALL") {
                gfm <- as.character(settings$g1)
            } else {
                gfm <- paste(
                    as.character(settings$g1),
                    as.character(settings$g2),
                    sep = " + "
                )
            }

            if (!is.null(gfm))
                fmla <- paste(fmla, "|", gfm)
        }
        if (grepl(" ~ \\.$", fmla)) {
            fmla <- eval(parse(text = paste("~", settings$x)))
        } else {
            fmla <- eval(parse(text = fmla))
        }
    } else {
        fmla <- eval(parse(text = paste("~", as.character(settings$x))))
    }
    settings <- c(list(f = fmla), settings)
    settings$x <- NULL
    settings$y <- NULL
    settings$g1 <- NULL
    settings$g2 <- NULL

    ## plot.features
    if (!is.null(settings$plot.features)) {
        if (length(settings$plot.features))
            settings$plot.features <- modifyList(list(), as.list(settings$plot.features))
        if (length(settings$plot.features) == 0)
            settings$plot.features <- NULL
    }

    ## transformations
    if (!is.null(settings$transform)) {
        settings$transform <- modifyList(list(), settings$transform)
        if (length(settings$transform) == 0)
            settings$transform <- NULL
    }

    # only include overwritten varnames
    # vnames <- settings$varnames
    # for (vn in names(vnames)) {
    #     if (is.null(settings[[vn]]) ||
    #         is.null(vnames[[vn]]) ||
    #         settings[[vn]] == vnames[[vn]])
    #         vnames[[vn]] <- NULL
    # }
    # settings$varnames <- if (length(vnames)) vnames else NULL
    settings$varnames <- NULL

    ## remove names:
    rem_names <- c("pch")
    for (n in rem_names) {
        names(settings[[n]]) <- NULL
    }

    ## g1.level/g2.level
    if (isTRUE(settings$g1.level == "_MULTI")) settings$g1.level <- NULL
    if (isTRUE(settings$g2.level == "_ALL")) settings$g2.level <- NULL

    ## fix "inference.type"
    if (vartypes$x == "num" && !is.null(vartypes$y) && vartypes$y == "num") {
        if ("conf" %in% settings$inference.type)
            settings$inference.type <- "conf"
    }

    if (what == "plot") {
        ## things unique to plots

    } else {
        ## things unique to summary/inference
        settings$plot.features <- NULL
        settings$plottype <- NULL

        if (what == "summary") {
            ## things unique to summary

        }
        if (what == "inference") {
            ## things unique to inference

        }
    }

    ## remove any NULLs
    settings <- modifyList(list(), settings)
    settings <- lapply(settings,
        function(x)
            if (is.null(x) || (is.character(x) && all(x == "NULL"))) NULL else x
    )

    ## drop "x = " and "y = "
    names(settings) <- ifelse(names(settings) %in% c("f", "x", "y"),
        paste0(names(settings), "DROP"),
        names(settings)
    )

    call <- capture.output(dput(settings))
    fn <- switch(what,
        plot = "inzplot",
        summary = "inzsummary",
        inference = "inzinference"
    )
    call <- gsub("^list", fn, call)
    call <- gsub(".DROP = ", "", call)

    parse(text = paste(call, collapse = "\n"))
}


#' Mend a plot call based on valid parameters
#' @param call a plot call string, or expression
#' @param data the dataset
#' @param design_name name of the design, if any
#' @param plot the result of `inzplot`, `inzsummary`, or `inzinference`
#' @return a plot call with extraneous arguments removed
#' @md
mend_call <- function(call, data, design_name, plot) {
    # adjust name
    dname <- attr(data, "name", exact = TRUE)
    if (is.null(dname) || dname == "") dname <- "data"
    dname <- iNZightTools::create_varname(dname)

    if (is.expression(call) && as.character(call[[1]])[1] != "getPlotSummary") {
        ## and remove invalid vars (for plot_type/method combination)
        cnames <- names(call[[1]])
        ptype <- attr(plot, "plottype")
        if (!is.null(ptype)) {
            if (ptype == "bar") {
                vnames <- attr(plot, "varnames")
                vtypes <- attr(plot, "vartypes")
                xcat <- vtypes[[vnames$x]] == "factor"
                ycat <- !is.null(vnames$y) && vtypes[[vnames$y]] == "factor"
                if (xcat && ycat)
                    ptype <- "bar2"
                else if (length(levels(data[[vnames$x]])) == 2L)
                    ptype <- "barBinary"
            }
            keep <- valid_par(
                cnames,
                ptype,
                switch(as.character(call[[1]])[1],
                    "inzplot" = "plot",
                    "inzsummary" = "summary",
                    "inzinference" = "inference"
                )
            )
            call[[1]] <- call[[1]][keep]
        }
    }

    code <- as.character(call)
    code <- gsub(".dataset", dname, code, fixed = TRUE)
    if (any(grepl(".design", code, fixed = TRUE))) {
        code <- gsub(".design", ".design", code, fixed = TRUE)
        code <- gsub(".design", design_name,
            code,
            fixed = TRUE
        )
    }
    code
}

parse_formula <- function(fmla) {
    ## Parse the formula
    x <- NULL
    y <- NULL
    g1 <- NULL
    g2 <- NULL

    f.list <- as.list(fmla)
    if (length(f.list) == 3) {
        # there is a y specified
        y <- f.list[[2]]
        f.list2 <- as.list(f.list[[3]])
    } else {
        f.list2 <- as.list(f.list[[2]])
    }

    if (length(f.list2) == 1) {
        # no grouping vars
        x <- f.list2[[1]]
    } else {
        if (as.character(f.list2[[1]]) == "|") {
            f.list3 <- as.list(f.list2[[3]])
            if (length(f.list3) == 1) {
                g1 <- f.list3[[1]]
            } else {
                g1 <- f.list3[[2]]
                g2 <- f.list3[[3]]
            }

            x <- f.list2[[2]]
        } else if (as.character(f.list2[[1]]) == "+") {
            s1 <- as.character(f.list2[[2]])
            s2 <- as.character(f.list2[[3]])
            if (length(s1) > 1L) s1 <- paste(s1[-1], collapse = " + ")
            if (length(s2) > 1L) s2 <- paste(s2[-1], collapse = " + ")
            str <- sprintf("%s + %s", s1, s2)
            x <- as.call(str2lang(str))
        } else {
            stop("unsupported formula")
        }
    }

    list(x = x, y = y, g1 = g1, g2 = g2)
}

single_level_factors <- function(df, vars = c("x", "y")) {
    vars <- vars[vars %in% names(df)]
    df <- df[, vars, drop = FALSE]
    sapply(df, function(x) {
        if (tibble::is_tibble(x) && ncol(x) > 1L) return(FALSE)
        if (tibble::is_tibble(x)) x <- x[[1]]
        length(levels(x)) == 1L
    })
}

`%||%` <- function(a, b) {
    if (!is.null(a)) a else b
}

add_units <- function(x, unit) {
    if (is.null(unit)) return(x)
    sprintf("%s (%s)", x, unit)
}

format_pval <- function(x, opts, ...) {
    format.pval(x, ..., eps = opts$min_pval, scientific = FALSE)
}
