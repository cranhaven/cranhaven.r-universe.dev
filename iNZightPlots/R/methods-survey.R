gSubset.inz.survey <- function(df, g1.level, g2.level, df.vs, missing) {
    # subset the data by g2 (keep everything, so xlims can be calculated)
    # g2 can take values (0 = "_ALL", 1:ng2, ng2+1 = "_MULTI")

    dd <- df$design$variables
    dd <- cbind(dd, df$data)

    vn <- as.list(df$varnames)

    des <- df$design


    matrix.plot <- FALSE
    g1 <- g2 <- NULL
    if ("g2" %in% names(df$varnames)) {
        g2 <- df$varnames["g2"]

        if (is.null(g2.level)) g2.level <- "_ALL"
        ng2 <- length(g2l <- if (is.null(g2.level)) "all" else levels(dd$g2))

        # if g2 specified numerically, check the value is ok, and then convert it to
        # character level anyway
        if (is.numeric(g2.level)) {
            if (as.integer(g2.level) != g2.level)
                warning(paste0("g2.level truncated to ", g2.level, "."))

            if (g2.level == 0) {
                g2.level <- "_ALL"
            } else if (g2.level == ng2 + 1) {
                g2.level <- "_MULTI"
            } else if (g2.level > ng2 + 1) {
                stop(paste("g2.level must be a number between 0 and", ng2 + 1))
            } else {
                g2.level <- g2l[g2.level]
            }
        }

        # separate function for drawing the matrix version
        if (g2.level == "_ALL") {
            df1 <- list(all = des)
            g2.level <- NULL
        } else {
            if (g2.level == "_MULTI") {
                matrix.plot <- TRUE
            }

            missing$g2 <- sum(is.na(dd[, g2]))
            df1 <- lapply(g2l,
                function(l) {
                    dft <- eval(
                        parse(
                            text = sprintf("subset(des, g2 == '%s')", l)
                        )
                    )
                }
            )
            names(df1) <- g2l
        }
    } else {
        g2l <- "all"
        df1 <- list(all = des)
    }

    # now, `df` is a list of data.frame of all levels of g2 (unless
    # g2.level = NULL/_ALL/0).  `missing` constains the number of
    # observations lost by subsetting g2 due to missing values of g2.

    if ("g1" %in% names(df$varnames)) {
        g1 <- df$varnames["g1"]
        # take two methods of specifying g1.level (numeric or level names), and convert to a vector
        # of only character names to be plotted
        g1l <- levels(df$data$g1)  # all levels of variable
        if (is.null(g1.level)) g1.level <- "_MULTI"

        if (is.numeric(g1.level)) {
            if (any(g1.level > length(levels(dd$g1)))) g1.level <- 0
            g1.level <-
                if (any(g1.level == 0)) "_MULTI"
                else levels(dd$g1)[g1.level]
        }

        if (any(g1.level == "_MULTI"))
            g1.level <- levels(df$data$g1)

        # track missing values due to missingness in g1
        missing$g1 <- sum(is.na(df$data$g1))
    } else {
        g1l <- "all"
        g1.level <- "all"
    }

    # this converts each data.frame in the list to a list of data
    # frames for all levels of g1

    if (any(c(g1, g2) %in% colnames(df$design$cluster))) {
        newcall <- modifyCall(df$design$call, "ids", "~1")
        df$design <- eval(parse(text = newcall))
    }
    oldcall <- df$design$call


    df.list <- lapply(df1,
        function(df2) {
            df3 <- lapply(g1l,
                function(x) {
                    if (x == "all") dfo <- df2
                    else dfo <- eval(
                        parse(
                            text = sprintf("subset(df2, g1 == '%s')", x)
                        )
                    )

                    if (nrow(dfo$variables) >= 1) return(dfo)
                    else return(NULL)
                }
            )
            names(df3) <- g1l
            df3
        }
    )

    ## sum up all of the missing values
    w.df <-
        if (is.null(g2.level)) "all"
        else if (g2.level == "_MULTI") 1:length(df.list)
        else g2.level

    missing$x <- sum(
        sapply(df.list[w.df],
            function(df) sum(
                sapply(df,
                    function(d)
                        if (!is.null(d)) sum(is.na(d$variables$x))
                        else 0
                )
            )
        )
    )

    if ("y" %in% df.vs)
        missing$y <- sum(
            sapply(df.list[w.df],
                function(df) sum(
                    sapply(df,
                        function(d)
                            if (!is.null(d)) sum(is.na(d$variables$y))
                            else 0
                    )
                )
            )
        )

    class(df.list) <- "inz.survey"
    list(
        df = df.list,
        matrix = matrix.plot,
        missing = missing,
        g1.level = g1.level,
        g2.level = g2.level
    )
}


modifyData <- function(oldcall, data) {
    args <- names(oldcall)
    vals <- as.character(oldcall)
    vals[args == "data"] <- data
    newcall <- paste0(
        vals[1],
        "(",
        paste(args[-1], vals[-1],
            sep = " = ",
            collapse = ", "
        ),
        ")",
        sep = ""
    )
    newcall
}

modifyCall <- function(oldcall, arg, val) {
    call <- as.list(oldcall)
    call[[arg]] <- as.name(val)

    ## fix namespacing of some survey packages
    if (!exists(as.character(call[[1]])) &&
        exists(as.character(call[[1]]), asNamespace("survey")))
        call[[1]] <- as.name(paste0("survey:::", call[[1]]))

    ## if it is an "as.svrepdesign" call, need to adjust data on the inner argument ...


    gsub("`", "", as.character(as.expression(as.call(call))))
}

get_weights <- function(obj) {
    if (is_svyrep(obj))
        return(as.numeric(weights(obj, type = "sampling")))

    if (is_survey(obj))
        return(weights(obj))

    return(NA)
}
