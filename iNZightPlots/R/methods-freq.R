## All of the methods only used for SIMPLE data types:

gSubset.inz.freq <- function(df, g1.level, g2.level, df.vs, missing) {
    # subset the data by g2 (keep everything, so xlims can be calculated)
    # g2 can take values (0 = "_ALL", 1:ng2, ng2+1 = "_MULTI")

    dd <- df$data
    dd$freq <- df$freq

    matrix.plot <- FALSE
    if ("g2" %in% df.vs) {
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
            df1 <- list(all = dd)
            g2.level <- NULL
        } else {
            if (g2.level == "_MULTI") {
                matrix.plot <- TRUE
            }

            missing$g2 <- sum(is.na(dd$g2))
            df1 <- lapply(g2l,
                function(l) {
                    dft <- dd[dd$g2 == l & !is.na(dd$g2), , drop = FALSE]
                    dft[, colnames(dft) != "g2"]
                }
            )
            names(df1) <- g2l
        }
    } else {
        g2l <- "all"
        df1 <- list(all = dd)
    }


    # now, `df` is a list of data.frame of all levels of g2 (unless
    # g2.level = NULL/_ALL/0).  `missing` constains the number of
    # observations lost by subsetting g2 due to missing values of g2.

    if ("g1" %in% df.vs) {
        # take two methods of specifying g1.level (numeric or level names), and convert to a vector
        # of only character names to be plotted
        g1l <- levels(dd$g1)  # all levels of variable
        if (is.null(g1.level)) g1.level <- "_MULTI"

        if (is.numeric(g1.level)) {
            if (any(g1.level > length(levels(dd$g1)))) g1.level <- 0
            g1.level <- if (any(g1.level == 0)) "_MULTI" else levels(dd$g1)[g1.level]
        }

        if (any(g1.level == "_MULTI"))
            g1.level <- levels(dd$g1)

        # track missing values due to missingness in g1
        missing$g1 <- sum(is.na(dd$g1))
    } else {
        g1l <- "all"
        g1.level <- "all"
    }

    # this converts each data.frame in the list to a list of data
    # frames for all levels of g1
    df.list <- lapply(df1,
        function(df2) {
            df3 <- lapply(g1l, function(x) inzDataList(df2, x))
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
                sapply(df, function(d) sum(is.na(d$x)))
            )
        )
    )
    if ("y" %in% df.vs)
        missing$y <- sum(
            sapply(df.list[w.df],
                function(df) sum(sapply(df, function(d) sum(is.na(d$y)))
            )
        )
    )

    class(df.list) <- "inz.freq"

    list(
        df = df.list,
        matrix = matrix.plot,
        missing = missing,
        g1.level = g1.level,
        g2.level = g2.level
    )
}
