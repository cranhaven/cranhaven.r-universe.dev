make_privacy_controls <- function(ctrls = NULL) {
    if (is.null(ctrls)) return(NULL)

    if (is.null(ctrls[["rounding"]])) ctrls$rounding <- ""
    if (is.null(ctrls[["suppression"]])) ctrls$suppression <- -Inf
    if (is.null(ctrls[["suppression_raw_counts"]])) ctrls$suppression_raw_counts <- NA_integer_
    if (is.null(ctrls[["secondary_suppression"]])) ctrls$secondary_suppression <- TRUE
    if (is.null(ctrls[["suppression_magnitude"]])) ctrls$suppression_magnitude <- ctrls$suppression
    if (is.null(ctrls[["suppression_quantiles"]])) ctrls["suppression_quantiles"] <- list(NULL)
    if (is.null(ctrls[["check_rse"]])) ctrls["check_rse"] <- list(NULL)
    if (is.null(ctrls[["symbol"]])) ctrls$symbol <- "S"
    if (is.null(ctrls[["seed"]])) ctrls$seed <- NA

    list(
        round =
            if (is.numeric(ctrls$rounding)) {
                function(x) round(x / ctrls$rounding) * ctrls$rounding
            } else {
                switch(as.character(ctrls$rounding),
                    "RR3" = function(x) {
                        d <- dim(x)
                        dn <- dimnames(x)
                        x <- rr(x, 3L)
                        dim(x) <- d
                        dimnames(x) <- dn
                        x
                    },
                    "GRR" = function(x) {
                        d <- dim(x)
                        dn <- dimnames(x)
                        x <- rr(x,
                            c(3L, 2L, 5L, 10L, 100L),
                            c(19L, 20L, 100L, 1000L)
                        )
                        dim(x) <- d
                        dimnames(x) <- dn
                        x
                    },
                    function(x) x
                )
            },
        suppression_matrix = function(x, using_raw = FALSE, using = "suppression") {
            d <- dim(x)
            dn <- dimnames(x)
            t <- ifelse(using_raw, ctrls$suppression_raw_counts, ctrls[[using]])
            x <- x < t
            dim(x) <- d
            dimnames(x) <- dn
            if (ctrls$secondary_suppression) {
                if (length(d) == 2) {
                    # two way
                    x <- cbind(x, apply(x, 1, sum) == 1L)
                } else {
                    # one way
                    x <- c(x, sum(x) == 1L)
                }
            } else {
                if (length(d) == 2) {
                    # two way
                    x <- cbind(x, FALSE)
                } else {
                    # one way
                    x <- c(x, FALSE)
                }
            }
            x
        },
        suppress = function(x, mat, symbol) {
            if (is.null(mat)) return(x)
            if (missing(symbol)) symbol <- ctrls$symbol
            x[mat] <- symbol
            x
        },
        suppress_quantile = function(x, n, q, symbol) {
            qi <- which(ctrls$suppression_quantiles$p == q)
            if (missing(symbol)) symbol <- ctrls$symbol
            if (length(qi) == 0) return(rep(symbol, length(x)))
            x[n < ctrls$suppression_quantiles$n[qi]] <- symbol
            x
        },
        rse_matrix = function(x, e) {
            d <- dim(x)
            dn <- dimnames(x)
            if (is.null(ctrls$check_rse)) {
                return(matrix(rep("", length(x)), d[1], d[2], dimnames = dn))
            }

            z <- 100 * e / x

            c <- unique(sort(c(0, ctrls$check_rse$cut)))
            o <- c(NA_character_, ctrls$check_rse$output)
            if (length(c) != length(o)) stop("invalid cut or output")

            ci <- sapply(z, function(zi) max(which(c <= zi)))
            o <- o[ci]
            dim(o) <- d
            dimnames(o) <- dn

            cbind(o, NA_character_)
        },
        markup = function(x, mat, exclude = "suppress") {
            d <- dim(x)
            dn <- dimnames(x)
            if (is.null(mat)) return(x)

            mat[is.na(mat)] <- ""
            y <- paste0(x, mat)
            y[mat %in% exclude] <- x[mat %in% exclude]
            dim(y) <- d
            dimnames(y) <- dn
            y

        },
        has = function(x) {
            !is.null(ctrls[[x]]) &&
            (length(ctrls[[x]]) > 1L || (
                !is.na(ctrls[[x]]) &&
                ifelse(is.numeric(ctrls[[x]]),
                    is.finite(ctrls[[x]]),
                    ctrls[[x]] != ""
                )
            ))
        },
        get = function(x) ctrls[[x]]
    )
}

# Adapted from code by Simon Anastasiadis at Social Wellbeing Agency, NZ.
# randomly round x to base b, optionally graduated at cut-points g
rr <- function(x, b, g) {
    if (!missing(g)) {
        g <- sort(unique(c(0, g)))
        if (length(b) != length(g)) stop("invalid g")
        gi <- sapply(x, function(xi) max(which(g <= xi)))
        b <- b[gi]
    }

    u <- runif(length(x))
    r <- x %% b
    p <- (b - r) / b

    x + b - r - b * (u < p)
}

#' Statistics New Zealand Privacy Controls
#'
#' Based off Microdata Output Guide 2020 v5-1
#'
#' @param type the type of data, used to specify the correct rules. Currently only survey (4.0.1) data is supported.
#' @param weighted logical indicating if the results are a weighted survey design or not.
#' @param ... additional arguments, used to override defaults
#'
#' @return a list of privacy control rules
#' @export
#' @md
snz_privacy_controls <- function(type = c("survey"),
                                 weighted = type == "survey",
                                 ...) {
    type <- match.arg(type)

    message("When calling inzsummary or inzinference, also set `round_percent = 1`")

    dots <- list(...)
    set <- list(
        rounding = if (weighted) 100L else "RR3",
        suppression = 1000L,
        secondary_suppression = FALSE,
        suppression_magnitude = 5L,
        suppress_quantile = list(
            p = c(0.25, 0.5, 0.75),
            n = c(20L, 10L, 20L)
        )
    )
    modifyList(set, dots)
}
