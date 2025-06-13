plot.AcrossTic <- function (x, X.values, y, grp.cols = c(2, 4), grp.pch = c(16, 17), ...) 
{
#
# plot an AcrossTic object. This is intended for a two-column X and
# a two-level y. 
#
# If X is supplied, use it. Otherwise use the "X" component of the
# AcrossTic object. If neither is supplied, that's an error.
#
    if (missing(X.values)) {
        if (!any(names(x) == "X")) 
            stop("Can't plot an AcrossTic object with no X", 
                call. = FALSE)
        X <- x$X
    }
    else {
        X <- X.values
    }
#
# If y is supplied, use it. If not, use the one in x, if there is one,
# and if there isn't, that's an error.
#
    if (missing (y)) {
        y <- x$y
        if (is.null(y)) 
            stop("This AcrossTic object has no 'y' entry", call. = FALSE)
    }
#
# This is for two-level y's only.
#
    if (length(unique(y)) != 2) 
        stop("This function only plots 'y' entries with two values", 
            call. = FALSE)
    y <- c(0, 1)[as.numeric(as.factor(y))]
    if (ncol(X) == 1) 
        stop("One-column X object?", call. = FALSE)
    if (ncol(X) > 2) 
        warning("Plotting two columns of a >2-col X object")
#
# Plot!
#
    M <- x$matches
    N <- x$nrow.X
    plot(X[, 1], X[, 2], col = grp.cols[y + 1], pch = grp.pch[y + 
        1], xlim = c(min(X[, 1]) - 0.5, max(X[, 1]) + 0.5), ylim = c(min(X[, 
        2]) - 0.5, max(X[, 2]) + 0.2), ...)
    legend("topleft", c("Group 1", "Group 2", "Within-group pairing", 
        "Across-group pairing"), lty = c(0, 0, 2, 1), col = c(grp.cols, 
        1, 1), pch = c(grp.pch, NA, NA))
#
# Draw lines to connect matched pairs.
#
    x.from <- X[M[, 1], 1]
    y.from <- X[M[, 1], 2]
    x.to <- X[M[, 2], 1]
    y.to <- X[M[, 2], 2]
    cross.match <- y[M[, 1]] != y[M[, 2]]
    solid.inds <- which(cross.match)
    dashed.inds <- which(!cross.match)
    segments(x.from[solid.inds], y.from[solid.inds], x.to[solid.inds], 
        y.to[solid.inds], lwd = 2)
    segments(x.from[dashed.inds], y.from[dashed.inds], x.to[dashed.inds], 
        y.to[dashed.inds], lty = 2)
}

