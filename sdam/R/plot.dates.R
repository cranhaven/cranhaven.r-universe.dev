
## 
## PLOT FUNCTION plot.dates() to plot time intervals
## (CC BY-SA 4.0) Antonio Rivero Ostoic, jaro@cas.au.dk 
##
## version 0.1.7 (09-08-2022)
##
## PARAMETERS
## x      (data frame or table of variables and observations)
## type   (timespans, mid point and range, only range)
## taq    (terminus ante quem)
## tpq    (terminus post quem)
## 
## OPTIONAL PARAMETERS
## y      (optional identifiers)
## id     (IDs as variable or rownames in x)
## out    (number of outliers to omit)
## col    (colors of pch and time interval segment)
## cex    (size of pch)
## lwd    (width)
## lty    (shape)
## pch    (symbol for taq and tpq)
## main   (main tile)
## xlab   (x label)
## ylab   (y label)
## xlim   (x limit)
## axes   (includes axes in plot?)
## alpha  (alpha color transparency)
## file   (path to file for a PDF format)
## ...    (other optional parameters)


plot.dates <-
function (x, y, type = c("ts", "mp", "rg"), taq, tpq, id, out, 
    col, cex, lwd, lty, pch, main = NULL, xlab = NULL, ylab = NULL, 
    xlim = NULL, axes = TRUE, alpha, file = NULL, ...) 
{
    if (missing(x) == TRUE || is.null(unlist(x)) == TRUE) 
        stop("'x' is missing or NULL.")
    if (isTRUE(x == "EDH") == TRUE) {
        warning("\"x\" is for \"EDH\" dataset.")
        flge <- TRUE
        x <- suppressWarnings(edhw(taq = "not_before", tpq = "not_after", 
            as = "df"))
    }
    else {
        flge <- FALSE
    }
    if (missing(taq) == TRUE) {
        ifelse(isTRUE(flge == TRUE) == TRUE, taq <- "not_before", 
            taq <- colnames(x)[1])
    }
    else if (is.null(taq) == TRUE) {
        invisible(NA)
    }
    else {
        ifelse(isTRUE(taq %in% names(x)) == FALSE, stop("\"taq\" not found in 'x'."), 
            NA)
    }
    if (missing(tpq) == TRUE) {
        ifelse(isTRUE(flge == TRUE) == TRUE, tpq <- "not_after", 
            tpq <- colnames(x)[2])
    }
    else if (is.null(tpq) == TRUE) {
        invisible(NA)
    }
    else {
        ifelse(isTRUE(tpq %in% names(x)) == FALSE, stop("\"tpq\" not found in 'x'."), 
            NA)
    }
    ifelse(missing(lwd) == TRUE, lwd <- 1L, NA)
    ifelse(missing(lty) == TRUE, lty <- 1L, NA)
    ifelse(missing(cex) == TRUE, cex <- 1L, NA)
    if (missing(pch) == TRUE) {
        pch <- 20
        if (match.arg(type) == "mp") {
            pch2 <- 3
        }
        else {
            pch2 <- ""
        }
    }
    else {
        if (match.arg(type) == "mp") {
            pch2 <- pch[1]
        }
        else {
            pch2 <- ""
        }
        ifelse(isTRUE(length(pch) > 1L) == TRUE, pch <- pch[1:2], 
            pch <- rep(pch, 2))
    }
    if (missing(col) == TRUE) {
        if (match.arg(type) == "mp") {
            col <- c("transparent", "transparent", 8, "#808080")
        }
        else if (match.arg(type) == "rg") {
            col <- c("transparent", "transparent", 8, "transparent")
        }
        else {
            col <- c("#C0C0C0", "#808080", 8, "transparent")
        }
    }
    else {
        if (match.arg(type) == "mp") {
            col <- c("transparent", "transparent", 8, col[1])
        }
        else if (match.arg(type) == "rg") {
            col <- c("transparent", "transparent", col[1], "transparent")
        }
        else {
            ifelse(isTRUE(length(col) < 3L) == TRUE, col <- rep(col, 
                3)[1:3], col <- col[1:3])
        }
    }
    ifelse(missing(alpha) == TRUE, alpha <- 0.25, NA)
    ifelse(is.null(xlab) == TRUE, xlab <- "years", NA)
    if (any(c("tbl_df", "tbl") %in% class(x)) == TRUE) {
        xdates <- x <- as.data.frame(x)
    }
    else if (is.data.frame(x) == TRUE) {
        xdates <- x
    }
    else if (is.list(x) == TRUE) {
        xdates <- suppressWarnings(edhw(x = x, vars = c(taq, 
            tpq), as = "df", ...))
    }
    else {
        stop("Unknown data format in \"x\"")
    }
    if (missing(id) == FALSE) {
        if (any(colnames(x) %in% id) == TRUE) {
            xdates <- cbind(id = x[, which(colnames(x) %in% id)], 
                xdates)
            ifelse(is.null(ylab) == TRUE, ylab <- id, NA)
        }
        else if (all(rownames(x) == id) == TRUE) {
            xdates <- cbind(id = rownames(x), xdates)
        }
        else {
            NA
        }
    }
    else {
        NA
    }
    nb <- as.numeric(as.vector(xdates[, which(colnames(xdates) %in% 
        taq)]))
    na <- as.numeric(as.vector(xdates[, which(colnames(xdates) %in% 
        tpq)]))
    ifelse(isTRUE(length(nb) == 0) == TRUE || all(is.na(nb)) == 
        TRUE, nb <- na, NA)
    ifelse(isTRUE(length(na) == 0) == TRUE || all(is.na(na)) == 
        TRUE, na <- nb, NA)
    if (missing(out) == FALSE) {
        outliert <- c(tail(sort(boxplot(nb, plot = FALSE)$out), 
            out[1]), tail(sort(boxplot(na, plot = FALSE)$out), 
            out[1]))
        if (isTRUE(length(out) > 1) == TRUE) {
            outlierh <- c(head(sort(boxplot(nb, plot = FALSE)$out), 
                out[2]), head(sort(boxplot(na, plot = FALSE)$out), 
                out[2]))
            outliers <- c(outliert, outlierh)
        }
        else {
            outliers <- outliert
        }
        xdates <- xdates[-c(which(nb %in% outliers), which(na %in% 
            outliers)), ]
        nb <- as.numeric(as.vector(xdates[, which(colnames(xdates) %in% 
            taq)]))
        na <- as.numeric(as.vector(xdates[, which(colnames(xdates) %in% 
            tpq)]))
        years <- c(min(nb, na.rm = TRUE), max(na, na.rm = TRUE))
    }
    else {
        years <- c(min(nb, na.rm = TRUE), max(na, na.rm = TRUE))
    }
    if (match.arg(type) == "mp") {
        mpp <- vector()
        for (k in seq_len(nrow(xdates))) {
            taqa <- nb[k]
            tpqa <- na[k]
            mpp <- append(mpp, (tpqa + taqa)/2L)
        }
        rm(k)
        rm(taqa, tpqa)
    }
    else {
        mpp <- vector(length = nrow(xdates))
    }
    ifelse(is.null(xlim) == TRUE, xlim <- years, NA)
    if (missing(y) == FALSE) {
        if (is.character(y) == TRUE) {
            ID <- seq_along(y)
        }
        else {
            ID <- as.numeric(y)
        }
    }
    else {
        if (is.data.frame(x) == TRUE) {
            ifelse(is.null(xdates$id) == FALSE, ID <- as.numeric(sub("[[:alpha:]]+", 
                "", xdates$id)), ID <- as.numeric(seq_along(rownames(x))))
        }
        else {
            ifelse(is.null(unlist(xdates$id)) == FALSE, ID <- as.numeric(sub("[[:alpha:]]+", 
                "", xdates$id)), ID <- seq_len(length(xdates$id)))
        }
    }
    if (isTRUE(length(c(na, nb)) > 0) == TRUE) {
        warns = -1
        if (is.null(file) == TRUE) {
            plot(nb, ID, pch = pch, cex = cex, col = col[1], 
                xlab = xlab, ylab = ylab, xlim = xlim, main = main, 
                axes = axes, ...)
            points(na, ID, pch = pch, cex = cex, col = col[2])
            segments(nb, ID, na, ID, lwd = lwd, lty = lty, col = grDevices::adjustcolor(col[3], 
                alpha = alpha))
            points(mpp, ID, pch = pch2, cex = (cex * 0.2), col = col[4])
        }
        else {
            pdf(file)
            plot(nb, ID, pch = pch, cex = cex, col = col[1], 
                xlab = xlab, ylab = ylab, xlim = xlim, main = main, 
                axes = axes, ...)
            points(na, ID, pch = pch, cex = cex, col = col[2])
            segments(nb, ID, na, ID, lwd = lwd, lty = lty, col = grDevices::adjustcolor(col[3], 
                alpha = alpha))
            points(mpp, ID, pch = pch2, cex = (cex * 0.2), col = col[4])
            dev.off()
        }
        warns = 0
    }
}
