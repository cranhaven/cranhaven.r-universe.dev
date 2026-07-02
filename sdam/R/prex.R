
## 
## FUNCTION prex() to compute probability of existence ot time events
## (CC BY-SA 4.0) Antonio Rivero Ostoic, jaro@cas.au.dk 
##
## version 0.1.3 (14-06-2022)
##
## PARAMETERS
## x        (list or data frame object with dating data)
## type     (aoristic sum, mid point and range, other)
## taq      (terminus ante quem)
## tpq      (terminus post quem)
## vars     (vector, variables or attributes to be chosen from x)
## bins     (bin periods, integer)
## cp       (chronological periods)
##
## OPTIONAL PARAMETERS
##
## weight   (weight to observations)
## DF       (ignored if plot, data frame in output?)
## out      (number of outliers to omit)
## plot     (plot results?)
## main     (plot's main title)
## ylim     (limit y axis, only with plot)
## keep     (only 'mp', keep variables in output?)
## ...      (additional parameters)


prex <-
function (x, type = c("aoristic", "mp", "other"), taq, tpq, vars, 
    bins = NULL, cp, weight = 1, DF, out, plot = FALSE, main = NULL, 
    ylim, keep, ...) 
{
    if (missing(vars) == FALSE && (missing(taq) == TRUE | missing(tpq) == 
        TRUE)) {
        ifelse(isTRUE(length(vars) == 1L) == TRUE, stop("'vars' needs two values."), 
            vars <- vars[1:2])
        taq <- vars[1]
        tpq <- vars[2]
    }
    else {
        ifelse(missing(taq) == TRUE, taq <- "not_before", NA)
        ifelse(missing(tpq) == TRUE, tpq <- "not_after", NA)
    }
    vars <- c(taq, tpq)
    if (isTRUE(nrow(x) == 0) == TRUE) 
        return(NULL)
    ifelse(is.data.frame(x) == TRUE, xdf <- as.data.frame(x[, 
        which(colnames(x) %in% c(taq, tpq))]), xdf <- suppressWarnings(edhw(x = x, 
        vars = c(taq, tpq), as = "df", ...)))
    if (isTRUE(ncol(xdf) == 0) == TRUE) {
        stop("'vars', 'taq', or 'tpq' not in 'x'.")
    }
    if (missing(out) == FALSE) {
        nb <- as.numeric(as.vector(xdf[, which(colnames(xdf) %in% 
            vars[1])]))
        na <- as.numeric(as.vector(xdf[, which(colnames(xdf) %in% 
            vars[2])]))
        outliert <- c(tail(sort(boxplot(nb, plot = FALSE)$out), 
            out[1]), tail(sort(boxplot(na, plot = FALSE)$out), 
            out[1]))
        if (isTRUE(length(out) > 1) == TRUE) {
            outlierh <- c(head(sort(boxplot(nb, plot = FALSE)$out), 
                out[2]), head(sort(boxplot(na, plot = FALSE)$out), 
                out[2]))
        }
        else {
            outlierh <- outliert
        }
        xdf <- xdf[-c(which(nb %in% outliert), which(na %in% 
            outlierh)), ]
    }
    else {
        NA
    }
    if (match.arg(type) != "mp") {
        ifelse(missing(cp) == TRUE, cp <- "bin5", NA)
        if (is.null(bins) == TRUE) {
            flgb <- FALSE
            if (isTRUE(cp == "bin8") == TRUE) {
                bins <- list(Arch = rev(seq(from = -500, to = -700)), 
                  Class = rev(seq(from = -325, to = -500)), Hell = rev(seq(from = -325, 
                    to = 0)), ERom = rev(seq(from = 0, to = 200)), 
                  MRom = rev(seq(from = 200, to = 350)), LRom = rev(seq(from = 350, 
                    to = 650)), EByz = rev(seq(from = 650, to = 900)), 
                  MByz = rev(seq(from = 900, to = 1200)))
            }
            else if (isTRUE(cp == "bin5") == TRUE) {
                bins <- list(Arch = rev(seq(from = -500, to = -700)), 
                  Class = rev(seq(from = -325, to = -500)), Hell = rev(seq(from = -325, 
                    to = 0)), Rom = rev(seq(from = 0, to = 650)), 
                  Byz = rev(seq(from = 650, to = 1200)))
            }
            else {
                if (isTRUE(typeof(cp) == "list") == FALSE) 
                  stop("\"cp\" must be a list type object.")
                bins <- cp
            }
        }
        else if (is.numeric(bins) == TRUE && isTRUE(length(bins) == 
            1L) == TRUE) {
            ifelse(is.list(bins) == TRUE, flgb <- FALSE, flgb <- TRUE)
            if (isTRUE(bins > 1000L) == TRUE) {
                bins <- 1000L
                warning("Value in \"bins\" too large, set to 1000.")
            }
            else {
                NA
            }
            if (all(is.na(as.numeric(as.vector(xdf[, which(colnames(xdf) == 
                vars[2])])))) == TRUE && all(is.na(as.numeric(as.vector(xdf[, 
                which(colnames(xdf) == vars[2])])))) == TRUE) 
                return(NULL)
            if (all(is.na(as.numeric(as.vector(xdf[, which(colnames(xdf) == 
                vars[2])])))) == TRUE || all(is.na(as.numeric(as.vector(xdf[, 
                which(colnames(xdf) == vars[2])])))) == TRUE) {
                obin <- bins <- seq(from = min(c(as.numeric(as.vector(xdf[, 
                  which(colnames(xdf) == vars[2])])), as.numeric(as.vector(xdf[, 
                  which(colnames(xdf) == vars[1])]))), na.rm = TRUE), 
                  to = max(c(as.numeric(as.vector(xdf[, which(colnames(xdf) == 
                    vars[2])])), as.numeric(as.vector(xdf[, which(colnames(xdf) == 
                    vars[1])]))), na.rm = TRUE))
            }
            else {
                pbins <- pretty(min(as.numeric(as.vector(xdf[, 
                  which(colnames(xdf) == vars[1])])), na.rm = TRUE):max(as.numeric(as.vector(xdf[, 
                  which(colnames(xdf) == vars[2])])), na.rm = TRUE))
                brks <- seq(from = range(pbins)[1], to = range(pbins)[2], 
                  by = bins)
                ifelse(isTRUE(max(brks) < max(pbins)) == TRUE, 
                  brks <- append(brks, max(brks) + bins), NA)
                obin <- bins
                bins <- list()
                for (i in seq_len(length(brks) - 1L)) {
                  bins[[i]] <- rev(seq(from = brks[i + 1L], to = brks[i]))
                }
                rm(i)
            }
        }
        else {
            NA
        }
        breaks <- vector()
        for (k in seq_len(length(bins))) {
            breaks <- append(breaks, min(bins[[k]]))
        }
        rm(k)
        ifelse(isTRUE(max(breaks) %in% bins[[length(bins)]]) == 
            TRUE && isTRUE(flgb == FALSE) == TRUE, NA, breaks <- append(breaks, 
            max(bins[[length(bins)]])))
        if (isTRUE(flgb == TRUE) == TRUE) {
            lbs <- numeric(0)
            for (i in 1:(length(breaks) - 1)) lbs[i] <- paste(breaks[i], 
                breaks[i + 1], sep = " to ")
            names(bins) <- lbs
        }
        else {
            NA
        }
        taq <- as.numeric(as.vector(xdf[, which(colnames(xdf) == 
            vars[1])]))
        tpq <- as.numeric(as.vector(xdf[, which(colnames(xdf) == 
            vars[2])]))
        dur <- vector("list", length = nrow(xdf))
        ifelse(isTRUE("id" %in% colnames(xdf)) == FALSE, NA, 
            names(dur) <- unlist(xdf$id))
        for (i in seq_len(nrow(xdf))) {
            if (is.na(tpq[i]) == TRUE || is.na(taq[i]) == TRUE) {
                ifelse(is.na(tpq[i]) == TRUE && is.na(taq[i]) == 
                  TRUE, dur[[i]] <- 0, dur[[i]] <- 1L)
            }
            else {
                if (isTRUE(tpq[i] - taq[i] < 0) == TRUE) 
                  stop("'Terminus ante quem' greater than 'terminus post quem' detected.")
                dur[[i]] <- tpq[i] - taq[i]
            }
        }
        rm(i)
        if (isTRUE(sum(unlist(dur)) < 2) == TRUE) {
            pertaq <- taq[which(!(is.na(taq)))]
            pertpq <- tpq[which(!(is.na(tpq)))]
        }
        else {
            pertaq <- vector("list", length = length(bins))
            pertpq <- vector("list", length = length(bins))
            names(pertaq) <- names(pertpq) <- names(bins)
            for (k in seq_len(length(bins))) {
                ifelse(isTRUE(length(which(taq %in% (bins[[k]] - 
                  1L))) > 0) == TRUE, pertaq[[k]] <- which(taq %in% 
                  (bins[[k]] - 1L)), NA)
                ifelse(isTRUE(length(which(tpq %in% (bins[[k]] + 
                  1L))) > 0) == TRUE, pertpq[[k]] <- which(tpq %in% 
                  (bins[[k]] + 1L)), NA)
            }
            rm(k)
        }
    }
    if (match.arg(type) == "aoristic") {
        pertmq <- vector("list", length = length(bins))
        names(pertmq) <- names(bins)
        for (i in seq_len(length(taq))) {
            if (is.na(taq[i]) == TRUE) {
                tmpmq <- NULL
            }
            else {
                ifelse(isTRUE(sum(unlist(dur)) < 2) == TRUE, 
                  tmpmq <- taq[i], tmpmq <- which(breaks %in% 
                    seq(taq[i], taq[i] + (unlist(dur)[i] - 1L))))
            }
            if (isTRUE(sum(abs(unlist(dur))) < 2) == FALSE) {
                for (k in tmpmq) {
                  tmpm <- append(pertmq[[k]], i)
                  pertmq[[k]] <- tmpm
                }
                rm(k)
            }
            else {
                pertmq <- NULL
            }
        }
        rm(i)
        if (isTRUE(sum(unlist(dur)) < 2) == TRUE) {
            pertq <- c(pertaq, pertpq)
        }
        else {
            pertq <- lapply(mapply(c, pertaq, pertmq, pertpq, 
                SIMPLIFY = FALSE), unique)
        }
        wpu <- weight/unlist(dur)
        wpu[which(wpu == Inf)] <- 0
        xaor <- data.frame(matrix(nrow = nrow(xdf), ncol = length(bins)))
        ifelse(isTRUE(flgb == TRUE) == TRUE, colnames(xaor) <- lbs, 
            colnames(xaor) <- names(bins))
        for (k in seq_len(length(pertq))) {
            slc <- pertq[[k]]
            if (isTRUE(flgb == FALSE) == TRUE) {
                xaor[slc, k] <- length(bins[[k]]) * wpu[slc]
            }
            else {
                ifelse(isTRUE(flgb == FALSE) == TRUE, xaor[slc, 
                  k] <- 1L * wpu[slc], xaor[slc, k] <- obin * 
                  wpu[slc])
            }
        }
        rm(k)
        prxs <- colSums(xaor, na.rm = TRUE)
    }
    else if (match.arg(type) == "mp" || match.arg(type) == "other") {
        xmp <- cbind(xdf, rep(NA, nrow(xdf)), rep(NA, nrow(xdf)))
        colnames(xmp) <- c(colnames(xdf), "Mid point", "Range")
        for (k in seq_len(nrow(xdf))) {
            taqa <- as.numeric(as.vector(xdf[k, 1]))
            tpqa <- as.numeric(as.vector(xdf[k, 2]))
            xmp[k, 3] <- (tpqa + taqa)/2L
            xmp[k, 4] <- (tpqa - taqa)
        }
        rm(k)
        if (match.arg(type) == "mp") {
            ifelse(missing(keep) == FALSE && isTRUE(keep == TRUE) == 
                TRUE, return(cbind(x, xmp[, 3:4])), return(xmp))
        }
        else {
            xmph <- graphics::hist(xmp$`Mid point`, breaks = obin, 
                plot = FALSE)
            return(xmph$counts)
        }
    }
    if (isTRUE(plot == FALSE) == TRUE) {
        if (missing(DF) == FALSE && isTRUE(DF == TRUE) == TRUE) {
            return(list(data_frame = xdf, prxs = unlist(prxs, 
                use.names = TRUE)))
        }
        else {
            return(unlist(prxs, use.names = TRUE))
        }
    }
    else {
        ifelse(missing(ylim) == FALSE, suppressMessages(graphics::barplot(unlist(prxs, 
            use.names = TRUE), main = main, ylim = ylim)), suppressMessages(graphics::barplot(unlist(prxs, 
            use.names = TRUE), main = main)))
    }
}
