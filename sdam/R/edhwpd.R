
## 
## FUNCTION edhwpd() to organize EDH dataset province and dates by similarity
## (CC BY-SA 4.0) Antonio Rivero Ostoic, jaro@cas.au.dk 
##
## version 0.0.5 (31-08-2022)
##
## PARAMETERS
##
## vars     (vector, variables or attributes to be chosen from x)
## province (choosen EDH province abbreviation)
##
## OPTIONAL PARAMETERS
##
## x        (list, typically fragments of EDH dataset or database API)
## dates    (vector with TAQ and TPQ)
## clean    (logical, to remove ?*+ characters and re-encode if needed)
##


edhwpd <-
function (x = "EDH", vars, province, dates, clean, ...) 
{
    if (is.null(x) == TRUE) 
        stop("'x' is NULL")
    ifelse(missing(dates) == TRUE, dates <- c("not_after", "not_before"), 
        NA)
    if (isTRUE(x == "EDH") == TRUE) {
        warning("\"x\" is for dataset \"EDH\".")
        if (!(exists("EDH"))) {
            if (is.na(suppressMessages(request("EDH.rda", "https://github.com/", 
                path = "sdam-au/sdam/raw/master/data/", anonymous = TRUE))) == 
                TRUE) {
                EDHurl <- "https://github.com/sdam-au/sdam/raw/master/data/EDH.rda"
            }
            else {
                EDHurl <- "https://github.com/mplex/cedhar/raw/master/pkg/sdam/data/EDH.rda"
            }
            tmp <- tempfile()
            utils::download.file(EDHurl, tmp)
            load(gzfile(tmp))
            unlink(tmp)
            rm(tmp)
        }
        else {
            NA
        }
        x <- EDH
        class(x) <- NULL
        comment(x) <- NULL
        xdf <- suppressWarnings(edhw(x = x, vars = c(dates, vars, 
            "province_label"), as = "df"))
    }
    else {
        xdf <- edhw(x = x, vars = c(dates, vars, "province_label"), 
            as = "df")
    }
    prv <- unique(edhw(x = xdf, vars = c(dates, vars), province = province, 
        as = "df"))
    if (isTRUE(nrow(prv) == 0) == TRUE) 
        return(NULL)
    ifelse(missing(clean) == FALSE && isTRUE(clean == TRUE) == 
        TRUE, prv <- cln(prv, ...), NA)
    smat <- simil(prv, vars = vars)
    mat <- multiplex::rm.isol(multiplex::dichot(smat, c = max(smat)))
    cmps <- multiplex::comps(mat)
    lbs0 <- unlist(cmps$com)
    ddf <- vector("list", length = length(cmps$com))
    for (i in seq_len(length(ddf))) {
        ddf[[i]] <- edhw(x = prv, id = cmps$com[[i]])
    }
    rm(i)
    if (isTRUE(length(which(!(dimnames(smat)[[1]] %in% lbs0))) > 
        0) == TRUE) {
        if (isTRUE(length(which(!(dimnames(smat)[[1]] %in% lbs0))) == 
            1) == TRUE) {
            ddf1 <- edhw(x = prv, id = dimnames(smat)[[1]][which(dimnames(smat)[[1]] %in% 
                lbs0 == FALSE)])
            mat1 <- smat1 <- NULL
            lbs1 <- dimnames(smat)[[1]][which(!(dimnames(smat)[[1]] %in% 
                lbs0))]
        }
        else {
            smat1 <- smat[which(!(dimnames(smat)[[1]] %in% lbs0)), 
                which(!(dimnames(smat)[[1]] %in% lbs0))]
            mat1 <- multiplex::rm.isol(multiplex::dichot(smat1, 
                c = max(smat1)))
            cmps1 <- multiplex::comps(mat1)
            lbs1 <- unlist(cmps1$com)
            ddf1 <- vector("list", length = length(cmps1$com))
            for (i in seq_len(length(ddf1))) {
                ddf1[[i]] <- edhw(x = prv, id = cmps1$com[[i]])
            }
            rm(i)
        }
        lddf <- list(ddf, ddf1)
    }
    else {
        lddf <- ddf
        lbs1 <- vector()
    }
    if (isTRUE(length(vars) > 2) == TRUE && isTRUE(all.equal(mat1, 
        smat1) == FALSE) == TRUE) {
        lbsx <- c(lbs0, lbs1)
        ddfx <- list()
        for (k in seq(3, length(vars))) {
            smatx <- smat[which(!(dimnames(smat)[[1]] %in% lbsx)), 
                which(!(dimnames(smat)[[1]] %in% lbsx))]
            matx <- multiplex::rm.isol(multiplex::dichot(smatx, 
                c = max(smatx)))
            cmpsx <- multiplex::comps(matx)
            lbsx <- append(lbsx, unlist(cmpsx$com))
            ddftmp <- vector("list", length = length(cmpsx$com))
            for (i in seq_len(length(ddftmp))) {
                ddftmp[[i]] <- edhw(x = prv, id = cmpsx$com[[i]])
            }
            rm(i)
            ddfx[[length(ddfx) + 1L]] <- ddftmp
        }
        rm(k)
        ifelse(isTRUE(length(cmpsx$isol) == 0) == TRUE, NA, ddfx[[length(ddfx) + 
            1L]] <- edhw(x = prv, id = cmpsx$isol))
        res <- c(lddf, ddfx)
    }
    else {
        ifelse(isTRUE(nrow(prv[which(prv$id %in% c(lbs0, lbs1) == 
            FALSE), ]) == 0) == TRUE, NA, lddf[[length(lddf) + 
            1L]] <- prv[which(prv$id %in% c(lbs0, lbs1) == FALSE), 
            ])
        res <- lddf
    }
    class(res) <- noquote(c("EDH", province, nrow(prv)))
    res
}
