
## 
## FUNCTION simil() for simple matching counting of data frame coocurrences
## (CC BY-SA 4.0) Antonio Rivero Ostoic, jaro@cas.au.dk 
##
## version 0.0.7 (11-05-2022)
##
## PARAMETERS
##
## x         (data frame or list object with vectors to compare)
##
## OPTIONAL PARAMETERS
##
## vars      (vector with column(s) in x representing attribute variables)
## uniq      (only unique elements?)
## diag.incl (logical, include entries in diagonal?)
## dichot    (dichotomize output?)
## rm.isol   (remove isolates?)
## k         (cut-off for dichotomization)


simil <-
function (x, vars, uniq, diag.incl, dichot, rm.isol, k) 
{
    ifelse(is.data.frame(x) == FALSE, x <- as.data.frame(do.call(rbind, 
        x)), NA)
    if (missing(vars) == TRUE) {
        vars <- seq_len(ncol(x))
    }
    else {
        if (is.vector(vars) == FALSE) 
            stop("\"vars\" must be a vector.")
    }
    ifelse(missing(uniq) == FALSE && isTRUE(uniq == FALSE) == 
        TRUE, NA, x <- unique(x))
    ifelse(is.null(x$id) == TRUE, mat <- matrix(0L, nrow = nrow(x), 
        ncol = nrow(x), dimnames = list(unlist(x[, 1]), unlist(x[, 
            1]))), mat <- matrix(0L, nrow = nrow(x), ncol = nrow(x), 
        dimnames = list(x$id, x$id)))
    if (any(duplicated(dimnames(mat)[[1]])) == TRUE) {
        ifelse(is.null(x$id) == TRUE, dimnames(mat)[[1]] <- dimnames(mat)[[2]] <- make.names(dimnames(mat)[[1]], 
            unique = TRUE), dimnames(mat)[[1]] <- dimnames(mat)[[2]] <- make.names(x$id, 
            unique = TRUE))
    }
    else {
        NA
    }
    for (at in vars) {
        if (at %in% colnames(x) == FALSE) {
            warning(paste("Variable", paste0("\"", at, "\""), 
                "not in 'x' and therefore is ignored.", sep = " "))
        }
        else {
            ccat <- unlist(unique(x[, at]))
            for (i in seq_len(length(ccat))) {
                slc <- which(x[, at] == ccat[i])
                mat[slc, slc] <- mat[slc, slc] + 1L
            }
            rm(i)
        }
    }
    rm(at)
    ifelse(missing(diag.incl) == FALSE && isTRUE(diag.incl == 
        TRUE) == TRUE, NA, diag(mat) <- 0)
    classmat <- noquote("SimMatrix")
    if (missing(dichot) == FALSE && isTRUE(dichot == TRUE) == 
        TRUE) {
        ifelse(missing(k) == TRUE, kmat <- max(mat), kmat <- k)
        mat <- multiplex::dichot(mat, c = kmat)
        classmat <- noquote(append(classmat, c("dichotomous", 
            kmat)))
    }
    else {
        NA
    }
    if (isTRUE(max(mat) > 0) == TRUE) {
        if (missing(rm.isol) == FALSE && isTRUE(rm.isol == TRUE) == 
            TRUE) {
            mat <- multiplex::rm.isol(mat)
            classmat <- noquote(append(classmat, "rm.isol"))
        }
        else {
            NA
        }
    }
    else {
        invisible(NA)
    }
    class(mat) <- classmat
    return(mat)
}
