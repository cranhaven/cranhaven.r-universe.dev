.Fragility <- setClass(
    "Fragility",
    slots = list(
        ieegts = "matrixOrNULL",
        adj = "arrayOrNULL",
        frag = "matrix",
        frag_ranked = "matrix",
        R2 = "matrix",
        lambdas = "numeric",
        startTimes = "numeric",
        electrodes = "characterOrNULL"
    )
)

Fragility <- function(ieegts, adj, frag, frag_ranked, R2, lambdas, startTimes, electrodes) {
    if (!pkgData$debug) {
        ieegts <- NULL
        adj <- NULL
    }
    .Fragility(
        ieegts = ieegts,
        adj = adj,
        frag = frag,
        frag_ranked = frag_ranked,
        R2 = R2,
        lambdas = lambdas,
        startTimes = startTimes,
        electrodes = electrodes
    )
}

#' @rdname cash-FragStat-method
#' @export
setMethod("$", "Fragility", function(x, name) {
    slot(x, name)
})

#' @rdname cash-FragStat-method
#' @export
setMethod("$<-", "Fragility", function(x, name, value) {
    slot(x, name) <- value
    invisible(x)
})


#' Print the Fragility object
#' @param object A Fragility object
#' @rdname show-Fragility-method
#' @return the object itself
#' @export
setMethod("show", "Fragility", function(object) {
    cat("\nFragility object\n")
    if (pkgData$debug) {
        slots <- c("ieegts", "adj", "frag", "frag_ranked", "R2", "lambdas")
    } else {
        slots <- c("frag", "R2", "lambdas", "startTimes", "electrodes")
    }
    printSlots(object, slots = slots)
    cat("Use '$attr' to access the data\n")
    invisible(object)
})

#' Subset a Fragility object
#'
#' @param x A Fragility object
#' @param i A logical vector or a numeric vector of indices to subset the electrodes
#' @param j A logical vector or a numeric vector of indices to subset the time windows
#' @param ... Additional arguments (not used)
#' @param drop Additional arguments (not used)
#' @return A new Fragility object with the subsetted data
#' @rdname subset-Fragility-method
#' @aliases [,Fragility-method
#' @export
setMethod("[", "Fragility", function(x, i, j, ..., drop = FALSE) {
    
    if (!missing(i)){
        i <- checkIndex(i, x$electrodes)
    }else{
        i <- TRUE
    }
    if(missing(j)){
        j <- TRUE
    }

    frag_subset <- x@frag[i, j, drop = FALSE]
    frag_ranked_subset <- x@frag_ranked[i, j, drop = FALSE]
    R2_subset <- x@R2[i, j, drop = FALSE]
    lambdas_subset <- x@lambdas[j]
    startTimes_subset <- x@startTimes[j]
    electrodes_subset <- x@electrodes[i]
    .Fragility(
        ieegts = x@ieegts,
        adj = x@adj,
        frag = frag_subset,
        frag_ranked = frag_ranked_subset,
        R2 = R2_subset,
        lambdas = lambdas_subset,
        startTimes = startTimes_subset,
        electrodes = electrodes_subset
    )
})

#' Get the number of rows or columns of a Fragility object
#'
#' @param x A Fragility object
#' @return 
#' - `nrow(x)`: The number of rows (electrodes) in the fragility matrix.
#' - `ncol(x)`: The number of columns (time points) in the fragility matrix.
#' - `dim(x)`: A vector of length 2 containing the number of rows and columns in the fragility matrix.
#' @rdname dim-Fragility-method
#' @export
setMethod("nrow", "Fragility", function(x) {
    nrow(x@frag)
})

#' @rdname dim-Fragility-method
#' @export
setMethod("ncol", "Fragility", function(x) {
    ncol(x@frag)
})