FragStat <- setClass(
    "FragStat",
    slots = list(
        qmatrix = "matrixOrNULL",
        groupMean = "numericOrNULL",
        refMean = "numericOrNULL",
        groupSD = "numericOrNULL",
        refSD = "numericOrNULL",
        groupSEM = "numericOrNULL",
        refSEM = "numericOrNULL"
    )
)



#' Getters and Setters for S4 object
#'
#' @param x S4 object
#' @param name Slot name
#' @param value Value to set
#' @return S4 object itself or slot value
#' @export
setMethod("$", "FragStat", function(x, name) {
    slot(x, name)
})

#' @rdname cash-FragStat-method
#' @export
setMethod("$<-", "FragStat", function(x, name, value) {
    slot(x, name) <- value
    invisible(x)
})

#' Print the FragStat object
#' @param object A FragStat object
#' @rdname show-FragStat-method
#' @return the object itself
#' @export
setMethod("show", "FragStat", function(object) {
    cat("\nFragStat object (Summary Statistics by Step)\n")
    printSlots(object)
    cat("Use '$attr' to access the data\n")
    invisible(object)
})
