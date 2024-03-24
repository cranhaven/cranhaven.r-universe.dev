##' @title Save and load Histry data
##' @description Save and load Histry data. Experimental.
##' @param trackr a history tracker object
##' @param append logical. Should any history already saved to \code{file} be retained when the new history is saved. defaults to TRUE
##' @param file character. The file to save your history within. When loading, assumed to be an RDS file. when saving, written as an RDS file.
##' @export
##' @rdname saveload
saveHistry = function(file = "./histry.rds", append = TRUE, trackr = histry_tracker()){
    out = hData(trackr)
    
    if(append && file.exists(file)) {
        old = readRDS(file)
        out = combineHistry(out, old, before=TRUE)
    }
    saveRDS(out, file = file)
}
##' @rdname saveload
##' @export
loadHistry = function(file = "./.histry.rds", trackr = histry_tracker()) {
    trackr$importHistory(impdata = file)
}


##' @title Combine History data from two sources/records
##' @description Combines the underlying history data from two
##'     representations.
##' @return The same class of object as \code{x}. I.e., if x is a
##'     HistoryTracker object, a HistoryTracker (of the same subclass)
##'     is returned. If X is a \code{HistoryData} object, that class
##'     of object is returned.
##' @param x An object representing captured history
##' @param y An object representing captured history
##' @param before logical. Should the data from y be placed before the
##'     data from x in the combined history. Defaults to \code{FALSE}
##' @note HistoryTracker objects are reference classes, meaning that
##'     if \code{x} is a HistoryTracker the changes will be reflected
##'     in all variables representing that trackr.
##' @docType methods
##' @export
##' @rdname combineHistry
setGeneric("combineHistry", function(x,y, before = FALSE) standardGeneric("combineHistry"))

.combineHist = function(x, y, before = FALSE) {
    if(before) {
        frst  = hData(y)
        scnd = hData(x)
    } else {
        frst = hData(x)
        scnd = hData(y)
    }

    ret = x ## ensure endomorphism
    exprs(ret) = c(exprs(frst), exprs(scnd))
    ret_classes(ret) = c(ret_classes(frst), ret_classes(scnd))
    hashes(ret) = c(hashes(frst), hashes(scnd))
    ret
}
##' @rdname combineHistry
##' @export
##' @aliases combineHistry,HistoryData-method
setMethod("combineHistry", "HistoryData", .combineHist)

##' @rdname combineHistry
##' @export
##' @aliases combineHistry,HistoryData-method
setMethod("combineHistry", "VirtHistoryTracker", .combineHist)
    
