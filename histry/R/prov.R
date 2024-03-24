##' @import CodeDepends roprov
NULL

.alloutputs = function(x) {
    stopifnot(is(x, "ScriptNodeInfo"))
    c(x@outputs, x@updates)
}

is.code = function(x) is.function(x) || is.expression(x) || is.call(x) || is.name(x)
##' @title Generate ProvStoreDF from histry
##' @description Generates a tabular 'provenance store' from the history
##' of evaluated expressions captured by histry
##' @param tracker The HistoryTracker object to mine for the prov table information.
##' @return a ProvStoreDF object
##' @export
histryProvDF = function(tracker = histry_tracker()) {
    hdata = tracker$hdata
    outhashes = hashes(hdata)
    outclasses = ret_classes(hdata)
    code = sapply(exprs(hdata), function(x) if(is.code(x)) paste(deparse(x), collapse = "\n") else x)
    scr = readScript(txt = code, type="R")
    codeinfo = getInputs(scr)
    outputs = lapply(codeinfo, .alloutputs)
    nout = sapply(outputs, length)
    if(max(nout) > 1)
        stop("One or more expressions captured by histry have >1",
             " output. Not currently supported.")
    ## because of above, equivalent to == 1
    hasout = nout > 0
    ## make it not a list
    outputs = unlist(outputs[hasout])
    outhashes = outhashes[hasout]
    outclasses = outclasses[hasout]
    codeinfo = codeinfo[hasout]
    ## should always be a single output var at this point
    inpvars = lapply(codeinfo, function(x) x@inputs)
    rows = list(length(outputs))
    posinds = seq(along = outputs)
    for(i in rev(posinds)){
        if(length(inpvars[[i]]) > 0) {
            previnds = posinds < i
            rows[[i]] = provFromHistory(outputs[i], outhashes[i],
                                        outclasses[i],
                                        invars = inpvars[[i]],
                                  prevouts = outputs[previnds],
                                  prevouthashes = outhashes[previnds],
                                  prevoutclasses = outclasses[previnds],
                                  code = deparse(codeinfo[[i]]@code))
        } else {
            rows[[i]] = makeProvStore(outvarhashes = outhashes[i],
                                      code = deparse(codeinfo[[i]]@code),
                                      outvars = outputs[i],
                                      outvarclasses = outclasses[i])
        }
    }
    do.call(ProvStores, rows)
}



.fixcode = function(code) paste(code, collapse="\n")
