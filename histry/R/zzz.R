set_histropts = function(val) histrstate$histropts <- val

initState = function() {
    knitrtracer(TRUE)
    evaltracer(TRUE)
    
}


.onAttach = function(libname, pkgname, ...) {
    stat = state$new(evalHistory = historyTracker("auto_tracker"),
                     knitrHistory = knitrTracker(),
                     inKnitr = !is.null(getOption("knitr.in.progress")))
    set_histropts(stat)
    initState()
    NULL
}

.onUnload = function(libname, pkgname, ...) {
    nms = getTaskCallbackNames()
    nms = grep("_tracker", nms, value=TRUE)
    lapply(nms, removeTaskCallback)



}
