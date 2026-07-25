params <- function(x = NULL) {
    ## A suite of parameters for controlling behaviour
    # (not user-configurable, those are in inzpar())
    if (!is.null(x)) {
        params()[[x]]
    } else {
        list(max.levels = 101)
    }
}
