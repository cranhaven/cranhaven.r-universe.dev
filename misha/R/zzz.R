#' An environment for storing the package global variables
#'
#' @keywords internal
#' @export
.misha <- new.env(parent = emptyenv())

.onLoad <- function(lib, pkg) {
    Sys.umask("0002")

    options(.ginteractive = FALSE)
    options(.gautocompletion = FALSE)
    options(gmax.data.size = 1e7)
    options(gmax.mem.usage = 10000000) # in KB
    options(gbig.intervals.size = 1000000)
    options(gbuf.size = 1000)
    options(gmax.processes = 16)
    options(gmax.processes2core = 2)
    options(gmin.scope4process = 10000)
    options(gmultitasking = TRUE)

    options(gquantile.edge.data.size = 100000)
    options(gpv.middle.size = 0.96)
    options(gpv.middle.precision = 10^(-4))
    options(gpv.edge.precision = 10^(-9))

    options(gtrack.chunk.size = 100000)
    options(gtrack.num.chunks = 0)

    # set the groot to samples dir
    if (!exists("GROOT", envir = .misha)) {
        gdb.init_examples()
    }

    utils::suppressForeignCheck(c("retv", "."))
    utils::globalVariables(c("retv", "."))
}

.onAttach <- function(lib, pkg) {
    assign(".GFUNCS", getNamespaceExports("misha"), envir = .misha)
    assign("GITERATOR.INTERVALS", NULL, envir = .misha)

    assign(".GLIBDIR", path.package("misha"), envir = .misha)
}

.onDetach <- function(lib) {

}

.onUnload <- function(lib) {
    if (exists(".GFUNCS", envir = .misha)) {
        remove(".GFUNCS", envir = .misha)
    }

    if (exists(".GLIBDIR", envir = .misha)) {
        remove(".GLIBDIR", envir = .misha)
    }

    if (exists("ALLGENOME", envir = .misha)) {
        remove("ALLGENOME", envir = .misha)
    }

    if (exists("GINTERVID", envir = .misha)) {
        remove("GINTERVID", envir = .misha)
    }

    if (exists("GITERATOR.INTERVALS", envir = .misha)) {
        remove("GITERATOR.INTERVALS", envir = .misha)
    }

    if (exists("GROOT", envir = .misha)) {
        remove("GROOT", envir = .misha)
    }

    if (exists("GWD", envir = .misha)) {
        remove("GWD", envir = .misha)
    }

    if (exists("GTRACKS", envir = .misha)) {
        remove(list = get("GTRACKS", envir = .misha), envir = .misha)
        remove("GTRACKS", envir = .misha)
    }

    if (exists("GINTERVS", envir = .misha)) {
        remove(list = get("GINTERVS", envir = .misha), envir = .misha)
        remove("GINTERVS", envir = .misha)
    }
}
