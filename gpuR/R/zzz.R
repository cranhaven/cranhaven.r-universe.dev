#' @importFrom utils strOptions packageVersion tail

.onLoad <- function(libname, pkgname) {
    options(gpuR.print.warning=TRUE)
    options(gpuR.default.type = "float")
    # options(gpuR.default.device.type = "gpu")
}

.onAttach <- function(libname, pkgname) {
    # Initialize all possible contexts
    if (!identical(Sys.getenv("APPVEYOR"), "True")) {
        # initialize contexts
      packageStartupMessage(paste0("gpuR ", packageVersion('gpuR')))
      packageStartupMessage(initContexts())
    }
}

.onUnload <- function(libpath) {
    options(gpuR.print.warning=NULL)
    options(gpuR.default.type = NULL)
    # options(gpuR.default.device.type = NULL)
}
