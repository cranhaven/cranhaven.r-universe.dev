.pkgenv <- new.env(parent=emptyenv())

.onLoad <- function(libname, pkgname) {
    
    # load package data
    utils::data("df.pnv", "sf.afe", "sf.afr", "sf.tiles_elbe", "sf.tiles_rhine",
                package = pkgname, envir = parent.env(environment()))
    
    # add a default hydflood.datadir to options
    if (!("hydflood.datadir" %in% names(options()))) {
        d <- Sys.getenv("hydflood_datadir")
        if (d != "") {
            if (!dir.exists(d)) {
                options("hydflood.datadir" = tempdir())
            } else {
                options("hydflood.datadir" = d)
            }
        } else {
            options("hydflood.datadir" = tempdir())
        }
    } else {
        if (!dir.exists(options()$hydflood.datadir)) {
            tryCatch(
                dir.create(options()$hydflood.datadir, TRUE, TRUE, "0700"),
                error = function(e){
                    t <- tempdir()
                    msg <- paste0("It was not possible to create:",
                                  options()$hydflood.datadir, "\n", t,
                                  " is used instead!")
                    .pkgenv[["msg"]] <- msg
                    options("hydflood.datadir" = t)
                }
            )
        }
    }
}

.onAttach <- function(libname, pkgname) {
    if (exists("msg", envir = .pkgenv)) {
        packageStartupMessage(.pkgenv[["msg"]])
    }
}

if(getRversion() >= "2.15.1") {
    utils::globalVariables(c("sf.afe_csa", "sf.afr_csa"))
}
