.onLoad <- function(libname, pkgname) {

    # .auth is created in R/khis_cred.R
    # this is to insure we get an instance of AuthCred
    utils::assignInMyNamespace(
        '.auth',
        init_AuthCred()
    )

    if (identical(Sys.getenv("IN_PKGDOWN"), "true")) {
        tryCatch(
            khis_cred_docs(),
            khis_cred_internal_error = function(e) NULL
        )
    }

    invisible()
}
