pkgconfig <- function(opt=c("PKG_LIBS", "PKG_CPPFLAGS"))
{   
    opt <- match.arg(opt)
    if (opt == "PKG_LIBS") {
        usrlib_dir <- system.file("usrlib", package="Rtwobitlib", mustWork=TRUE)
        platform <- Sys.info()[["sysname"]]
        if (platform == "Windows") {
            r_arch <- .Platform[["r_arch"]]
            usrlib_dir <- file.path(usrlib_dir, r_arch)
        }
        usrlib_path <- sprintf("'%s'", file.path(usrlib_dir, "libtwobit.a"))
        if (platform == "Windows") {
            ## See how PKG_LIBS is defined in Rtwobitlib/src/Makevars.win
            ## and make sure to produce the same value here.
            libs <- "crypto"
            libs <- paste(sprintf("-l%s", libs), collapse=" ")
        } else {
            ## See how PKG_LIBS is defined in Rtwobitlib/src/Makevars
            ## and make sure to produce the same value here.
            libs <- "-lcrypto"
        }
        ## We will need -lcrypto only if we decide to support the
        ## twoBitOpenExternalBptIndex functionality. See README.txt
        ## in Rtwobitlib/inst/unused_but_kept_just_in_case/ for more
        ## information.
        #config <- paste(usrlib_path, libs)
        config <- usrlib_path
    } else {
        ## See how PKG_CPPFLAGS is defined in Rtwobitlib/src/Makevars.common
        ## and make sure to produce the same value here.
        config <- "-D_FILE_OFFSET_BITS=64"
        ## Packages that link to Rtwobitlib should have Rtwobitlib in their
        ## LinkingTo field so the preprocessor option below will automatically
        ## be added. There is no need to add it again here.
        #include_dir <- system.file("include", package="Rtwobitlib")
        #config <- paste(config, sprintf("-I'%s'", include_dir))
    }
    cat(config)
}

