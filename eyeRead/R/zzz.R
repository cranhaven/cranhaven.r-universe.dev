.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    paste0( "This is eyeRead version ", utils::packageVersion( pkgname ) )
  )
}