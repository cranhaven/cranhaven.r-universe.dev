.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    pleaseCite(pkgname),
    comain = NULL, appendLF = TRUE
  )
}
