# To display an informative message when the package loads:
#  - to make usage conditions clear
#  - to display useful tips

.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Welcome to the MNREAD R package!")
}


# To set custom options for the package
.onLoad <- function(libname, pkgname) {
  op <- options()
  op.devtools <- list(
    devtools.path = "~/R-dev",
    devtools.install.args = "",
    devtools.name = "mnreadR",
    devtools.desc.author = '"Aurelie Calabrese <aurelie.calabrese@univ-amu.fr> [aut, cre]"',
    devtools.desc.license = "What license is it under?",
    devtools.desc.suggests = NULL,
    devtools.desc = list()
  )
  toset <- !(names(op.devtools) %in% names(op))
  if(any(toset)) options(op.devtools[toset])
  invisible()
}


