.onAttach <- function(libname, pkgname) {
  if (!requireNamespace("torch", quietly = TRUE)) {
    packageStartupMessage(
      "The 'torch' package is not installed.\n",
      "Install it with: install.packages('torch')"
    )
  }
}
