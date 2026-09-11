.onLoad <- function(lib, pkg) {
       .C('setNThreads', n = as.integer(1L), PACKAGE = "HRTnomaly")
}

.onAttach <- function(lib, pkg) {
  if (.Call("isOmp", PACKAGE = "HRTnomaly")) {
    packageStartupMessage(sprintf("Package compiled with openMP (version released in %.2f)",
      .Call("openMP_version", PACKAGE = "HRTnomaly")))
  }
  else {
    packageStartupMessage("Package compiled without openMP.")
  }
  nn <- setCores()
  if (nn > 0L)
    packageStartupMessage("Use the function setCores() to change the number of CPU cores.")
}
