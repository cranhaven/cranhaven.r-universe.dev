.onAttach <- function(...) {
  date <- date()
  greet <- paste("# This package was created for research supported by NIH Grant R01 CA157528")
  packageStartupMessage(greet)
}
