rng.state <- new.env()

.onLoad <- function(lib, pkg) {
  delayedAssign("stream", assign.env=rng.state,
                eval.env=environment(),
                value=iRNGStream(runif(1))) #nocovr
}

stop_unused <- function() NULL

cpu_info <- function() {
  cmd <- "awk '/model name/' /proc/cpuinfo"
  gsub("model name\t: ", "", unique(system(cmd, intern = TRUE)))
}

skip_if_no_python <- function() {

  if (identical(getOption("reticulate.python.disabled"), TRUE))
    testthat::skip("Python bindings not available for testing")

  if (!reticulate::py_available(initialize = TRUE))
    testthat::skip("Python bindings not available for testing")

}
