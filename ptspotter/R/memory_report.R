#' Perform garbage collection and log allocated memory.
#'
#' Used to log memory allocation at points during sequential script execution.
#'
#' @return Performs garbage collection then messages memory size and script name
#' currently being executed.
#'
#' @import this.path utils
#' @importFrom pryr mem_used
#'
#' @examples
#'
#' try(memory_report())
#'
#' @export
memory_report <- utils::removeSource(function() {
  # perform a manual garbage collection
  gc()
  # show me the filename of current file
  thisfile <- basename(this.path())

  # message the used memory at this point
  message(paste(
    "Memory size checked at", thisfile, "is",
    if (Sys.info()["sysname"] == "Darwin") {
      mem_used()
    } else {
      (memory.size()
      )
    }
  ))
})
