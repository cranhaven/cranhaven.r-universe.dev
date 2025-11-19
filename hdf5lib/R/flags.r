#' Get C/C++ Compiler Flags for hdf5lib
#'
#' @description
#' Provides the required C/C++ compiler flags to find the HDF5 header
#' files bundled with the `hdf5lib` package.
#'
#' @return A scalar character vector containing the compiler flags (e.g., the
#'   `-I` path to the package's `inst/include` directory).
#'
#' @export
#' @seealso [ld_flags()]
#' @examples
#' c_flags()
#' 
c_flags <- function() {

  # Find the directory /path/to/R/library/hdf5lib/include
  include_dir <- system.file("include", package = "hdf5lib")
  
  # Ensure the directory exists
  if (include_dir == "" || !dir.exists(include_dir))
    stop("C flags not found: The 'inst/include' directory is missing from hdf5lib.")
  
  # Ensure a header file actually exists in that directory
  if (!file.exists(file.path(include_dir, "hdf5.h")))
    stop("Header file not found: 'include/hdf5.h' is missing from hdf5lib.")
  
  # Quote if the path contains spaces or other shell-special characters.
  # Don't quote by default, as that can sometimes cause other problems.
  normalized_path <- normalizePath(include_dir, winslash = "/", mustWork = TRUE)
  if (grepl("[ &'();]", normalized_path)) {
    normalized_path <- shQuote(normalized_path)
  }
  
  # Return the compiler flag
  # Use normalizePath and winslash for robust paths
  paste0("-I", normalized_path)
}


#' Get C/C++ Linker Flags for hdf5lib
#'
#' @description
#' Provides the required linker flags to link against the static HDF5
#' library (`libhdf5z.a`) bundled with the `hdf5lib` package.
#'
#' @return A scalar character vector containing the linker flags.
#'
#' @export
#' @seealso [c_flags()]
#' @examples
#' ld_flags()
#' 
ld_flags <- function() {

  # Find the package's 'lib' directory (e.g., /path/to/R/library/hdf5lib/lib)
  # This corresponds to the 'inst/lib' directory in the source package.
  lib_dir <- system.file("lib", package = "hdf5lib")
  if (lib_dir == "" || !dir.exists(lib_dir))
    stop("Linker flags not found: The 'inst/lib' directory is missing from hdf5lib.")
  
  # Ensure the static library file actually exists in that directory
  if (!file.exists(file.path(lib_dir, "libhdf5z.a")))
    stop("Static library not found: 'lib/libhdf5z.a' is missing from hdf5lib.")
  
  # Quote if the path contains spaces or other shell-special characters.
  # Don't quote by default, as that can sometimes cause other problems.
  normalized_path <- normalizePath(lib_dir, winslash = "/", mustWork = TRUE)
  if (grepl("[ &'();]", normalized_path)) {
    normalized_path <- shQuote(normalized_path)
  }
  
  lib_dir_flag <- paste0("-L", normalized_path)

  # Create a vector of all flags.
  # The downstream package must now link to hdf5 and its dependencies.
  flags <- c(
    lib_dir_flag, # Pass the full path hdf5lib's /lib directory
    "-lhdf5z",    # Link to our libhdf5z.a static library
    "-lpthread",  # HDF5 dependency for thread-safety
    if (.Platform$OS.type == "unix") "-ldl" # HDF5 dependency on Unix
  )
  
  # Collapse all flags into a single, space-separated string
  paste(flags, collapse = " ")
}
