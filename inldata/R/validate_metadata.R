#' Validate Metadata
#'
#' @description Validates the Extensible Markup Language (XML) formatted metadata
#'   that is intended for a USGS data release.
#'   Checks your [FGDC](https://www.fgdc.gov/metadata/csdgm/)-compatible geospatial metadata record using
#'   [mp (metadata parser)](https://geology.usgs.gov/tools/metadata/tools/doc/mp.html).
#'   Requires the `mp` executable is on your path.
#'   [Download](https://geology.usgs.gov/tools/metadata/#download) and install the software if not found.
#'
#' @param file 'character' string.
#'   Path to a XML formatted metadata file that contains the information for a USGS data release.
#' @param destdir 'character' string.
#'   Destination directory to write files, with tilde-expansion performed.
#'   Defaults to the `file` directory.
#' @param opts 'character' vector.
#'   Output option codes.
#'   Choose from one or more of the following codes:
#'     `e` direct syntax errors to file,
#'     `t` write indented ASCII text file,
#'     `h` write outline-style HTML file,
#'     `f` write FAQ-style HTML file,
#'     `s` write Standard Generalized Markup Language (SGML) file,
#'     `x` write XML file, and
#'     `d` write Directory Interchange Format (DIF) file.
#' @param error 'logical' flag.
#'   Whether to stop execution if validation errors are found (default is `FALSE`) or
#'   mp executable is not accessible.
#'
#' @return Invisibly returns a flag indicating whether validation errors were found.
#'   Returns `NA` if mp executable is not accessible.
#'
#' @author J.C. Fisher, U.S. Geological Survey, Idaho Water Science Center
#'
#' @seealso [`make_data_release`] function for creating a USGS data release.
#'
#' @export
#'
#' @keywords internal
#'
#' @examples
#' validate_metadata(
#'   file = system.file("extdata/test.xml", package = "inldata"),
#'   destdir = "validate"
#' )
#' list.files("validate")
#'
#' unlink("validate", recursive = TRUE)

validate_metadata <- function(file,
                              destdir = NULL,
                              opts = c("e", "h", "f"),
                              error = FALSE) {

  # check that the 'mp' executable is accessible
  is <- Sys.which("mp") |> checkmate::test_file_exists(access = "x")
  if (!is) {
    txt <- "Unable to validate metadata because 'mp' executable is not accessible."
    if (error) {
      stop(txt, call. = FALSE)
    } else {
      message(txt)
      return(invisible(NA))
    }
  }

  # define file extensions for each option
  exts <- c(
    "e" = "err",
    "t" = "txt",
    "h" = "html",
    "f" = "faq.html",
    "s" = "sgml",
    "x" = "xml",
    "d" = "dif"
  )

  # check arguments
  checkmate::assert_string(file)
  file <- path.expand(file) |> normalizePath(winslash = "/", mustWork = FALSE)
  checkmate::assert_file_exists(file, access = "r", extension = "xml")
  checkmate::assert_string(destdir, null.ok = TRUE)
  checkmate::assert_character(opts, n.chars = 1, min.len = 1, null.ok = TRUE)
  checkmate::assert_subset(opts, choices = names(exts))
  checkmate::assert_flag(error)

  # set destination directory
  if (is.null(destdir)) {
    destdir <- dirname(file)
  } else {
    destdir <- path.expand(destdir) |> normalizePath(winslash = "/", mustWork = FALSE)
    dir.create(destdir, showWarnings = FALSE, recursive = TRUE)
    checkmate::assert_directory_exists(destdir, access = "rw")
  }

  # get identifier
  id <- basename(file) |> tools::file_path_sans_ext()

  # account for no error-file option
  if (!("e" %in% opts)) {
    opts <- c("e", opts)
    path <- file.path(destdir, paste(id, exts["e"], sep = "."), sep = "/")
    on.exit(unlink(path))
  }

  # make arguments
  f <- paste(destdir, paste(id, exts[opts], sep = "."), sep = "/")
  names(f) <- opts
  o <- sprintf("-%s %s", opts, f) |> paste(collapse = " ")
  args <- paste(file, o)

  # run validation
  system2(command = "mp", args = args, stdout = FALSE, stderr = "")

  # read errors
  txt <- readLines(con = f["e"])

  # print message
  paste(txt, collapse = "\n") |> message()

  # stop for errors
  is <- txt[length(txt)] != "No errors"
  if (error && is) {
    stop("Metadata validation failed", call. = FALSE)
  }

  invisible(is)
}
