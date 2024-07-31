#' Parse Station Names
#'
#' @description Convert station names to common site names.
#'
#' @param x 'character' vector.
#'   Station names, such as `03N 29E 01DBB1    USGS 98`.
#'
#' @return A vector of common site names parsed from `x`.
#'
#' @author J.C. Fisher, U.S. Geological Survey, Idaho Water Science Center
#'
#' @export
#'
#' @keywords internal
#'
#' @examples
#' x <- c(
#'   "02N 30E 08DAD1               BADGING FACILITY",
#'   "03N 29E 33CCC1 MIDDLE 2051 PORT6 ZONE6 826.8FT",
#'   "MUD LAKE NR TERRETON ID"
#' )
#' parse_station_nm(x)

parse_station_nm <- function(x) {
  checkmate::assert_character(x, any.missing = FALSE)
  is <- grepl(
    pattern = "^[0-9]+[A-Za-z]\\s[0-9]+[A-Za-z]\\s[A-Za-z0-9]+\\s",
    x = x
  )
  x[is] <- substr(x[is],
    start = 15,
    stop = nchar(x[is])
  )
  is <- grepl(" PORT", x) & grepl(" ZONE", x)
  x[is] <- substr(x[is],
    start = 1,
    stop = regexpr(" PORT", x[is]) - 1L
  )
  trimws(x)
}


#' Trim Station Names
#'
#' @description Convert station names to common site names.
#'
#' @param x 'character' vector.
#'   Station names, such as `03N 29E 01DBB1    USGS 98`.
#'
#' @return A vector of common site names parsed from `x`.
#'
#' @author J.C. Fisher, U.S. Geological Survey, Idaho Water Science Center
#'
#' @export
#'
#' @keywords internal
#'
#' @examples
#' x <- c(
#'   "03N 29E 12DDB1    FIRE STA 2",
#'   "03N 29E 24CCA1 MIDDLE 2050A PORT15 ZONE15 516.8FT"
#' )
#' trim_station_nm(x)

trim_station_nm <- function(x) {
  checkmate::assert_character(x, any.missing = FALSE)
  strsplit(x, split = "  ") |>
    vapply(
      FUN = function(e) {
        e[nchar(e) > 0] |> trimws() |> paste(collapse = " ")
      },
      FUN.VALUE = character(1)
    )
}


#' Round Numbers
#'
#' @description Rounds the values in its first argument to the specified number of decimal places.
#'   This function uses the U.S. Geological Survey rounding method.
#'
#' @param x 'numeric' vector.
#'   Value to be rounded.
#' @param digits 'integer' vector or value.
#'   Number of decimal places to use (default 0).
#'   Values are recycled to match the vector length of `x`.
#'
#' @return A numeric vector of rounded values.
#'
#' @author J.C. Fisher, U.S. Geological Survey, Idaho Water Science Center
#'
#' @export
#'
#' @keywords internal
#'
#' @examples
#' round_numbers(x = rep(pi, 3), digits = c(1, 2, 3))

round_numbers <- function(x, digits = 0) {
  checkmate::assert_numeric(x)
  checkmate::assert_integerish(digits, max.len = length(x))
  z <- abs(x) * 10^digits + 0.5 + sqrt(.Machine$double.eps)
  trunc(z) / 10^digits * sign(x)
}


#' Concatenate Character Vectors
#'
#' @description Concatenate character vectors and omit empty strings.
#'
#' @param ... 'character' vectors (or objects coercible to character vectors).
#'   Corresponding elements are to be concatenated.
#' @param collapse 'character' string.
#'   Seperates the results.
#' @param recycle0 'logical' flag.
#'   Whether a zero-length character argument should lead to a
#'   zero-length character (`character(0)`) being returned.
#'
#' @return Returns a character vector.
#'
#' @author J.C. Fisher, U.S. Geological Survey, Idaho Water Science Center
#'
#' @export
#'
#' @keywords internal
#'
#' @examples
#' paste_strings(letters, c(), c(1, 2, 3))
#'
#' paste_strings(letters, c(), c(1, 2, 3), recycle0 = TRUE)

paste_strings <- function(..., collapse = " ", recycle0 = FALSE) {
  check_package(pkg = "stringi", msg = "Concatenating character vectors")
  checkmate::assert_string(collapse)
  checkmate::assert_flag(recycle0)
  dots <- list(...) |> lapply(FUN = as.character)
  lens <- lengths(dots)
  if (recycle0 && 0L %in% lens) {
    return(character(0))
  }
  lapply(dots, rep_len, length.out = max(lens)) |>
    as.data.frame() |>
    apply(
      MARGIN = 1,
      FUN = stringi::stri_flatten,
      collapse = collapse,
      na_empty = TRUE,
      omit_empty = TRUE
    )
}


#' Check Package
#'
#' @description Check that a package is available.
#'
#' @param pkg 'character' string.
#'   Package name.
#' @param msg 'character' string.
#'   Action package is used for.
#' @param call. 'logical' flag.
#'   Whether the function should be part of the error message.
#'
#' @return Stops execution if the package is missing from the name space.
#'
#' @author J.C. Fisher, U.S. Geological Survey, Idaho Water Science Center
#'
#' @export
#'
#' @keywords internal
#'
#' @examples
#' check_package(pkg = "inldata", msg = "Dataset access")

check_package <- function(pkg, msg = NULL, call. = FALSE) {
  checkmate::assert_string(pkg)
  checkmate::assert_string(msg, null.ok = TRUE)
  checkmate::assert_flag(call.)
  if (!requireNamespace(pkg, quietly = TRUE)) {
    s <- sprintf("requires the '%s' package", pkg)
    if (!is.null(msg)) {
      s <- paste(msg, s)
    }
    stop(s, call. = call.)
  }
}


#' Extract File Size
#'
#' @description Extract size on the user's file system.
#'
#' @param paths 'character' vector.
#'   File paths.
#'
#' @return Formatted file size(s).
#'
#' @author J.C. Fisher, U.S. Geological Survey, Idaho Water Science Center
#'
#' @export
#'
#' @keywords internal
#'
#' @examples
#' system.file("CITATION", package = "inldata") |>
#'   get_file_size()

get_file_size <- function(paths) {
  checkmate::assert_character(paths, min.chars = 1, any.missing = FALSE, min.len = 1)
  file_sizes <- file.size(paths) # in bytes
  vapply(file_sizes,
    FUN = function(x) {
      if (is.na(x)) return(NA_character_)

      # code adapted from Rohit Jain's Stack Overflow post at
      # https://stackoverflow.com/questions/13539871
      # accessed on 2024-04-08,
      # licensed under Creative Commons Attribute-ShareAlike license, version 3.0
      k <- x / 1024
      m <- x / 1024^2
      g <- x / 1024^3
      t <- x / 1024^4
      if (t > 1) {
        t |> round(digits = 2) |> paste("TB") # terabytes
      } else if (g > 1) {
        g |> round(digits = 2) |> paste("GB") # gigabytes
      } else if (m > 1) {
        m |> round(digits = 2) |> paste("MB") # megabytes
      } else if (k > 1) {
        k |> round(digits = 2) |> paste("KB") # kilobytes
      } else {
        x |> round(digits = 2) |> paste("B") # bytes
      }

    },
    FUN.VALUE = character(1)
  )
}


#' Test Location is Package Directory
#'
#' @description Test whether the working directory is located at the top level directory of the package.
#'
#' @param pkg 'character' string.
#'   Package name.
#'
#' @return Formatted file size(s).
#'
#' @author J.C. Fisher, U.S. Geological Survey, Idaho Water Science Center
#'
#' @export
#'
#' @keywords internal
#'
#' @examples
#' test_pkg_dir("inldata")

test_pkg_dir <- function(pkg) {
  checkmate::assert_string(pkg)
  if (checkmate::test_file_exists("DESCRIPTION", access = "r")) {
    package <- read.dcf("DESCRIPTION", fields = "Package") |> as.character()
    if (identical(package, pkg)) {
      if (checkmate::test_directory_exists("inst", access = "r")) {
        return(TRUE)
      }
    }
  }
  FALSE
}


#' Date-Time Conversion
#'
#' @description Convert calendar date and times.
#'
#' @param dt 'character' vector.
#'   Calendar date formatted as YYYY-MM-DD.
#' @param tm 'character' vector.
#'   Time in Greenwich Mean Time (UTC) formatted as HH:MM.
#'   Vector length equal to the length of `dt`.
#' @param tm_unset 'character' string.
#'   Value if time is missing, noon by default.
#' @param tz 'character' string.
#'   Time zone specification to convert to.
#'
#' @return Returns an object of class 'POSIXct'.
#'
#' @author J.C. Fisher, U.S. Geological Survey, Idaho Water Science Center
#'
#' @export
#'
#' @keywords internal
#'
#' @examples
#' as_posix_ct(
#'   dt = c("2024-01-01", "2024-02-15", NA),
#'   tm = c("14:30", NA, "11:11"),
#'   tz = "America/Denver"
#' )

as_posix_ct <- function(dt, tm, tm_unset = "12:00", tz = "") {
  checkmate::assert_character(dt, min.len = 1)
  checkmate::assert_character(tm, len = length(dt))
  checkmate::assert_string(tm_unset, n.chars = 5)
  time <- tm
  is <- is.na(time)
  time[is] <- tm_unset
  paste(dt, time) |>
    as.POSIXct(tz = "GMT", format = "%Y-%m-%d %H:%M") |>
    format(tz = tz) |>
    as.POSIXct(tz = tz)
}


#' Extract Archive Contents
#'
#' @description Extract contents of an archive to a directory.
#'   Requires that the \pkg{archive} package is available.
#'
#' @param file 'character' string.
#'   File path to the archive.
#' @param destdir 'character' string.
#'   Destination directory to extract files to.
#'   It will be created if necessary.
#'   Defaults to the temporary directory.
#'
#' @return Invisibly returns the extracted path(s).
#'
#' @author J.C. Fisher, U.S. Geological Survey, Idaho Water Science Center
#'
#' @export
#'
#' @keywords internal
#'
#' @examples
#' files <- system.file("extdata", "test.zip", package = "inldata") |>
#'   extract_archive()
#'
#' unlink(files)

extract_archive <- function(file, destdir = tempdir()) {

  # check arguments
  file <- path.expand(file) |>
    normalizePath(winslash = "/", mustWork = FALSE)
  checkmate::assert_file_exists(file, access = "r")
  destdir <- path.expand(destdir) |>
    normalizePath(winslash = "/", mustWork = FALSE)
  dir.create(destdir, showWarnings = FALSE, recursive = TRUE)
  checkmate::assert_directory_exists(destdir, access = "rw")

  # check packages
  check_package(pkg = "archive", msg = "Extracting contents of an archive")

  # extract contents
  files <- archive::archive_extract(archive = file, dir = destdir)

  # get paths
  paths <- file.path(destdir, files, fsep = "/")

  invisible(paths)
}


#' Write Lines to a File
#'
#' @description Write text lines to a file.
#'
#' @param text 'character' vector.
#'   Text to write to file.
#' @param path 'character' string.
#'   Path to the file.
#' @param gz 'logical' flag.
#'   Whether to compress the file using Gzip.
#'   The `.gz` extension is added to the file `path`.
#'
#' @return Invisibly returns the extracted path(s).
#'
#' @author J.C. Fisher, U.S. Geological Survey, Idaho Water Science Center
#'
#' @export
#'
#' @keywords internal
#'
#' @examples
#' path <- write_lines(
#'   text = "Test",
#'   path = tempfile(fileext = ".txt"),
#'   gz = TRUE
#' )
#'
#' unlink(path)

write_lines <- function(text, path, gz = FALSE) {
  checkmate::assert_character(text)
  checkmate::assert_path_for_output(path, overwrite = TRUE)
  checkmate::assert_flag(gz)
  if (gz) {
    path <- paste0(path, ".gz")
    con <- gzfile(path, open = "w", encoding = "UTF-8")
    on.exit(close(con))
    writeLines(text, con = con, useBytes = TRUE)
  } else {
    writeLines(text, con = path, useBytes = TRUE)
  }
  invisible(path)
}


#' Extract File Extension
#'
#' @description Extract the extension of a file.
#'
#' @param x 'character' vector.
#'   File path(s).
#' @param compression 'logical' flag.
#'   Whether to account for the compression extension '.gz', '.bz2' or '.xz'.
#'
#' @return Returns the file (name) extensions (excluding the leading dot).
#'
#' @author J.C. Fisher, U.S. Geological Survey, Idaho Water Science Center
#'
#' @export
#'
#' @keywords internal
#'
#' @examples
#' c("dir/file.txt", "file.txt.gz") |>
#'   get_file_ext()

get_file_ext <- function(x, compression = TRUE) {
  checkmate::assert_character(x, any.missing = FALSE, min.len = 1)
  checkmate::assert_flag(compression)
  base_names <- basename(x)
  patterns <- sprintf("^%s.",
    tools::file_path_sans_ext(base_names, compression = compression)
  )
  vapply(seq_along(x),
    FUN = function(i) {
      sub(pattern = patterns[i], replacement = "", x = base_names[i])
    },
    FUN.VALUE = character(1)
  )
}


#' Remove NULL List Elements
#'
#' @description Removes elements from a list that are equal to `NULL`.
#'
#' @param x 'list'.
#'
#' @return List
#'
#' @author J.C. Fisher, U.S. Geological Survey, Idaho Water Science Center
#'
#' @export
#'
#' @keywords internal
#'
#' @examples
#' d <- list("a", "b", NULL, "c")
#' d
#' d <- ccp(d)
#' d

ccp <- function(x) {
  checkmate::assert_vector(x, null.ok = TRUE)
  Filter(Negate(is.null), x)
}
