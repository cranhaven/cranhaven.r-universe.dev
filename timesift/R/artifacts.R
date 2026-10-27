# The three artifacts that cross the language boundary, in the format
# inst/spec/representation.md defines: CSV, UTF-8, a header row, no quoting, LF line endings on
# every platform, and numbers at twelve significant digits. The Python side writes the same bytes
# from the same artifact, which is what makes a round trip through a file checkable rather than
# assumed.

#' Read and write the artifacts that cross the language boundary
#'
#' The representation is built twice, once per language, from one shared core. The response
#' matrix, the fold map and the mask of scorable cells are not: they are built once and read
#' wherever they are needed, because a fold map drawn from a seed in R and one drawn from the same
#' seed in Python are different maps, and aligning the two random streams would be the wrong fix.
#'
#' These six functions are the handover. The Python side carries `write_folds()` and its siblings
#' under the same names; they write the same bytes from the same artifact and read the same files,
#' so a fold map built in either language is usable in the other without the caller knowing the
#' format.
#'
#' The format is normative and is given in `inst/spec/representation.md`: CSV, UTF-8, a header
#' row, no quoting, LF line endings on every platform, numbers at twelve significant digits, and
#' rows ordered by the identifier under C collation.
#'
#' @param x A fold map from [fold_map()], or any named integer vector.
#' @param y A response matrix, as [scorable_cells()] takes one.
#' @param cells A mask from [scorable_cells()].
#' @param file Path to write to or read from.
#' @param units The units to align to, in the order they are wanted. A unit the file has no row
#'   for is an error; a unit the file carries beyond these is dropped. `NULL`, the default, returns
#'   the file's own rows in the file's own order.
#'
#' @return The writers return `file`, invisibly. `read_folds()` returns a named integer vector of
#'   class `timesift_folds`, `read_response()` a numeric matrix with the units in its row names,
#'   and `read_cells()` a `timesift_cells` data frame.
#'
#' @examples
#' set.seed(1)
#' y <- matrix(rbinom(120, 1, 0.3), nrow = 20,
#'             dimnames = list(sprintf("p%02d", 1:20), paste0("sp", 1:6)))
#' f <- fold_map(y, v = 4)
#'
#' path <- tempfile(fileext = ".csv")
#' write_folds(f, path)
#' identical(read_folds(path, names(f)), f[names(f)])
#' unlink(path)
#'
#' @name artifacts
NULL

#' @rdname artifacts
#' @export
write_folds <- function(x, file) {
  units <- names(x)
  if (is.null(units)) {
    stop("a fold map is written by unit, so it needs names.", call. = FALSE)
  }
  o <- order(units, method = "radix")
  .write_csv(c("id", "fold"), list(units[o], .num(as.integer(x)[o])), file)
}

#' @rdname artifacts
#' @export
read_folds <- function(file, units = NULL) {
  d <- .read_csv(file, c("id", "fold"))
  fold <- suppressWarnings(as.integer(d$fold))
  if (anyNA(fold)) {
    stop("the fold column of ", basename(file), " holds a value that is not a whole number.",
         call. = FALSE)
  }
  duplicated_at <- anyDuplicated(d$id)
  if (duplicated_at) {
    stop("the fold map names ", d$id[duplicated_at], " more than once.", call. = FALSE)
  }
  out <- stats::setNames(fold, d$id)
  if (!is.null(units)) {
    out <- out[.align_units(names(out), units, "fold map")]
  }
  structure(out, v = length(unique(out)), class = "timesift_folds")
}

#' @rdname artifacts
#' @export
write_response <- function(y, file) {
  y <- .as_response(y)
  o <- order(rownames(y), method = "radix")
  .write_csv(c("id", colnames(y)),
             c(list(rownames(y)[o]), lapply(seq_len(ncol(y)), function(j) .num(y[o, j]))), file)
}

#' @rdname artifacts
#' @export
read_response <- function(file, units = NULL) {
  d <- .read_csv(file, "id")
  vars <- setdiff(names(d), "id")
  if (!length(vars)) {
    stop(basename(file), " holds an id column and no variable.", call. = FALSE)
  }
  out <- matrix(suppressWarnings(as.numeric(unlist(d[vars], use.names = FALSE))),
                nrow = length(d$id), dimnames = list(d$id, vars))
  if (anyNA(out)) {
    stop(basename(file), " holds a value that is not a number.", call. = FALSE)
  }
  if (is.null(units)) out else out[.align_units(rownames(out), units, "response"), , drop = FALSE]
}

#' @rdname artifacts
#' @export
write_cells <- function(cells, file) {
  missing <- setdiff(.cell_columns, names(cells))
  if (length(missing)) {
    stop("a scorable mask needs ", paste(missing, collapse = ", "), ".", call. = FALSE)
  }
  o <- order(cells$variable, cells$fold, method = "radix")
  columns <- lapply(.cell_columns, function(nm) {
    v <- cells[[nm]][o]
    if (is.logical(v)) {
      ifelse(v, "TRUE", "FALSE")
    } else if (is.character(v)) {
      v
    } else {
      .num(v)
    }
  })
  .write_csv(.cell_columns, columns, file)
}

#' @rdname artifacts
#' @export
read_cells <- function(file) {
  d <- .read_csv(file, .cell_columns)
  out <- data.frame(variable = d$variable, stringsAsFactors = FALSE)
  for (nm in .cell_columns[2:7]) {
    out[[nm]] <- as.integer(d[[nm]])
  }
  out$scorable <- .as_logical(d$scorable, file)
  rownames(out) <- NULL
  structure(out[.cell_columns], class = c("timesift_cells", "data.frame"))
}

.cell_columns <- c("variable", "fold", "n_occ", "pres_train", "abs_train", "pres_test",
                   "abs_test", "scorable")

# ---- the format itself -------------------------------------------------------------------------

# Twelve significant digits, which renders 0 and 1 as 0 and 1 and carries any measurement a
# response holds. Python's format() and R's sprintf() hand %.12g to the same C library.
.num <- function(v) sprintf("%.12g", as.numeric(v))

# write.csv() would do most of this, and would also emit CRLF on Windows, which makes the bytes of
# an artifact depend on the machine that wrote it. The bytes are the point, so they are written.
.write_csv <- function(header, columns, file) {
  body <- paste0(paste(header, collapse = ","), "\n")
  if (length(columns[[1L]])) {
    rows <- do.call(paste, c(columns, list(sep = ",")))
    body <- paste0(body, paste(rows, collapse = "\n"), "\n")
  }
  con <- file(file, open = "wb")
  on.exit(close(con), add = TRUE)
  writeBin(charToRaw(enc2utf8(body)), con)
  invisible(file)
}

.read_csv <- function(file, need) {
  if (!file.exists(file)) {
    stop("no file at ", file, ".", call. = FALSE)
  }
  # na.strings is emptied so a unit or a variable literally called NA reads as its own name
  # rather than as a missing value.
  d <- utils::read.csv(file, stringsAsFactors = FALSE, colClasses = "character",
                       check.names = FALSE, na.strings = character(0))
  missing <- setdiff(need, names(d))
  if (length(missing)) {
    stop(basename(file), " has no ", paste(missing, collapse = " and "), " column.", call. = FALSE)
  }
  d
}

.as_logical <- function(v, file) {
  out <- ifelse(v %in% c("TRUE", "true", "True", "1"), TRUE,
                ifelse(v %in% c("FALSE", "false", "False", "0"), FALSE, NA))
  if (anyNA(out)) {
    stop(basename(file), " holds ", v[which(is.na(out))[1L]], " where a logical was expected.",
         call. = FALSE)
  }
  out
}

# Aligning is by name and never by position. A unit the artifact does not carry is an error naming
# it; a unit it carries beyond those asked for is dropped, because reading a subset of a fold map
# covering a whole study is a normal thing to do.
.align_units <- function(have, units, what) {
  position <- match(units, have)
  if (anyNA(position)) {
    missing <- units[is.na(position)]
    stop(length(missing), " unit", if (length(missing) > 1L) "s have" else " has",
         " no row in the ", what, ", first: ", missing[1L], ".", call. = FALSE)
  }
  position
}
