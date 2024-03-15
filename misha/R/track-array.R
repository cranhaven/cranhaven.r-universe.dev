.gslice <- function(trackstr, slice) {
    res <- list()
    colnames <- .gtrack.array.get_colnames(trackstr)

    if (is.null(slice)) {
        res$slice <- NULL
        res$colnames <- names(colnames)
    } else if (typeof(slice) == "character") {
        slice <- unique(slice)
        res$slice <- colnames[slice]
        res$colnames <- names(res$slice)

        idx <- match(NA, res$slice)
        if (!is.na(idx)) {
            stop(sprintf("%s does not appear among the column names of track %s", slice[idx], trackstr), call. = FALSE)
        }
    } else if (is.numeric(slice) || is.integer(slice)) {
        if (TRUE %in% (as.integer(slice) != slice)) {
            stop("Invalid type of slice parameter", call. = FALSE)
        }

        slice <- unique(slice)
        slice <- as.integer(slice)

        outofrange <- slice < 1 | slice > length(colnames)
        if (TRUE %in% outofrange) {
            stop(sprintf("Slice index %d is out of range", slice[match(TRUE, outofrange)], trackstr), call. = FALSE)
        }

        res$slice <- colnames[slice]
        res$colnames <- names(res$slice)
    } else {
        stop("Invalid type of slice parameter", call. = FALSE)
    }

    res
}

# .gtrack.create_test_arrays <- function(track, minsize, maxsize, intervals = get("ALLGENOME", envir = .misha), iterator = NULL) {
#     if (is.null(substitute(track))) {
#         stop("Usage: .gtrack.create_test_arrays(track, expr, iterator = NULL, band = NULL)", call. = FALSE)
#     }
#     .gcheckroot()

#     trackstr <- do.call(.gexpr2str, list(substitute(track)), envir = parent.frame())
#     .iterator <- do.call(.giterator, list(substitute(iterator)), envir = parent.frame())
#     trackdir <- sprintf("%s.track", paste(get("GWD", envir = .misha), gsub("\\.", "/", trackstr), sep = "/"))

#     direxisted <- file.exists(trackdir)

#     if (!is.na(match(trackstr, get("GTRACKS", envir = .misha)))) {
#         stop(sprintf("Track %s already exists", trackstr), call. = FALSE)
#     }

#     .gconfirmtrackcreate(trackstr)
#     success <- FALSE
#     tryCatch(
#         {
#             colnames <- .gcall("gcreate_arrays_track", trackstr, minsize, maxsize, "1", intervals, .iterator, .misha_env(), silent = TRUE)
#             .gdb.add_track(trackstr)
#             .gtrack.array.set_colnames(trackstr, colnames, FALSE)
#             .gtrack.attr.set(trackstr, "created.by", ".gtrack.create_test_arrays", TRUE)
#             .gtrack.attr.set(trackstr, "created.date", date(), TRUE)
#             success <- TRUE
#         },
#         finally = {
#             if (!success && !direxisted) {
#                 unlink(trackdir, recursive = TRUE)
#                 .gdb.rm_track(trackstr)
#             }
#         }
#     )
#     retv <- 0 # suppress return value
# }


.gtrack.array.get_colnames <- function(trackstr) {
    .gcheckroot()

    if (is.na(match(trackstr, get("GTRACKS", envir = .misha)))) {
        stop(sprintf("Track %s does not exist", trackstr), call. = FALSE)
    }

    if (.gcall_noninteractive(gtrack.info, trackstr)$type != "array") {
        stop("gtrack.array.get_colnames can only be applied to array tracks", call. = FALSE)
    }

    trackdir <- sprintf("%s.track", paste(get("GWD", envir = .misha), gsub("\\.", "/", trackstr), sep = "/"))
    filename <- paste(trackdir, ".colnames", sep = "/")

    if (!file.exists(filename)) {
        stop(sprintf("File %s does not exist", filename))
    }

    f <- file(filename, "rb")
    colnames <- unserialize(f)
    close(f)
    colnames
}

.gtrack.array.set_colnames <- function(trackstr, names, check_num_cols) {
    .gcheckroot()

    if (is.na(match(trackstr, get("GTRACKS", envir = .misha)))) {
        stop(sprintf("Track %s does not exist", trackstr), call. = FALSE)
    }

    if (typeof(names) != "character") {
        stop(sprintf("names parameter must be a character vector", trackstr), call. = FALSE)
    }

    if (.gcall_noninteractive(gtrack.info, trackstr)$type != "array") {
        stop("gtrack.array.set_colnames can only be applied to array tracks", call. = FALSE)
    }

    if ("" %in% names) {
        stop(sprintf("Column names cannot be empty", duplicated[1]), call. = FALSE)
    }

    duplicated <- names[duplicated(names)]
    if (length(duplicated)) {
        stop(sprintf("Column %s appears more than once", duplicated[1]), call. = FALSE)
    }

    if (check_num_cols) {
        oldnames <- .gtrack.array.get_colnames(trackstr)
        if (length(oldnames) != length(names)) {
            stop(sprintf(
                "The number of columns in the track (%d) does not match the number of column names (%d)",
                length(oldnames), length(names)
            ), call. = FALSE)
        }
    }

    colnames <- as.integer(1:length(names))
    names(colnames) <- names

    trackdir <- sprintf("%s.track", paste(get("GWD", envir = .misha), gsub("\\.", "/", trackstr), sep = "/"))
    filename <- paste(trackdir, ".colnames", sep = "/")
    f <- file(filename, "wb")
    serialize(colnames, f)
    close(f)
}




#' Returns values from 'Array' track
#'
#' Returns values from 'Array' track.
#'
#' This function returns the column values of an 'Array' track in the genomic
#' scope specified by 'intervals'. 'slice' parameter determines which columns
#' should appear in the result. The columns can be indicated by their names or
#' their indices. If 'slice' is 'NULL' the values of all track columns are
#' returned.
#'
#' The order inside the result might not be the same as the order of intervals.
#' An additional column 'intervalID' is added to the return value. Use this
#' column to refer to the index of the original interval from the supplied
#' 'intervals'.
#'
#' If 'file' parameter is not 'NULL' the result is saved to a tab-delimited
#' text file (without 'intervalID' column) rather than returned to the user.
#' This can be especially useful when the result is too big to fit into the
#' physical memory.  The resulted file can be used as an input for
#' 'gtrack.array.import' function.
#'
#' If 'intervals.set.out' is not 'NULL' the result is saved as an intervals
#' set. Similarly to 'file' parameter 'intervals.set.out' can be useful to
#' overcome the limits of the physical memory.
#'
#' @param track track name
#' @param slice a vector of column names or column indices or 'NULL'
#' @param intervals genomic scope for which the function is applied
#' @param file file name where the function result is to be saved. If 'NULL'
#' result is returned to the user.
#' @param intervals.set.out intervals set name where the function result is
#' optionally outputted
#' @return If 'file' and 'intervals.set.out' are 'NULL' a set of intervals with
#' additional columns for 'Array' track column values and 'columnID'.
#' @seealso \code{\link{gextract}}, \code{\link{gtrack.array.get_colnames}},
#' \code{\link{gtrack.array.import}}
#' @keywords ~extract ~array
#' @examples
#' \dontshow{
#' options(gmax.processes = 2)
#' }
#'
#' gdb.init_examples()
#' gtrack.array.extract(
#'     "array_track", c("col3", "col5"),
#'     gintervals(1, 0, 2000)
#' )
#'
#' @export gtrack.array.extract
gtrack.array.extract <- function(track = NULL, slice = NULL, intervals = NULL, file = NULL, intervals.set.out = NULL) {
    if (is.null(substitute(track)) || is.null(intervals)) {
        stop("Usage: gtrack.array.extract(track, slice, intervals, file = NULL, intervals.set.out = NULL)", call. = FALSE)
    }
    .gcheckroot()

    intervals <- rescue_ALLGENOME(intervals, as.character(substitute(intervals)))

    trackstr <- do.call(.gexpr2str, list(substitute(track)), envir = parent.frame())
    slice <- .gslice(trackstr, slice)

    intervals.set.out <- do.call(.gexpr2str, list(substitute(intervals.set.out)), envir = parent.frame())
    if (!is.null(intervals.set.out)) {
        fullpath <- .gintervals.check_new_set(intervals.set.out)
    }

    # intervals can be NULL if the function is piped with gscreen and the latter returns NULL
    success <- FALSE
    res <- NULL
    tryCatch(
        {
            if (!is.null(intervals)) {
                res <- .gcall("garrayextract", trackstr, slice$slice, slice$colnames, file, intervals, intervals.set.out, .misha_env())

                if (!is.null(intervals.set.out) && .gintervals.is_bigset(intervals.set.out, FALSE) && !.gintervals.needs_bigset(intervals.set.out)) {
                    .gintervals.big2small(intervals.set.out)
                }
            }

            success <- TRUE
        },
        finally = {
            if (!success && !is.null(intervals.set.out)) {
                unlink(fullpath, recursive = TRUE)
            }
        }
    )

    # refresh the list of GINTERVS, etc.
    if (!is.null(intervals.set.out)) {
        .gdb.add_intervals.set(intervals.set.out)
        retv <- 0 # suppress return value
    } else if (!is.null(file)) {
        retv <- 0
    } # suppress return value
    else {
        res
    }
}



#' Returns column names of array track
#'
#' Returns column names of array track.
#'
#' This function returns the column names of an array track.
#'
#' @param track track name
#' @return A character vector with column names.
#' @seealso \code{\link{gtrack.array.set_colnames}},
#' \code{\link{gtrack.array.extract}}, \code{\link{gvtrack.array.slice}},
#' \code{\link{gtrack.info}}
#' @keywords ~array ~columns
#' @examples
#' \dontshow{
#' options(gmax.processes = 2)
#' }
#'
#' gtrack.array.get_colnames("array_track")
#'
#' @export gtrack.array.get_colnames
gtrack.array.get_colnames <- function(track = NULL) {
    if (is.null(substitute(track))) {
        stop("Usage: gtrack.array.get_colnames(track)", call. = FALSE)
    }

    trackstr <- do.call(.gexpr2str, list(substitute(track)), envir = parent.frame())
    names(.gtrack.array.get_colnames(trackstr))
}



#' Creates an array track from array tracks or files
#'
#' Creates an array track from array tracks or files.
#'
#' This function creates a new 'Array' track from one or more "sources". Each
#' source can be either another 'Array' track or a tab-delimited file that
#' contains one-dimensional intervals and column values that should be added to
#' the newly created track. One can learn about the exact format of the file by
#' running 'gtrack.array.extract' or 'gextract' functions with a 'file'
#' parameter and inspecting the output file.
#'
#' There might be more than one source used to create the new track. In that
#' case the new track will contain the columns from all the sources. The
#' equally named columns are merged. Intervals that appear in one source but
#' not in the other are added and the values for the missing columns are set to
#' NaN. Intervals with all NaN values are not added. Partial overlaps between
#' two intervals from different sources are forbidden.
#'
#' 'description' is added as a track attribute.
#'
#' @param track name of the newly created track
#' @param description a character string description
#' @param ... array track or name of a tab-delimited file
#' @return None.
#' @seealso \code{\link{gextract}}, \code{\link{gtrack.array.extract}},
#' \code{\link{gtrack.array.set_colnames}}, \code{\link{gtrack.rm}},
#' \code{\link{gtrack.info}}, \code{\link{gdir.create}}
#' @keywords ~array ~import ~create ~track
#' @examples
#' \dontshow{
#' options(gmax.processes = 2)
#' }
#'
#' f1 <- tempfile()
#' gextract("sparse_track", gintervals(1, 5000, 20000), file = f1)
#' f2 <- tempfile()
#' gtrack.array.extract("array_track", c("col2", "col3", "col4"),
#'     gintervals(1, 0, 20000),
#'     file = f2
#' )
#' f3 <- tempfile()
#' gtrack.array.extract("array_track", c("col1", "col3"),
#'     gintervals(1, 0, 20000),
#'     file = f3
#' )
#'
#' gtrack.array.import("test_track1", "Test array track 1", f1, f2)
#' gtrack.array.extract("test_track1", NULL, .misha$ALLGENOME)
#'
#' gtrack.array.import(
#'     "test_track2", "Test array track 2",
#'     "test_track1", f3
#' )
#' gtrack.array.extract("test_track2", NULL, .misha$ALLGENOME)
#'
#' gtrack.rm("test_track1", TRUE)
#' gtrack.rm("test_track2", TRUE)
#' unlink(c(f1, f2, f3))
#'
#' @export gtrack.array.import
gtrack.array.import <- function(track = NULL, description = NULL, ...) {
    args <- as.list(substitute(list(...)))[-1L]
    if (is.null(substitute(track)) || is.null(description) || !length(args)) {
        stop("Usage: gtrack.array.import(track, description, [src]+)", call. = FALSE)
    }
    .gcheckroot()

    trackstr <- do.call(.gexpr2str, list(substitute(track)), envir = parent.frame())

    srcs <- c()
    colnames <- list()
    for (src in args) {
        src <- do.call(.gexpr2str, list(src), envir = parent.frame())
        srcs <- c(srcs, src)
        if (is.na(match(src, get("GTRACKS", envir = .misha)))) {
            colnames[[length(colnames) + 1]] <- as.character(NULL)
        } else {
            if (.gcall_noninteractive(gtrack.info, src)$type != "array") {
                stop(sprintf("Track %s: only array tracks can be used as a source", src), call. = FALSE)
            }
            colnames[[length(colnames) + 1]] <- names(.gtrack.array.get_colnames(src))
        }
    }

    trackdir <- sprintf("%s.track", paste(get("GWD", envir = .misha), gsub("\\.", "/", trackstr), sep = "/"))

    direxisted <- file.exists(trackdir)

    if (!is.na(match(trackstr, get("GTRACKS", envir = .misha)))) {
        stop(sprintf("Track %s already exists", trackstr), call. = FALSE)
    }

    .gconfirmtrackcreate(trackstr)
    success <- FALSE
    tryCatch(
        {
            colnames <- .gcall("garrays_import", trackstr, srcs, colnames, .misha_env(), silent = TRUE)
            .gdb.add_track(trackstr)
            .gtrack.array.set_colnames(trackstr, colnames, FALSE)
            created.by <- sprintf("gtrack.array.import(\"%s\", description, src = c(\"%s\"))", trackstr, paste(srcs, collapse = "\", \""))
            .gtrack.attr.set(trackstr, "created.by", created.by, TRUE)
            .gtrack.attr.set(trackstr, "created.date", date(), TRUE)
            .gtrack.attr.set(trackstr, "description", description, TRUE)
            success <- TRUE
        },
        finally = {
            if (!success && !direxisted) {
                unlink(trackdir, recursive = TRUE)
                .gdb.rm_track(trackstr)
            }
        }
    )
    retv <- 0 # suppress return value
}



#' Sets column names of array track
#'
#' Sets column names of array track.
#'
#' This sets the column names of an array track.
#'
#' @param track track name
#' @param names vector of column names
#' @return None.
#' @seealso \code{\link{gtrack.array.get_colnames}},
#' \code{\link{gtrack.array.extract}}, \code{\link{gvtrack.array.slice}},
#' \code{\link{gtrack.info}}
#' @keywords ~array ~columns
#' @examples
#' \dontshow{
#' options(gmax.processes = 2)
#' }
#'
#' old.names <- gtrack.array.get_colnames("array_track")
#' new.names <- paste("modified", old.names, sep = "_")
#' gtrack.array.set_colnames("array_track", new.names)
#' gtrack.array.get_colnames("array_track")
#' gtrack.array.set_colnames("array_track", old.names)
#' gtrack.array.get_colnames("array_track")
#'
#' @export gtrack.array.set_colnames
gtrack.array.set_colnames <- function(track = NULL, names = NULL) {
    if (is.null(substitute(track)) || is.null(names)) {
        stop("Usage: gtrack.array.set_colnames(track, names)", call. = FALSE)
    }

    trackstr <- do.call(.gexpr2str, list(substitute(track)), envir = parent.frame())
    .gtrack.array.set_colnames(trackstr, names, TRUE)
}
