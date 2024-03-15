.gtrack.create_test_computer2d <- function(track = NULL, prob.skip.chrom = 0.5, max.rect = 100000, max.rect.size = 10000) {
    if (is.null(substitute(track))) {
        stop("Usage: gtrack.create_test_computer2d(trackname, prob.skip.chrom, max.rect, max.rect.size)", call. = FALSE)
    }
    .gcheckroot()

    trackstr <- do.call(.gexpr2str, list(substitute(track)), envir = parent.frame())
    trackdir <- sprintf("%s.track", paste(get("GWD", envir = .misha), gsub("\\.", "/", trackstr), sep = "/"))

    direxisted <- file.exists(trackdir)

    if (!is.na(match(trackstr, get("GTRACKS", envir = .misha)))) {
        stop(sprintf("Track %s already exists", trackstr), call. = FALSE)
    }

    .gconfirmtrackcreate(trackstr)
    success <- FALSE
    tryCatch(
        {
            .gcall("gcreate_test_computer2d_track", trackstr, prob.skip.chrom, max.rect, max.rect.size, .misha_env(), silent = TRUE)
            .gdb.add_track(trackstr)
            .gtrack.attr.set(trackstr, "created.by", sprintf(".gtrack.create_test_computer2d(%s, %g, %g, %g)", trackstr, prob.skip.chrom, max.rect, max.rect.size), TRUE)
            .gtrack.attr.set(trackstr, "created.date", date(), TRUE)
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




#' Creates a 'Rectangles' track from intervals and values
#'
#' Creates a 'Rectangles' track from intervals and values.
#'
#' This function creates a new 'Rectangles' (two-dimensional) track with values
#' at given intervals. 'description' is added as a track attribute.
#'
#' @param track track name
#' @param description a character string description
#' @param intervals a set of two-dimensional intervals
#' @param values an array of numeric values - one for each interval
#' @return None.
#' @seealso \code{\link{gtrack.create}}, \code{\link{gtrack.create_sparse}},
#' \code{\link{gtrack.smooth}}, \code{\link{gtrack.modify}},
#' \code{\link{gtrack.rm}}, \code{\link{gtrack.info}},
#' \code{\link{gdir.create}}, \code{\link{gtrack.attr.get}}
#' @keywords ~create ~track
#' @examples
#' \dontshow{
#' options(gmax.processes = 2)
#' }
#'
#' gdb.init_examples()
#' intervs1 <- gintervals.2d(
#'     1, (1:4) * 200, (1:4) * 200 + 100,
#'     1, (1:4) * 300, (1:4) * 300 + 200
#' )
#' intervs2 <- gintervals.2d(
#'     "X", (7:10) * 100, (7:10) * 100 + 50,
#'     2, (1:4) * 200, (1:4) * 200 + 130
#' )
#' intervs <- rbind(intervs1, intervs2)
#' gtrack.2d.create(
#'     "test_rects", "Test 2d track", intervs,
#'     runif(dim(intervs)[1], 1, 100)
#' )
#' gextract("test_rects", .misha$ALLGENOME)
#' gtrack.rm("test_rects", force = TRUE)
#'
#' @export gtrack.2d.create
gtrack.2d.create <- function(track = NULL, description = NULL, intervals = NULL, values = NULL) {
    if (is.null(substitute(track)) || is.null(description) || is.null(intervals) || is.null(values)) {
        stop("Usage: gtrack.2d.create(track, description, intervals, values)", call. = FALSE)
    }
    .gcheckroot()

    intervals <- rescue_ALLGENOME(intervals, as.character(substitute(intervals)))

    trackstr <- do.call(.gexpr2str, list(substitute(track)), envir = parent.frame())
    intervalsstr <- deparse(substitute(intervals), width.cutoff = 500)[1]
    valuesstr <- deparse(substitute(values), width.cutoff = 500)[1]
    trackdir <- sprintf("%s.track", paste(get("GWD", envir = .misha), gsub("\\.", "/", trackstr), sep = "/"))

    direxisted <- file.exists(trackdir)

    if (!is.na(match(trackstr, get("GTRACKS", envir = .misha)))) {
        stop(sprintf("Track %s already exists", trackstr), call. = FALSE)
    }

    .gconfirmtrackcreate(trackstr)
    success <- FALSE
    tryCatch(
        {
            .gcall("gtrack_create_track2d", trackstr, intervals, values, .misha_env(), silent = TRUE)
            .gdb.add_track(trackstr)
            .gtrack.attr.set(trackstr, "created.by", sprintf("gtrack.2d.create(%s, description, %s, %s)", trackstr, intervalsstr, valuesstr), TRUE)
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



#' Creates a 2D track from tab-delimited file
#'
#' Creates a 2D track from tab-delimited file(s).
#'
#' This function creates a 2D track track from one or more tab-delimited files.
#' Each file must start with a header describing the columns. The first 6
#' columns must have the following names: 'chrom1', 'start1', 'end1', 'chrom2',
#' 'start2', 'end2'. The last column is designated for the value and it may
#' have an arbitrary name. The header is followed by a list of intervals and a
#' value for each interval. Overlapping intervals are forbidden.
#'
#' One can learn about the format of the tab-delimited file by running
#' 'gextract' function on a 2D track with a 'file' parameter set to the name of
#' the file.
#'
#' If all the imported intervals represent a point (i.e. end == start + 1) a
#' 'Points' track is created otherwise it is a 'Rectangles' track.
#'
#' 'description' is added as a track attribute.
#'
#' Note: temporary files are created in the directory of the track during the
#' run of the function. A few of them need to be kept simultaneously open. If
#' the number of chromosomes and / or intervals is particularly high, a few
#' thousands files might be needed to be opened simultaneously. Some operating
#' systems limit the number of open files per user, in which case the function
#' might fail with "Too many open files" or similar error. The workaround could
#' be:
#'
#' 1. Increase the limit of simultaneously opened files (the way varies
#' depending on your operating system). 2. Increase the value of
#' 'gmax.data.size' option. Higher values of 'gmax.data.size' option will
#' increased memory usage of the function but create fewer temporary files.
#'
#' @param track track name
#' @param description a character string description
#' @param file vector of file paths
#' @return None.
#' @seealso \code{\link{gtrack.rm}}, \code{\link{gtrack.info}},
#' \code{\link{gdir.create}}
#' @keywords ~track
#' @export gtrack.2d.import
gtrack.2d.import <- function(track = NULL, description = NULL, file = NULL) {
    if (is.null(substitute(track)) || is.null(description) || is.null(file)) {
        stop("Usage: gtrack.2d.import(track, description, file)", call. = FALSE)
    }

    .gcheckroot()

    trackstr <- do.call(.gexpr2str, list(substitute(track)), envir = parent.frame())
    trackdir <- sprintf("%s.track", paste(get("GWD", envir = .misha), gsub("\\.", "/", trackstr), sep = "/"))

    direxisted <- file.exists(trackdir)

    if (!is.na(match(trackstr, get("GTRACKS", envir = .misha)))) {
        stop(sprintf("Track %s already exists", trackstr), call. = FALSE)
    }

    .gconfirmtrackcreate(trackstr)
    retv <- 0
    success <- FALSE

    tryCatch(
        {
            .gcall("gtrack_2d_import", trackstr, file, .misha_env(), silent = TRUE)
            .gdb.add_track(trackstr)
            .gtrack.attr.set(
                trackstr, "created.by",
                sprintf("gtrack.2d.import(%s, description, c(\"%s\"))", trackstr, paste(file, collapse = "\", \"")), TRUE
            )
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



#' Creates a track from a file of inter-genomic contacts
#'
#' Creates a track from a file of inter-genomic contacts.
#'
#' This function creates a 'Points' (two-dimensional) track from contacts
#' files. If 'allow.duplicates' is 'TRUE' duplicated contacts are allowed and
#' summed up, otherwise an error is reported.
#'
#' Contacts (coord1, coord2) within the same chromosome are automatically
#' doubled to include also '(coord2, coord1)' unless 'coord1' equals to
#' 'coord2'.
#'
#' Contacts may come in one or more files.
#'
#' If 'fends' is 'NULL' contacts file is expected to be in "intervals-value"
#' tab-separated format. The file starts with a header defining the column
#' names. The first 6 columns must have the following names: 'chrom1',
#' 'start1', 'end1', 'chrom2', 'start2', 'end2'. The last column is designated
#' for the value and it may have an arbitrary name. The header is followed by a
#' list of intervals and a value for each interval. An interval of form
#' (chrom1, start1, end1, chrom2, start2, end2) is added as a point (X, Y) to
#' the resulted track where X = (start1 + end1) / 2 and Y = (start2 + end2) /
#' 2.
#'
#' One can see an example of "intervals-value" format by running 'gextract'
#' function on a 2D track with a 'file' parameter set to the name of the file.
#'
#' If 'fends' is not 'NULL' contacts file is expected to be in "fends-value"
#' tab-separated format. It should start with a header containing at least 3
#' column names 'fend1', 'fend2' and 'count' in arbitrary order followed by
#' lines each defining a contact between two fragment ends.
#'
#' \tabular{lll}{ COLUMN \tab VALUE \tab DESCRIPTION\cr fend1 \tab Integer \tab
#' ID of the first fragment end \cr fend2 \tab Integer \tab ID of the second
#' fragment end \cr count \tab Numeric \tab Value associated with the contact
#' \cr }
#'
#' A fragment ends file is also in tab-separated format. It should start with a
#' header containing at least 3 column names 'fend', 'chr' and 'coord' in
#' arbitrary order followed by lines each defining a single fragment end.
#'
#' \tabular{lll}{ COLUMN \tab VALUE \tab DESCRIPTION\cr fend \tab Unique
#' integer \tab ID of the fragment end \cr chr \tab Chromosome name \tab Can be
#' specified with or without "chr" prefix, like: "X" or "chrX" \cr coord \tab
#' Integer \tab Coordinate\cr }
#'
#' 'description' is added as a track attribute.
#'
#' Note: temporary files are created in the directory of the track during the
#' run of the function. A few of them need to be kept simultaneously open. If
#' the number of chromosomes and / or contacts is particularly high, a few
#' thousands files might be needed to be opened simultaneously. Some operating
#' systems limit the number of open files per user, in which case the function
#' might fail with "Too many open files" or similar error. The workaround could
#' be:
#'
#' 1. Increase the limit of simultaneously opened files (the way varies
#' depending on your operating system). 2. Increase the value of
#' 'gmax.data.size' option. Higher values of 'gmax.data.size' option will
#' increased memory usage of the function but create fewer temporary files.
#'
#' @param track track name
#' @param description a character string description
#' @param contacts vector of contacts files
#' @param fends name of fragment ends file
#' @param allow.duplicates if 'TRUE' duplicated contacts are allowed
#' @return None.
#' @seealso \code{\link{gtrack.2d.import}}, \code{\link{gtrack.rm}},
#' \code{\link{gtrack.info}}, \code{\link{gdir.create}}
#' @keywords ~contacts ~fragment ~track
#' @export gtrack.2d.import_contacts
gtrack.2d.import_contacts <- function(track = NULL, description = NULL, contacts = NULL, fends = NULL, allow.duplicates = TRUE) {
    if (is.null(substitute(track)) || is.null(description) || is.null(contacts)) {
        stop("Usage: gtrack.2d.import_contacts(track, description, contacts, fends = NULL, allow.duplicates = TRUE)", call. = FALSE)
    }
    .gcheckroot()

    trackstr <- do.call(.gexpr2str, list(substitute(track)), envir = parent.frame())
    trackdir <- sprintf("%s.track", paste(get("GWD", envir = .misha), gsub("\\.", "/", trackstr), sep = "/"))

    direxisted <- file.exists(trackdir)

    if (!is.na(match(trackstr, get("GTRACKS", envir = .misha)))) {
        stop(sprintf("Track %s already exists", trackstr), call. = FALSE)
    }

    .gconfirmtrackcreate(trackstr)
    success <- FALSE
    tryCatch(
        {
            .gcall("gtrack_import_contacts", trackstr, contacts, fends, allow.duplicates, .misha_env(), silent = TRUE)
            .gdb.add_track(trackstr)
            .gtrack.attr.set(
                trackstr, "created.by",
                sprintf(
                    "gtrack.2d.import_contacts(\"%s\", description, c(\"%s\"), \"%s\", %s)",
                    trackstr, paste(contacts, collapse = "\", \""), ifelse(is.null(fends), "NULL", fends), allow.duplicates
                ),
                TRUE
            )
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
