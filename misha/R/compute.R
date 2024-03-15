#' Calculates distribution of contact distances
#'
#' Calculates distribution of contact distances.
#'
#' A 2D iterator interval '(chrom1, start1, end1, chrom2, start2, end2)' is
#' said to represent a contact between two 1D intervals I1 and I2: '(chrom1,
#' start1, end1)' and '(chrom2, start2, end2)'.
#'
#' For contacts where 'chrom1' equals to 'chrom2' and I1 is within source
#' intervals the function calculates the distribution of distances between I1
#' and I2. The distribution is calculated separately for intra-domain and
#' inter-domain contacts.
#'
#' An interval is within source intervals if the unification of all source
#' intervals fully overlaps it. 'src' intervals are allowed to contain
#' overlapping intervals.
#'
#' Two intervals I1 and I2 are within the same domain (intra-domain contact) if
#' among the domain intervals exists an interval that fully overlaps both I1
#' and I2. Otherwise the contact is considered to be inter-domain. 'domain'
#' must contain only non-overlapping intervals.
#'
#' The distance between I1 and I2 is the absolute distance between the centers
#' of these intervals, i.e.: '|(start1 + end1 - start2 - end2) / 2|'.
#'
#' The range of distances for which the distribution is calculated is defined
#' by 'breaks' argument. For example: 'breaks=c(x1, x2, x3, x4)' represents
#' three different intervals (bins): (x1, x2], (x2, x3], (x3, x4].
#'
#' If 'include.lowest' is 'TRUE' the the lowest value will be included in the
#' first interval, i.e. in [x1, x2]
#'
#' @param expr track expression
#' @param breaks breaks that determine the bin
#' @param src source intervals
#' @param domain domain intervals
#' @param intervals genomic scope for which the function is applied
#' @param include.lowest if 'TRUE', the lowest value of the range determined by
#' breaks is included
#' @param iterator 2D track expression iterator. If 'NULL' iterator is
#' determined implicitly based on track expressions.
#' @param band track expression band. If 'NULL' no band is used.
#' @return 2-dimensional vector representing the distribution of contact
#' distances for inter and intra domains.
#' @seealso \code{\link{gdist}}, \code{\link{gtrack.2d.import_contacts}}
#' @keywords ~contacts
#' @examples
#' \dontshow{
#' options(gmax.processes = 2)
#' }
#'
#' gdb.init_examples()
#'
#' src <- rbind(
#'     gintervals(1, 10, 100),
#'     gintervals(1, 200, 300),
#'     gintervals(1, 400, 500),
#'     gintervals(1, 600, 700),
#'     gintervals(1, 7000, 9100),
#'     gintervals(1, 9000, 18000),
#'     gintervals(1, 30000, 31000),
#'     gintervals(2, 1130, 15000)
#' )
#'
#' domain <- rbind(
#'     gintervals(1, 0, 483000),
#'     gintervals(2, 0, 300000)
#' )
#'
#' gcis_decay("rects_track", 50000 * (1:10), src, domain)
#'
#' @export gcis_decay
gcis_decay <- function(expr = NULL, breaks = NULL, src = NULL, domain = NULL, intervals = NULL, include.lowest = FALSE, iterator = NULL, band = NULL) {
    if (is.null(substitute(expr)) || is.null(breaks) || is.null(src) || is.null(domain)) {
        stop("Usage: gcis_decay(expr, breaks, src, domain, intervals = .misha$ALLGENOME, include.lowest = FALSE, iterator = NULL, band = NULL)", call. = FALSE)
    }
    .gcheckroot()

    intervals <- rescue_ALLGENOME(intervals, as.character(substitute(intervals)))

    if (is.null(intervals)) {
        intervals <- get("ALLGENOME", envir = .misha)
    }

    exprstr <- do.call(.gexpr2str, list(substitute(expr)), envir = parent.frame())
    .iterator <- do.call(.giterator, list(substitute(iterator)), envir = parent.frame())

    res <- .gcall("C_gcis_decay", exprstr, breaks, src, domain, intervals, include.lowest, .iterator, band, .misha_env())
    attr(res, "breaks") <- breaks
    res
}



#' Calculates distribution of track expressions
#'
#' Calculates distribution of track expressions' values over the given set of
#' bins.
#'
#' This function calculates the distribution of values of the numeric track
#' expressions over the given set of bins.
#'
#' The range of bins is determined by 'breaks' argument. For example:
#' 'breaks=c(x1, x2, x3, x4)' represents three different intervals (bins): (x1,
#' x2], (x2, x3], (x3, x4].
#'
#' If 'include.lowest' is 'TRUE' the the lowest value will be included in the
#' first interval, i.e. in [x1, x2]
#'
#' 'gdist' can work with any number of dimensions. If more than one
#' 'expr'-'breaks' pair is passed, the result is a multidimensional vector, and
#' an individual value can be accessed by [i1,i2,...,iN] notation, where 'i1'
#' is the first track and 'iN' is the last track expression.
#'
#' @param ... pairs of 'expr', 'breaks' where 'expr' is a track expression and the breaks determine the bin
#' @param intervals genomic scope for which the function is applied
#' @param include.lowest if 'TRUE', the lowest value of the range determined by
#' breaks is included
#' @param iterator track expression iterator. If 'NULL' iterator is determined
#' implicitly based on track expressions.
#' @param band track expression band. If 'NULL' no band is used.
#' @return N-dimensional vector where N is the number of 'expr'-'breaks' pairs.
#' @seealso \code{\link{gextract}}
#' @keywords ~distribution
#' @examples
#' \dontshow{
#' options(gmax.processes = 2)
#' }
#'
#' gdb.init_examples()
#'
#' ## calculate the distribution of dense_track for bins:
#' ## (0, 0.2], (0.2, 0.5] and (0.5, 1]
#' gdist("dense_track", c(0, 0.2, 0.5, 1))
#'
#' ## calculate two-dimensional distribution:
#' ## dense_track vs. sparse_track
#' gdist("dense_track", seq(0, 1, by = 0.1), "sparse_track",
#'     seq(0, 2, by = 0.2),
#'     iterator = 100
#' )
#'
#' @export gdist
gdist <- function(..., intervals = NULL, include.lowest = FALSE, iterator = NULL, band = NULL) {
    args <- as.list(substitute(list(...)))[-1L]
    if (length(args) < 2 || (length(args) %% 2 != 0 && (length(args) - 1) %% 2 != 0)) {
        stop("Usage: gdist([expr, breaks]+, intervals = .misha$ALLGENOME, include.lowest = FALSE, iterator = NULL, band = NULL)", call. = FALSE)
    }
    .gcheckroot()

    intervals <- rescue_ALLGENOME(intervals, as.character(substitute(intervals)))

    if (length(args) %% 2 != 0) {
        intervals <- eval.parent(args[[length(args)]])
    } else if (is.null(intervals)) {
        intervals <- get("ALLGENOME", envir = .misha)
    }

    exprs <- c()
    breaks <- list()

    for (i in (0:(length(args) / 2 - 1))) {
        exprs <- append(exprs, do.call(.gexpr2str, list(args[[i * 2 + 1]]), envir = parent.frame()))
        breaks[[length(breaks) + 1]] <- eval.parent(args[[i * 2 + 2]])
    }

    .iterator <- do.call(.giterator, list(substitute(iterator)), envir = parent.frame())

    if (.ggetOption("gmultitasking")) {
        res <- .gcall("gtrackdist_multitask", intervals, exprs, breaks, include.lowest, .iterator, band, .misha_env())
    } else {
        res <- .gcall("gtrackdist", intervals, exprs, breaks, include.lowest, .iterator, band, .misha_env())
    }
    attr(res, "breaks") <- breaks
    res
}



#' Returns evaluated track expression
#'
#' Returns the result of track expressions evaluation for each of the iterator
#' intervals.
#'
#' This function returns the result of track expressions evaluation for each of
#' the iterator intervals. The returned value is a set of intervals with an
#' additional column for each of the track expressions. This value can be used
#' as an input for any other function that accepts intervals. If the intervals
#' inside 'intervals' argument overlap gextract returns the overlapped
#' coordinate more than once.
#'
#' The order inside the result might not be the same as the order of intervals.
#' An additional column 'intervalID' is added to the return value. Use this
#' column to refer to the index of the original interval from the supplied
#' 'intervals'.
#'
#' If 'file' parameter is not 'NULL' the result is outputted to a tab-delimited
#' text file (without 'intervalID' column) rather than returned to the user.
#' This can be especially useful when the result is too big to fit into the
#' physical memory.  The resulted file can be used as an input for
#' 'gtrack.import' or 'gtrack.array.import' functions.
#'
#' If 'intervals.set.out' is not 'NULL' the result is saved as an intervals
#' set. Similarly to 'file' parameter 'intervals.set.out' can be useful to
#' overcome the limits of the physical memory.
#'
#' 'colnames' parameter controls the names of the columns that contain the
#' evaluated expressions. By default the column names match the track
#' expressions.
#'
#' @param ... track expression
#' @param intervals genomic scope for which the function is applied
#' @param colnames sets the columns names in the returned value. If 'NULL'
#' names are set to track expression.
#' @param iterator track expression iterator. If 'NULL' iterator is determined
#' implicitly based on track expressions.
#' @param band track expression band. If 'NULL' no band is used.
#' @param file file name where the function result is optionally outputted in
#' tab-delimited format
#' @param intervals.set.out intervals set name where the function result is
#' optionally outputted
#' @return If 'file' and 'intervals.set.out' are 'NULL' a set of intervals with
#' an additional column for each of the track expressions and 'columnID'
#' column.
#' @seealso \code{\link{gtrack.array.extract}}, \code{\link{gsample}},
#' \code{\link{gtrack.import}}, \code{\link{gtrack.array.import}},
#' \code{\link{glookup}}, \code{\link{gpartition}}, \code{\link{gdist}}
#' @keywords ~extract
#' @examples
#' \dontshow{
#' options(gmax.processes = 2)
#' }
#'
#' gdb.init_examples()
#'
#' ## get values of 'dense_track' for [0, 500), chrom 1
#' gextract("dense_track", gintervals(1, 0, 500))
#'
#' ## get values of 'rects_track' (a 2D track) for a 2D interval
#' gextract(
#'     "rects_track",
#'     gintervals.2d("chr1", 0, 4000, "chr2", 2000, 5000)
#' )
#'
#' ## get values of two track expressions 'dense_track' and
#' ## 'array_track * 2' running over '100' iterator
#' gextract("dense_track", "array_track * 2", gintervals(1, 0, 500),
#'     iterator = 100, colnames = c("expr1", "expr2")
#' )
#'
#' @export gextract
gextract <- function(..., intervals = NULL, colnames = NULL, iterator = NULL, band = NULL, file = NULL, intervals.set.out = NULL) {
    args <- as.list(substitute(list(...)))[-1L]
    if (is.null(intervals) && length(args) < 2 || !is.null(intervals) && length(args) < 1) {
        stop("Usage: gextract([expr]+, intervals, colnames = NULL, iterator = NULL, band = NULL, file = NULL, intervals.set.out = NULL)", call. = FALSE)
    }
    .gcheckroot()

    intervals <- rescue_ALLGENOME(intervals, as.character(substitute(intervals)))

    if (is.null(intervals)) {
        intervals <- eval.parent(args[[length(args)]])
        args <- args[1:(length(args) - 1)]
    }

    tracks <- c()
    for (track in args) {
        tracks <- c(tracks, do.call(.gexpr2str, list(track), envir = parent.frame()))
    }

    .iterator <- do.call(.giterator, list(substitute(iterator)), envir = parent.frame())
    intervals.set.out <- do.call(.gexpr2str, list(substitute(intervals.set.out)), envir = parent.frame())

    if (!is.null(intervals.set.out)) {
        fullpath <- .gintervals.check_new_set(intervals.set.out)
    }

    # intervals can be NULL if gextract is piped with gscreen and the latter returns NULL
    success <- FALSE
    res <- NULL
    tryCatch(
        {
            if (!is.null(intervals)) {
                if (.ggetOption("gmultitasking")) {
                    res <- .gcall("gextract_multitask", intervals, tracks, colnames, .iterator, band, file, intervals.set.out, .misha_env())
                } else {
                    res <- .gcall("C_gextract", intervals, tracks, colnames, .iterator, band, file, intervals.set.out, .misha_env())
                }

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



#' Partitions the values of track expression
#'
#' Converts the values of track expression to intervals that match
#' corresponding bin.
#'
#' This function converts first the values of track expression into 1-based
#' bin's index according 'breaks' argument. It returns then the intervals with
#' the corresponding bin's index.
#'
#' The range of bins is determined by 'breaks' argument. For example:
#' 'breaks=c(x1, x2, x3, x4)' represents three different intervals (bins): (x1,
#' x2], (x2, x3], (x3, x4].
#'
#' If 'include.lowest' is 'TRUE' the the lowest value will be included in the
#' first interval, i.e. in [x1, x2].
#'
#' If 'intervals.set.out' is not 'NULL' the result is saved as an intervals
#' set. Use this parameter if the result size exceeds the limits of the
#' physical memory.
#'
#' @param expr track expression
#' @param breaks breaks that determine the bin
#' @param intervals genomic scope for which the function is applied
#' @param include.lowest if 'TRUE', the lowest value of the range determined by
#' breaks is included
#' @param iterator track expression iterator. If 'NULL' iterator is determined
#' implicitly based on track expression.
#' @param band track expression band. If 'NULL' no band is used.
#' @param intervals.set.out intervals set name where the function result is
#' optionally outputted
#' @return If 'intervals.set.out' is 'NULL' a set of intervals with an
#' additional column that indicates the corresponding bin index.
#' @seealso \code{\link{gscreen}}, \code{\link{gextract}},
#' \code{\link{glookup}}, \code{\link{gdist}}
#' @keywords ~partition
#' @examples
#' \dontshow{
#' options(gmax.processes = 2)
#' }
#'
#' gdb.init_examples()
#' breaks <- seq(0, 0.2, by = 0.05)
#' gpartition("dense_track", breaks, gintervals(1, 0, 5000))
#'
#' @export gpartition
gpartition <- function(expr = NULL, breaks = NULL, intervals = NULL, include.lowest = FALSE, iterator = NULL, band = NULL, intervals.set.out = NULL) {
    if (is.null(substitute(expr)) || is.null(breaks) || is.null(intervals)) {
        stop("Usage: gpartition(expr, breaks, intervals, include.lowest = FALSE, iterator = NULL, band = NULL, intervals.set.out = NULL)", call. = FALSE)
    }
    .gcheckroot()

    intervals <- rescue_ALLGENOME(intervals, as.character(substitute(intervals)))

    exprstr <- do.call(.gexpr2str, list(substitute(expr)), envir = parent.frame())
    .iterator <- do.call(.giterator, list(substitute(iterator)), envir = parent.frame())
    intervals.set.out <- do.call(.gexpr2str, list(substitute(intervals.set.out)), envir = parent.frame())
    if (!is.null(intervals.set.out)) {
        fullpath <- .gintervals.check_new_set(intervals.set.out)
    }

    # intervals can be NULL if piped with gscreen and the latter returns NULL
    success <- FALSE
    res <- NULL
    tryCatch(
        {
            if (!is.null(intervals)) {
                res <- .gcall("C_gpartition", intervals, exprstr, breaks, include.lowest, .iterator, band, intervals.set.out, .misha_env())
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
    } else {
        res
    }
}



#' Calculates quantiles of a track expression
#'
#' Calculates the quantiles of a track expression for the given percentiles.
#'
#' This function calculates the quantiles for the given percentiles.
#'
#' If data size exceeds the limit (see: 'getOption(gmax.data.size)'), the data
#' is randomly sampled to fit the limit. A warning message is generated. The
#' seed of the pseudo-random generator can be controlled through 'grnd.seed'
#' option.
#'
#' Note: this function is capable to run in multitasking mode. Sampling may
#' vary according to the extent of multitasking. Since multitasking depends on
#' the number of available CPU cores, running the function on two different
#' machines might give different results. Please switch off multitasking if you
#' want to achieve identical results on any machine. For more information
#' regarding multitasking please refer "User Manual".
#'
#' @param expr track expression
#' @param percentiles an array of percentiles of quantiles in [0, 1] range
#' @param intervals genomic scope for which the function is applied
#' @param iterator track expression iterator. If 'NULL' iterator is determined
#' implicitly based on track expression.
#' @param band track expression band. If 'NULL' no band is used.
#' @return An array that represent quantiles.
#' @seealso \code{\link{gbins.quantiles}}, \code{\link{gintervals.quantiles}},
#' \code{\link{gdist}}
#' @keywords ~quantiles ~percentiles
#' @examples
#' \dontshow{
#' options(gmax.processes = 2)
#' }
#'
#' gdb.init_examples()
#' gquantiles("dense_track", c(0.1, 0.6, 0.8), gintervals(c(1, 2)))
#'
#' @export gquantiles
gquantiles <- function(expr = NULL, percentiles = 0.5, intervals = get("ALLGENOME", envir = .misha), iterator = NULL, band = NULL) {
    if (is.null(substitute(expr))) {
        stop("Usage: gquantiles(expr, percentiles = 0.5, intervals = .misha$ALLGENOME, iterator = NULL, band = NULL)", call. = FALSE)
    }
    .gcheckroot()

    intervals <- rescue_ALLGENOME(intervals, as.character(substitute(intervals)))

    exprstr <- do.call(.gexpr2str, list(substitute(expr)), envir = parent.frame())
    .iterator <- do.call(.giterator, list(substitute(iterator)), envir = parent.frame())

    if (.ggetOption("gmultitasking")) {
        res <- .gcall("gquantiles_multitask", intervals, exprstr, percentiles, .iterator, band, .misha_env())
    } else {
        res <- .gcall("C_gquantiles", intervals, exprstr, percentiles, .iterator, band, .misha_env())
    }
    res
}



#' Returns values from a lookup table based on track expression
#'
#' Evaluates track expression and translates the values into bin indices that
#' are used in turn to retrieve and return values from a lookup table.
#'
#' This function evaluates the track expression for all iterator intervals and
#' translates this value into an index based on the breaks. This index is then
#' used to address the lookup table and return the according value. More than
#' one 'expr'-'breaks' pair can be used. In that case 'lookup_table' is
#' addressed in a multidimensional manner, i.e. 'lookup_table[i1, i2, ...]'.
#'
#' The range of bins is determined by 'breaks' argument. For example: 'breaks =
#' c(x1, x2, x3, x4)' represents three different intervals (bins): (x1, x2],
#' (x2, x3], (x3, x4].
#'
#' If 'include.lowest' is 'TRUE' then the lowest value is included in the first
#' interval, i.e. in [x1, x2].
#'
#' 'force.binning' parameter controls what should be done when the value of
#' 'expr' exceeds the range determined by 'breaks'. If 'force.binning' is
#' 'TRUE' then values smaller than the minimal break will be translated to
#' index 1, and the values exceeding the maximal break will be translated to
#' index 'M-1' where 'M' is the number of breaks. If 'force.binning' is 'FALSE'
#' the out-of-range values will produce 'NaN' values.
#'
#' Regardless of 'force.binning' value if the value of 'expr' is 'NaN' then
#' result is 'NaN' too.
#'
#' The order inside the result might not be the same as the order of intervals.
#' Use 'intervalID' column to refer to the index of the original interval from
#' the supplied 'intervals'.
#'
#' If 'intervals.set.out' is not 'NULL' the result (without 'columnID' column)
#' is saved as an intervals set. Use this parameter if the result size exceeds
#' the limits of the physical memory.
#'
#' @param lookup_table a multi-dimensional array containing the values that are
#' returned by the function
#' @param ... pairs of 'expr', 'breaks' where 'expr' is a track expression and the breaks determine the bin
#' @param intervals genomic scope for which the function is applied
#' @param include.lowest if 'TRUE', the lowest value of the range determined by
#' breaks is included
#' @param force.binning if 'TRUE', the values smaller than the minimal break
#' will be translated to index 1, and the values that exceed the maximal break
#' will be translated to index N-1 where N is the number of breaks. If 'FALSE'
#' the out-of-range values will produce NaN values.
#' @param iterator track expression iterator. If 'NULL' iterator is determined
#' implicitly based on track expressions.
#' @param band track expression band. If 'NULL' no band is used.
#' @param intervals.set.out intervals set name where the function result is
#' optionally outputted
#' @return If 'intervals.set.out' is 'NULL' a set of intervals with additional
#' 'value' and 'columnID' columns.
#' @seealso \code{\link{gtrack.lookup}}, \code{\link{gextract}},
#' \code{\link{gpartition}}, \code{\link{gdist}}
#' @keywords ~lookup ~extract
#' @examples
#' \dontshow{
#' options(gmax.processes = 2)
#' }
#'
#' gdb.init_examples()
#'
#' ## one-dimensional lookup table
#' breaks1 <- seq(0.1, 0.2, length.out = 6)
#' glookup(1:5, "dense_track", breaks1, gintervals(1, 0, 200))
#'
#' ## two-dimensional lookup table
#' t <- array(1:15, dim = c(5, 3))
#' breaks2 <- seq(0.31, 0.37, length.out = 4)
#' glookup(
#'     t, "dense_track", breaks1, "2 * dense_track", breaks2,
#'     gintervals(1, 0, 200)
#' )
#'
#' @export glookup
glookup <- function(lookup_table = NULL, ..., intervals = NULL, include.lowest = FALSE, force.binning = TRUE, iterator = NULL, band = NULL, intervals.set.out = NULL) {
    args <- as.list(substitute(list(...)))[-1L]
    if (is.null(lookup_table) || length(args) < 2 || (!is.null(intervals) && length(args) %% 2 != 0) || (is.null(intervals) && length(args) %% 2 == 0)) {
        stop("Usage: glookup(lookup_table, [expr, breaks]+, intervals, include.lowest = FALSE, force.binning = TRUE, iterator = NULL, band = NULL, intervals.set.out = NULL)", call. = FALSE)
    }
    .gcheckroot()

    if (length(args) %% 2 != 0) {
        intervals <- eval.parent(args[[length(args)]])
    }

    intervals <- rescue_ALLGENOME(intervals, as.character(substitute(intervals)))

    exprs <- c()
    breaks <- list()

    for (i in (0:(length(args) / 2 - 1))) {
        exprs <- append(exprs, do.call(.gexpr2str, list(args[[i * 2 + 1]]), envir = parent.frame()))
        breaks[[length(breaks) + 1]] <- eval.parent(args[[i * 2 + 2]])
    }

    .iterator <- do.call(.giterator, list(substitute(iterator)), envir = parent.frame())
    intervals.set.out <- do.call(.gexpr2str, list(substitute(intervals.set.out)), envir = parent.frame())
    if (!is.null(intervals.set.out)) {
        fullpath <- .gintervals.check_new_set(intervals.set.out)
    }

    # intervals can be NULL if gextract is piped with gscreen and the latter returns NULL
    success <- FALSE
    res <- NULL
    tryCatch(
        {
            if (!is.null(intervals)) {
                res <- .gcall("gbintransform", intervals, exprs, breaks, include.lowest, force.binning, lookup_table, .iterator, band, intervals.set.out, .misha_env())
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
    } else {
        res
    }
}



#' Returns samples from the values of track expression
#'
#' Returns a sample of the specified size from the values of track expression.
#'
#' This function returns a sample of the specified size from the values of
#' track expression. If 'n' is less than the total number of values, the data
#' is randomly sampled. The seed of the pseudo-random generator can be
#' controlled through 'grnd.seed' option.
#'
#' If 'n' is higher than the total number of values, all values are returned
#' (yet reshuffled).
#'
#' @param expr track expression
#' @param n a number of items to choose
#' @param intervals genomic scope for which the function is applied
#' @param iterator track expression iterator. If 'NULL' iterator is determined
#' implicitly based on track expression.
#' @param band track expression band. If 'NULL' no band is used.
#' @return An array that represent quantiles.
#' @seealso \code{\link{gextract}}
#' @keywords ~sample
#' @examples
#' \dontshow{
#' options(gmax.processes = 2)
#' }
#'
#' gdb.init_examples()
#' gsample("sparse_track", 10)
#'
#' @export gsample
gsample <- function(expr = NULL, n = NULL, intervals = NULL, iterator = NULL, band = NULL) {
    if (is.null(substitute(expr))) {
        stop("Usage: gsample(expr, n, intervals = .misha$ALLGENOME, iterator = NULL, band = NULL)", call. = FALSE)
    }
    .gcheckroot()

    intervals <- rescue_ALLGENOME(intervals, as.character(substitute(intervals)))

    if (is.null(intervals)) {
        intervals <- get("ALLGENOME", envir = .misha)
    }

    exprstr <- do.call(.gexpr2str, list(substitute(expr)), envir = parent.frame())
    .iterator <- do.call(.giterator, list(substitute(iterator)), envir = parent.frame())

    .gcall("C_gsample", exprstr, n, intervals, .iterator, band, .misha_env())
}




#' Finds intervals that match track expression
#'
#' Finds all intervals where track expression is 'TRUE'.
#'
#' This function finds all intervals where track expression's value is 'TRUE'.
#'
#' If 'intervals.set.out' is not 'NULL' the result is saved as an intervals
#' set. Use this parameter if the result size exceeds the limits of the
#' physical memory.
#'
#' @param expr logical track expression
#' @param intervals genomic scope for which the function is applied
#' @param iterator track expression iterator. If 'NULL' iterator is determined
#' implicitly based on track expression.
#' @param band track expression band. If 'NULL' no band is used.
#' @param intervals.set.out intervals set name where the function result is
#' optionally outputted
#' @return If 'intervals.set.out' is 'NULL' a set of intervals that match track
#' expression.
#' @seealso \code{\link{gsegment}}, \code{\link{gextract}}
#' @keywords ~screen ~interval ~intervals
#' @examples
#' \dontshow{
#' options(gmax.processes = 2)
#' }
#'
#' gdb.init_examples()
#' gscreen("dense_track > 0.2 & sparse_track < 0.4",
#'     iterator = "dense_track"
#' )
#'
#' @export gscreen
gscreen <- function(expr = NULL, intervals = NULL, iterator = NULL, band = NULL, intervals.set.out = NULL) {
    if (is.null(substitute(expr))) {
        stop("Usage: gscreen(expr, intervals = .misha$ALLGENOME, iterator = NULL, band = NULL, intervals.set.out = NULL)", call. = FALSE)
    }
    .gcheckroot()

    intervals <- rescue_ALLGENOME(intervals, as.character(substitute(intervals)))

    if (is.null(intervals)) {
        intervals <- get("ALLGENOME", envir = .misha)
    }

    exprstr <- do.call(.gexpr2str, list(substitute(expr)), envir = parent.frame())
    .iterator <- do.call(.giterator, list(substitute(iterator)), envir = parent.frame())
    intervals.set.out <- do.call(.gexpr2str, list(substitute(intervals.set.out)), envir = parent.frame())

    if (!is.null(intervals.set.out)) {
        fullpath <- .gintervals.check_new_set(intervals.set.out)
    }

    success <- FALSE
    res <- NULL
    tryCatch(
        {
            if (.ggetOption("gmultitasking")) {
                res <- .gcall("gscreen_multitask", exprstr, intervals, .iterator, band, intervals.set.out, .misha_env())
            } else {
                res <- .gcall("C_gscreen", exprstr, intervals, .iterator, band, intervals.set.out, .misha_env())
            }

            if (!is.null(intervals.set.out) && .gintervals.is_bigset(intervals.set.out, FALSE) && !.gintervals.needs_bigset(intervals.set.out)) {
                .gintervals.big2small(intervals.set.out)
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
    } else {
        res
    }
}



#' Divides track expression into segments
#'
#' Divides the values of track expression into segments by using Wilcoxon test.
#'
#' This function divides the values of track expression into segments, where
#' each segment size is at least of 'minsegment' size and the P-value of
#' comparing the segment with the first 'minsegment' values from the next
#' segment is at most 'maxpval'. Comparison is done using Wilcoxon (also known
#' as Mann-Whitney) test.
#'
#' If 'intervals.set.out' is not 'NULL' the result is saved as an intervals
#' set. Use this parameter if the result size exceeds the limits of the
#' physical memory.
#'
#' @param expr track expression
#' @param minsegment minimal segment size
#' @param maxpval maximal P-value that separates two adjacent segments
#' @param onetailed if 'TRUE', Wilcoxon test is performed one tailed, otherwise
#' two tailed
#' @param intervals genomic scope for which the function is applied
#' @param iterator track expression iterator of "fixed bin" type. If 'NULL'
#' iterator is determined implicitly based on track expression.
#' @param intervals.set.out intervals set name where the function result is
#' optionally outputted
#' @return If 'intervals.set.out' is 'NULL' a set of intervals where each
#' interval represents a segment.
#' @seealso \code{\link{gscreen}}, \code{\link{gwilcox}}
#' @keywords ~segment ~wilcoxon ~Mann-Whitney
#' @examples
#' \dontshow{
#' options(gmax.processes = 2)
#' }
#'
#' gdb.init_examples()
#' gsegment("dense_track", 5000, 0.0001)
#'
#' @export gsegment
gsegment <- function(expr = NULL, minsegment = NULL, maxpval = 0.05, onetailed = TRUE, intervals = NULL, iterator = NULL, intervals.set.out = NULL) {
    if (is.null(substitute(expr)) || is.null(minsegment)) {
        stop("Usage: gsegment(expr, minsegment, maxpval = 0.05, onetailed = TRUE, intervals = .misha$ALLGENOME, iterator = NULL, intervals.set.out = NULL)", call. = FALSE)
    }
    .gcheckroot()

    intervals <- rescue_ALLGENOME(intervals, as.character(substitute(intervals)))

    if (is.null(intervals)) {
        intervals <- get("ALLGENOME", envir = .misha)
    }

    exprstr <- do.call(.gexpr2str, list(substitute(expr)), envir = parent.frame())
    .iterator <- do.call(.giterator, list(substitute(iterator)), envir = parent.frame())
    intervals.set.out <- do.call(.gexpr2str, list(substitute(intervals.set.out)), envir = parent.frame())
    if (!is.null(intervals.set.out)) {
        fullpath <- .gintervals.check_new_set(intervals.set.out)
    }

    if (!onetailed) {
        maxpval <- maxpval / 2
    }

    # intervals can be NULL if piped with gscreen and the latter returns NULL
    success <- FALSE
    res <- NULL
    tryCatch(
        {
            if (!is.null(intervals)) {
                res <- .gcall("C_gsegment", exprstr, intervals, minsegment, stats::qnorm(maxpval), onetailed, .iterator, intervals.set.out, .misha_env())
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
    } else {
        res
    }
}



#' Calculates summary statistics of track expression
#'
#' Calculates summary statistics of track expression.
#'
#' This function returns summary statistics of a track expression: total number
#' of bins, total number of bins whose value is NaN, min, max, sum, mean and
#' standard deviation of the values.
#'
#' @param expr track expression
#' @param intervals genomic scope for which the function is applied
#' @param iterator track expression iterator. If 'NULL' iterator is determined
#' implicitly based on track expression.
#' @param band track expression band. If 'NULL' no band is used.
#' @return An array that represents summary statistics.
#' @seealso \code{\link{gintervals.summary}}, \code{\link{gbins.summary}}
#' @keywords ~summary ~statistics
#' @examples
#' \dontshow{
#' options(gmax.processes = 2)
#' }
#'
#' gdb.init_examples()
#' gsummary("rects_track")
#'
#' @export gsummary
gsummary <- function(expr = NULL, intervals = NULL, iterator = NULL, band = NULL) {
    if (is.null(substitute(expr))) {
        stop("Usage: gsummary(expr, intervals = .misha$ALLGENOME, iterator = NULL, band = NULL)", call. = FALSE)
    }
    .gcheckroot()

    intervals <- rescue_ALLGENOME(intervals, as.character(substitute(intervals)))

    if (is.null(intervals)) {
        intervals <- get("ALLGENOME", envir = .misha)
    }

    exprstr <- do.call(.gexpr2str, list(substitute(expr)), envir = parent.frame())
    .iterator <- do.call(.giterator, list(substitute(iterator)), envir = parent.frame())

    if (.ggetOption("gmultitasking")) {
        res <- .gcall("gtracksummary_multitask", exprstr, intervals, .iterator, band, .misha_env())
    } else {
        res <- .gcall("gtracksummary", exprstr, intervals, .iterator, band, .misha_env())
    }
    res
}




#' Calculates Wilcoxon test on sliding windows over track expression
#'
#' Calculates Wilcoxon test on sliding windows over the values of track
#' expression.
#'
#' This function runs a Wilcoxon test (also known as a Mann-Whitney test) over
#' the values of track expression in the two sliding windows having an
#' identical center. The sizes of the windows are specified by 'winsize1' and
#' 'winsize2'. 'gwilcox' returns intervals where the smaller window tested
#' against a larger window gives a P-value below 'maxpval'. The test can be one
#' or two tailed.
#'
#' 'what2find' argument controls what should be searched: peaks, lows or both.
#'
#' If 'intervals.set.out' is not 'NULL' the result is saved as an intervals
#' set. Use this parameter if the result size exceeds the limits of the
#' physical memory.
#'
#' @param expr track expression
#' @param winsize1 number of values in the first sliding window
#' @param winsize2 number of values in the second sliding window
#' @param maxpval maximal P-value
#' @param onetailed if 'TRUE', Wilcoxon test is performed one tailed, otherwise
#' two tailed
#' @param what2find if '-1', lows are searched. If '1', peaks are searched. If
#' '0', both peaks and lows are searched
#' @param intervals genomic scope for which the function is applied
#' @param iterator track expression iterator of "fixed bin" type. If 'NULL'
#' iterator is determined implicitly based on track expression.
#' @param intervals.set.out intervals set name where the function result is
#' optionally outputted
#' @return If 'intervals.set.out' is 'NULL' a data frame representing the
#' intervals with an additional 'pval' column where P-value is below 'maxpval'.
#' @seealso \code{\link{gscreen}}, \code{\link{gsegment}}
#' @keywords ~wilcoxon ~Mann-Whitney
#' @examples
#' \dontshow{
#' options(gmax.processes = 2)
#' }
#'
#' gdb.init_examples()
#' gwilcox("dense_track", 100000, 1000,
#'     maxpval = 0.01,
#'     what2find = 1
#' )
#'
#' @export gwilcox
gwilcox <- function(expr = NULL, winsize1 = NULL, winsize2 = NULL, maxpval = 0.05, onetailed = TRUE, what2find = 1, intervals = NULL, iterator = NULL, intervals.set.out = NULL) {
    if (is.null(substitute(expr)) || is.null(winsize1) || is.null(winsize2)) {
        stop("Usage: gwilcox(expr, winsize1, winsize2, maxpval = 0.05, onetailed = TRUE, what2find = 1 (-1=lows, 0=lows/highs, 1=highs), intervals = .misha$ALLGENOME, iterator = NULL, intervals.set.out = NULL)", call. = FALSE)
    }
    .gcheckroot()

    intervals <- rescue_ALLGENOME(intervals, as.character(substitute(intervals)))

    if (is.null(intervals)) {
        intervals <- get("ALLGENOME", envir = .misha)
    }

    exprstr <- do.call(.gexpr2str, list(substitute(expr)), envir = parent.frame())
    .iterator <- do.call(.giterator, list(substitute(iterator)), envir = parent.frame())
    intervals.set.out <- do.call(.gexpr2str, list(substitute(intervals.set.out)), envir = parent.frame())
    if (!is.null(intervals.set.out)) {
        fullpath <- .gintervals.check_new_set(intervals.set.out)
    }

    if (!onetailed) {
        maxpval <- maxpval / 2
    }

    # intervals can be NULL if piped with gscreen and the latter returns NULL
    success <- FALSE
    res <- NULL
    tryCatch(
        {
            if (!is.null(intervals)) {
                res <- .gcall("C_gwilcox", exprstr, intervals, winsize1, winsize2, qnorm(maxpval), onetailed, as.integer(what2find), .iterator, intervals.set.out, .misha_env())
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
    } else {
        res
    }
}



#' Calculates quantiles of a track expression for bins
#'
#' Calculates quantiles of a track expression for bins.
#'
#' This function is a binned version of 'gquantiles'. For each iterator
#' interval the value of 'bin_expr' is calculated and assigned to the
#' corresponding bin determined by 'breaks'. The quantiles of 'expr' are
#' calculated then separately for each bin.
#'
#' The bins can be multi-dimensional depending on the number of
#' 'bin_expr'-'breaks' pairs.
#'
#' The range of bins is determined by 'breaks' argument. For example:
#' 'breaks=c(x1, x2, x3, x4)' represents three different intervals (bins): (x1,
#' x2], (x2, x3], (x3, x4].
#'
#' If 'include.lowest' is 'TRUE' the the lowest value will be included in the
#' first interval, i.e. in [x1, x2].
#'
#' @param ... pairs of track expressions ('bin_expr') that determines the bins and breaks that define the bins. See \code{\link{gdist}}.
#' @param expr track expression for which quantiles are calculated
#' @param percentiles an array of percentiles of quantiles in [0, 1] range
#' @param intervals genomic scope for which the function is applied.
#' @param include.lowest if 'TRUE', the lowest value of the range determined by
#' breaks is included
#' @param iterator track expression iterator. If 'NULL' iterator is determined
#' implicitly based on track expressions.
#' @param band track expression band. If 'NULL' no band is used.
#' @return Multi-dimensional array representing quantiles for each percentile
#' and bin.
#' @seealso \code{\link{gquantiles}}, \code{\link{gintervals.quantiles}},
#' \code{\link{gdist}}
#' @keywords ~quantiles ~percentiles
#' @examples
#' \dontshow{
#' options(gmax.processes = 2)
#' }
#'
#' gdb.init_examples()
#' gbins.quantiles("dense_track", c(0, 0.2, 0.4, 2), "sparse_track",
#'     percentiles = c(0.2, 0.5),
#'     intervals = gintervals(1),
#'     iterator = "dense_track"
#' )
#'
#' @export gbins.quantiles
gbins.quantiles <- function(..., expr = NULL, percentiles = 0.5, intervals = get("ALLGENOME", envir = .misha), include.lowest = FALSE, iterator = NULL, band = NULL) {
    args <- as.list(substitute(list(...)))[-1L]

    if (length(args) >= 0 && length(args) %% 2 != 0) {
        expr <- args[[length(args)]]
    }

    if (length(args) < 2 || is.null(substitute(expr))) {
        stop("Usage: gbins.quantiles([bin_expr, breaks]+, expr, percentiles = 0.5, intervals = .misha$ALLGENOME, include.lowest = FALSE, iterator = NULL, band = NULL)", call. = FALSE)
    }
    .gcheckroot()

    intervals <- rescue_ALLGENOME(intervals, as.character(substitute(intervals)))

    exprs <- c()
    breaks <- list()

    exprs <- append(exprs, do.call(.gexpr2str, list(substitute(expr)), envir = parent.frame()))
    for (i in (0:((length(args) - 1) / 2 - 1))) {
        exprs <- append(exprs, do.call(.gexpr2str, list(args[[i * 2 + 1]]), envir = parent.frame()))
        breaks[[length(breaks) + 1]] <- eval.parent(args[[i * 2 + 2]])
    }

    .iterator <- do.call(.giterator, list(substitute(iterator)), envir = parent.frame())

    res <- .gcall("gbins_quantiles", exprs, breaks, include.lowest, percentiles, intervals, .iterator, band, .misha_env())
    attr(res, "breaks") <- breaks
    res
}



#' Calculates summary statistics of a track expression for bins
#'
#' Calculates summary statistics of a track expression for bins.
#'
#' This function is a binned version of 'gsummary'. For each iterator interval
#' the value of 'bin_expr' is calculated and assigned to the corresponding bin
#' determined by 'breaks'. The summary statistics of 'expr' are calculated then
#' separately for each bin.
#'
#' The bins can be multi-dimensional depending on the number of
#' 'bin_expr'-'breaks' pairs.
#'
#' The range of bins is determined by 'breaks' argument. For example:
#' 'breaks=c(x1, x2, x3, x4)' represents three different intervals (bins): (x1,
#' x2], (x2, x3], (x3, x4].
#'
#' If 'include.lowest' is 'TRUE' the the lowest value will be included in the
#' first interval, i.e. in [x1, x2].
#'
#' @param ... pairs of track expressions ('bin_expr') that determines the bins and breaks that define the bins. See \code{\link{gdist}}.
#' @param expr track expression for which summary statistics is calculated
#' @param intervals genomic scope for which the function is applied
#' @param include.lowest if 'TRUE', the lowest value of the range determined by
#' breaks is included
#' @param iterator track expression iterator. If 'NULL' iterator is determined
#' implicitly based on track expressions.
#' @param band track expression band. If 'NULL' no band is used.
#' @return Multi-dimensional array representing summary statistics for each
#' bin.
#' @seealso \code{\link{gsummary}}, \code{\link{gintervals.summary}},
#' \code{\link{gdist}}
#' @keywords ~summary
#' @examples
#' \dontshow{
#' options(gmax.processes = 2)
#' }
#'
#' gdb.init_examples()
#' gbins.summary("dense_track", c(0, 0.2, 0.4, 2), "sparse_track",
#'     intervals = gintervals(1), iterator = "dense_track"
#' )
#'
#' @export gbins.summary
gbins.summary <- function(..., expr = NULL, intervals = get("ALLGENOME", envir = .misha), include.lowest = FALSE, iterator = NULL, band = NULL) {
    args <- as.list(substitute(list(...)))[-1L]

    if (length(args) >= 0 && length(args) %% 2 != 0) {
        expr <- args[[length(args)]]
    }

    if (length(args) < 2 || is.null(substitute(expr))) {
        stop("Usage: gbins.summary([expr, breaks]+, expr, intervals = .misha$ALLGENOME, include.lowest = FALSE, iterator = NULL, band = NULL)", call. = FALSE)
    }
    .gcheckroot()

    intervals <- rescue_ALLGENOME(intervals, as.character(substitute(intervals)))

    exprs <- c()
    breaks <- list()

    exprs <- append(exprs, do.call(.gexpr2str, list(substitute(expr)), envir = parent.frame()))
    for (i in (0:((length(args) - 1) / 2 - 1))) {
        exprs <- append(exprs, do.call(.gexpr2str, list(args[[i * 2 + 1]]), envir = parent.frame()))
        breaks[[length(breaks) + 1]] <- eval.parent(args[[i * 2 + 2]])
    }

    .iterator <- do.call(.giterator, list(substitute(iterator)), envir = parent.frame())

    res <- .gcall("gbins_summary", exprs, breaks, include.lowest, intervals, .iterator, band, .misha_env())
    attr(res, "breaks") <- breaks
    res
}


.gseq.import <- function(groot = NULL, path = NULL) {
    chroms <- c()
    files <- c()
    tmp.dirname <- tempfile(pattern = "", tmpdir = paste(groot, "/downloads", sep = ""))
    if (!dir.create(tmp.dirname, recursive = TRUE, mode = "0777")) {
        stop(sprintf("Failed to create a directory %s", tmp.dirname), call. = FALSE)
    }

    tryCatch(
        {
            files <- c()

            ftp_files <- grep("^ftp://", path, perl = TRUE, value = TRUE)
            other_files <- setdiff(path, ftp_files)
            if (length(other_files) + length(ftp_files) != length(path)) {
                stop("Some paths are not supported", call. = FALSE)
            }
            if (length(ftp_files) > 0) {
                files <- c(files, gwget(ftp_files, tmp.dirname))
            }
            if (any(!file.exists(other_files))) {
                stop("Some files do not exist", call. = FALSE)
            }
            files <- c(files, other_files)

            message("Building Seq files...")
            fastas <- files[grep("^chr.+$", basename(files), perl = TRUE)]
            for (fasta in fastas) {
                chrom <- gsub("^chr(\\w+)(\\..*)*$", "\\1", basename(fasta), perl = TRUE)
                if (!is.na(match(chrom, chroms))) {
                    next
                }

                fasta.original <- fasta

                if (length(grep("^.+\\.gz$", fasta, perl = TRUE))) {
                    .chrom <- basename(gsub("^(.+)\\.gz$", "\\1", fasta, perl = TRUE))
                    fasta.unzipped <- paste(tmp.dirname, "/", .chrom, sep = "")
                    cmd <- paste("/bin/sh -c \"gunzip -q -c", fasta, ">", fasta.unzipped, "\"")
                    if (system(cmd)) {
                        stop(sprintf("Command failed: %s", cmd), call. = FALSE)
                    }
                    fasta <- fasta.unzipped
                }

                seq <- sprintf("%s/seq/chr%s.seq", groot, chrom)

                message(sprintf("chr%s", chrom))
                .gcall("gseqimport", fasta, seq, .misha_env(), silent = TRUE)

                chroms <- c(chroms, chrom)
            }
        },
        finally = {
            unlink(tmp.dirname, recursive = TRUE)
        }
    )
    chroms
}





#' Returns DNA sequences
#'
#' Returns DNA sequences for given intervals
#'
#' This function returns an array of sequence strings for each interval from
#' 'intervals'. If intervals contain an additional 'strand' column and its
#' value is '-1', the reverse-complementary sequence is returned.
#'
#' @param intervals intervals for which DNA sequence is returned
#' @return An array of character strings representing DNA sequence.
#' @seealso \code{\link{gextract}}
#' @keywords ~extract ~DNA ~sequence
#' @examples
#' \dontshow{
#' options(gmax.processes = 2)
#' }
#'
#' gdb.init_examples()
#' intervs <- gintervals(c(1, 2), 10000, 10020)
#' gseq.extract(intervs)
#'
#' @export gseq.extract
gseq.extract <- function(intervals = NULL) {
    if (is.null(intervals)) {
        stop("Usage: gseq.extract(intervals)", call. = FALSE)
    }
    .gcheckroot()

    intervals <- rescue_ALLGENOME(intervals, as.character(substitute(intervals)))

    res <- .gcall("gseqread", intervals, .misha_env())
    res
}




#' Computes auto-correlation between the strands for a file of mapped sequences
#'
#' Calculates auto-correlation between plus and minus strands for the given
#' chromosome in a file of mapped sequences.
#'
#' This function calculates auto-correlation between plus and minus strands for
#' the given chromosome in a file of mapped sequences. Each line in the file
#' describes one read. Each column is separated by a TAB character.
#'
#' The following columns must be presented in the file: sequence, chromosome,
#' coordinate and strand. The position of these columns are controlled by
#' 'cols.order' argument accordingly. The default value of 'cols.order' is a
#' vector (9,11,13,14) meaning that sequence is expected to be found at column
#' number 9, chromosome - at column 11, coordinate - at column 13 and strand -
#' at column 14. The first column should be referenced by 1 and not by 0.
#'
#' Coordinates that are not in [min.coord, max.coord] range are ignored.
#'
#' gcompute_strands_autocorr outputs the total statistics and the
#' auto-correlation given by bins. The size of the bin is indicated by
#' 'binsize' parameter. Statistics is calculated for bins in the range of
#' [-maxread, maxread].
#'
#' @param file the name of the file containing mapped sequences
#' @param chrom chromosome for which the auto-correlation is computed
#' @param binsize calculate the auto-correlation for bins in the range of
#' [-maxread, maxread]
#' @param maxread maximal length of the sequence used for statistics
#' @param cols.order order of sequence, chromosome, coordinate and strand
#' columns in file
#' @param min.coord minimal coordinate used for statistics
#' @param max.coord maximal coordinate used for statistics
#' @return Statistics for each strand and auto-correlation by given bins.
#' @keywords ~gcompute_strands_autocorr ~auto-correlation ~autocorrelation
#' ~correlation
#' @examples
#' \dontshow{
#' options(gmax.processes = 2)
#' }
#'
#' gdb.init_examples()
#' gcompute_strands_autocorr(paste(.misha$GROOT, "reads", sep = "/"),
#'     "chr1", 50,
#'     maxread = 300
#' )
#'
#' @export gcompute_strands_autocorr
gcompute_strands_autocorr <- function(file = NULL, chrom = NULL, binsize = NULL, maxread = 400, cols.order = c(9, 11, 13, 14), min.coord = 0, max.coord = 3e+8) {
    if (is.null(file) || is.null(chrom) || is.null(binsize)) {
        stop("Usage: gcompute_strands_autocorr(file, chrom, binsize, maxread = 400, cols.order = c(9, 11, 13, 14), min.coord = 0, max.coord = 3e+8)", call. = FALSE)
    }
    .gcheckroot()

    if (substr(chrom, 1, 3) != "chr") {
        chrom <- paste("chr", chrom, sep = "")
    }

    res <- .gcall("C_gcompute_strands_autocorr", file, chrom, binsize, maxread, cols.order, min.coord, max.coord, .misha_env())
    res
}
