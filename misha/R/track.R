# tries to locate track name in a path, return the track name
# examples:
#   .gfindtrackinpath("aaa/bbb/ccc.track/ddd/eee")       returns "aaa.bbb.ccc"
#   .gfindtrackinpath("aaa/bbb/ccc.track/ddd.track/eee") returns "aaa.bbb.ccc"
#   .gfindtrackinpath("aaa/bbb/ccc/ddd/eee")             returns NULL
.gfindtrackinpath <- function(path) {
    dirs <- unlist(strsplit(path, split = "/"))
    r <- grep("\\.track$", unlist(dirs))
    if (length(r) > 0) {
        idx <- r[1]
        dirs[idx] <- paste(substr(dirs[idx], 0, nchar(dirs[idx]) - nchar(".tracks") + 1))
        return(paste(dirs[1:idx], collapse = "."))
    }
    NULL
}


.gtrack.prepare.pvals <- function(track) {
    .gcheckroot()

    trackstr <- do.call(.gexpr2str, list(substitute(track)), envir = parent.frame())

    if (is.na(match(trackstr, get("GTRACKS", envir = .misha)))) {
        stop(sprintf("Track %s does not exist", trackstr), call. = FALSE)
    }

    quantile.edge.data.size <- .ggetOption("gquantile.edge.data.size", 100000)
    middle.size <- .ggetOption("gpv.middle.size", 0.98)
    edge.size <- (1 - middle.size) / 2
    middle.precision <- .ggetOption("gpv.middle.precision", 10^(-4))
    edge.precision <- .ggetOption("gpv.edge.precision", 10^(-9))

    # In the middle section the percentiles increase by a constant step = middle.precision.
    # At the edges the step is exponential. For instance, step i at the edge close to 0 follows the following function:
    #   k+i
    #  b     , where b and k are unknown. We also don't know n - the number of total steps required to cover the edge.
    #
    # b, k, n be calculated from the following:
    #
    # 1. b^(k+1) - b^k = edge.precision
    # 2. b^(k+n) - b^(k+n-1) = middle.precision
    # 3. b^(k+n) - b^k = edge.size

    b <- (edge.precision - edge.size) / (middle.precision - edge.size)
    k <- log(edge.precision / (b - 1), b)
    n <- ceiling(log(edge.size + b^k, b) - k)

    percentiles <- 0
    percentiles <- c(percentiles, b^(k + (0:n)) - b^k)
    percentiles <- c(percentiles, 1 - percentiles)
    num.middle.steps <- middle.size / middle.precision
    percentiles <- c(percentiles, edge.size + (0:num.middle.steps) * middle.precision)
    percentiles <- sort(percentiles)

    selected.percentiles <- NULL

    multitasking <- .ggetOption("gmultitasking")
    on.exit(options(gmultitasking = multitasking))
    tryCatch(
        {
            suppressWarnings({ # disable warnings since gquantiles is going to warn about random sampling
                options(gmultitasking = FALSE)
                quantiles <- do.call(gquantiles, list(substitute(track), percentiles = c(0, percentiles)), envir = parent.frame())
                names(quantiles) <- NULL
                minval <- quantiles[1]
                maxval <- quantiles[length(quantiles)]
                quantiles <- quantiles[2:length(quantiles)]

                # for each group of quantiles with identical value choose the maximal one
                selected.percentiles <- sapply(split(percentiles, quantiles), max)
                names(selected.percentiles) <- NULL

                # if all percentiles are equal create an artificial table
                if (length(selected.percentiles) == 1) {
                    selected.percentiles <- c(1, 1)
                    attr(selected.percentiles, "breaks") <- c(minval, maxval + 1)
                } else {
                    indices <- match(selected.percentiles, percentiles)
                    selected.quantiles <- quantiles[indices]
                    attr(selected.percentiles, "breaks") <- selected.quantiles
                }

                attr(selected.percentiles, "minval") <- minval
                attr(selected.percentiles, "maxval") <- maxval
            })
        },
        finally = {
            options(gmultitasking = multitasking)
        }
    )

    # save the percentiles
    .gtrack.var.set(trackstr, "pv.percentiles", selected.percentiles)

    retv <- 0
}



#' Converts a track to the most current format
#'
#' Converts a track (if needed) to the most current format.
#'
#' This function converts a track to the most current format. It should be used
#' if a track created by an old version of the library cannot be read anymore
#' by the newer version. The old track is given by 'src.track'. After
#' conversion a new track 'tgt.track' is created. If 'tgt.track' is 'NULL' the
#' source track is overwritten.
#'
#' @param src.track source track name
#' @param tgt.track target track name. If 'NULL' the source track is
#' overwritten.
#' @return None
#' @seealso \code{\link{gtrack.create}}, \code{\link{gtrack.2d.create}},
#' \code{\link{gtrack.create_sparse}}
#' @keywords ~track ~convert
#' @export gtrack.convert
gtrack.convert <- function(src.track = NULL, tgt.track = NULL) {
    if (is.null(substitute(src.track))) {
        stop("Usage: gtrack.convert(src.track, tgt.track = NULL)", call. = FALSE)
    }
    .gcheckroot()

    src.trackstr <- do.call(.gexpr2str, list(substitute(src.track)), envir = parent.frame())
    if (is.na(match(src.trackstr, get("GTRACKS", envir = .misha)))) {
        stop(sprintf("Track %s does not exist", src.trackstr), call. = FALSE)
    }

    tgt.trackstr <- ""
    if (is.null(substitute(tgt.track))) {
        tgt.trackstr <- paste(src.trackstr, "_converted", sep = "")
        counter <- 2
        while (!is.na(match(tgt.trackstr, get("GTRACKS", envir = .misha)))) {
            tgt.trackstr <- paste(src.trackstr, "_converted", counter, sep = "")
            counter <- counter + 1
        }
    } else {
        tgt.trackstr <- do.call(.gexpr2str, list(substitute(tgt.track)), envir = parent.frame())
        .gconfirmtrackcreate(tgt.trackstr)
    }

    src.dirname <- sprintf("%s.track", paste(get("GWD", envir = .misha), gsub("\\.", "/", src.trackstr), sep = "/"))
    tgt.dirname <- sprintf("%s.track", paste(get("GWD", envir = .misha), gsub("\\.", "/", tgt.trackstr), sep = "/"))
    success <- FALSE

    tryCatch(
        {
            .gcall("gtrackconvert", src.trackstr, tgt.trackstr, .misha_env(), silent = TRUE)

            # copy all supplimentary data of a track (vars, etc.)
            if (!system(sprintf("cp -r -u %s/. %s", src.dirname, tgt.dirname))) {
                # if tgt track is null move it to the source track
                if (is.null(substitute(tgt.track))) {
                    unlink(src.dirname, recursive = TRUE)
                    success <- TRUE
                    file.rename(tgt.dirname, src.dirname)
                }
            } else {
                msg <- sprintf("Failed to copy some or all track supplementary data from %s to %s", src.dirname, tgt.dirname)
                if (is.null(substitute(tgt.track))) {
                    msg <- paste(msg,
                        sprintf(
                            "Track %s will remain unchanged.\nA new converted track named %s was created without supplementary data.",
                            src.trackstr, tgt.trackstr
                        ),
                        sep = "\n"
                    )
                }
                warning(msg, call. = FALSE)
            }
            success <- TRUE
        },
        finally = {
            if (!success) {
                unlink(tgt.dirname, recursive = TRUE)
            }
            .gdb.rm_track(tgt.trackstr)
        }
    )
    retv <- 0 # suppress return value
}



#' Creates a track from a track expression
#'
#' Creates a track from a track expression.
#'
#' This function creates a new track named track. The values of the track are
#' determined by evaluation of 'expr' - a numeric track expression. The type of
#' the new track is determined by the type of the iterator. 'Fixed bin',
#' 'Sparse' or 'Rectangles' track can be created accordingly. 'description' is
#' added as a track attribute.
#'
#' @param track track name
#' @param description a character string description
#' @param expr track expression
#' @param iterator track expression iterator. If 'NULL' iterator is determined
#' implicitly based on track expression.
#' @param band track expression band. If 'NULL' no band is used.
#' @return None.
#' @seealso \code{\link{gtrack.2d.create}}, \code{\link{gtrack.create_sparse}},
#' \code{\link{gtrack.smooth}}, \code{\link{gtrack.modify}},
#' \code{\link{gtrack.rm}}, \code{\link{gtrack.info}},
#' \code{\link{gdir.create}}
#' @keywords ~create ~track
#' @examples
#' \dontshow{
#' options(gmax.processes = 2)
#' }
#'
#' gdb.init_examples()
#'
#' ## Creates a new track that is a sum of values from 'dense' and
#' ## 2 * non-nan values of 'sparse' track. The new track type is
#' ## Dense with a bin size that equals to '70'.
#' gtrack.create("mixed_track", "Test track",
#'     "dense_track +
#'               replace(sparse_track, is.nan(sparse_track), 0) * 2",
#'     iterator = 70
#' )
#' gtrack.info("mixed_track")
#' gtrack.rm("mixed_track", force = TRUE)
#'
#' @export gtrack.create
gtrack.create <- function(track = NULL, description = NULL, expr = NULL, iterator = NULL, band = NULL) {
    if (is.null(substitute(track)) || is.null(description) || is.null(substitute(expr))) {
        stop("Usage: gtrack.create(track, description, expr, iterator = NULL, band = NULL)", call. = FALSE)
    }
    .gcheckroot()

    trackstr <- do.call(.gexpr2str, list(substitute(track)), envir = parent.frame())
    exprstr <- do.call(.gexpr2str, list(substitute(expr)), envir = parent.frame())
    .iterator <- do.call(.giterator, list(substitute(iterator)), envir = parent.frame())
    trackdir <- sprintf("%s.track", paste(get("GWD", envir = .misha), gsub("\\.", "/", trackstr), sep = "/"))

    direxisted <- file.exists(trackdir)

    if (!is.na(match(trackstr, get("GTRACKS", envir = .misha)))) {
        stop(sprintf("Track %s already exists", trackstr), call. = FALSE)
    }

    .gconfirmtrackcreate(trackstr)
    success <- FALSE
    tryCatch(
        {
            if (.ggetOption("gmultitasking")) {
                .gcall("gtrackcreate_multitask", trackstr, exprstr, .iterator, band, .misha_env(), silent = TRUE)
            } else {
                .gcall("gtrackcreate", trackstr, exprstr, .iterator, band, .misha_env(), silent = TRUE)
            }
            .gdb.add_track(trackstr)
            .gtrack.attr.set(
                trackstr, "created.by",
                sprintf("gtrack.create(%s, description, %s, iterator=%s)", trackstr, exprstr, deparse(substitute(iterator), width.cutoff = 500)[1]), TRUE
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



#' Creates a new track from PSSM energy function
#'
#' Creates a new track from PSSM energy function.
#'
#' This function creates a new track with values of a PSSM energy function.
#' PSSM parameters (nucleotide probability per position and pluralization) are
#' determined by 'pssmset' key and data files ('pssmset.key' and
#' 'pssmset.data'). These two files must be located in 'GROOT/pssms' directory.
#' The type of the created track is determined by the type of the iterator.
#' 'description' is added as a track attribute.
#'
#' @param track track name
#' @param description a character string description
#' @param pssmset name of PSSM set: 'pssmset.key' and 'pssmset.data' must be
#' presented in 'GROOT/pssms' directory
#' @param pssmid PSSM id
#' @param prior prior
#' @param iterator track expression iterator for the newly created track
#' @return None.
#' @seealso \code{\link{gtrack.create}}, \code{\link{gtrack.2d.create}},
#' \code{\link{gtrack.create_sparse}}, \code{\link{gtrack.smooth}},
#' \code{\link{gtrack.modify}}, \code{\link{gtrack.rm}},
#' \code{\link{gtrack.info}}, \code{\link{gdir.create}}
#' @keywords ~energy ~pssm ~pwm ~track
#' @examples
#' \donttest{
#' gdb.init_examples()
#' gtrack.create_pwm_energy("pwm_energy_track", "Test track", "pssm",
#'     3, 0.01,
#'     iterator = 100
#' )
#' gextract("pwm_energy_track", gintervals(1, 0, 1000))
#' }
#'
#' @export gtrack.create_pwm_energy
gtrack.create_pwm_energy <- function(track = NULL, description = NULL, pssmset = NULL, pssmid = NULL, prior = NULL, iterator = NULL) {
    if (is.null(substitute(track)) || is.null(description) || is.null(pssmset) || is.null(pssmid) || is.null(prior) || is.null(iterator)) {
        stop("Usage: gtrack.create_pwm_energy(track, description, pssmset, pssmid, prior, iterator)", call. = FALSE)
    }
    .gcheckroot()

    trackstr <- do.call(.gexpr2str, list(substitute(track)), envir = parent.frame())
    trackdir <- sprintf("%s.track", paste(get("GWD", envir = .misha), gsub("\\.", "/", trackstr), sep = "/"))
    direxisted <- file.exists(trackdir)
    .iterator <- do.call(.giterator, list(substitute(iterator)), envir = parent.frame())

    if (!is.na(match(trackstr, get("GTRACKS", envir = .misha)))) {
        stop(sprintf("Track %s already exists", trackstr), call. = FALSE)
    }

    .gconfirmtrackcreate(trackstr)
    success <- FALSE
    tryCatch(
        {
            if (.ggetOption("gmultitasking")) {
                .gcall("gcreate_pwm_energy_multitask", trackstr, pssmset, pssmid, prior, .iterator, .misha_env(), silent = TRUE)
            } else {
                .gcall("gcreate_pwm_energy", trackstr, pssmset, pssmid, prior, .iterator, .misha_env(), silent = TRUE)
            }
            .gdb.add_track(trackstr)
            .gtrack.attr.set(
                trackstr, "created.by",
                sprintf(
                    "gtrack.create_pwm_energy(%s, description, \"%s\", %g, %g, iterator=%s)",
                    trackstr, pssmset, as.numeric(pssmid), as.numeric(prior), deparse(substitute(iterator), width.cutoff = 500)[1]
                ), TRUE
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



#' Creates a 'Sparse' track from intervals and values
#'
#' Creates a 'Sparse' track from intervals and values.
#'
#' This function creates a new 'Sparse' track with values at given intervals.
#' 'description' is added as a track attribute.
#'
#' @param track track name
#' @param description a character string description
#' @param intervals a set of one-dimensional intervals
#' @param values an array of numeric values - one for each interval
#' @return None.
#' @seealso \code{\link{gtrack.create}}, \code{\link{gtrack.2d.create}},
#' \code{\link{gtrack.smooth}}, \code{\link{gtrack.modify}},
#' \code{\link{gtrack.rm}}, \code{\link{gtrack.info}},
#' \code{\link{gdir.create}}
#' @keywords ~create ~sparse ~track
#' @examples
#' \dontshow{
#' options(gmax.processes = 2)
#' }
#'
#' gdb.init_examples()
#' intervs <- gintervals.load("annotations")
#' gtrack.create_sparse(
#'     "test_sparse", "Test track", intervs,
#'     1:dim(intervs)[1]
#' )
#' gextract("test_sparse", .misha$ALLGENOME)
#' gtrack.rm("test_sparse", force = TRUE)
#'
#' @export gtrack.create_sparse
gtrack.create_sparse <- function(track = NULL, description = NULL, intervals = NULL, values = NULL) {
    if (is.null(substitute(track)) || is.null(description) || is.null(intervals) || is.null(values)) {
        stop("Usage: gtrack.create_sparse(track, description, intervals, values)", call. = FALSE)
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
            .gcall("gtrack_create_sparse", trackstr, intervals, values, .misha_env(), silent = TRUE)
            .gdb.add_track(trackstr)
            .gtrack.attr.set(trackstr, "created.by", sprintf("gtrack.create_sparse(%s, description, %s, %s)", trackstr, intervalsstr, valuesstr), TRUE)
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



#' Tests for a track existence
#'
#' Tests for a track existence.
#'
#' This function returns 'TRUE' if a track exists in Genomic Database.
#'
#' @param track track name
#' @return 'TRUE' if a track exists. Otherwise 'FALSE'.
#' @seealso \code{\link{gtrack.ls}}, \code{\link{gtrack.info}},
#' \code{\link{gtrack.create}}, \code{\link{gtrack.rm}}
#' @keywords ~track
#' @examples
#' \dontshow{
#' options(gmax.processes = 2)
#' }
#'
#' gdb.init_examples()
#' gtrack.exists("dense_track")
#'
#' @export gtrack.exists
gtrack.exists <- function(track = NULL) {
    if (is.null(substitute(track))) {
        stop("Usage: gtrack.exists(track)", call. = FALSE)
    }
    .gcheckroot()

    trackstr <- do.call(.gexpr2str, list(substitute(track)), envir = parent.frame())
    !is.na(match(trackstr, get("GTRACKS", envir = .misha)))
}


get_bigWigToWig_bin <- function() {
    dir <- tempdir()
    utils::untar(system.file("bigWigToWig.tar.gz", package = "misha"), exdir = dir)
    return(file.path(dir, "bigWigToWig"))
}


#' Creates a track from WIG / BigWig / BedGraph / tab-delimited file
#'
#' Creates a track from WIG / BigWig / BedGraph / tab-delimited file
#'
#' This function creates a track from WIG / BigWig / BedGraph / tab-delimited
#' file.  One can learn about the format of the tab-delimited file by running
#' 'gextract' function on a 1D track with a 'file' parameter set to the name of
#' the file. Zipped files are supported (file name must have '.gz' or '.zip'
#' suffix).
#'
#' If 'binsize' is 0 the resulted track is created in 'Sparse' format.
#' Otherwise the 'Dense' format is chosen with a bin size equal to 'binsize'.
#' The values that were not defined in input file file are substituted by
#' 'defval' value.
#'
#' 'description' is added as a track attribute.
#'
#' @param track track name
#' @param description a character string description
#' @param file file path
#' @param binsize bin size of the newly created 'Dense' track or '0' for a
#' 'Sparse' track
#' @param defval default track value
#' @return None.
#' @seealso \code{\link{gtrack.import_set}}, \code{\link{gtrack.rm}},
#' \code{\link{gtrack.info}}, \code{\link{gdir.create}}, \code{\link{gextract}}
#' @keywords ~wig ~bigwig ~bedgraph ~track
#' @export gtrack.import
gtrack.import <- function(track = NULL, description = NULL, file = NULL, binsize = NULL, defval = NaN) {
    if (is.null(substitute(track)) || is.null(description) || is.null(file)) {
        stop("Usage: gtrack.import(track, description, file, binsize, defval = NaN)", call. = FALSE)
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

    tmp.dirname <- ""
    file.original <- file

    tryCatch(
        {
            report.progress <- FALSE

            if (length(grep("^.+\\.gz$", file, perl = TRUE)) || length(grep("^.+\\.zip$", file, perl = TRUE))) {
                message("Unzipping...\n")
                report.progress <- TRUE
                tmp.dirname <- tempfile()
                if (!dir.create(tmp.dirname, recursive = TRUE, mode = "0777")) {
                    stop(sprintf("Failed to create a directory %s", tmp.dirname), call. = FALSE)
                }

                file.noext <- basename(gsub("^(.+)\\.(.+)$", "\\1", file, perl = TRUE))
                file.unzipped <- paste(tmp.dirname, "/", file.noext, sep = "")
                if (system(paste("/bin/sh -c \"gunzip -q -c", file, ">", file.unzipped, "\""))) {
                    stop(sprintf("Failed to unzip file %s", file), call. = FALSE)
                }
                file <- file.unzipped
            }

            # looks like all bigWig files start with "fc26" in their first two bytes
            if (length(grep("^.+\\.bw$", file, perl = TRUE)) || length(grep("^.+\\.bigWig$", file, perl = TRUE)) ||
                system(sprintf("od -x -N 2 \"%s\"", file), intern = TRUE)[1] == "0000000 fc26") {
                message("Converting from BigWig to WIG...\n")
                report.progress <- TRUE
                if (tmp.dirname == "") {
                    tmp.dirname <- tempfile()
                    if (!dir.create(tmp.dirname, recursive = TRUE, mode = "0777")) {
                        stop(sprintf("Failed to create a directory %s", tmp.dirname), call. = FALSE)
                    }
                }

                file.noext <- basename(gsub("^(.+)\\.(.+)$", "\\1", file, perl = TRUE))
                file.converted <- paste(tmp.dirname, "/", file.noext, ".wig", sep = "")
                if (paste(system(get_bigWigToWig_bin(), file, file.converted))) {
                    stop("Command failed", call. = FALSE)
                }
                file <- file.converted
            }

            if (report.progress) {
                message("Converting to track...\n")
            }

            .gcall("gtrackimportwig", trackstr, file, binsize, defval, .misha_env(), silent = TRUE)
            .gdb.add_track(trackstr)
            .gtrack.attr.set(
                trackstr, "created.by",
                sprintf("gtrack.import(%s, description, \"%s\", %d, %g)", trackstr, file.original, binsize, defval), TRUE
            )
            .gtrack.attr.set(trackstr, "created.date", date(), TRUE)
            .gtrack.attr.set(trackstr, "description", description, TRUE)
            success <- TRUE
        },
        finally = {
            if (tmp.dirname != "") {
                unlink(tmp.dirname, recursive = TRUE)
            }

            if (!success && !direxisted) {
                unlink(trackdir, recursive = TRUE)
                .gdb.rm_track(trackstr)
            }
        }
    )
    retv <- 0 # suppress return value
}



#' Creates a track from a file of mapped sequences
#'
#' Creates a track from a file of mapped sequences.
#'
#' This function creates a track from a file of mapped sequences. The file can
#' be in SAM format or in a general TAB delimited text format where each line
#' describes a single read.
#'
#' For a SAM file 'cols.order' must be set to 'NULL'.
#'
#' For a general TAB delimited text format the following columns must be
#' presented in the file: sequence, chromosome, coordinate and strand. The
#' position of these columns should be specified in 'cols.order' argument. The
#' default value of 'cols.order' is an array of (9, 11, 13, 14) meaning that
#' sequence is expected to be found at column number 9, chromosome - at column
#' 11, coordinate - at column 13 and strand - at column 14. The column indices
#' are 1-based, i.e. the first column is referenced by 1. Chromosome needs a
#' prefix 'chr' e.g. 'chr1'. Valid strand values are '+' or 'F' for forward
#' strand and '-' or 'R' for the reverse strand.
#'
#' Each read at given coordinate can be "expanded" to cover an interval rather
#' than a single point. The length of the interval is controlled by 'pileup'
#' argument. The direction of expansion depends on the strand value. If
#' 'pileup' is '0', no expansion is performed and the read is converted to a
#' single point. The track is created in sparse format. If 'pileup' is greater
#' than zero, the output track is in dense format. 'binsize' controls the bin
#' size of the dense track.
#'
#' If 'remove.dups' is 'TRUE' the duplicated coordinates are counted only once.
#'
#' 'description' is added as a track attribute.
#'
#' 'gtrack.import_mappedseq' returns the statistics of the conversion process.
#'
#' @param track track name
#' @param description a character string description
#' @param file name of mapped sequences file
#' @param pileup interval expansion
#' @param binsize bin size of a dense track
#' @param cols.order order of sequence, chromosome, coordinate and strand
#' columns in mapped sequences file or NULL if SAM file is used
#' @param remove.dups if 'TRUE' the duplicated coordinates are counted only
#' once.
#' @return A list of conversion process statistics.
#' @seealso \code{\link{gtrack.rm}}, \code{\link{gtrack.info}},
#' \code{\link{gdir.create}}
#' @keywords ~mapped ~sequence ~track
#' @export gtrack.import_mappedseq
gtrack.import_mappedseq <- function(track = NULL, description = NULL, file = NULL, pileup = 0, binsize = -1, cols.order = c(9, 11, 13, 14), remove.dups = TRUE) {
    if (is.null(substitute(track)) || is.null(description) || is.null(file)) {
        stop("Usage: gtrack.import_mappedseq(track, description, file, pileup = 0, binsize = -1, cols.order = c(9, 11, 13, 14), remove.dups = TRUE)", call. = FALSE)
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
            retv <- .gcall("gtrackimport_mappedseq", trackstr, file, pileup, binsize, cols.order, remove.dups, .misha_env(), silent = TRUE)
            .gdb.add_track(trackstr)
            .gtrack.attr.set(
                trackstr, "created.by",
                sprintf("gtrack.import_mappedseq(%s, description, \"%s\", pileup=%d, binsize=%d, remove.dups=%s)", trackstr, file, pileup, binsize, remove.dups), TRUE
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
    retv
}



#' Creates one or more tracks from multiple WIG / BigWig / BedGraph /
#' tab-delimited files on disk or FTP
#'
#' Creates one or more tracks from WIG / BigWig / BedGraph / tab-delimited
#' files on disk or FTP.
#'
#' This function is similar to 'gtrack.import' however unlike the latter it can
#' create multiple tracks. Additionally the files can be fetched from an FTP
#' server.
#'
#' The files are expected to be in WIG / BigWig / BedGraph / tab-delimited
#' formats. One can learn about the format of the tab-delimited file by running
#' 'gextract' function with a 'file' parameter set to the name of the file.
#' Zipped files are supported (file name must have '.gz' or '.zip' suffix).
#'
#' Files are specified by 'path' argument. 'path' can be also a URL of an FTP
#' server in the form of 'ftp://[address]/[files]'. If 'path' is a URL, the
#' files are first downloaded from FTP server to a temporary directory and then
#' imported to tracks. The temporary directory is created at 'GROOT/downloads'.
#'
#' Regardless whether 'path' is file path or to a URL, it can contain
#' wildcards. Hence multiple files can be imported (and downloaded) at once.
#'
#' If 'binsize' is 0 the resulted tracks are created in 'Sparse' format.
#' Otherwise the 'Dense' format is chosen with a bin size equal to 'binsize'.
#' The values that were not defined in input file file are substituted by
#' 'defval' value.
#'
#' The name of a each created track is of '[track.prefix][filename]' form,
#' where 'filename' is the name of the WIG file. For example, if 'track.prefix'
#' equals to "wigs."" and an input file name is 'mydata', a track named
#' 'wigs.mydata' is created. If 'track.prefix' is 'NULL' no prefix is appended
#' to the name of the created track.
#'
#' Existing tracks are not overwritten and no new directories are automatically
#' created.
#'
#' 'description' is added to the created tracks as an attribute.
#'
#' 'gtrack.import_set' does not stop if an error occurs while importing a file.
#' It rather continues importing the rest of the files.
#'
#' 'gtrack.import_set' returns the names of the files that were successfully
#' imported and those that failed.
#'
#' @param path file path or URL (may contain wildcards)
#' @param description a character string description
#' @param binsize bin size of the newly created 'Dense' track or '0' for a
#' 'Sparse' track
#' @param track.prefix prefix for a track name
#' @param defval default track value
#' @return Names of files that were successfully imported and those that
#' failed.
#' @seealso \code{\link{gtrack.import}}, \code{\link{gwget}},
#' \code{\link{gtrack.rm}}, \code{\link{gtrack.info}},
#' \code{\link{gdir.create}}, \code{\link{gextract}}
#' @keywords ~wig ~bigwig ~bedgraph ~track
#' @export gtrack.import_set
gtrack.import_set <- function(description = NULL, path = NULL, binsize = NULL, track.prefix = NULL, defval = NaN) {
    .gcheckroot()

    if (is.null(description) || is.null(path) || is.null(binsize)) {
        stop("Usage: gtrack.import_set(description, path, binsize, track.prefix = NULL, defval = NaN)", call. = FALSE)
    }

    if (is.null(substitute(track.prefix))) {
        track.prefix <- ""
    } else {
        track.prefix <- do.call(.gexpr2str, list(substitute(track.prefix)), envir = parent.frame())
    }

    files <- c()
    tmp.dirname <- ""

    tryCatch(
        {
            tmp.dirname <- tempfile(pattern = "", tmpdir = paste(get("GROOT", envir = .misha), "/downloads", sep = ""))
            if (!dir.create(tmp.dirname, recursive = TRUE, mode = "0777")) {
                stop(sprintf("Failed to create a directory %s", tmp.dirname), call. = FALSE)
            }
            protocol <- "ftp://"
            if (substr(path, 1, nchar(protocol)) == protocol) {
                # ftp
                files <- gwget(path, tmp.dirname)

                if (!length(files)) {
                    stop("No files downloaded. Exiting.", call. = FALSE)
                }
            } else {
                # local path
                files <- system(paste("/bin/sh -c \"ls -d -A", path, "\""), intern = TRUE)
            }

            files <- files[!file.info(files)$isdir]
            if (!length(files)) {
                stop("No files to import. Exiting.", call. = FALSE)
            }

            files.imported <- c()

            for (file in files) {
                tryCatch(
                    {
                        message(sprintf("Importing file %s", file))
                        file.noext <- basename(gsub("^([^.]+)(\\..*)*$", "\\1", file, perl = TRUE))
                        trackstr <- paste(track.prefix, file.noext, sep = "")

                        .gcall_noninteractive(gtrack.import, trackstr, description, file, binsize, defval)
                        files.imported <- c(files.imported, file)
                        success <- TRUE
                    },
                    error = function(e) {
                        msg <- as.character(e)
                        if (msg == "Error: Command interrupted!\n") {
                            stop("Command interrupted!", call. = FALSE)
                        } else {
                            message(sprintf("%s", msg))
                        }
                    }
                )
            }

            files <- basename(files)
            if (length(files.imported)) {
                files.imported <- basename(files.imported)
            }
            files.failed <- setdiff(files, files.imported)
            res <- new.env()
            if (length(files.failed)) {
                res$files.failed <- files.failed
            }
            if (length(files.imported)) {
                res$files.imported <- files.imported
            }
            as.list(res)
        },
        finally = {
            unlink(tmp.dirname, recursive = TRUE)
        }
    )
}



#' Returns information about a track
#'
#' Returns information about a track.
#'
#' Returns information about the track (type, dimensions, size in bytes, etc.).
#' The fields in the returned value vary depending on the type of the track.
#'
#' @param track track name
#' @return A list that contains track properties
#' @seealso \code{\link{gtrack.exists}}, \code{\link{gtrack.ls}}
#' @keywords ~track ~info ~property
#' @examples
#' \dontshow{
#' options(gmax.processes = 2)
#' }
#'
#' gdb.init_examples()
#' gtrack.info("dense_track")
#' gtrack.info("rects_track")
#'
#' @export gtrack.info
gtrack.info <- function(track = NULL) {
    if (is.null(substitute(track))) {
        stop("Usage: gtrack.info(track)", call. = FALSE)
    }
    .gcheckroot()

    trackstr <- do.call(.gexpr2str, list(substitute(track)), envir = parent.frame())
    .gcall("gtrackinfo", trackstr, .misha_env())
}



#' Imports a track from another assembly
#'
#' Imports a track from another assembly.
#'
#' This function imports a track located in 'src.track.dir' of another assembly
#' to the current database. Chain file instructs how the conversion of
#' coordinates should be done. It can be either a name of a chain file or a
#' data frame in the same format as returned by 'gintervals.load_chain'
#' function. The name of the newly created track is specified by 'track'
#' argument and 'description' is added as a track attribute.
#'
#' @param track name of a created track
#' @param description a character string description
#' @param src.track.dir path to the directory of the source track
#' @param chain name of chain file or data frame as returned by
#' 'gintervals.load_chain'
#' @return None.
#' @seealso \code{\link{gintervals.load_chain}},
#' \code{\link{gintervals.liftover}}
#' @keywords ~track ~liftover ~chain
#' @export gtrack.liftover
gtrack.liftover <- function(track = NULL, description = NULL, src.track.dir = NULL, chain = NULL) {
    if (is.null(substitute(track)) || is.null(description) || is.null(src.track.dir) || is.null(chain)) {
        stop("Usage: gtrack.liftover(track, description, src.track.dir, chain)", call. = FALSE)
    }
    .gcheckroot()

    trackstr <- do.call(.gexpr2str, list(substitute(track)), envir = parent.frame())
    trackdir <- sprintf("%s.track", paste(get("GWD", envir = .misha), gsub("\\.", "/", trackstr), sep = "/"))

    direxisted <- file.exists(trackdir)

    if (!is.na(match(trackstr, get("GTRACKS", envir = .misha)))) {
        stop(sprintf("Track %s already exists", trackstr), call. = FALSE)
    }

    if (is.character(chain)) {
        chain.intervs <- gintervals.load_chain(chain)
    } else {
        chain.intervs <- chain
    }

    .gconfirmtrackcreate(trackstr)
    success <- FALSE
    tryCatch(
        {
            .gcall("gtrack_liftover", trackstr, src.track.dir, chain.intervs, .misha_env(), silent = TRUE)
            .gdb.add_track(trackstr)
            if (is.character(chain)) {
                .gtrack.attr.set(trackstr, "created.by", sprintf("gtrack.liftover(%s, description, \"%s\", \"%s\")", trackstr, src.track.dir, chain), TRUE)
            } else {
                .gtrack.attr.set(trackstr, "created.by", sprintf("gtrack.liftover(%s, description, \"%s\", chain)", trackstr, src.track.dir), TRUE)
            }
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



#' Creates a new track from a lookup table based on track expression
#'
#' Evaluates track expression and translates the values into bin indices that
#' are used in turn to retrieve values from a lookup table and create a track.
#'
#' This function evaluates the track expression for all iterator intervals and
#' translates this value into an index based on the breaks. This index is then
#' used to address the lookup table and create with its values a new track.
#' More than one 'expr'-'breaks' pair can be used. In that case 'lookup_table'
#' is addressed in a multidimensional manner, i.e. 'lookup_table[i1, i2, ...]'.
#'
#' The range of bins is determined by 'breaks' argument. For example: 'breaks =
#' c(x1, x2, x3, x4)' represents three different intervals (bins): (x1, x2],
#' (x2, x3], (x3, x4].
#'
#' If 'include.lowest' is 'TRUE' the the lowest value is included in the first
#' interval, i.e. in [x1, x2].
#'
#' 'force.binning' parameter controls what should be done when the value of
#' 'expr' exceeds the range determined by 'breaks'. If 'force.binning' is
#' 'TRUE' then values smaller than the minimal break will be translated to
#' index 1, and the values exceeding the maximal break will be translated to
#' index 'M-1' where 'M' is the number of breaks. If 'force.binning' is 'FALSE'
#' the out-of-range values will produce 'NaN' values.
#'
#' Regardless of 'force.binning' value if the value of 'expr' is 'NaN' then the
#' value in the track would be 'NaN' too.
#'
#' 'description' is added as a track attribute.
#'
#' @param track track name
#' @param description a character string description
#' @param lookup_table a multi-dimensional array containing the values that are
#' returned by the function
#' @param ... pairs of track expressions and breaks
#' @param include.lowest if 'TRUE', the lowest value of the range determined by
#' breaks is included
#' @param force.binning if 'TRUE', the values smaller than the minimal break
#' will be translated to index 1, and the values that exceed the maximal break
#' will be translated to index N-1 where N is the number of breaks. If 'FALSE'
#' the out-of-range values will produce NaN values.
#' @param iterator track expression iterator. If 'NULL' iterator is determined
#' implicitly based on track expressions.
#' @param band track expression band. If 'NULL' no band is used.
#' @return None.
#' @seealso \code{\link{glookup}}, \code{\link{gtrack.2d.create}},
#' \code{\link{gtrack.create_sparse}}, \code{\link{gtrack.smooth}},
#' \code{\link{gtrack.modify}}, \code{\link{gtrack.rm}},
#' \code{\link{gtrack.info}}, \code{\link{gdir.create}}
#' @keywords ~lookup ~track
#' @examples
#' \dontshow{
#' options(gmax.processes = 2)
#' }
#'
#' gdb.init_examples()
#'
#' ## one-dimensional example
#' breaks1 <- seq(0.1, 0.2, length.out = 6)
#' gtrack.lookup(
#'     "lookup_track", "Test track", 1:5, "dense_track",
#'     breaks1
#' )
#' gtrack.rm("lookup_track", force = TRUE)
#'
#' ## two-dimensional example
#' t <- array(1:15, dim = c(5, 3))
#' breaks2 <- seq(0.31, 0.37, length.out = 4)
#' gtrack.lookup(
#'     "lookup_track", "Test track", t, "dense_track",
#'     breaks1, "2 * dense_track", breaks2
#' )
#' gtrack.rm("lookup_track", force = TRUE)
#'
#' @export gtrack.lookup
gtrack.lookup <- function(track = NULL, description = NULL, lookup_table = NULL, ..., include.lowest = FALSE, force.binning = TRUE, iterator = NULL, band = NULL) {
    args <- as.list(substitute(list(...)))[-1L]
    if (is.null(substitute(track)) || is.null(description) || is.null(lookup_table) || length(args) < 2 || length(args) %% 2 != 0) {
        stop("Usage: gtrack.lookup(track, description, lookup_table, [expr, breaks]+, include.lowest = FALSE, force.binning = TRUE, iterator = NULL, band = NULL)", call. = FALSE)
    }
    .gcheckroot()

    trackstr <- do.call(.gexpr2str, list(substitute(track)), envir = parent.frame())
    trackdir <- sprintf("%s.track", paste(get("GWD", envir = .misha), gsub("\\.", "/", trackstr), sep = "/"))
    direxisted <- file.exists(trackdir)

    exprs <- c()
    breaks <- list()

    for (i in (0:(length(args) / 2 - 1))) {
        exprs <- append(exprs, do.call(.gexpr2str, list(args[[i * 2 + 1]]), envir = parent.frame()))
        breaks[[length(breaks) + 1]] <- eval.parent(args[[i * 2 + 2]])
    }

    .iterator <- do.call(.giterator, list(substitute(iterator)), envir = parent.frame())

    if (!is.na(match(trackstr, get("GTRACKS", envir = .misha)))) {
        stop(sprintf("Track %s already exists", trackstr), call. = FALSE)
    }

    .gconfirmtrackcreate(trackstr)
    success <- FALSE
    tryCatch(
        {
            .gcall("gtrack_bintransform", trackstr, exprs, breaks, include.lowest, force.binning, lookup_table, .iterator, band, .misha_env(), silent = TRUE)
            .gdb.add_track(trackstr)
            created.by <- sprintf("gtrack.lookup(%s, description, lookup_table", trackstr)
            for (i in (1:length(exprs))) {
                created.by <- sprintf("%s, %s, breaks%d", created.by, exprs[i], i)
            }
            created.by <- sprintf("%s, include.lowest = %s, force.binning = %s)", created.by, include.lowest, force.binning)
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



#' Returns a list of track names
#'
#' Returns a list of track names in Genomic Database.
#'
#' This function returns a list of tracks whose name or track attribute value
#' match a pattern (see 'grep'). If called without any arguments all tracks are
#' returned.
#'
#' If pattern is specified without a track attribute (i.e. in the form of
#' 'pattern') then filtering is applied to the track names. If pattern is
#' supplied with a track attribute (i.e. in the form of 'name = pattern') then
#' track attribute is matched against the pattern.
#'
#' Multiple patterns are applied one after another. The resulted list of tracks
#' should match all the patterns.
#'
#' @param ... these arguments are of either form 'pattern' or 'attribute =
#' pattern'
#' @param ignore.case,perl,fixed,useBytes see 'grep'
#' @return An array that contains the names of tracks that match the supplied
#' patterns.
#' @seealso \code{\link{grep}}, \code{\link{gtrack.exists}},
#' \code{\link{gtrack.create}}, \code{\link{gtrack.rm}}
#' @keywords ~intervals ~ls
#' @examples
#' \dontshow{
#' options(gmax.processes = 2)
#' }
#'
#' gdb.init_examples()
#'
#' # get all track names
#' gtrack.ls()
#'
#' # get track names that match the pattern "den*"
#' gtrack.ls("den*")
#'
#' # get track names whose "created.by" attribute match the pattern
#' # "create_sparse"
#' gtrack.ls(created.by = "create_sparse")
#'
#' # get track names whose names match the pattern "den*" and whose
#' # "created.by" attribute match the pattern "track"
#' gtrack.ls("den*", created.by = "track")
#'
#' @export gtrack.ls
gtrack.ls <- function(..., ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE) {
    .gcheckroot()

    args <- as.list(substitute(list(...)))[-1L]
    args <- list(...)

    tracks <- get("GTRACKS", envir = .misha)

    if (is.null(tracks) || !length(tracks)) {
        return(NULL)
    }

    if (length(args) >= 1) {
        attrs <- c()
        patterns <- c()

        # first filter out file names (this filtering is faster than filtering by track variable)
        for (i in 1:length(args)) {
            arg <- as.character(args[[i]])
            if (is.null(names(args)) || names(args)[i] == "") {
                tracks <- grep(arg, tracks, value = TRUE, ignore.case = ignore.case, perl = perl, fixed = fixed, useBytes = useBytes)
            } else {
                attrs <- c(attrs, names(args)[i])
                patterns <- c(patterns, as.character(args[[i]]))
            }
        }

        # filter out by attributes
        if (length(attrs)) {
            attrs_table <- .gcall("gget_tracks_attrs", tracks, attrs, .misha_env())
            if (is.null(attrs_table)) {
                return(NULL)
            }

            cols <- colnames(attrs_table)
            for (i in 1:length(attrs)) {
                idx <- which(cols == attrs[i])[1]
                if (!is.na(idx)) {
                    attrs_table <- subset(attrs_table, grepl(patterns[i], attrs_table[, idx], ignore.case = ignore.case, perl = perl, fixed = fixed, useBytes = useBytes))
                    if (!nrow(attrs_table)) {
                        return(NULL)
                    }
                }
            }
            tracks <- rownames(attrs_table)
        }
    }

    tracks
}



#' Modifies track contents
#'
#' Modifies 'Dense' track contents.
#'
#' This function modifies the contents of a 'Dense' track by the values of
#' 'expr'. 'intervals' argument controls which portion of the track is
#' modified. The iterator policy is set internally to the bin size of the
#' track.
#'
#' @param track track name
#' @param expr track expression
#' @param intervals genomic scope for which track is modified
#' @return None.
#' @seealso \code{\link{gtrack.create}}, \code{\link{gtrack.rm}}
#' @keywords ~modify ~track
#' @examples
#' \dontshow{
#' options(gmax.processes = 2)
#' }
#'
#' gdb.init_examples()
#' intervs <- gintervals(1, 300, 800)
#' gextract("dense_track", intervs)
#' gtrack.modify("dense_track", "dense_track * 2", intervs)
#' gextract("dense_track", intervs)
#' gtrack.modify("dense_track", "dense_track / 2", intervs)
#'
#' @export gtrack.modify
gtrack.modify <- function(track = NULL, expr = NULL, intervals = NULL) {
    if (is.null(substitute(track)) || is.null(substitute(expr))) {
        stop("Usage: gtrack.modify(track, expr, intervals = .misha$ALLGENOME)", call. = FALSE)
    }
    .gcheckroot()

    intervals <- rescue_ALLGENOME(intervals, as.character(substitute(intervals)))

    if (is.null(intervals)) {
        intervals <- get("ALLGENOME", envir = .misha)
    }

    trackstr <- do.call(.gexpr2str, list(substitute(track)), envir = parent.frame())
    exprstr <- do.call(.gexpr2str, list(substitute(expr)), envir = parent.frame())

    .gcall("gtrack_modify", trackstr, exprstr, intervals, iterator = trackstr, .misha_env())

    str <- sprintf("gtrack.modify(%s, %s, intervs)", trackstr, exprstr)
    created.by.str <- gtrack.attr.export(trackstr, "created.by")[1, 1]
    if (is.null(created.by.str)) {
        created.by.str <- str
    } else {
        created.by.str <- paste(created.by.str, str, sep = "\n")
    }

    .gtrack.attr.set(trackstr, "created.by", created.by.str, TRUE)

    retv <- 0 # suppress return value
}



#' Deletes a track
#'
#' Deletes a track.
#'
#' This function deletes a track from the Genomic Database. By default
#' 'gtrack.rm' requires the user to interactively confirm the deletion. Set
#' 'force' to 'TRUE' to suppress the user prompt.
#'
#' @param track track name
#' @param force if 'TRUE', suppresses user confirmation of a named track removal
#' @return None.
#' @seealso \code{\link{gtrack.exists}}, \code{\link{gtrack.ls}},
#' \code{\link{gtrack.create}}, \code{\link{gtrack.2d.create}},
#' \code{\link{gtrack.create_sparse}}, \code{\link{gtrack.smooth}}
#' @keywords ~track
#' @examples
#' \dontshow{
#' options(gmax.processes = 2)
#' }
#'
#' gdb.init_examples()
#' gtrack.create("new_track", "Test track", "2 * dense_track")
#' gtrack.exists("new_track")
#' gtrack.rm("new_track", force = TRUE)
#' gtrack.exists("new_track")
#'
#' @export gtrack.rm
gtrack.rm <- function(track = NULL, force = FALSE) {
    if (is.null(substitute(track))) {
        stop("Usage: gtrack.rm(track, force = FALSE)", call. = FALSE)
    }
    .gcheckroot()

    trackname <- do.call(.gexpr2str, list(substitute(track)), envir = parent.frame())

    # check whether track appears among GTRACKS
    if (is.na(match(trackname, get("GTRACKS", envir = .misha)))) {
        if (force) {
            return(invisible())
        }
        stop(sprintf("Track %s does not exist", trackname), call. = FALSE)
    }

    answer <- "N"
    if (force) {
        answer <- "Y"
    } else {
        str <- sprintf("Are you sure you want to delete track %s (Y/N)? ", trackname)
        message(str)
        answer <- toupper(readLines(n = 1))
    }

    if (answer == "Y" || answer == "YES") {
        dirname <- sprintf("%s.track", paste(get("GWD", envir = .misha), gsub("\\.", "/", trackname), sep = "/"))

        # remove the track
        unlink(dirname, recursive = TRUE)

        if (file.exists(dirname)) {
            message(sprintf("Failed to delete track %s", trackname))
        } else {
            # refresh the list of GTRACKS, etc.
            .gdb.rm_track(trackname)
        }
    }
}



#' Creates a new track from smoothed values of track expression
#'
#' Creates a new track from smoothed values of track expression.
#'
#' This function creates a new 'Dense' track named 'track'. The values of the
#' track are results of smoothing the values of 'expr'.
#'
#' Each track value at coordinate 'C' is determined by smoothing non 'NaN'
#' values of 'expr' over the window around 'C'. The window size is controlled
#' by 'winsize' and is given in coordinate units (not in number of bins),
#' defining the total regions to be considered when smoothing (on both sides of
#' the central point). Two different algorithms can be used for smoothing:
#'
#' "MEAN" - an arithmetic average.
#'
#' "LINEAR_RAMP" - a weighted arithmetic average, where the weights linearly
#' decrease as the distance from the center of the window increases.
#'
#' 'weight_thr' determines the function behavior when some of the values in the
#' window are missing or 'NaN' (missing values may occur at the edges of each
#' chromosome when the window covers an area beyond chromosome boundaries).
#' 'weight_thr' sets the weight sum threshold below which smoothing algorithm
#' returns 'NaN' rather than a smoothing value based on non 'NaN' values in the
#' window.
#'
#' 'smooth_nans' controls what would be the smoothed value if the central value
#' in the window is 'NaN'. If 'smooth_nans' is 'FALSE' then the smoothed value
#' is set to 'NaN' regardless of 'weight_thr' parameter. Otherwise it is
#' calculated normally.
#'
#' 'description' is added as a track attribute.
#'
#' Iterator policy must be of "fixed bin" type.
#'
#' @param track track name
#' @param description a character string description
#' @param expr track expression
#' @param winsize size of smoothing window
#' @param weight_thr smoothing weight threshold
#' @param smooth_nans if 'FALSE' track value is always set to 'NaN' if central
#' window value is 'NaN', otherwise it is calculated from the rest of non 'NaN'
#' values
#' @param alg smoothing algorithm - "MEAN" or "LINEAR_RAMP"
#' @param iterator track expression iterator of 'Fixed bin' type
#' @return None.
#' @seealso \code{\link{gtrack.create}}, \code{\link{gtrack.2d.create}},
#' \code{\link{gtrack.create_sparse}}, \code{\link{gtrack.modify}},
#' \code{\link{gtrack.rm}}, \code{\link{gtrack.info}},
#' \code{\link{gdir.create}}
#' @keywords ~smooth ~track
#' @examples
#' \dontshow{
#' options(gmax.processes = 2)
#' }
#'
#' gdb.init_examples()
#' gtrack.smooth("smoothed_track", "Test track", "dense_track", 500)
#' gextract("dense_track", "smoothed_track", gintervals(1, 0, 1000))
#' gtrack.rm("smoothed_track", force = TRUE)
#'
#' @export gtrack.smooth
gtrack.smooth <- function(track = NULL, description = NULL, expr = NULL, winsize = NULL, weight_thr = 0, smooth_nans = FALSE, alg = "LINEAR_RAMP", iterator = NULL) {
    if (is.null(substitute(track)) || is.null(description) || is.null(substitute(expr)) || is.null(winsize)) {
        stop("Usage: gtrack.smooth(track, description, expr, winsize, weight_thr = 0, smooth_nans = FALSE, alg = \"LINEAR_RAMP\" (\"LINEAR_RAMP\" | \"MEAN\"), iterator = NULL)",
            call. = FALSE
        )
    }
    .gcheckroot()

    trackstr <- do.call(.gexpr2str, list(substitute(track)), envir = parent.frame())
    exprstr <- do.call(.gexpr2str, list(substitute(expr)), envir = parent.frame())
    trackdir <- sprintf("%s.track", paste(get("GWD", envir = .misha), gsub("\\.", "/", trackstr), sep = "/"))
    direxisted <- file.exists(trackdir)
    .iterator <- do.call(.giterator, list(substitute(iterator)), envir = parent.frame())

    if (!is.na(match(trackstr, get("GTRACKS", envir = .misha)))) {
        stop(sprintf("Track %s already exists", trackstr), call. = FALSE)
    }

    .gconfirmtrackcreate(trackstr)
    success <- FALSE
    tryCatch(
        {
            .gcall("gsmooth", trackstr, exprstr, winsize, weight_thr, smooth_nans, alg, .iterator, .misha_env(), silent = TRUE)
            .gdb.add_track(trackstr)
            .gtrack.attr.set(
                trackstr, "created.by",
                sprintf("gtrack.smooth(%s, description, %s, %s, %s, %s, %s)", trackstr, exprstr, as.character(winsize), as.character(weight_thr), as.character(smooth_nans), alg), TRUE
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
