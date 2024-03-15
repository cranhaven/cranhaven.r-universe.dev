.gintervals <- function(chroms, starts, ends, strands) {
    if (is.null(strands)) {
        intervals <- data.frame(chrom = .gchroms(chroms), start = starts, end = ends)
    } else {
        intervals <- data.frame(chrom = .gchroms(chroms), start = starts, end = ends, strand = strands)
    }

    numintervals <- nrow(intervals)

    maxends <- get("ALLGENOME", envir = .misha)[[1]]$end[match(as.character(intervals$chrom), get("ALLGENOME", envir = .misha)[[1]]$chrom)]
    maxidx <- intervals$end == -1
    intervals$end[maxidx] <- maxends[maxidx]

    err.intervs <- intervals[intervals$start < 0, ]
    if (nrow(err.intervs) > 0) {
        stop(sprintf("Invalid interval (%s, %g, %g): start coordinate is out of range", err.intervs$chrom[1], err.intervs$start[1], err.intervs$end[1]), call. = FALSE)
    }

    err.intervs <- intervals[intervals$end > maxends, ]
    if (nrow(err.intervs) > 0) {
        stop(sprintf("Invalid interval (%s, %g, %g): end coordinate exceeds chromosome boundaries", err.intervs$chrom[1], err.intervs$start[1], err.intervs$end[1]), call. = FALSE)
    }

    err.intervs <- intervals[intervals$start >= intervals$end, ]
    if (nrow(err.intervs) > 0) {
        stop(sprintf("Invalid interval (%s, %g, %g): start coordinate exceeds or equals to end coordinate", err.intervs$chrom[1], err.intervs$start[1], err.intervs$end[1]), call. = FALSE)
    }

    if (!is.null(strands)) {
        if (!is.numeric(intervals$strand)) {
            stop("Invalid strand values", call. = FALSE)
        }

        err.intervs <- intervals[intervals$strand != as.integer(intervals$strand) | intervals$strand < -1 | intervals$strand > 1, ]
        if (nrow(err.intervs) > 0) {
            stop(sprintf("Invalid strand value %g of interval (%s, %g, %g)", err.intervs$strand[1], err.intervs$chrom[1], err.intervs$start[1], err.intervs$end[1]))
        }
    }

    intervals
}

.gintervals.apply <- function(chroms, intervals, intervals.set.out, FUN, ...) {
    if (!is.null(intervals.set.out)) {
        fullpath <- .gintervals.check_new_set(intervals.set.out)
    }

    if (is.data.frame(intervals)) {
        intervals <- list(intervals)
    }

    # sort chroms
    chroms$size <- NULL
    if ("chrom" %in% colnames(chroms)) {
        chroms <- data.frame(chrom = chroms[with(chroms, order(chrom)), ])
    } else {
        chroms <- chroms[with(chroms, order(chrom1, chrom2)), ]
    }


    # let's assume that if any of the input intervals sets is big then intervals.set.out should be big as well
    if (any(unlist(lapply(intervals, function(intervals) {
        .gintervals.is_bigset(intervals) || .gintervals.needs_bigset(intervals)
    })))) {
        stats <- NULL
        zeroline <- NULL
        success <- FALSE
        t <- Sys.time()
        progress.percentage <- -1
        tryCatch(
            {
                # if any of the source intervals sets is big then create the output intervals set big too
                if (!is.null(intervals.set.out)) {
                    dir.create(fullpath, recursive = TRUE, mode = "0777")
                }

                if (.gintervals.is1d(intervals[[1]])) {
                    mapply(function(chrom) {
                        loaded_intervals <- lapply(intervals, function(intervals) {
                            .gintervals.load_ext(intervals, chrom = chrom)
                        })
                        res <- do.call(FUN, list(loaded_intervals, ...))
                        if (!is.null(intervals.set.out) && !is.null(res) && nrow(res) > 0) {
                            zeroline <<- res[0, ]
                            .gintervals.big.save(fullpath, res, chrom = chrom)
                            stat <- .gcall("gintervals_stats", res, .misha_env())
                            stats <<- rbind(stats, data.frame(chrom = chrom, stat))
                        }
                        if (as.integer(difftime(Sys.time(), t, units = "secs")) > 3) {
                            t <<- Sys.time()
                            percentage <- as.integer(100 * match(chrom, chroms$chrom) / nrow(chroms))
                            if (percentage < 100 && progress.percentage != percentage) {
                                message(sprintf("%d%%...", percentage), appendLF = FALSE)
                                progress.percentage <<- percentage
                            }
                        }
                    }, chroms$chrom)
                } else {
                    mapply(function(chrom1, chrom2) {
                        loaded_intervals <- lapply(intervals, function(intervals) {
                            .gintervals.load_ext(intervals, chrom1 = chrom1, chrom2 = chrom2)
                        })
                        res <- do.call(FUN, list(loaded_intervals, ...))
                        if (!is.null(intervals.set.out) && !is.null(res) && nrow(res) > 0) {
                            zeroline <<- res[0, ]
                            .gintervals.big.save(fullpath, res, chrom1 = chrom1, chrom2 = chrom2)
                            stat <- .gcall("gintervals_stats", res, .misha_env())
                            stats <<- rbind(stats, data.frame(chrom1 = chrom1, chrom2 = chrom2, stat))
                        }
                        if (as.integer(difftime(Sys.time(), t, units = "secs")) > 3) {
                            t <<- Sys.time()
                            percentage <- as.integer(100 * which(chroms$chrom1 == chrom1 & chroms$chrom2 == chrom2) / nrow(chroms))
                            if (percentage < 100 && progress.percentage != percentage) {
                                message(sprintf("%d%%...", percentage), appendLF = FALSE)
                                progress.percentage <<- percentage
                            }
                        }
                    }, chroms$chrom1, chroms$chrom2)
                }

                if (!is.null(intervals.set.out)) {
                    if (is.null(stats)) {
                        return(retv <- NULL)
                    }
                    .gintervals.big.save_meta(fullpath, stats, zeroline)
                }

                if (progress.percentage >= 0) {
                    message("100%")
                }

                success <- TRUE

                # check whether the output intervals set needs to remain in big format
                if (!is.null(intervals.set.out) && !.gintervals.needs_bigset(intervals.set.out)) {
                    .gintervals.big2small(intervals.set.out)
                }
            },
            finally = {
                if (!success && !is.null(intervals.set.out)) {
                    unlink(fullpath, recursive = TRUE)
                }
            }
        )
    } else {
        loaded_intervals <- lapply(intervals, .gintervals.load_ext)
        res <- do.call(FUN, list(loaded_intervals, ...))
        if (!is.null(intervals.set.out) && !is.null(res) && nrow(res) > 0) {
            if (.gintervals.is1d(res)) {
                res <- res[order(res$chrom), ]
            } else {
                res <- res[order(res$chrom1, res$chrom2), ]
            }
            if (.gintervals.needs_bigset(res)) {
                .gintervals.small2big(intervals.set.out, res)
            } else {
                .gintervals.save_file(fullpath, res)
            }
        } else {
            return(NULL)
        }
    }

    # refresh the list of GINTERVS, etc.
    if (!is.null(intervals.set.out)) {
        .gdb.add_intervals.set(intervals.set.out)
    }
}

.gintervals.check_new_set <- function(intervals.set) {
    if (!is.na(match(intervals.set, get("GINTERVS", envir = .misha)))) {
        stop(sprintf("Intervals set %s already exists", intervals.set), call. = FALSE)
    }

    if (!length(grep("^[A-Za-z][\\w.]*$", intervals.set, perl = TRUE))) {
        stop("Invalid interval name %s. Only alphanumeric characters and _ are allowed in the name.")
    }

    path <- gsub(".", "/", intervals.set, fixed = TRUE)
    path <- paste(path, ".interv", sep = "")
    fullpath <- paste(get("GWD", envir = .misha), path, sep = "/")
    dir <- dirname(path)
    fulldir <- paste(get("GWD", envir = .misha), dir, sep = "/")

    if (!file.exists(fulldir)) {
        stop(sprintf("Directory %s does not exist", dir), call. = FALSE)
    }

    if (file.exists(fullpath)) {
        stop(sprintf("File %s already exists", path), call. = FALSE)
    }

    if (!is.na(match(intervals.set, get("GTRACKS", envir = .misha)))) {
        stop(sprintf("Track %s already exists", intervals.set), call. = FALSE)
    }

    if (!is.na(match(intervals.set, gvtrack.ls()))) {
        stop(sprintf("Virtual track %s already exists", intervals.set), call. = FALSE)
    }

    if (.ggetOption(".gautocompletion", FALSE) && exists(intervals.set, envir = .misha)) {
        stop(sprintf("Variable \"%s\" shadows the name of the new intervals set.\nPlease remove this variable from the environment or switch off autocompletion mode.", intervals.set), call. = FALSE)
    }

    fullpath
}

.gintervals.load_ext <- function(intervals.set = NULL, chrom = NULL, chrom1 = NULL, chrom2 = NULL, progress = FALSE) {
    if (is.null(intervals.set)) {
        stop("Usage: gintervals.load(intervals.set, chrom = NULL, chrom1 = NULL, chrom2 = NULL)", call. = FALSE)
    }
    .gcheckroot()

    if (is.character(intervals.set) && length(intervals.set) == 1 && is.na(match(intervals.set, get("GINTERVS", envir = .misha))) && is.na(match(intervals.set, get("GTRACKS", envir = .misha)))) {
        stop(sprintf("Intervals set %s does not exist", intervals.set), call. = FALSE)
    }

    .gintervals.load(intervals.set, chrom, chrom1, chrom2, progress)
}

.gintervals.load <- function(intervals.set = NULL, chrom = NULL, chrom1 = NULL, chrom2 = NULL, progress = FALSE) {
    if (!is.null(chrom)) {
        chrom <- .gchroms(chrom)
        if (length(chrom) > 1) {
            stop("chrom parameter should mark only one chromosome")
        }
    }

    if (!is.null(chrom1)) {
        chrom1 <- .gchroms(chrom1)
        if (length(chrom1) > 1) {
            stop("chrom1 parameter should mark only one chromosome")
        }
    }

    if (!is.null(chrom2)) {
        chrom2 <- .gchroms(chrom2)
        if (length(chrom2) > 1) {
            stop("chrom2 parameter should mark only one chromosome")
        }
    }

    if (!is.null(chrom) && !is.null(chrom1)) {
        stop("Cannot use chrom and chrom1 parameters in the same call", call. = FALSE)
    }

    if (!is.null(chrom) && !is.null(chrom2)) {
        stop("Cannot use chrom and chrom2 parameters in the same call", call. = FALSE)
    }

    if (is.character(intervals.set) && length(intervals.set) != 1 || !is.character(intervals.set) && !.gintervals.is1d(intervals.set) && !.gintervals.is2d(intervals.set)) {
        stop("Invalid format of intervals", call. = FALSE)
    }

    res <- NULL
    if (.gintervals.is_bigset(intervals.set)) {
        meta <- .gintervals.big.meta(intervals.set)
        zeroline <- meta$zeroline
        t <- Sys.time()
        progress.percentage <- -1

        if (.gintervals.big.is1d(intervals.set)) {
            if (!is.null(chrom1) || !is.null(chrom2)) {
                stop(sprintf("%s is a 1D big intervals set.\nchrom1 or chrom2 parameters can be applied only to 2D intervals.", intervals.set), call. = FALSE)
            }

            if (!is.null(chrom)) {
                meta$stats <- meta$stats[meta$stats$chrom == chrom, ]
            }

            if (!.gintervals.loadable(intervals.set, chrom = chrom)) {
                if (is.null(chrom)) {
                    stop(sprintf(
                        "Cannot load a big intervals set %s: its size (%d) exceeds the limit (%d) controlled by gmax.data.size option.\nFor big intervals sets only one chromosome pair can be loaded at a time.",
                        intervals.set, sum(meta$stats$size), .ggetOption("gmax.data.size", 10000000)
                    ), call. = FALSE)
                } else {
                    stop(sprintf(
                        "Cannot load chromosome %s of an intervals set %s: its size (%d) exceeds the limit (%d) controlled by gmax.data.size option.",
                        chrom, intervals.set, sum(meta$stats$size), .ggetOption("gmax.data.size", 10000000)
                    ), call. = FALSE)
                }
            }

            if (nrow(meta$stats) > 1) {
                res <- list(zeroline)
                lapply(
                    meta$stats$chrom,
                    function(chrom) {
                        loaded_intervs <- .gintervals.load_file(intervals.set, chrom = chrom)
                        if (!identical(sapply(loaded_intervs, "class"), sapply(zeroline, "class"))) {
                            stop(sprintf("Intervals set %s, chrom %s: invalid columns definition", intervals.set, chrom), call. = FALSE)
                        }
                        res <<- c(res, list(loaded_intervs))
                        if (as.integer(difftime(Sys.time(), t, units = "secs")) > 3) {
                            t <<- Sys.time()
                            percentage <- as.integer(100 * match(chrom, meta$stats$chrom) / length(meta$stats$chrom))
                            if (progress && percentage < 100 && progress.percentage != percentage) {
                                message(sprintf("%d%%...", percentage), appendLF = FALSE)
                                progress.percentage <<- percentage
                            }
                        }
                    }
                )
                res <- do.call(.grbind, res) # much faster than calling rbind incrementally in mapply
            } else if (nrow(meta$stats) == 1) {
                res <- .gintervals.load_file(intervals.set, chrom = meta$stat$chrom[1])
                if (!identical(sapply(res, "class"), sapply(zeroline, "class"))) {
                    stop(sprintf("Intervals set %s, chrom %s: invalid columns definition", intervals.set, meta$stat$chrom[1]), call. = FALSE)
                }
            } else {
                res <- meta$zeroline
            }
        } else {
            if (!is.null(chrom)) {
                stop(sprintf("%s is a 2D big intervals set.\nchrom parameter can be applied only to 1D intervals.", intervals.set), call. = FALSE)
            }

            if (!is.null(chrom1)) {
                meta$stats <- meta$stats[meta$stats$chrom1 == chrom1, ]
            }
            if (!is.null(chrom2)) {
                meta$stats <- meta$stats[meta$stats$chrom2 == chrom2, ]
            }

            if (!.gintervals.loadable(intervals.set, chrom1 = chrom1, chrom2 = chrom2)) {
                if (!is.null(chrom1) && !is.null(chrom2)) {
                    stop(sprintf(
                        "Cannot load chromosome pair (%s, %s) of an intervals set %s: its size (%d) exceeds the limit (%d) controlled by gmax.data.size option.",
                        chrom1, chrom2, intervals.set, sum(meta$stats$size), .ggetOption("gmax.data.size", 10000000)
                    ), call. = FALSE)
                } else if (!is.null(chrom1)) {
                    stop(sprintf(
                        "Cannot load chromosome %s of an intervals set %s: its size (%d) exceeds the limit (%d) controlled by gmax.data.size option.",
                        chrom1, intervals.set, sum(meta$stats$size), .ggetOption("gmax.data.size", 10000000)
                    ), call. = FALSE)
                } else if (!is.null(chrom2)) {
                    stop(sprintf(
                        "Cannot load chromosome %s of an intervals set %s: its size (%d) exceeds the limit (%d) controlled by gmax.data.size option.",
                        chrom2, intervals.set, sum(meta$stats$size), .ggetOption("gmax.data.size", 10000000)
                    ), call. = FALSE)
                } else {
                    stop(sprintf(
                        "Cannot load a big intervals set %s: its size (%d) exceeds the limit (%d) controlled by gmax.data.size option.\nFor big intervals sets only one chromosome pair can be loaded at a time.",
                        intervals.set, sum(meta$stats$size), .ggetOption("gmax.data.size", 10000000)
                    ), call. = FALSE)
                }
            }

            if (nrow(meta$stats) > 1) {
                res <- list(zeroline)
                mapply(
                    function(chrom1, chrom2) {
                        loaded_intervs <- .gintervals.load_file(intervals.set, chrom1 = chrom1, chrom2 = chrom2)
                        if (!identical(sapply(loaded_intervs, "class"), sapply(zeroline, "class"))) {
                            stop(sprintf("Interval set %s, chrom1 %s, chrom2 %s: invalid columns definition", intervals.set, chrom1, chrom2), call. = FALSE)
                        }
                        res <<- c(res, list(loaded_intervs))
                        if (as.integer(difftime(Sys.time(), t, units = "secs")) > 3) {
                            t <<- Sys.time()
                            percentage <- as.integer(100 * which(meta$stats$chrom1 == chrom1 & meta$stats$chrom2 == chrom2) / nrow(meta$stats))
                            if (progress && percentage < 100 && progress.percentage != percentage) {
                                message(sprintf("%d%%...", percentage), appendLF = FALSE)
                                progress.percentage <<- percentage
                            }
                        }
                    },
                    meta$stats$chrom1, meta$stats$chrom2
                )
                res <- do.call(.grbind, res) # much faster than calling rbind incrementally in mapply
            } else if (nrow(meta$stats) == 1) {
                res <- .gintervals.load_file(intervals.set, chrom1 = meta$stat$chrom1[1], chrom2 = meta$stat$chrom2[1])
                if (!identical(sapply(res, "class"), sapply(zeroline, "class"))) {
                    stop(sprintf("Interval set %s, chrom1 %s, chrom2 %s: invalid columns definition", intervals.set, meta$stat$chrom1[1], meta$stat$chrom2[1]), call. = FALSE)
                }
            } else {
                res <- meta$zeroline
            }
        }

        if (progress.percentage >= 0) {
            message("100%")
        }
    } else {
        if (is.character(intervals.set) && length(intervals.set) == 1) {
            res <- .gintervals.load_file(intervals.set)
        } else {
            res <- intervals.set
        }
        if (!is.null(res)) {
            if (!.gintervals.is1d(res) && !is.null(chrom)) {
                stop("chrom parameter can be applied only to 1D intervals", call. = FALSE)
            }

            if (!.gintervals.is2d(res) && (!is.null(chrom1) || !is.null(chrom2))) {
                stop("chrom1 or chrom2 parameters can be applied only to 2D intervals", call. = FALSE)
            }

            if (nrow(res) > 0) {
                if (!is.null(chrom)) {
                    res <- res[res$chrom == chrom, ]
                    if (nrow(res)) {
                        rownames(res) <- 1:nrow(res)
                    }
                }
                if (!is.null(chrom1)) {
                    res <- res[res$chrom1 == chrom1, ]
                    if (nrow(res)) {
                        rownames(res) <- 1:nrow(res)
                    }
                }
                if (!is.null(chrom2)) {
                    res <- res[res$chrom2 == chrom2, ]
                    if (nrow(res)) {
                        rownames(res) <- 1:nrow(res)
                    }
                }
            }
        }
    }
    res
}

.gintervals.load_file <- function(intervals.set, chrom = NULL, chrom1 = NULL, chrom2 = NULL) {
    intervfname <- sprintf("%s.interv", paste(get("GWD", envir = .misha), gsub("\\.", "/", intervals.set), sep = "/"))
    if (!is.null(chrom)) {
        chrom <- .gchroms(chrom)
        intervfname <- sprintf("%s/%s", intervfname, chrom)
    } else if (!is.null(chrom1) && !is.null(chrom2)) {
        chrom1 <- .gchroms(chrom1)
        chrom2 <- .gchroms(chrom2)
        intervfname <- sprintf("%s/%s-%s", intervfname, chrom1, chrom2)
    }

    if (intervals.set %in% get("GTRACKS", envir = .misha)) {
        .gcall("gtrack_intervals_load", intervals.set, chrom, chrom1, chrom2, .misha_env())
    } else {
        if (file.exists(intervfname) || (is.null(chrom) && is.null(chrom1) && is.null(chrom2))) {
            f <- file(intervfname, "rb")
            intervals.set <- unserialize(f)
            close(f)
            intervals.set
        } else {
            if (.gintervals.is_bigset(intervals.set)) {
                .gintervals.big.meta(intervals.set)$zeroline
            } else {
                stop(sprintf("File %s does not exist", intervfname), call. = FALSE)
            }
        }
    }
}

.gintervals.save_file <- function(filename, intervs) {
    if (nrow(intervs)) {
        if (.gintervals.is1d(intervs)) {
            intervs$chrom <- as.factor(intervs$chrom)
            point.intervs <- intervs$start == intervs$end
            intervs[point.intervs, ]$end <- intervs[point.intervs, ]$end + 1
        } else {
            intervs$chrom1 <- as.factor(intervs$chrom1)
            intervs$chrom2 <- as.factor(intervs$chrom2)
            point.intervs <- intervs$start1 == intervs$end1
            intervs[point.intervs, ]$end1 <- intervs[point.intervs, ]$end1 + 1
            point.intervs <- intervs$start2 == intervs$end2
            intervs[point.intervs, ]$end2 <- intervs[point.intervs, ]$end2 + 1
        }
    }

    f <- file(filename, "wb")
    serialize(intervs, f)
    close(f)
    nrow(intervs)
}

.gintervals.is_bigset <- function(intervals.set, err_if_non_exist = TRUE) {
    if (is.character(intervals.set) & length(intervals.set) == 1) {
        if (intervals.set %in% get("GTRACKS", envir = .misha)) {
            intervfname <- sprintf("%s.track", paste(get("GWD", envir = .misha), gsub("\\.", "/", intervals.set), sep = "/"))
        } else {
            intervfname <- sprintf("%s.interv", paste(get("GWD", envir = .misha), gsub("\\.", "/", intervals.set), sep = "/"))
        }
        if (file.exists(intervfname)) {
            if (file.info(intervfname)$isdir) {
                return(TRUE)
            }
        } else if (err_if_non_exist) {
            stop(sprintf("Intervals set %s does not exist", intervals.set), call. = FALSE)
        }
    }
    FALSE
}

.gintervals.needs_bigset <- function(intervals = NULL, size = NULL) {
    if (!is.null(intervals)) {
        chromsizes <- gintervals.chrom_sizes(intervals)
        nrow(chromsizes) && sum(chromsizes$size) > min(.ggetOption("gmax.data.size", 10000000), .ggetOption("gbig.intervals.size", 1000000))
    } else {
        size > min(.ggetOption("gmax.data.size", 10000000), .ggetOption("gbig.intervals.size", 1000000))
    }
}

.gintervals.loadable <- function(intervals = NULL, size = NULL, chrom = NULL, chrom1 = NULL, chrom2 = NULL) {
    if (!is.null(intervals)) {
        chromsizes <- gintervals.chrom_sizes(intervals)
        if (nrow(chromsizes)) {
            if (!is.null(chrom)) {
                chromsizes <- chromsizes[chromsizes$chrom == chrom, ]
            }
            if (!is.null(chrom1)) {
                chromsizes <- chromsizes[chromsizes$chrom1 == chrom1, ]
            }
            if (!is.null(chrom2)) {
                chromsizes <- chromsizes[chromsizes$chrom2 == chrom2, ]
            }
        }
        !nrow(chromsizes) || sum(chromsizes$size) <= .ggetOption("gmax.data.size", 10000000)
    } else {
        size <= .ggetOption("gmax.data.size", 10000000)
    }
}

.gintervals.big.is1d <- function(intervals.set) {
    "chrom" %in% colnames(.gintervals.big.meta(intervals.set)$stats)
}

.gintervals.big.is2d <- function(intervals.set) {
    "chrom1" %in% colnames(.gintervals.big.meta(intervals.set)$stats)
}

.gintervals.big2small <- function(intervals.set) {
    # We assume that writing the intervals might be a lengthy process.
    # During this time the process might get interrupted leaving the intervals set in incomplete state.
    # Even though it's not fully transaction-safe, we prefer to create a temporary file and then move it hoping it's fast enough.
    path <- gsub(".", "/", intervals.set, fixed = TRUE)
    path <- paste(get("GWD", envir = .misha), path, sep = "/")
    path <- paste(path, ".interv", sep = "")

    intervals <- .gintervals.load(intervals.set)
    tmpfilename <- tempfile(".", dirname(path)) # tmpdir = the parent directory of intervals set, otherwise rename might not work
    # (tmpdir might be at another file system)
    file.rename(path, tmpfilename)
    success <- FALSE
    tryCatch(
        {
            .gintervals.save_file(path, intervals)
            success <- TRUE
        },
        finally = {
            if (!success) {
                unlink(path, recursive = TRUE)
                file.rename(tmpfilename, path)
            }
            unlink(tmpfilename, recursive = TRUE)
        }
    )
}

.gintervals.small2big <- function(intervals.set, intervals = NULL) {
    # We assume that writing the intervals might be a lengthy process.
    # During this time the process might get interrupted leaving the intervals set in incomplete state.
    # Even though it's not fully transaction-safe, we prefer to create a temporary file and then move it hoping it's fast enough.
    if (is.null(intervals)) {
        intervals <- .gintervals.load(intervals.set)
    }

    path <- gsub(".", "/", intervals.set, fixed = TRUE)
    path <- paste(get("GWD", envir = .misha), path, sep = "/")
    path <- paste(path, ".interv", sep = "")

    tmpfilename <- tempfile(".", dirname(path)) # tmpdir = the parent directory of intervals set, otherwise rename might not work
    # (tmpdir might be at another file system)
    file.rename(path, tmpfilename)
    gintervs <- get("GINTERVS", envir = .misha)
    assign("GINTERVS", gintervs[gintervs != intervals.set], envir = .misha)
    success <- FALSE
    tryCatch(
        {
            .gcall_noninteractive(gintervals.save, intervals.set, intervals)
            success <- TRUE
        },
        finally = {
            if (!success) {
                unlink(path, recursive = TRUE)
                file.rename(tmpfilename, path)
                gintervs <- c(gintervs, intervals.set)
                gintervs <- unique(gintervs)
                sort(gintervs)
                assign("GINTERVS", gintervs, envir = .misha)
            }
            unlink(tmpfilename, recursive = TRUE)
        }
    )
}

.gintervals.big.meta <- function(intervals.set) {
    metafname <- ""
    if (intervals.set %in% get("GTRACKS", envir = .misha)) {
        metafname <- sprintf("%s.track/.meta", paste(get("GWD", envir = .misha), gsub("\\.", "/", intervals.set), sep = "/"))
        if (!file.exists(metafname)) {
            .gcall("gtrack_create_meta", intervals.set, .misha_env())
        }
    } else {
        metafname <- sprintf("%s.interv/.meta", paste(get("GWD", envir = .misha), gsub("\\.", "/", intervals.set), sep = "/"))
    }
    f <- file(metafname, "rb")
    res <- unserialize(f)
    close(f)
    res
}

.gintervals.big.save_meta <- function(path, stats, zeroline) {
    f <- file(sprintf("%s/.meta", path), "wb")
    serialize(list(stats = stats, zeroline = zeroline), f)
    close(f)
}

.gintervals.big.save <- function(path, intervs, chrom = NULL, chrom1 = NULL, chrom2 = NULL) {
    if (!is.null(chrom)) {
        filename <- sprintf("%s/%s", path, chrom)
    } else {
        filename <- sprintf("%s/%s-%s", path, chrom1, chrom2)
    }

    if (is.null(intervs) || nrow(intervs) == 0) {
        unlink(filename, recursive = TRUE)
    } else {
        if (!is.null(chrom)) {
            intervs <- intervs[intervs$chrom == chrom, ]
        } else {
            intervs <- intervs[intervs$chrom1 == chrom1 & intervs$chrom2 == chrom2, ]
        }
        .gintervals.save_file(filename, intervs)
    }
}

.gintervals.is1d <- function(intervals) {
    if (is.character(intervals)) {
        if (.gintervals.is_bigset(intervals)) {
            return(.gintervals.big.is1d(intervals))
        }
        intervals <- .gintervals.load(intervals)
    }
    all(colnames(intervals)[1:3] == c("chrom", "start", "end"))
}

.gintervals.is2d <- function(intervals) {
    if (is.character(intervals)) {
        if (.gintervals.is_bigset(intervals)) {
            return(.gintervals.big.is2d(intervals))
        }
        intervals <- .gintervals.load(intervals)
    }
    all(colnames(intervals)[1:6] == c("chrom1", "start1", "end1", "chrom2", "start2", "end2"))
}



#' Creates a set of 1D intervals
#'
#' Creates a set of 1D intervals.
#'
#' This function returns a set of one-dimensional intervals. The returned value
#' can be used in all functions that accept 'intervals' argument.
#'
#' One-dimensional intervals is a data frame whose first three columns are
#' 'chrom', 'start' and 'end'. Each row of the data frame represents a genomic
#' interval of the specified chromosome in the range of [start, end).
#' Additional columns can be presented in 1D intervals object yet these columns
#' must be added after the three obligatory ones.
#'
#' If 'strands' argument is not 'NULL' an additional column "strand" is added
#' to the intervals. The possible values of a strand can be '1' (plus strand),
#' '-1' (minus strand) or '0' (unknown).
#'
#' @param chroms chromosomes - an array of strings with or without "chr"
#' prefixes or an array of integers (like: '1' for "chr1")
#' @param starts an array of start coordinates
#' @param ends an array of end coordinates. If '-1' chromosome size is assumed.
#' @param strands 'NULL' or an array consisting of '-1', '0' or '1' values
#' @return A data frame representing the intervals.
#' @seealso \code{\link{gintervals.2d}}, \code{\link{gintervals.force_range}}
#' @keywords ~intervals
#' @examples
#' \dontshow{
#' options(gmax.processes = 2)
#' }
#'
#' gdb.init_examples()
#'
#' ## the following 3 calls produce identical results
#' gintervals(1)
#' gintervals("1")
#' gintervals("chrX")
#'
#' gintervals(1, 1000)
#' gintervals(c("chr2", "chrX"), 10, c(3000, 5000))
#'
#' @export gintervals
gintervals <- function(chroms = NULL, starts = 0, ends = -1, strands = NULL) {
    if (is.null(chroms)) {
        stop("Usage: gintervals(chroms, starts = 0, ends = -1, strands = NULL)", call. = FALSE)
    }
    .gcheckroot()

    intervals <- .gintervals(chroms, starts, ends, strands)
    .gcall("gintervsort", intervals, .misha_env())
}



#' Creates a set of 2D intervals
#'
#' Creates a set of 2D intervals.
#'
#' This function returns a set of two-dimensional intervals. The returned value
#' can be used in all functions that accept 'intervals' argument.
#'
#' Two-dimensional intervals is a data frame whose first six columns are
#' 'chrom1', 'start1', 'end1', 'chrom2', 'start2' and 'end2'. Each row of the
#' data frame represents two genomic intervals from two chromosomes in the
#' range of [start, end). Additional columns can be presented in 2D intervals
#' object yet these columns must be added after the six obligatory ones.
#'
#' @param chroms1 chromosomes1 - an array of strings with or without "chr"
#' prefixes or an array of integers (like: '1' for "chr1")
#' @param starts1 an array of start1 coordinates
#' @param ends1 an array of end1 coordinates. If '-1' chromosome size is
#' assumed.
#' @param chroms2 chromosomes2 - an array of strings with or without "chr"
#' prefixes or an array of integers (like: '1' for "chr1"). If 'NULL',
#' 'chroms2' is assumed to be equal to 'chroms1'.
#' @param starts2 an array of start2 coordinates
#' @param ends2 an array of end2 coordinates. If '-1' chromosome size is
#' assumed.
#' @return A data frame representing the intervals.
#' @seealso \code{\link{gintervals}}, \code{\link{gintervals.force_range}}
#' @keywords ~intervals
#' @examples
#' \dontshow{
#' options(gmax.processes = 2)
#' }
#'
#' gdb.init_examples()
#'
#' ## the following 3 calls produce identical results
#' gintervals.2d(1)
#' gintervals.2d("1")
#' gintervals.2d("chrX")
#'
#' gintervals.2d(1, 1000, 2000, "chrX", 400, 800)
#' gintervals.2d(c("chr2", "chrX"), 10, c(3000, 5000), 1)
#'
#' @export gintervals.2d
gintervals.2d <- function(chroms1 = NULL, starts1 = 0, ends1 = -1, chroms2 = NULL, starts2 = 0, ends2 = -1) {
    if (is.null(chroms1)) {
        stop("Usage: gintervals.2d(chroms1, starts1 = 0, ends1 = -1, chroms2 = NULL, starts2 = 0, ends2 = -1)", call. = FALSE)
    }
    .gcheckroot()

    if (is.null(chroms2)) {
        chroms2 <- chroms1
    }

    intervals1 <- .gintervals(chroms1, starts1, ends1, NULL)
    intervals2 <- .gintervals(chroms2, starts2, ends2, NULL)

    intervals <- data.frame(
        chrom1 = intervals1$chrom, start1 = intervals1$start, end1 = intervals1$end,
        chrom2 = intervals2$chrom, start2 = intervals2$start, end2 = intervals2$end
    )

    .gcall("gintervsort", intervals, .misha_env())
}



#' Returns 2D intervals that cover the whole genome
#'
#' Returns 2D intervals that cover the whole genome.
#'
#' This function returns a set of two-dimensional intervals that cover the
#' whole genome as it is defined by 'chrom_sizes.txt' file.
#'
#' @return A data frame representing the intervals.
#' @seealso \code{\link{gintervals.2d}}
#' @keywords ~genome ~chromosome ~chromosomes ~ALLGENOME
#' @export gintervals.2d.all
gintervals.2d.all <- function() {
    .gcheckroot()
    get("ALLGENOME", envir = .misha)[[2]]
}



#' Returns 1D intervals that cover the whole genome
#'
#' Returns 1D intervals that cover the whole genome.
#'
#' This function returns a set of one-dimensional intervals that cover the
#' whole genome as it is defined by 'chrom_sizes.txt' file.
#'
#' @return A data frame representing the intervals.
#' @seealso \code{\link{gintervals}}
#' @keywords ~genome ~chromosome ~chromosomes ~ALLGENOME
#' @export gintervals.all
gintervals.all <- function() {
    .gcheckroot()
    get("ALLGENOME", envir = .misha)[[1]]
}



#' Intersects two-dimensional intervals with a band
#'
#' Intersects two-dimensional intervals with a band.
#'
#' This function intersects each two-dimensional interval from 'intervals' with
#' 'band'. If the intersection is not empty, the interval is shrunk to the
#' minimal rectangle that contains the band and added to the return value.
#'
#' If 'intervals.set.out' is not 'NULL' the result is saved as an intervals
#' set. Use this parameter if the result size exceeds the limits of the
#' physical memory.
#'
#' @param intervals two-dimensional intervals
#' @param band track expression band. If 'NULL' no band is used.
#' @param intervals.set.out intervals set name where the function result is
#' optionally outputted
#' @return If 'intervals.set.out' is 'NULL' a data frame representing the
#' intervals.
#' @seealso \code{\link{gintervals.2d}}, \code{\link{gintervals.intersect}}
#' @keywords ~band ~intersect
#' @examples
#' \dontshow{
#' options(gmax.processes = 2)
#' }
#'
#' gdb.init_examples()
#' gintervals.2d.band_intersect(gintervals.2d(1), c(10000, 20000))
#'
#' @export gintervals.2d.band_intersect
gintervals.2d.band_intersect <- function(intervals = NULL, band = NULL, intervals.set.out = NULL) {
    if (is.null(intervals)) {
        stop("Usage: gintervals.2d.band_intersect(intervals, band = NULL, intervals.set.out = NULL)", call. = FALSE)
    }

    intervals <- rescue_ALLGENOME(intervals, as.character(substitute(intervals)))

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
                res <- .gcall("ginterv_intersectband", intervals, band, intervals.set.out, .misha_env())
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



#' Converts intervals to canonic form
#'
#' Converts intervals to canonic form.
#'
#' This function converts 'intervals' into a "canonic" form: properly sorted
#' with no overlaps. The result can be used later in the functions that require
#' the intervals to be in canonic form. Use 'unify_touching_intervals' to
#' control whether the intervals that touch each other (i.e. the end coordinate
#' of one equals to the start coordinate of the other) are unified.
#' 'unify_touching_intervals' is ignored if two-dimensional intervals are used.
#'
#' Since 'gintervals.canonic' unifies overlapping or touching intervals, the
#' number of the returned intervals might be less than the number of the
#' original intervals. To allow the user to find the origin of the new interval
#' 'mapping' attribute is attached to the result. It maps between the original
#' intervals and the resulted intervals. Use 'attr(retv_of_gintervals.canonic,
#' "mapping")' to retrieve the map.
#'
#' @param intervals intervals to be converted
#' @param unify_touching_intervals if 'TRUE', touching one-dimensional
#' intervals are unified
#' @return A data frame representing the canonic intervals and an attribute
#' 'mapping' that maps the original intervals to the resulted ones.
#' @seealso \code{\link{gintervals}}, \code{\link{gintervals.2d}}
#' @keywords ~intervals ~canonic
#' @examples
#' \dontshow{
#' options(gmax.processes = 2)
#' }
#'
#' gdb.init_examples()
#'
#' ## Create intervals manually by using 'data.frame'.
#' ## Note that we add an additional column 'data'.
#' ## Return value:
#' ##   chrom start   end data
#' ## 1  chr1 11000 12000   10
#' ## 2  chr1   100   200   20
#' ## 3  chr1 10000 13000   30
#' ## 4  chr1 10500 10600   40
#' intervs <- data.frame(
#'     chrom = "chr1",
#'     start = c(11000, 100, 10000, 10500),
#'     end = c(12000, 200, 13000, 10600),
#'     data = c(10, 20, 30, 40)
#' )
#'
#' ## Convert the intervals into the canonic form.
#' ## The function discards any columns besides chrom, start and end.
#' ## Return value:
#' ##  chrom start   end
#' ## 1  chr1   100   200
#' ## 2  chr1 10000 13000
#' res <- gintervals.canonic(intervs)
#'
#' ## By inspecting mapping attribute we can see how the new
#' ## intervals were created: "2 1 2 2" means that the first
#' ## interval in the result was created from the second interval in
#' ## the original set (we look for the indices in mapping where "1"
#' ## appears). Likewise the second interval in the result was
#' ## created from 3 intervals in the original set. Their indices are
#' ## 1, 3 and 4 (once again we look for the indices in mapping where
#' ## "2" appears).
#' ## Return value:
#' ## 2 1 2 2
#' attr(res, "mapping")
#'
#' ## Finally (and that is the most useful part of 'mapping'
#' ## attribute): we add a new column 'data' to our result which is
#' ## the mean value of the original data column. The trick is done
#' ## using 'tapply' on par with 'mapping' attribute. For example,
#' ## 20.00000 equals is a result of 'mean(intervs[2,]$data' while
#' ## 26.66667 is a result of 'mean(intervs[c(1,3,4),]$data)'.
#' ## 'res' after the following call:
#' ##  chrom start   end     data
#' ## 1  chr1   100   200 20.00000
#' ## 2  chr1 10000 13000 26.66667
#' res$data <- tapply(intervs$data, attr(res, "mapping"), mean)
#'
#' @export gintervals.canonic
gintervals.canonic <- function(intervals = NULL, unify_touching_intervals = TRUE) {
    if (is.null(intervals)) {
        stop("Usage: gintervals.canonic(intervals, unify_touching_intervals = TRUE)", call. = FALSE)
    }

    intervals <- rescue_ALLGENOME(intervals, as.character(substitute(intervals)))

    res <- .gcall("gintervcanonic", intervals, unify_touching_intervals, .misha_env())
    res
}



#' Calculates difference of two intervals sets
#'
#' Returns difference of two sets of intervals.
#'
#' This function returns a genomic space that is covered by 'intervals1' but
#' not covered by 'intervals2'.
#'
#' If 'intervals.set.out' is not 'NULL' the result is saved as an intervals
#' set. Use this parameter if the result size exceeds the limits of the
#' physical memory.
#'
#' @param intervals1,intervals2 set of one-dimensional intervals
#' @param intervals.set.out intervals set name where the function result is
#' optionally outputted
#' @return If 'intervals.set.out' is 'NULL' a data frame representing the
#' intervals.
#' @seealso \code{\link{gintervals}}, \code{\link{gintervals.intersect}},
#' \code{\link{gintervals.union}}
#' @keywords ~diff
#' @examples
#' \dontshow{
#' options(gmax.processes = 2)
#' }
#'
#' gdb.init_examples()
#'
#' intervs1 <- gscreen("dense_track > 0.15")
#' intervs2 <- gscreen("dense_track < 0.2")
#'
#' ## 'res3' equals to 'res4'
#' res3 <- gintervals.diff(intervs1, intervs2)
#' res4 <- gscreen("dense_track >= 0.2")
#'
#' @export gintervals.diff
gintervals.diff <- function(intervals1 = NULL, intervals2 = NULL, intervals.set.out = NULL) {
    if (is.null(intervals1) || is.null(intervals2)) {
        stop("Usage: gintervals.diff(intervals1, intervals2, intervals.set.out = NULL)", call. = FALSE)
    }

    intervals.set.out <- do.call(.gexpr2str, list(substitute(intervals.set.out)), envir = parent.frame())

    if (.gintervals.is_bigset(intervals1) || .gintervals.is_bigset(intervals2) || !is.null(intervals.set.out)) {
        res <- NULL

        FUN <- function(intervals, intervals.set.out, envir) {
            intervals1 <- intervals[[1]]
            intervals2 <- intervals[[2]]
            chrom_res <- .gcall("gintervdiff", intervals1, intervals2, .misha_env())
            if (!is.null(chrom_res) && nrow(chrom_res) > 0) {
                if (is.null(intervals.set.out)) {
                    assign("res", c(get("res", envir = envir), list(chrom_res)), envir = envir)
                    .gverify_max_data_size(sum(unlist(lapply(get("res", envir), nrow))), arguments = "intervals.set.out")
                }
            }
            chrom_res
        }

        .gintervals.apply(gintervals.chrom_sizes(intervals1), list(intervals1, intervals2), intervals.set.out, FUN, intervals.set.out, environment())

        if (!is.null(res)) {
            res <- do.call(.grbind, res)
        } # much faster than calling rbind incrementally in FUN

        if (is.null(intervals.set.out)) {
            if (!is.null(res) && nrow(res)) {
                res
            } else {
                NULL
            }
        } else {
            retv <- 0
        } # suppress return value
    } else {
        res <- .gcall("gintervdiff", intervals1, intervals2, .misha_env())
        res
    }
}



#' Tests for a named intervals set existence
#'
#' Tests for a named intervals set existence.
#'
#' This function returns 'TRUE' if a named intervals set exists in Genomic
#' Database.
#'
#' @param intervals.set name of an intervals set
#' @return 'TRUE' if a named intervals set exists. Otherwise 'FALSE'.
#' @seealso \code{\link{gintervals.ls}}, \code{\link{gintervals.load}},
#' \code{\link{gintervals.rm}}, \code{\link{gintervals.save}},
#' \code{\link{gintervals}}, \code{\link{gintervals.2d}}
#' @keywords ~intervals
#' @examples
#' \dontshow{
#' options(gmax.processes = 2)
#' }
#'
#' gdb.init_examples()
#' gintervals.exists("annotations")
#'
#' @export gintervals.exists
gintervals.exists <- function(intervals.set = NULL) {
    if (is.null(substitute(intervals.set))) {
        stop("Usage: gintervals.exists(intervals.set)", call. = FALSE)
    }
    .gcheckroot()

    intervals.set <- do.call(.gexpr2str, list(substitute(intervals.set)), envir = parent.frame())
    !is.na(match(intervals.set, get("GINTERVS", envir = .misha)))
}



#' Limits intervals to chromosomal range
#'
#' Limits intervals to chromosomal range.
#'
#' This function enforces the intervals to be within the chromosomal range [0,
#' chrom length) by altering the intervals' boundaries. Intervals that lay
#' entirely outside of the chromosomal range are eliminated. The new intervals
#' are returned.
#'
#' If 'intervals.set.out' is not 'NULL' the result is saved as an intervals
#' set. Use this parameter if the result size exceeds the limits of the
#' physical memory.
#'
#' @param intervals intervals
#' @param intervals.set.out intervals set name where the function result is
#' optionally outputted
#' @return If 'intervals.set.out' is 'NULL' a data frame representing the
#' intervals.
#' @seealso \code{\link{gintervals}}, \code{\link{gintervals.2d}},
#' \code{\link{gintervals.canonic}}
#' @keywords ~intervals
#' @examples
#' \dontshow{
#' options(gmax.processes = 2)
#' }
#'
#' gdb.init_examples()
#' intervs <- data.frame(
#'     chrom = "chr1",
#'     start = c(11000, -100, 10000, 10500),
#'     end = c(12000, 200, 13000000, 10600)
#' )
#' gintervals.force_range(intervs)
#'
#' @export gintervals.force_range
gintervals.force_range <- function(intervals = NULL, intervals.set.out = NULL) {
    if (is.null(substitute(intervals))) {
        stop("Usage: gintervals.force_range(intervals, intervals.set.out = NULL)", call. = FALSE)
    }
    .gcheckroot()

    intervals <- rescue_ALLGENOME(intervals, as.character(substitute(intervals)))

    intervals.set.out <- do.call(.gexpr2str, list(substitute(intervals.set.out)), envir = parent.frame())

    res <- NULL
    FUN <- function(intervals, intervals.set.out, envir) {
        intervals <- intervals[[1]]
        if (.gintervals.is1d(intervals)) {
            intervals$start <- pmax(intervals$start, 0)
            intervals$end <- pmin(intervals$end, gintervals.all()[match(intervals$chrom, gintervals.all()$chrom), ]$end)
            intervals <- intervals[!is.na(intervals$end) & intervals$start < intervals$end, ]
        } else {
            intervals$start1 <- pmax(intervals$start1, 0)
            intervals$end1 <- pmin(intervals$end1, gintervals.all()[match(intervals$chrom1, gintervals.all()$chrom), ]$end)
            intervals$start2 <- pmax(intervals$start2, 0)
            intervals$end2 <- pmin(intervals$end2, gintervals.all()[match(intervals$chrom2, gintervals.all()$chrom), ]$end)
            intervals <- intervals[!is.na(intervals$end1) & !is.na(intervals$end2) & intervals$start1 < intervals$end1 & intervals$start2 < intervals$end2, ]
        }
        if (is.null(intervals.set.out)) {
            assign("res", c(get("res", envir = envir), list(intervals)), envir = envir)
            .gverify_max_data_size(sum(unlist(lapply(get("res", envir), nrow))), arguments = "intervals.set.out")
        }
        intervals
    }

    .gintervals.apply(gintervals.chrom_sizes(intervals), intervals, intervals.set.out, FUN, intervals.set.out, environment())

    if (!is.null(res)) {
        res <- do.call(.grbind, res)
    } # much faster than calling rbind incrementally in FUN

    if (is.null(intervals.set.out)) {
        if (!is.null(res) && nrow(res)) {
            res
        } else {
            NULL
        }
    } else {
        retv <- 0
    } # suppress return value
}



#' Imports genes and annotations from files
#'
#' Imports genes and annotations from files.
#'
#' This function reads a definition of genes from 'genes.file' and returns four
#' sets of intervals: TSS, exons, 3utr and 5utr. In addition to the regular
#' intervals columns 'strand' column is added. It contains '1' values for '+'
#' strands and '-1' values for '-' strands.
#'
#' If annotation file 'annots.file' is given then annotations are attached too
#' to the intervals. The names of the annotations as they would appear in the
#' return value must be specified in 'annots.names' argument.
#'
#' Both 'genes.file' and 'annots.file' can be either a file path or URL in a
#' form of 'ftp://[address]/[file]'. Files that these arguments point to can be
#' zipped or unzipped.
#'
#' Examples of 'genes.file' and 'annots.file' can be found here:
#'
#' \code{ftp://hgdownload.soe.ucsc.edu/goldenPath/hg19/database/knownGene.txt.gz}
#' \code{ftp://hgdownload.soe.ucsc.edu/goldenPath/hg19/database/kgXref.txt.gz}
#'
#' If a few intervals overlap (for example: two TSS regions) they are all
#' unified to an interval that covers the whole overlapping region. 'strand'
#' value is set to '0' if two or more of the overlapping intervals have
#' different strands. The annotations of the overlapping intervals are
#' concatenated to a single character string separated by semicolons. Identical
#' values of overlapping intervals' annotation are eliminated.
#'
#' @param genes.file name or URL of file that contains genes
#' @param annots.file name of URL file that contains annotations. If 'NULL' no
#' annotations are imported
#' @param annots.names annotations names
#' @return A list of four intervals sets named 'tss', 'exons', 'utr3' and
#' 'utr5'. 'strand' column and annotations are attached to the intevals.
#' @seealso \code{\link{gintervals}}, \code{\link{gdb.create}}
#' @keywords ~intervals ~import ~genes
#' @export gintervals.import_genes
gintervals.import_genes <- function(genes.file = NULL, annots.file = NULL, annots.names = NULL) {
    if (is.null(genes.file)) {
        stop("Usage: gintervals.import_genes(genes.file, annots.file = NULL, annots.names = NULL)", call. = FALSE)
    }

    tmp.dirname <- tempfile(pattern = "", tmpdir = paste(get("GROOT", envir = .misha), "/downloads", sep = ""))
    if (!dir.create(tmp.dirname, recursive = TRUE, mode = "0777")) {
        stop(sprintf("Failed to create a directory %s", tmp.dirname), call. = FALSE)
    }

    files <- list(genes.file, annots.file)
    file.types <- c("genes.file", "annots.file")

    tryCatch(
        {
            for (i in 1:length(files)) {
                if (is.null(files[[i]])) {
                    next
                }

                protocol <- "ftp://"
                if (substr(files[[i]], 1, nchar(protocol)) == protocol) {
                    # ftp
                    f <- gwget(files[[i]], tmp.dirname)
                    if (length(f) != 1) {
                        stop(sprintf("More than one file matches %s argument", file.types[i]), call. = FALSE)
                    }
                    files[[i]] <- f
                }

                if (length(grep("^.+\\.gz$", files[[i]], perl = TRUE))) {
                    f.unzipped <- basename(gsub("^(.+)\\.gz$", "\\1", files[[i]], perl = TRUE))
                    f.unzipped <- paste(tmp.dirname, "/", f.unzipped, sep = "")
                    cmd <- paste("/bin/sh -c \"gunzip -q -c", files[[i]], ">", f.unzipped, "\"")
                    if (system(cmd)) {
                        stop(sprintf("Command failed: %s", cmd), call. = FALSE)
                    }
                    files[[i]] <- f.unzipped
                }
            }

            res <- .gcall("gintervals_import_genes", files[[1]], files[[2]], annots.names, .misha_env())
            res
        },
        finally = {
            unlink(tmp.dirname, recursive = TRUE)
        }
    )
}



#' Calculates an intersection of two sets of intervals
#'
#' Calculates an intersection of two sets of intervals.
#'
#' This function returns intervals that represent a genomic space which is
#' achieved by intersection of 'intervals1' and 'intervals2'.
#'
#' If 'intervals.set.out' is not 'NULL' the result is saved as an intervals
#' set. Use this parameter if the result size exceeds the limits of the
#' physical memory.
#'
#' @param intervals1,intervals2 set of intervals
#' @param intervals.set.out intervals set name where the function result is
#' optionally outputted
#' @return If 'intervals.set.out' is 'NULL' a data frame representing the
#' intersection of intervals.
#' @seealso \code{\link{gintervals.2d.band_intersect}},
#' \code{\link{gintervals.diff}}, \code{\link{gintervals.union}},
#' \code{\link{gintervals}}, \code{\link{gintervals.2d}}
#' @keywords ~intersect
#' @examples
#' \dontshow{
#' options(gmax.processes = 2)
#' }
#'
#' gdb.init_examples()
#'
#' intervs1 <- gscreen("dense_track > 0.15")
#' intervs2 <- gscreen("dense_track < 0.2")
#'
#' ## 'intervs3' and 'intervs4' are identical
#' intervs3 <- gintervals.intersect(intervs1, intervs2)
#' intervs4 <- gscreen("dense_track > 0.15 & dense_track < 0.2")
#'
#' @export gintervals.intersect
gintervals.intersect <- function(intervals1 = NULL, intervals2 = NULL, intervals.set.out = NULL) {
    if (is.null(intervals1) || is.null(intervals2)) {
        stop("Usage: gintervals.intersect(intervals1, intervals2, intervals.set.out = NULL)", call. = FALSE)
    }

    intervals.set.out <- do.call(.gexpr2str, list(substitute(intervals.set.out)), envir = parent.frame())

    if (.gintervals.is_bigset(intervals1) || .gintervals.is_bigset(intervals2) || !is.null(intervals.set.out)) {
        res <- NULL

        FUN <- function(intervals, intervals.set.out, envir) {
            intervals1 <- intervals[[1]]
            intervals2 <- intervals[[2]]
            chrom_res <- .gcall("gintervintersect", intervals1, intervals2, .misha_env())
            if (!is.null(chrom_res) && nrow(chrom_res) > 0) {
                if (is.null(intervals.set.out)) {
                    assign("res", c(get("res", envir = envir), list(chrom_res)), envir = envir)
                    .gverify_max_data_size(sum(unlist(lapply(get("res", envir), nrow))), arguments = "intervals.set.out")
                }
            }
            chrom_res
        }

        chroms1 <- gintervals.chrom_sizes(intervals1)
        chroms1$size <- NULL
        chroms2 <- gintervals.chrom_sizes(intervals2)
        chroms2$size <- NULL
        .gintervals.apply(merge(chroms1, chroms2), list(intervals1, intervals2), intervals.set.out, FUN, intervals.set.out, environment())

        if (!is.null(res)) {
            res <- do.call(.grbind, res)
        } # much faster than calling rbind incrementally in FUN

        if (is.null(intervals.set.out)) {
            if (!is.null(res) && nrow(res)) {
                res
            } else {
                NULL
            }
        } else {
            retv <- 0
        } # suppress return value
    } else {
        res <- .gcall("gintervintersect", intervals1, intervals2, .misha_env())
        res
    }
}



#' Returns number of intervals per chromosome
#'
#' Returns number of intervals per chromosome (or chromosome pair).
#'
#' This function returns number of intervals per chromosome (for 1D intervals)
#' or chromosome pair (for 2D intervals).
#'
#' @param intervals intervals set
#' @return Data frame representing number of intervals per chromosome (for 1D
#' intervals) or chromosome pair (for 2D intervals).
#' @seealso \code{\link{gintervals.load}}, \code{\link{gintervals.save}},
#' \code{\link{gintervals.exists}}, \code{\link{gintervals.ls}},
#' \code{\link{gintervals}}, \code{\link{gintervals.2d}}
#' @keywords ~intervals
#' @examples
#' \dontshow{
#' options(gmax.processes = 2)
#' }
#'
#' gdb.init_examples()
#' gintervals.chrom_sizes("annotations")
#'
#' @export gintervals.chrom_sizes
gintervals.chrom_sizes <- function(intervals = NULL) {
    if (is.null(intervals)) {
        stop("Usage: gintervals.chrom_sizes(intervals)", call. = FALSE)
    }
    .gcheckroot()

    intervals <- rescue_ALLGENOME(intervals, as.character(substitute(intervals)))

    if (.gintervals.is_bigset(intervals)) {
        if (.gintervals.big.is1d(intervals)) {
            stats <- .gintervals.big.meta(intervals)$stats
            res <- stats[, match(c("chrom", "size"), colnames(stats))]
        } else {
            stats <- .gintervals.big.meta(intervals)$stats
            res <- stats[, match(c("chrom1", "chrom2", "size"), colnames(stats))]
        }
    } else {
        res <- .gcall("gintervals_chrom_sizes", .gintervals.load_ext(intervals), .misha_env())
    }

    if (nrow(res) > 1) {
        rownames(res) <- 1:nrow(res)
    }
    res
}



#' Tests for big intervals set
#'
#' Tests for big intervals set.
#'
#' This function tests whether 'intervals.set' is a big intervals set.
#' Intervals set is big if it is stored in big intervals set format and given
#' the current limits it cannot be fully loaded into memory.
#'
#' Memory limit is controlled by 'gmax.data.size' option (see:
#' 'getOption("gmax.data.size")').
#'
#' @param intervals.set name of an intervals set
#' @return 'TRUE' if intervals set is big, otherwise 'FALSE'.
#' @seealso \code{\link{gintervals.load}}, \code{\link{gintervals.save}},
#' \code{\link{gintervals.exists}}, \code{\link{gintervals.ls}}
#' @keywords ~intervals
#' @examples
#' \dontshow{
#' options(gmax.processes = 2)
#' }
#'
#' gdb.init_examples()
#' gintervals.is.bigset("annotations")
#'
#' @export gintervals.is.bigset
gintervals.is.bigset <- function(intervals.set = NULL) {
    if (is.null(intervals.set)) {
        stop("Usage: gintervals.is.bigset(intervals.set)", call. = FALSE)
    }
    .gcheckroot()

    .gintervals.is_bigset(intervals.set) && !.gintervals.loadable(intervals.set)
}



#' Converts intervals from another assembly
#'
#' Converts intervals from another assembly to the current one.
#'
#' This function converts 'intervals' from another assembly to the current one.
#' Chain file instructs how the conversion of coordinates should be done. It
#' can be either a name of a chain file or a data frame in the same format as
#' returned by 'gintervals.load_chain' function.
#'
#' The converted intervals are returned. An additional column named
#' 'intervalID' is added to the resulted data frame. For each interval in the
#' resulted intervals it indicates the index of the original interval.
#'
#' @param intervals intervals from another assembly
#' @param chain name of chain file or data frame as returned by
#' 'gintervals.load_chain'
#' @return A data frame representing the converted intervals.
#' @seealso \code{\link{gintervals.load_chain}}, \code{\link{gtrack.liftover}},
#' \code{\link{gintervals}}
#' @keywords ~intervals ~liftover ~chain
#' @examples
#' \dontshow{
#' options(gmax.processes = 2)
#' }
#'
#' gdb.init_examples()
#' chainfile <- paste(.misha$GROOT, "data/test.chain", sep = "/")
#' intervs <- data.frame(
#'     chrom = "chr25", start = c(0, 7000),
#'     end = c(6000, 20000)
#' )
#' gintervals.liftover(intervs, chainfile)
#'
#' @export gintervals.liftover
gintervals.liftover <- function(intervals = NULL, chain = NULL) {
    if (is.null(intervals) || is.null(chain)) {
        stop("Usage: gintervals.liftover(intervals, chain)", call. = FALSE)
    }
    .gcheckroot()

    intervals <- rescue_ALLGENOME(intervals, as.character(substitute(intervals)))

    if (is.character(chain)) {
        chain.intervs <- gintervals.load_chain(chain)
    } else {
        chain.intervs <- chain
    }

    .gcall("gintervs_liftover", intervals, chain.intervs, .misha_env())
}



#' Loads a named intervals set
#'
#' Loads a named intervals set.
#'
#' This function loads and returns intervals stored in a named intervals set.
#'
#' If intervals set contains 1D intervals and 'chrom' is not 'NULL' only the
#' intervals of the given chromosome are returned.
#'
#' Likewise if intervals set contains 2D intervals and 'chrom1', 'chrom2' are
#' not 'NULL' only the intervals of the given pair of chromosomes are returned.
#'
#' For big intervals sets 'chrom' parameter (1D case) / 'chrom1', 'chrom2'
#' parameters (2D case) must be specified. In other words: big intervals sets
#' can be loaded only by chromosome or chromosome pair.
#'
#' @param intervals.set name of an intervals set
#' @param chrom chromosome for 1D intervals set
#' @param chrom1 first chromosome for 2D intervals set
#' @param chrom2 second chromosome for 2D intervals set
#' @return A data frame representing the intervals.
#' @seealso \code{\link{gintervals.save}}, \code{\link{gintervals.is.bigset}},
#' \code{\link{gintervals.exists}}, \code{\link{gintervals.ls}},
#' \code{\link{gintervals}}, \code{\link{gintervals.2d}}
#' @keywords ~intervals
#' @examples
#' \dontshow{
#' options(gmax.processes = 2)
#' }
#'
#' gdb.init_examples()
#' gintervals.load("annotations")
#'
#' @export gintervals.load
gintervals.load <- function(intervals.set = NULL, chrom = NULL, chrom1 = NULL, chrom2 = NULL) {
    .gintervals.load_ext(intervals.set, chrom, chrom1, chrom2, TRUE)
}



#' Loads assembly conversion table from a chain file
#'
#' Loads assembly conversion table from a chain file.
#'
#' This function reads a file in 'chain' format and returns assembly conversion
#' table that can be used in 'gtrack.liftover' and 'gintervals.liftover'.
#'
#' Note: chain file might map a few different source intervals into a single
#' target one. These ambiguous mappings are not presented in the data frame
#' returned by 'gintervals.load_chain'.
#'
#' @param file name of chain file
#' @return A data frame representing assembly conversion table.
#' @seealso \code{\link{gintervals.liftover}}, \code{\link{gtrack.liftover}}
#' @keywords ~intervals ~liftover ~chain
#' @examples
#' \dontshow{
#' options(gmax.processes = 2)
#' }
#'
#' gdb.init_examples()
#' chainfile <- paste(.misha$GROOT, "data/test.chain", sep = "/")
#' gintervals.load_chain(chainfile)
#'
#' @export gintervals.load_chain
gintervals.load_chain <- function(file = NULL) {
    if (is.null(file)) {
        stop("Usage: gintervals.load_chain(file)", call. = FALSE)
    }
    .gcall("gchain2interv", file, .misha_env())
}



#' Returns a list of named intervals sets
#'
#' Returns a list of named intervals sets in Genomic Database.
#'
#' This function returns a list of named intervals sets that match the pattern
#' (see 'grep'). If called without any arguments all named intervals sets are
#' returned.
#'
#' @param pattern,ignore.case,perl,fixed,useBytes see 'grep'
#' @return An array that contains the names of intervals sets.
#' @seealso \code{\link{grep}}, \code{\link{gintervals.exists}},
#' \code{\link{gintervals.load}}, \code{\link{gintervals.save}},
#' \code{\link{gintervals.rm}}, \code{\link{gintervals}},
#' \code{\link{gintervals.2d}}
#' @keywords ~intervals ~ls
#' @examples
#' \dontshow{
#' options(gmax.processes = 2)
#' }
#'
#' gdb.init_examples()
#' gintervals.ls()
#' gintervals.ls(pattern = "annot*")
#'
#' @export gintervals.ls
gintervals.ls <- function(pattern = "", ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE) {
    .gcheckroot()
    grep(pattern, get("GINTERVS", envir = .misha), value = TRUE, ignore.case = ignore.case, perl = perl, fixed = fixed, useBytes = useBytes)
}



#' Applies a function to values of track expressions
#'
#' Applies a function to values of track expressions for each interval.
#'
#' This function evaluates track expressions for each interval from
#' 'intervals'. The resulted vectors are passed then as arguments to 'FUN'.
#'
#' If the intervals are one-dimensional and have an additional column named
#' 'strand' whose value is '-1', the values of the track expression are placed
#' to the vector in reverse order.
#'
#' The current interval index (1-based) is stored in 'GAPPLY.INTERVID' variable
#' that is available during the execution of 'gintervals.mapply'. There is no
#' guarantee about the order in which the intervals are processed. Do not rely
#' on any specific order and use 'GITERATOR.INTERVID' variable to detect the
#' current interval id.
#'
#' If 'enable.gapply.intervals' is 'TRUE', an additional variable
#' 'GAPPLY.INTERVALS' is defined during the execution of 'gintervals.mapply'.
#' This variable stores the current iterator intervals prior to track
#' expression evaluation. Please note that setting 'enable.gapply.intervals' to
#' 'TRUE' might severely affect the run-time of the function.
#'
#' Note: all the changes made in R environment by 'FUN' will be void if
#' multitasking mode is switched on. One should also refrain from performing
#' any other operations in 'FUN' that might be not "thread-safe" such as
#' updating files, etc. Please switch off multitasking ('options(gmultitasking
#' = FALSE)') if you wish to perform such operations.
#'
#' If 'intervals.set.out' is not 'NULL' the result is saved as an intervals
#' set. Use this parameter if the result size exceeds the limits of the
#' physical memory.
#'
#' @param FUN function to apply, found via ‘match.fun’
#' @param ... track expressions whose values are used as arguments for 'FUN'
#' @param intervals intervals for which track expressions are calculated
#' @param enable.gapply.intervals if 'TRUE', then a variable 'GAPPLY.INTERVALS'
#' is available
#' @param iterator track expression iterator. If 'NULL' iterator is determined
#' implicitly based on track expressions.
#' @param band track expression band. If 'NULL' no band is used.
#' @param intervals.set.out intervals set name where the function result is
#' optionally outputted
#' @return If 'intervals.set.out' is 'NULL' a data frame representing intervals
#' with an additional column that contains the return values of 'FUN'.
#' @seealso \code{\link{mapply}}
#' @keywords ~apply ~mapply
#' @examples
#' \dontshow{
#' options(gmax.processes = 2)
#' }
#'
#' gdb.init_examples()
#' gintervals.mapply(
#'     max, "dense_track",
#'     gintervals(c(1, 2), 0, 10000)
#' )
#' gintervals.mapply(
#'     function(x, y) {
#'         max(x + y)
#'     }, "dense_track",
#'     "sparse_track", gintervals(c(1, 2), 0, 10000),
#'     iterator = "sparse_track"
#' )
#'
#' @export gintervals.mapply
gintervals.mapply <- function(FUN = NULL, ..., intervals = NULL, enable.gapply.intervals = FALSE, iterator = NULL, band = NULL, intervals.set.out = NULL) {
    assign("GINTERVID", -1, envir = .misha)
    args <- as.list(substitute(list(...)))[-1L]
    if (is.null(intervals) && length(args) < 2 || !is.null(intervals) && length(args) < 1) {
        stop("Usage: gintervals.mapply(FUN, [expr]+, intervals, enable.gapply.intervals = FALSE, iterator = NULL, intervals.set.out = NULL)", call. = FALSE)
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

    if (exists("GAPPLY.INTERVALS", envir = .misha)) {
        remove(list = "GAPPLY.INTERVALS", envir = .misha)
    }

    intervals.set.out <- do.call(.gexpr2str, list(substitute(intervals.set.out)), envir = parent.frame())

    if (.gintervals.is_bigset(intervals) || !is.null(intervals.set.out)) {
        res <- NULL

        INTERVALS_FUN <- function(intervals, intervals.set.out, envir) {
            intervals <- intervals[[1]]
            chrom_res <- .gcall("gmapply", intervals, FUN, tracks, enable.gapply.intervals, .iterator, band, FALSE, .misha_env())
            if (!is.null(chrom_res) && nrow(chrom_res) > 0) {
                if (is.null(intervals.set.out)) {
                    assign("res", c(get("res", envir = envir), list(chrom_res)), envir = envir)
                    .gverify_max_data_size(sum(unlist(lapply(get("res", envir), nrow))), arguments = "intervals.set.out")
                }
            }
            chrom_res
        }

        .gintervals.apply(gintervals.chrom_sizes(intervals), intervals, intervals.set.out, INTERVALS_FUN, intervals.set.out, environment())

        if (!is.null(res)) {
            res <- do.call(.grbind, res)
        } # much faster than calling rbind incrementally in FUN

        if (is.null(intervals.set.out)) {
            if (!is.null(res) && nrow(res)) {
                res
            } else {
                NULL
            }
        } else {
            retv <- 0
        } # suppress return value
    } else {
        if (.ggetOption("gmultitasking")) {
            .gcall("gmapply_multitask", intervals, FUN, tracks, enable.gapply.intervals, .iterator, band, TRUE, .misha_env())
        } else {
            .gcall("gmapply", intervals, FUN, tracks, enable.gapply.intervals, .iterator, band, TRUE, .misha_env())
        }
    }
}



#' Finds neighbors between two sets of intervals
#'
#' Finds neighbors between two sets of intervals.
#'
#' This function finds for each interval in 'intervals1' the closest
#' 'maxneighbors' intervals from 'intervals2'.
#'
#' For 1D intervals the distance must fall in the range of ['mindist',
#' 'maxdist']. If 'intervals2' contains a 'strand' column the distance can be
#' positive or negative depending on the 'strand' value and the position of
#' interval2 relatively to interval1. If 'strand' column is missing the
#' distance is always positive.
#'
#' For 2D intervals two distances are calculated and returned for each axis.
#' The distances must fall in the range of ['mindist1', 'maxdist1'] for axis 1
#' and ['mindist2', 'maxdist2'] for axis 2. For selecting the closest
#' 'maxneighbors' intervals Manhattan distance is used (i.e. dist1+dist2).
#'
#' The names of the returned columns are made unique using
#' \code{make.unique(colnames(df), sep = "")}, assuming 'df' is the result.
#'
#' If 'intervals.set.out' is not 'NULL' the result is saved as an intervals
#' set. Use this parameter if the result size exceeds the limits of the
#' physical memory.
#'
#' @param intervals1,intervals2 intervals
#' @param maxneighbors maximal number of neighbors
#' @param mindist,maxdist distance range for 1D intervals
#' @param mindist1,maxdist1,mindist2,maxdist2 distance range for 2D intervals
#' @param na.if.notfound if 'TRUE' return 'NA' interval if no matching
#' neighbors were found, otherwise omit the interval in the answer
#' @param intervals.set.out intervals set name where the function result is
#' optionally outputted
#' @return If 'intervals.set.out' is 'NULL' a data frame containing the pairs
#' of intervals from 'intervals1', intervals from 'intervals2' and an
#' additional column named 'dist' ('dist1' and 'dist2' for 2D intervals)
#' representing the distance between the corresponding intervals. The intervals
#' from intervals2 would be changed to 'chrom1', 'start1', and 'end1' and for
#' 2D intervals chrom11, start11, end11 and chrom22, start22, end22. If
#' 'na.if.notfound' is 'TRUE', the data frame contains all the intervals from
#' 'intervals1' including those for which no matching neighbor was found. For
#' the latter intervals an 'NA' neighboring interval is stated. If
#' 'na.if.notfound' is 'FALSE', the data frame contains only intervals from
#' 'intervals1' for which matching neighbor(s) was found.
#' @seealso \code{\link{gintervals}},
#' @keywords ~intervals ~annotate ~nearest ~neighbor ~neighbors
#' @examples
#' \dontshow{
#' options(gmax.processes = 2)
#' }
#'
#' gdb.init_examples()
#' intervs1 <- giterator.intervals("dense_track",
#'     gintervals(1, 0, 4000),
#'     iterator = 233
#' )
#' intervs2 <- giterator.intervals(
#'     "sparse_track",
#'     gintervals(1, 0, 2000)
#' )
#' gintervals.neighbors(intervs1, intervs2, 10,
#'     mindist = -300,
#'     maxdist = 500
#' )
#' intervs2$strand <- c(1, 1, -1, 1)
#' gintervals.neighbors(intervs1, intervs2, 10,
#'     mindist = -300,
#'     maxdist = 500
#' )
#'
#' @export gintervals.neighbors
gintervals.neighbors <- function(intervals1 = NULL, intervals2 = NULL, maxneighbors = 1, mindist = -1e+09, maxdist = 1e+09,
                                 mindist1 = -1e+09, maxdist1 = 1e+09, mindist2 = -1e+09, maxdist2 = 1e+09,
                                 na.if.notfound = FALSE, intervals.set.out = NULL) {
    if (is.null(intervals1) || is.null(intervals2)) {
        stop(paste("Usage: gintervals.neighbors(intervals1, intervals2, maxneighbors = 1, mindist = -1e+09, maxdist = 1e+09, ",
            "mindist1 = -1e+09, maxdist1 = 1e+09, mindist2 = -1e+09, maxdist2 = 1e+09, na.if.notfound = FALSE, intervals.set.out = NULL)",
            sep = ""
        ), call. = FALSE)
    }

    if (is.null(colnames)) {
        intervals1name <- deparse(substitute(intervals1), width.cutoff = 500)[1]
        intervals2name <- deparse(substitute(intervals2), width.cutoff = 500)[1]
    }

    intervals.set.out <- do.call(.gexpr2str, list(substitute(intervals.set.out)), envir = parent.frame())

    if (.gintervals.is_bigset(intervals1) || .gintervals.is_bigset(intervals2) || !is.null(intervals.set.out)) {
        res <- NULL

        FUN <- function(intervals, intervals.set.out, envir) {
            intervals1 <- intervals[[1]]
            intervals2 <- intervals[[2]]
            chrom_res <- .gcall("gfind_neighbors", intervals1, intervals2, maxneighbors, mindist, maxdist, mindist1, maxdist1, mindist2, maxdist2, na.if.notfound, FALSE, .misha_env())
            if (!is.null(chrom_res) && is.null(intervals.set.out)) {
                assign("res", c(get("res", envir = envir), list(chrom_res)), envir = envir)
                .gverify_max_data_size(sum(unlist(lapply(get("res", envir), nrow))), arguments = "intervals.set.out")
            }
            chrom_res
        }

        if (na.if.notfound) {
            .gintervals.apply(gintervals.chrom_sizes(intervals1), list(intervals1, intervals2), intervals.set.out, FUN, intervals.set.out, environment())
        } else {
            chroms1 <- gintervals.chrom_sizes(intervals1)
            chroms1$size <- NULL
            chroms2 <- gintervals.chrom_sizes(intervals2)
            chroms2$size <- NULL
            .gintervals.apply(merge(chroms1, chroms2), list(intervals1, intervals2), intervals.set.out, FUN, intervals.set.out, environment())
        }

        if (!is.null(res)) {
            res <- do.call(.grbind, res)
        } # much faster than calling rbind incrementally in FUN

        if (is.null(intervals.set.out)) {
            if (!is.null(res) && nrow(res)) {
                repair_names(res)
            } else {
                NULL
            }
        } else {
            retv <- 0
        } # suppress return value
    } else {
        intervals1 <- .gintervals.load_ext(intervals1)
        intervals2 <- .gintervals.load_ext(intervals2)
        res <- .gcall("gfind_neighbors", intervals1, intervals2, maxneighbors, mindist, maxdist, mindist1, maxdist1, mindist2, maxdist2, na.if.notfound, TRUE, .misha_env())
        repair_names(res)
    }
}

repair_names <- function(dataframe) {
    # if there are duplicate names - add a numeric suffix
    if (length(unique(names(dataframe))) < length(names(dataframe))) {
        names(dataframe) <- make.unique(names(dataframe), sep = "")
    }
    return(dataframe)
}



#' Calculates quantiles of a track expression for intervals
#'
#' Calculates quantiles of a track expression for intervals.
#'
#' This function calculates quantiles of 'expr' for each interval in
#' 'intervals'.
#'
#' If 'intervals.set.out' is not 'NULL' the result is saved as an intervals
#' set. Use this parameter if the result size exceeds the limits of the
#' physical memory.
#'
#' @param expr track expression for which quantiles are calculated
#' @param percentiles an array of percentiles of quantiles in [0, 1] range
#' @param intervals set of intervals
#' @param iterator track expression iterator. If 'NULL' iterator is determined
#' implicitly based on track expressions.
#' @param band track expression band. If 'NULL' no band is used.
#' @param intervals.set.out intervals set name where the function result is
#' optionally outputted
#' @return If 'intervals.set.out' is 'NULL' a set of intervals with additional
#' columns representing quantiles for each percentile.
#' @seealso \code{\link{gquantiles}}, \code{\link{gbins.quantiles}}
#' @keywords ~quantiles ~percentiles
#' @examples
#' \dontshow{
#' options(gmax.processes = 2)
#' }
#'
#' gdb.init_examples()
#' intervs <- gintervals(c(1, 2), 0, 5000)
#' gintervals.quantiles("dense_track",
#'     percentiles = c(0.5, 0.3, 0.9), intervs
#' )
#'
#' @export gintervals.quantiles
gintervals.quantiles <- function(expr = NULL, percentiles = 0.5, intervals = NULL, iterator = NULL, band = NULL, intervals.set.out = NULL) {
    if (is.null(substitute(expr)) || is.null(intervals)) {
        stop("Usage: gintervals.quantiles(expr, percentiles = 0.5, intervals, iterator = NULL, band = NULL, intervals.set.out = NULL)", call. = FALSE)
    }
    .gcheckroot()

    intervals <- rescue_ALLGENOME(intervals, as.character(substitute(intervals)))

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
                res <- .gcall("gintervals_quantiles_multitask", intervals, exprstr, percentiles, .iterator, band, intervals.set.out, .misha_env())
            } else {
                res <- .gcall("gintervals_quantiles", intervals, exprstr, percentiles, .iterator, band, intervals.set.out, .misha_env())
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
    if (is.null(intervals.set.out)) {
        res
    } else {
        .gdb.add_intervals.set(intervals.set.out)
        retv <- 0 # suppress return value
    }
}



#' Combines several sets of intervals
#'
#' Combines several sets of intervals into one set.
#'
#' This function combines several intervals sets into one set. It works in a
#' similar manner as 'rbind' yet it is faster. Also it supports intervals sets
#' that are stored in files including the big intervals sets.
#'
#' If 'intervals.set.out' is not 'NULL' the result is saved as an intervals
#' set. If the format of the output intervals is set to be "big" (determined
#' implicitly based on the result size and options), the order of the resulted
#' intervals is altered as they are sorted by chromosome (or chromosomes pair -
#' for 2D).
#'
#' @param ... intervals sets to combine
#' @param intervals intervals set
#' @param intervals.set.out intervals set name where the function result is
#' optionally outputted
#' @return If 'intervals.set.out' is 'NULL' a data frame combining intervals
#' sets.
#' @seealso \code{\link{gintervals}}, \code{\link{gintervals.2d}},
#' \code{\link{gintervals.canonic}}
#' @keywords ~rbind
#' @examples
#' \dontshow{
#' options(gmax.processes = 2)
#' }
#'
#' gdb.init_examples()
#'
#' intervs1 <- gextract("sparse_track", gintervals(c(1, 2), 1000, 4000))
#' intervs2 <- gextract("sparse_track", gintervals(c(2, "X"), 2000, 5000))
#' gintervals.save("testintervs", intervs2)
#' gintervals.rbind(intervs1, "testintervs")
#' gintervals.rm("testintervs", force = TRUE)
#'
#' @export gintervals.rbind
gintervals.rbind <- function(..., intervals.set.out = NULL) {
    intervals <- list(...)
    if (!length(intervals)) {
        stop("Usage: gintervals.rbind([intervals]+, intervals.set.out = NULL)", call. = FALSE)
    }
    .gcheckroot()

    intervals.set.out <- do.call(.gexpr2str, list(substitute(intervals.set.out)), envir = parent.frame())

    res <- NULL
    if (any(unlist(lapply(intervals, function(intervals) {
        .gintervals.is_bigset(intervals)
    }))) || !is.null(intervals.set.out)) {
        if (is.null(intervals.set.out)) {
            FUN <- function(intervals, intervals.set.out, envir) {
                assign("res", c(get("res", envir = envir), intervals), envir = envir)
                .gverify_max_data_size(sum(unlist(lapply(get("res", envir), nrow))), arguments = "intervals.set.out")
                intervals[[1]]
            }

            # preserve the order of intervals inside the answer
            lapply(intervals, f <- function(intervals) {
                .gintervals.apply(gintervals.chrom_sizes(intervals), intervals, NULL, FUN, NULL, parent.frame(2))
            })
            if (!is.null(res)) {
                res <- do.call(.grbind, res)
            } # much faster than calling rbind incrementally in FUN
        } else {
            FUN <- function(intervals, intervals.set.out, envir) {
                intervals <- do.call(.grbind, intervals)
                intervals
            }

            # use for .gintervals.apply chromosomes from all intervals
            chroms <- NULL
            chroms <- lapply(intervals, gintervals.chrom_sizes)
            chroms <- do.call(rbind, chroms)
            if (.gintervals.is1d(intervals[[1]])) {
                chroms <- factor(chroms$chrom, levels(chroms$chrom))
                chroms <- unique(chroms)
                chroms <- sort(chroms)
                chroms <- data.frame(chrom = chroms)
            } else {
                chroms <- data.frame(chrom1 = chroms$chrom1, chrom2 = chroms$chrom2)
                chroms <- unique(chroms)
                chroms <- chroms[with(chroms, order(chrom1, chrom2)), ]
            }

            .gintervals.apply(chroms, intervals, intervals.set.out, FUN, intervals.set.out, environment())
        }
    } else {
        intervals <- lapply(intervals, .gintervals.load_ext)
        res <- do.call(.grbind, intervals) # much faster than calling rbind incrementally in FUN
    }

    if (is.null(intervals.set.out)) {
        if (!is.null(res) && nrow(res)) {
            res
        } else {
            NULL
        }
    } else {
        retv <- 0
    } # suppress return value
}



#' Deletes a named intervals set
#'
#' Deletes a named intervals set.
#'
#' This function deletes a named intervals set from the Genomic Database. By
#' default 'gintervals.rm' requires the user to interactively confirm the
#' deletion. Set 'force' to 'TRUE' to suppress the user prompt.
#'
#' @param intervals.set name of an intervals set
#' @param force if 'TRUE', suppresses user confirmation of a named intervals set
#' removal
#' @return None.
#' @seealso \code{\link{gintervals.save}}, \code{\link{gintervals.exists}},
#' \code{\link{gintervals.ls}}, \code{\link{gintervals}},
#' \code{\link{gintervals.2d}}
#' @keywords ~intervals
#' @examples
#' \dontshow{
#' options(gmax.processes = 2)
#' }
#'
#' gdb.init_examples()
#' intervs <- gintervals(c(1, 2))
#' gintervals.save("testintervs", intervs)
#' gintervals.ls()
#' gintervals.rm("testintervs", force = TRUE)
#' gintervals.ls()
#'
#' @export gintervals.rm
gintervals.rm <- function(intervals.set = NULL, force = FALSE) {
    if (is.null(substitute(intervals.set))) {
        stop("Usage: gintervals.rm(intervals.set, force = FALSE)", call. = FALSE)
    }
    .gcheckroot()

    intervals.set <- do.call(.gexpr2str, list(substitute(intervals.set)), envir = parent.frame())

    # check whether intervals.set appears among GINTERVS
    if (is.na(match(intervals.set, get("GINTERVS", envir = .misha)))) {
        if (force) {
            return(invisible())
        }
        stop(sprintf("Intervals set %s does not exist", intervals.set), call. = FALSE)
    }

    answer <- "N"
    if (force) {
        answer <- "Y"
    } else {
        str <- sprintf("Are you sure you want to delete intervals set %s (Y/N)? ", intervals.set)
        message(str)
        answer <- toupper(readLines(n = 1))
    }

    if (answer == "Y" || answer == "YES") {
        fname <- sprintf("%s.interv", paste(get("GWD", envir = .misha), gsub("\\.", "/", intervals.set), sep = "/"))

        # remove the intervals set
        unlink(fname, recursive = TRUE)

        if (file.exists(fname)) {
            message(sprintf("Failed to delete intervals set %s", intervals.set))
        } else {
            # refresh the list of GINTERVS, etc.
            .gdb.rm_intervals.set(intervals.set)
        }
    }
}



#' Creates a named intervals set
#'
#' Saves intervals to a named intervals set.
#'
#' This function saves 'intervals' as a named intervals set.
#'
#' @param intervals.set.out name of the new intervals set
#' @param intervals intervals to save
#' @return None.
#' @seealso \code{\link{gintervals.rm}}, \code{\link{gintervals.load}},
#' \code{\link{gintervals.exists}}, \code{\link{gintervals.ls}},
#' \code{\link{gintervals}}, \code{\link{gintervals.2d}}
#' @keywords ~intervals
#' @examples
#' \dontshow{
#' options(gmax.processes = 2)
#' }
#'
#' gdb.init_examples()
#' intervs <- gintervals(c(1, 2))
#' gintervals.save("testintervs", intervs)
#' gintervals.ls()
#' gintervals.rm("testintervs", force = TRUE)
#'
#' @export gintervals.save
gintervals.save <- function(intervals.set.out = NULL, intervals = NULL) {
    if (is.null(substitute(intervals.set.out)) || is.null(intervals)) {
        stop("Usage: gintervals.save(intervals.set.out, intervals)", call. = FALSE)
    }
    .gcheckroot()

    intervals <- rescue_ALLGENOME(intervals, as.character(substitute(intervals)))

    intervals.set.out <- do.call(.gexpr2str, list(substitute(intervals.set.out)), envir = parent.frame())
    .gintervals.apply(gintervals.chrom_sizes(intervals), intervals, intervals.set.out, function(intervs, ...) {
        intervs[[1]]
    })
    retv <- NULL
}



#' Updates a named intervals set
#'
#' Updates a named intervals set.
#'
#' This function replaces all intervals of given chromosome (or chromosome
#' pair) within 'intervals.set' with 'intervals'. Chromosome is specified by
#' 'chrom' for 1D intervals set or 'chrom1', 'chrom2' for 2D intervals set.
#'
#' If 'intervals' is 'NULL' all intervals of given chromosome are removed from
#' 'intervals.set'.
#'
#' @param intervals.set name of an intervals set
#' @param intervals intervals or 'NULL'
#' @param chrom chromosome for 1D intervals set
#' @param chrom1 first chromosome for 2D intervals set
#' @param chrom2 second chromosome for 2D intervals set
#' @return None.
#' @seealso \code{\link{gintervals.save}}, \code{\link{gintervals.load}},
#' \code{\link{gintervals.exists}}, \code{\link{gintervals.ls}}
#' @keywords ~intervals
#' @examples
#' \dontshow{
#' options(gmax.processes = 2)
#' }
#'
#' gdb.init_examples()
#' intervs <- gscreen(
#'     "sparse_track > 0.2",
#'     gintervals(c(1, 2), 0, 10000)
#' )
#' gintervals.save("testintervs", intervs)
#' gintervals.load("testintervs")
#' gintervals.update("testintervs", intervs[intervs$chrom == "chr2", ][1:5, ], chrom = 2)
#' gintervals.load("testintervs")
#' gintervals.update("testintervs", NULL, chrom = 2)
#' gintervals.load("testintervs")
#' gintervals.rm("testintervs", force = TRUE)
#'
#' @export gintervals.update
gintervals.update <- function(intervals.set = NULL, intervals = "", chrom = NULL, chrom1 = NULL, chrom2 = NULL) {
    if (is.null(substitute(intervals.set)) || identical(intervals, "")) {
        stop("Usage: gintervals.update(intervals.set, intervals, chrom = NULL, chrom1 = NULL, chrom2 = NULL)", call. = FALSE)
    }
    .gcheckroot()

    intervals <- rescue_ALLGENOME(intervals, as.character(substitute(intervals)))

    if (identical(intervals.set, intervals)) {
        return(retv <- NULL)
    }

    if (is.null(chrom) && is.null(chrom1) && is.null(chrom2)) {
        stop("Chromosome must be specified in chrom (for 2D intervals: chrom1, chrom2) parameter", call. = FALSE)
    }

    if (!is.null(chrom)) {
        chrom <- .gchroms(chrom)
        if (length(chrom) > 1) {
            stop("chrom parameter should mark only one chromosome")
        }
    }

    if (!is.null(chrom1)) {
        chrom1 <- .gchroms(chrom1)
        if (length(chrom1) > 1) {
            stop("chrom1 parameter should mark only one chromosome")
        }
    }

    if (!is.null(chrom2)) {
        chrom2 <- .gchroms(chrom2)
        if (length(chrom2) > 1) {
            stop("chrom2 parameter should mark only one chromosome")
        }
    }

    if (!is.null(chrom) && !is.null(chrom1)) {
        stop("Cannot use chrom and chrom1 parameters in the same call", call. = FALSE)
    }

    if (!is.null(chrom) && !is.null(chrom2)) {
        stop("Cannot use chrom and chrom2 parameters in the same call", call. = FALSE)
    }

    if (!is.character(intervals.set) || length(intervals.set) != 1) {
        stop("Invalid format of intervals.set parameter", call. = FALSE)
    }

    if (is.na(match(intervals.set, get("GINTERVS", envir = .misha)))) {
        stop(sprintf("Intervals set %s does not exist", intervals.set), call. = FALSE)
    }

    path <- gsub(".", "/", intervals.set, fixed = TRUE)
    path <- paste(path, ".interv", sep = "")
    fullpath <- paste(get("GWD", envir = .misha), path, sep = "/")

    if (!is.null(intervals)) {
        if (!is.null(chrom)) {
            intervals <- .gintervals.load_ext(intervals, chrom = chrom)
        } else {
            intervals <- .gintervals.load_ext(intervals, chrom1 = chrom1, chrom2 = chrom2)
        }
    }

    # big: update stats (including delete), save chrom (or delete), convert to small if needed
    if (.gintervals.is_bigset(intervals.set)) {
        is1d <- .gintervals.big.is1d(intervals.set)
        meta <- .gintervals.big.meta(intervals.set)
        stats <- meta$stats
        zeroline <- meta$zeroline

        if (!is.null(intervals) && !identical(sapply(intervals, "class"), sapply(zeroline, "class"))) {
            stop(sprintf("Cannot update intervals set %s: columns differ", intervals.set), call. = FALSE)
        }

        if (is1d) {
            if (is.null(chrom)) {
                stop("chrom parameter is not specified", call. = FALSE)
            }
            idx <- which(stats$chrom == chrom)
            if (length(idx) > 0) {
                stats <- stats[-idx, ]
            }
            if (!is.null(intervals) && nrow(intervals)) {
                stat <- .gcall("gintervals_stats", intervals, .misha_env())
                stats <- rbind(stats, data.frame(chrom = chrom, stat))
                stats <- stats[order(stats$chrom), ]
            }
            .gintervals.big.save(fullpath, intervals, chrom = chrom)
        } else {
            if (is.null(chrom1) || is.null(chrom2)) {
                stop("chrom1 and chrom2 parameters must be specified", call. = FALSE)
            }
            idx <- which(stats$chrom1 == chrom1 & stats$chrom2 == chrom2)
            if (length(idx) > 0) {
                stats <- stats[-idx, ]
            }
            if (!is.null(intervals) && nrow(intervals)) {
                stat <- .gcall("gintervals_stats", intervals, .misha_env())
                stats <- rbind(stats, data.frame(chrom1 = chrom1, chrom2 = chrom2, stat))
                stats <- stats[order(stats$chrom1, stats$chrom2), ]
            }
            .gintervals.big.save(fullpath, intervals, chrom1 = chrom1, chrom2 = chrom2)
        }

        if (nrow(stats) > 1) {
            rownames(stats) <- 1:nrow(stats)
        }
        .gintervals.big.save_meta(fullpath, stats, zeroline)

        if (!.gintervals.needs_bigset(intervals.set)) {
            .gintervals.big2small(intervals.set)
        }
    }

    # small: load all, update in place (including delete), save back, convert to big if needed
    else {
        tgt.intervals <- .gintervals.load_ext(intervals.set)
        is1d <- .gintervals.is1d(intervals.set)

        if (!is.null(intervals) && !identical(sapply(intervals, "class"), sapply(tgt.intervals, "class"))) {
            stop(sprintf("Cannot update intervals set %s: columns differ", intervals.set), call. = FALSE)
        }

        if (is1d) {
            if (is.null(chrom)) {
                stop("chrom parameter is not specified", call. = FALSE)
            }
            idx <- which(tgt.intervals$chrom == chrom)
            if (length(idx) > 0) {
                tgt.intervals <- tgt.intervals[-idx, ]
            }
            if (!is.null(intervals) && nrow(intervals)) {
                tgt.intervals <- .grbind(tgt.intervals, intervals)
                tgt.intervals <- tgt.intervals[order(tgt.intervals$chrom), ]
            }
        } else {
            if (is.null(chrom1) || is.null(chrom2)) {
                stop("chrom1 and chrom2 parameters must be specified", call. = FALSE)
            }
            idx <- which(tgt.intervals$chrom1 == chrom1 & tgt.intervals$chrom2 == chrom2)
            if (length(idx) > 0) {
                tgt.intervals <- tgt.intervals[-idx, ]
            }
            if (!is.null(intervals) && nrow(intervals)) {
                tgt.intervals <- .grbind(tgt.intervals, intervals)
                tgt.intervals <- tgt.intervals[order(tgt.intervals$chrom1, tgt.intervals$chrom2), ]
            }
        }
        if (.gintervals.needs_bigset(tgt.intervals)) {
            .gintervals.small2big(intervals.set, tgt.intervals)
        } else {
            .gintervals.save_file(fullpath, tgt.intervals)
        }
    }

    retv <- 0 # suppress return value
}



#' Calculates summary statistics of track expression for intervals
#'
#' Calculates summary statistics of track expression for intervals.
#'
#' This function returns summary statistics of a track expression for each
#' interval 'intervals': total number of bins, total number of bins whose value
#' is NaN, min, max, sum, mean and standard deviation of the values.
#'
#' If 'intervals.set.out' is not 'NULL' the result is saved as an intervals
#' set. Use this parameter if the result size exceeds the limits of the
#' physical memory.
#'
#' @param expr track expression
#' @param intervals set of intervals
#' @param iterator track expression iterator. If 'NULL' iterator is determined
#' implicitly based on track expression.
#' @param band track expression band. If 'NULL' no band is used.
#' @param intervals.set.out intervals set name where the function result is
#' optionally outputted
#' @return If 'intervals.set.out' is 'NULL' a set of intervals with additional
#' columns representing summary statistics for each percentile and interval.
#' @seealso \code{\link{gsummary}}, \code{\link{gbins.summary}}
#' @keywords ~summary ~statistics
#' @examples
#' \dontshow{
#' options(gmax.processes = 2)
#' }
#'
#' gdb.init_examples()
#' intervs <- gintervals(c(1, 2), 0, 5000)
#' gintervals.summary("dense_track", intervs)
#'
#' @export gintervals.summary
gintervals.summary <- function(expr = NULL, intervals = NULL, iterator = NULL, band = NULL, intervals.set.out = NULL) {
    if (is.null(substitute(expr)) || is.null(intervals)) {
        stop("Usage: gintervals.summary(expr, intervals, iterator = NULL, band = NULL, intervals.set.out = NULL)", call. = FALSE)
    }
    .gcheckroot()

    intervals <- rescue_ALLGENOME(intervals, as.character(substitute(intervals)))

    exprstr <- do.call(.gexpr2str, list(substitute(expr)), envir = parent.frame())
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
                res <- .gcall("gintervals_summary", exprstr, intervals, .iterator, band, intervals.set.out, .misha_env())
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



#' Calculates a union of two sets of intervals
#'
#' Calculates a union of two sets of intervals.
#'
#' This function returns intervals that represent a genomic space covered by
#' either 'intervals1' or 'intervals2'.
#'
#' If 'intervals.set.out' is not 'NULL' the result is saved as an intervals
#' set. Use this parameter if the result size exceeds the limits of the
#' physical memory.
#'
#' @param intervals1,intervals2 set of one-dimensional intervals
#' @param intervals.set.out intervals set name where the function result is
#' optionally outputted
#' @return If 'intervals.set.out' is 'NULL' a data frame representing the union
#' of intervals.
#' @seealso \code{\link{gintervals.intersect}}, \code{\link{gintervals.diff}},
#' \code{\link{gintervals}}, \code{\link{gintervals.2d}}
#' @keywords ~union
#' @examples
#' \dontshow{
#' options(gmax.processes = 2)
#' }
#'
#' gdb.init_examples()
#'
#' intervs1 <- gscreen("dense_track > 0.15 & dense_track < 0.18")
#' intervs2 <- gscreen("dense_track >= 0.18 & dense_track < 0.2")
#'
#' ## 'intervs3' and 'intervs4' are identical
#' intervs3 <- gintervals.union(intervs1, intervs2)
#' intervs4 <- gscreen("dense_track > 0.15 & dense_track < 0.2")
#'
#' @export gintervals.union
gintervals.union <- function(intervals1 = NULL, intervals2 = NULL, intervals.set.out = NULL) {
    if (is.null(intervals1) || is.null(intervals2)) {
        stop("Usage: gintervals.union(intervals1, intervals2, intervals.set.out = NULL)", call. = FALSE)
    }

    if (.gintervals.is_bigset(intervals1) || .gintervals.is_bigset(intervals2) || !is.null(intervals.set.out)) {
        res <- NULL

        FUN <- function(intervals, intervals.set.out, envir) {
            intervals1 <- intervals[[1]]
            intervals2 <- intervals[[2]]
            chrom_res <- .gcall("gintervunion", intervals1, intervals2, .misha_env())
            if (!is.null(chrom_res) && nrow(chrom_res) > 0) {
                if (is.null(intervals.set.out)) {
                    assign("res", c(get("res", envir = envir), list(chrom_res)), envir = envir)
                    .gverify_max_data_size(sum(unlist(lapply(get("res", envir), nrow))), arguments = "intervals.set.out")
                }
            }
            chrom_res
        }

        # use for .gintervals.apply chromosomes from both intervals1 and intervals2
        chroms <- NULL
        if (.gintervals.is1d(intervals1)) {
            chroms <- rbind(gintervals.chrom_sizes(intervals1), gintervals.chrom_sizes(intervals2))
            chroms <- factor(chroms$chrom, levels(chroms$chrom))
            chroms <- unique(chroms)
            chroms <- sort(chroms)
            chroms <- data.frame(chrom = chroms)
        } else {
            chroms <- rbind(gintervals.chrom_sizes(intervals1), gintervals.chrom_sizes(intervals2))
            chroms <- data.frame(chrom1 = chroms$chrom1, chrom2 = chroms$chrom2)
            chroms <- unique(chroms)
            chroms <- chroms[with(chroms, order(chrom1, chrom2)), ]
        }
        .gintervals.apply(chroms, list(intervals1, intervals2), intervals.set.out, FUN, intervals.set.out, environment())

        if (!is.null(res)) {
            res <- do.call(.grbind, res)
        } # much faster than calling rbind incrementally in FUN

        if (is.null(intervals.set.out)) {
            if (!is.null(res) && nrow(res)) {
                res
            } else {
                NULL
            }
        } else {
            retv <- 0
        } # suppress return value
    } else {
        res <- .gcall("gintervunion", intervals1, intervals2, .misha_env())
        res
    }
}




#' Creates a cartesian-grid iterator
#'
#' Creates a cartesian grid two-dimensional iterator that can be used by any
#' function that accepts an iterator argument.
#'
#' This function creates and returns a cartesian grid two-dimensional iterator
#' that can be used by any function that accepts an iterator argument.
#'
#' Assume 'centers1' and 'centers2' to be the central points of each interval
#' from 'intervals1' and 'intervals2', and 'C1', 'C2' to be two points from
#' 'centers1', 'centers2' accordingly. Assume also that the values in
#' 'expansion1' and 'expansion2' are unique and sorted.
#'
#' 'giterator.cartesian_grid' creates a set of all possible unique and
#' non-overlapping two-dimensional intervals of form: '(chrom1, start1, end1,
#' chrom2, start2, end2)'. Each '(chrom1, start1, end1)' is created by taking a
#' point 'C1' - '(chrom1, coord1)' and converting it to 'start1' and 'end1'
#' such that 'start1 == coord1+E1[i]', 'end1 == coord1+E1[i+1]', where 'E1[i]'
#' is one of the sorted 'expansion1' values. Overlaps between rectangles or
#' expansion beyond the limits of chromosome are avoided.
#'
#' 'min.band.idx' and 'max.band.idx' parameters control whether a pair of 'C1'
#' and 'C2' is skipped or not. If both of these parameters are not 'NULL' AND
#' if both 'C1' and 'C2' share the same chromosome AND the delta of indices of
#' 'C1' and 'C2' ('C1 index - C2 index') lays within '[min.band.idx,
#' max.band.idx]' range - only then the pair will be used to create the
#' intervals. Otherwise 'C1-C2' pair is filtered out. Note: if 'min.band.idx'
#' and 'max.band.idx' are not 'NULL', i.e. band indices filtering is applied,
#' then 'intervals2' parameter must be set to 'NULL'.
#'
#' @param intervals1 one-dimensional intervals
#' @param expansion1 an array of integers that define expansion around
#' intervals1 centers
#' @param intervals2 one-dimensional intervals. If 'NULL' then 'intervals2' is
#' considered to be equal to 'intervals1'
#' @param expansion2 an array of integers that define expansion around
#' intervals2 centers. If 'NULL' then 'expansion2' is considered to be equal to
#' 'expansion1'
#' @param min.band.idx,max.band.idx integers that limit iterator intervals to
#' band
#' @return A list containing the definition of cartesian iterator.
#' @seealso \code{\link{giterator.intervals}}
#' @keywords ~iterator ~cartesian
#' @examples
#' \dontshow{
#' options(gmax.processes = 2)
#' }
#'
#' gdb.init_examples()
#'
#' intervs1 <- gintervals(
#'     c(1, 1, 2), c(100, 300, 200),
#'     c(300, 500, 300)
#' )
#' intervs2 <- gintervals(
#'     c(1, 2, 2), c(400, 1000, 3000),
#'     c(800, 2000, 4000)
#' )
#' itr <- giterator.cartesian_grid(
#'     intervs1, c(-20, 100), intervs2,
#'     c(-40, -10, 50)
#' )
#' giterator.intervals(iterator = itr)
#'
#' itr <- giterator.cartesian_grid(intervs1, c(-20, 50, 100))
#' giterator.intervals(iterator = itr)
#'
#' itr <- giterator.cartesian_grid(intervs1, c(-20, 50, 100),
#'     min.band.idx = -1,
#'     max.band.idx = 0
#' )
#' giterator.intervals(iterator = itr)
#'
#' @export giterator.cartesian_grid
giterator.cartesian_grid <- function(intervals1 = NULL, expansion1 = NULL, intervals2 = NULL, expansion2 = NULL, min.band.idx = NULL, max.band.idx = NULL) {
    if (is.null(intervals1) || is.null(expansion1)) {
        stop("Usage: giterator.cartesian_grid(intervals1, expansion1, intervals2 = NULL, expansion2 = NULL, min.band.idx = NULL, max.band.idx = NULL)", call. = FALSE)
    }

    use.band.idx.limit <- !is.null(min.band.idx) && !is.null(max.band.idx)
    if (use.band.idx.limit) {
        if (min.band.idx > max.band.idx) {
            stop("min.band.idx exceeds max.band.idx", call. = FALSE)
        }

        if (!is.null(intervals2)) {
            stop("band.idx limit can only be used when intervals2 is set to NULL", call. = FALSE)
        }
    } else {
        min.band.idx <- 0
        max.band.idx <- 0
    }

    r <- list(
        intervals1 = intervals1, intervals2 = intervals2, expansion1 = expansion1, expansion2 = expansion2,
        band.idx = c(min.band.idx, max.band.idx, use.band.idx.limit)
    )
    class(r) <- "cartesian.grid"
    .gcall("gcheck_iterator", r, .misha_env())
    r
}



#' Returns iterator intervals
#'
#' Returns iterator intervals given track expression, scope, iterator and band.
#'
#' This function returns a set of intervals used by the iterator intervals for
#' the given track expression, genomic scope, iterator and band. Some functions
#' accept an iterator without accepting a track expression (like
#' 'gtrack.create_pwm_energy'). These functions generate the values for each
#' iterator interval by themselves. Use set 'expr' to 'NULL' to simulate the
#' work of these functions.
#'
#' If 'intervals.set.out' is not 'NULL' the result is saved as an intervals
#' set. Use this parameter if the result size exceeds the limits of the
#' physical memory.
#'
#' @param expr track expression
#' @param intervals genomic scope
#' @param iterator track expression iterator. If 'NULL' iterator is determined
#' implicitly based on track expression.
#' @param band track expression band. If 'NULL' no band is used.
#' @param intervals.set.out intervals set name where the function result is
#' optionally outputted
#' @return If 'intervals.set.out' is 'NULL' a data frame representing iterator
#' intervals.
#' @seealso \code{\link{giterator.cartesian_grid}}
#' @keywords ~iterator ~intervals
#' @examples
#' \dontshow{
#' options(gmax.processes = 2)
#' }
#'
#' gdb.init_examples()
#'
#' ## iterator is set implicitly to bin size of 'dense' track
#' giterator.intervals("dense_track", gintervals(1, 0, 200))
#'
#' ## iterator = 30
#' giterator.intervals("dense_track", gintervals(1, 0, 200), 30)
#'
#' ## iterator is an intervals set named 'annotations'
#' giterator.intervals("dense_track", .misha$ALLGENOME, "annotations")
#'
#' ## iterator is set implicitly to intervals of 'array_track' track
#' giterator.intervals("array_track", gintervals(1, 0, 200))
#'
#' ## iterator is a rectangle 100000 by 50000
#' giterator.intervals(
#'     "rects_track",
#'     gintervals.2d(chroms1 = 1, chroms2 = "chrX"),
#'     c(100000, 50000)
#' )
#'
#' @export giterator.intervals
giterator.intervals <- function(expr = NULL, intervals = .misha$ALLGENOME, iterator = NULL, band = NULL, intervals.set.out = NULL) {
    if (is.null(substitute(expr)) && is.null(substitute(iterator))) {
        stop("Usage: giterator.intervals(expr = NULL, intervals = .misha$ALLGENOME, iterator = NULL, band = NULL, intervals.set.out = NULL)", call. = FALSE)
    }

    intervals <- rescue_ALLGENOME(intervals, as.character(substitute(intervals)))

    if (is.null(substitute(expr))) {
        exprstr <- "0"
    } else {
        exprstr <- do.call(.gexpr2str, list(substitute(expr)), envir = parent.frame())
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
                res <- .gcall("giterator_intervals", exprstr, intervals, .iterator, band, intervals.set.out, .misha_env())

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
