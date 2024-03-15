.gcall <- function(...) {
    tryCatch(
        {
            res <- .Call(...)
        },
        interrupt = function(interrupt) {
            stop("Command interrupted!", call. = FALSE)
        }
    )
    res
}

.misha_env <- function() {
    e <- new.env(parent = parent.frame(2))
    assign(".misha", .misha, envir = e)
    return(e)
}

.gcall_noninteractive <- function(FUN, ...) {
    .ginteractive <- .ggetOption(".ginteractive")
    tryCatch(
        {
            options(.ginteractive = FALSE)
            on.exit(options(.ginteractive = .ginteractive))
            do.call(FUN, list(...))
        },
        finally = {
            options(.ginteractive = .ginteractive)
        }
    )
}


.ggetOption <- function(x, default = NULL) {
    if (missing(default)) {
        return(options(x)[[1L]])
    }
    if (x %in% names(options())) {
        options(x)[[1L]]
    } else {
        default
    }
}

.gexpr2str <- function(x) {
    if (.ggetOption(".ginteractive", FALSE)) {
        if (is.null(substitute(x))) {
            NULL
        } else {
            str <- deparse(substitute(x), width.cutoff = 500)[1]
            gsub("^\"(.*)\"$", "\\1", str, perl = TRUE)
        }
    } else {
        eval.parent(x)
    }
}

.giterator <- function(iterator) {
    if (typeof(iterator) == "integer" || typeof(iterator) == "double") {
        return(iterator)
    }

    iterator.str <- do.call(.gexpr2str, list(substitute(iterator)), envir = parent.frame())

    if (typeof(iterator.str) == "character") {
        if (!is.na(match(iterator.str, get("GTRACKS", envir = .misha))) || !is.na(match(iterator.str, get("GINTERVS", envir = .misha)))) {
            return(iterator.str)
        }
    }

    iterator
}

.grbind <- function(...) {
    objs <- list(...)

    zerolines <- lapply(objs, function(obj) {
        obj[0, ]
    })

    diffs <- sapply(zerolines, FUN = attr.all.equal, zerolines[[1]])
    if (!all(sapply(diffs, FUN = is.null))) {
        stop("Cannot rbind objects: columns differ", call. = FALSE)
    }

    .gcall("grbind", objs, .misha_env())
}

.gverify_max_data_size <- function(size, data_name = "Result", arguments = NULL) {
    max.data.size <- .ggetOption("gmax.data.size", 10000000)

    if (size > max.data.size) {
        if (is.null(arguments)) {
            stop(sprintf(
                paste("%s size exceeded the maximal allowed (%d).",
                    "Note: the maximum data size is controlled via gmax.data.size option (see options, getOptions).",
                    sep = "\n"
                ),
                data_name, max.data.size
            ), call. = FALSE)
        } else {
            stop(sprintf(
                paste("%s size exceeded the maximal allowed (%d).",
                    "Consider saving the result in a file (use %s argument).",
                    "Note: the maximum data size is controlled via gmax.data.size option (see options, getOptions).",
                    sep = "\n"
                ),
                data_name, max.data.size, paste(arguments, collapse = " or ")
            ), call. = FALSE)
        }
    }
}


#' Downloads files from FTP server
#'
#' Downloads multiple files from FTP server
#'
#' This function downloads files from FTP server given by 'url'. The address in
#' 'url' can contain wildcards to download more than one file at once. Files
#' are downloaded to a directory given by 'path' argument.  If 'path' is
#' 'NULL', file are downloaded into 'GROOT/downloads'.
#'
#' @param url URL of FTP server
#' @param path directory path where the downloaded files are stored
#' @return An array of file names that have been downloaded.
#' @seealso \code{\link{gtrack.import_set}}
#' @keywords ~ftp
#' @examples
#' gdb.init_examples()
#' \donttest{
#' outdir <- tempdir()
#' gwget("ftp://hgdownload.soe.ucsc.edu/goldenPath/hg19/chromosomes/md5sum.txt", path = outdir)
#' }
#'
#' @export gwget
gwget <- function(url = NULL, path = NULL) {
    if (is.null(url)) {
        stop("Usage: gwget(url, path = NULL)", call. = FALSE)
    }

    if (is.null(path)) {
        .gcheckroot()
        path <- paste(get("GROOT", envir = .misha), "/downloads", sep = "")
        dir.create(path, showWarnings = FALSE, recursive = TRUE, mode = "0777")
    }

    if (!length(grep("^ftp\\:\\/\\/(\\w+(\\.\\w+)+)\\/(.+)", url, perl = TRUE))) {
        url <- paste("ftp://", url, sep = "")
    }

    if (!length(grep("^ftp\\:\\/\\/(\\w+(\\.\\w+)+)\\/(.+)", url, perl = TRUE))) {
        stop("Invalid format of URL", call. = FALSE)
    }

    gftp_download_glob(url, path, verbose = FALSE, handle = NULL)
}




#' Runs R commands on a cluster
#'
#' Runs R commands on a cluster that supports SGE.
#'
#' This function runs R commands on a cluster by distributing them among
#' cluster nodes. It must run on a machine that supports Sun Grid Engine (SGE).
#' The order in which the commands are executed can not be guaranteed,
#' therefore the commands must be inter-independent.
#'
#' Optional flags to 'qsub' command can be passed through 'opt.flags'
#' parameter. Users are strongly recommended to use only '-l' flag as other
#' flags might interfere with those that are already used (-terse, -S, -o, -e,
#' -V). For additional information please refer to the manual of 'qsub'.
#'
#' The maximal number of simultaneously submitted jobs is controlled by
#' 'max.jobs'.
#'
#' Set 'debug' argument to 'TRUE to allow additional report prints.
#'
#' 'gcluster.run' launches R on the cluster nodes to execute the commands. 'R'
#' argument specifies how R executable should be invoked.
#'
#' @param ... R commands
#' @param opt.flags optional flags for qsub command
#' @param max.jobs maximal number of simultaneously submitted jobs
#' @param debug if 'TRUE', additional reports are printed
#' @param R command that launches R
#' @param control_dir directory where the control files are stored. Note that this
#' directory should be accessible from all nodes. If 'NULL', a temporary directory
#' would be created under the current misha database.
#' @return Return value ('retv') is a list, such that 'retv[[i]]' represents
#' the result of the run of command number 'i'. Each result consists of 4
#' fields that can be accessed by 'retv[[i]]$FIELDNAME':
#'
#' \tabular{ll}{ \emph{FIELDNAME} \tab \emph{DESCRIPTION}\cr exit.status \tab
#' Exit status of the command. Possible values: 'success', 'failure' or
#' 'interrupted'.\cr retv \tab Return value of the command.\cr stdout \tab
#' Standard output of the command.\cr stderr \tab Standard error of the
#' command. }
#' @keywords ~cluster
#' @examples
#' \dontshow{
#' options(gmax.processes = 2)
#' }
#' \donttest{
#' gdb.init_examples()
#' # Run only on systems with Sun Grid Engine (SGE)
#' if (FALSE) {
#'     v <- 17
#'     gcluster.run(
#'         gsummary("dense_track + v"),
#'         {
#'             intervs <- gscreen("dense_track > 0.1", gintervals(1, 2))
#'             gsummary("sparse_track", intervs)
#'         },
#'         gsummary("rects_track")
#'     )
#' }
#' }
#'
#' @export gcluster.run
gcluster.run <- function(..., opt.flags = "", max.jobs = 400, debug = FALSE, R = "R", control_dir = NULL) {
    commands <- as.list(substitute(list(...))[-1L])

    if (length(commands) < 1) {
        stop("Usage: gcluster.run(..., opt.flags = \"\" max.jobs = 400, debug = FALSE)", call. = FALSE)
    }

    if (!length(system("which qsub", ignore.stderr = TRUE, intern = TRUE))) {
        stop("gcluster.run must run on a host that supports Sun Grid Engine (qsub)", call. = FALSE)
    }

    .gcheckroot()

    tmp.dirname <- ""
    if (is.null(control_dir)) {
        control_dir <- paste(get("GROOT", envir = .misha), "/tmp", sep = "")
    }

    # if the path of control dir starts with '/tmp' warn that the tempdir needs to be shared
    if (grepl("^/tmp", control_dir)) {
        message("Warning: The control directory is in /tmp. Make sure that it is shared between all nodes.")
    }

    submitted.jobs <- c()

    tryCatch(
        {
            tmp.dirname <- tempfile(pattern = "", tmpdir = control_dir)
            if (!dir.create(tmp.dirname, recursive = TRUE, mode = "0777")) {
                stop(sprintf("Failed to create a directory %s", tmp.dirname), call. = FALSE)
            }

            # save the environment + options
            # parent.frame() is the environment of the caller
            message("Preparing for distribution...\n")

            save(.misha, file = paste(tmp.dirname, "misha", sep = "/"))
            vars <- ls(all.names = TRUE, envir = parent.frame())
            envir <- parent.frame()
            while (!identical(envir, .GlobalEnv)) {
                envir <- parent.env(envir)
                vars <- union(vars, ls(all.names = TRUE, envir = envir))
            }
            save(list = vars, file = paste(tmp.dirname, "envir", sep = "/"), envir = parent.frame())
            .GSGECMD <- commands
            save(.GSGECMD, file = paste(tmp.dirname, "commands", sep = "/"))
            opts <- options()
            save(opts, file = paste(tmp.dirname, "opts", sep = "/"))

            message("Running the commands...")
            completed.jobs <- c()
            progress <- -1
            repeat {
                # submit the commands
                num.running.jobs <- length(submitted.jobs) - length(completed.jobs)
                if (length(submitted.jobs) < length(commands) && num.running.jobs < max.jobs) {
                    istart <- length(submitted.jobs) + 1
                    iend <- min(length(commands), istart + (max.jobs - num.running.jobs) - 1)

                    for (i in istart:iend) {
                        out.file <- sprintf("%s/%d.out", tmp.dirname, i)
                        err.file <- sprintf("%s/%d.err", tmp.dirname, i)
                        script <- paste(get(".GLIBDIR", envir = .misha), "exec", "sgjob.sh", sep = "/")
                        command <- sprintf(
                            "unset module; qsub -terse -S /bin/bash -o %s -e %s -V %s %s %d '%s' '%s'",
                            out.file, err.file, opt.flags, script, i, tmp.dirname, R
                        )
                        jobid <- system(command, intern = TRUE)
                        if (length(jobid) != 1) {
                            stop("Failed to run qsub", call. = FALSE)
                        }
                        if (debug) {
                            message(sprintf("\tSubmitted job %d (id: %s)", i, jobid))
                        }
                        submitted.jobs <- c(submitted.jobs, jobid)
                    }
                }

                # monitor progress
                Sys.sleep(3)
                running.jobs <- .gcluster.running.jobs(submitted.jobs)

                old.completed.jobs <- completed.jobs
                completed.jobs <- setdiff(submitted.jobs, running.jobs)
                if (debug) {
                    delta.jobs <- setdiff(completed.jobs, old.completed.jobs)
                    if (length(delta.jobs) > 0) {
                        for (jobid in delta.jobs) {
                            message(sprintf("\tJob %d (id: %s) completed", match(jobid, submitted.jobs), jobid))
                        }
                    }

                    if (!length(running.jobs) && length(submitted.jobs) == length(commands)) {
                        break
                    }

                    new.progress <- length(completed.jobs)
                    if (new.progress != progress) {
                        progress <- new.progress
                        message(sprintf("\t%d job(s) still in progress", length(commands) - progress))
                    }
                } else {
                    if (!length(running.jobs) && length(submitted.jobs) == length(commands)) {
                        break
                    }

                    new.progress <- as.integer(100 * length(completed.jobs) / length(commands))
                    if (new.progress != progress) {
                        progress <- new.progress
                        message(sprintf("%d%%...", progress), appendLF = FALSE)
                    } else {
                        message(".", appendLF = FALSE)
                    }
                }
            }
            if (!debug && progress != -1 && progress != 100) {
                message("100%")
            }
        },
        interrupt = function(interrupt) {
            message("\n")
            stop("Command interrupted!", call. = FALSE)
        },
        finally = {
            # We should perform now cleanup. If the user presses again Ctr+C "finaly" statement will be interrupted and the cleanup will
            # be incomplete. Unfortunately there is no way to block interrupts up until "finally" is done.
            # The only way is to solve the problem is to run "finally" in a loop.
            cleanup.finished <- FALSE
            while (!cleanup.finished) {
                tryCatch(
                    {
                        # kill still running jobs
                        if (length(submitted.jobs) > 0) {
                            # pack the answer
                            running.jobs <- .gcluster.running.jobs(submitted.jobs)
                            answer <- c()
                            for (i in 1:length(commands)) {
                                res <- list()
                                res$exit.status <- NA
                                res$retv <- NA
                                res$stdout <- NA
                                res$stderr <- NA

                                if (submitted.jobs[i] %in% running.jobs) {
                                    res$exit.status <- "interrupted"
                                } else {
                                    fname <- sprintf("%s/%d.retv", tmp.dirname, i)
                                    if (file.exists(fname)) {
                                        load(fname)
                                        res$exit.status <- "success"
                                        res$retv <- retv
                                    } else {
                                        res$exit.status <- "failure"
                                    }
                                }

                                out.file <- sprintf("%s/%d.out", tmp.dirname, i)
                                if (file.exists(out.file)) {
                                    f <- file(out.file, "rc")
                                    res$stdout <- readChar(f, 1e6)
                                    close(f)
                                }

                                err.file <- sprintf("%s/%d.err", tmp.dirname, i)
                                if (file.exists(err.file)) {
                                    f <- file(err.file, "rc")
                                    res$stderr <- readChar(f, 1e6)
                                    close(f)
                                }

                                answer[[i]] <- res
                            }
                            for (job in running.jobs) {
                                system(sprintf("qdel %s", job), ignore.stderr = TRUE, intern = TRUE)
                            }

                            unlink(tmp.dirname, recursive = TRUE)
                            return(answer)
                        }
                        unlink(tmp.dirname, recursive = TRUE)
                        cleanup.finished <- TRUE
                    },
                    interrupt = function(interrupt) {}
                )
            }
        }
    )
}


.gcluster.running.jobs <- function(jobids) {
    str <- system("qstat | sed 's/^[ ]*//' | cut -f 1 -d\" \"", intern = TRUE)
    if (length(str) > 2) {
        intersect(jobids, str)
    } else {
        c()
    }
}

rescue_ALLGENOME <- function(intervals, intervals_name) {
    if (length(intervals_name) == 0) {
        return(intervals)
    }
    if (intervals_name[1] == "ALLGENOME") {
        warning("ALLGENOME was deprecated at version 4.2.0. Use .misha$ALLGENOME instead.", call. = FALSE)
        intervals <- .misha$ALLGENOME
    }
    if (intervals_name[1] == "ALLGENOME[[1]]") {
        warning("ALLGENOME[[1]] was deprecated at version 4.2.0. Use .misha$ALLGENOME[[1]] instead.", call. = FALSE)
        intervals <- .misha$ALLGENOME[[1]]
    }
    if (intervals_name[1] == "ALLGENOME[[2]]") {
        warning("ALLGENOME[[2]] was deprecated at version 4.2.0. Use .misha$ALLGENOME[[2]] instead.", call. = FALSE)
        intervals <- .misha$ALLGENOME[[2]]
    }
    return(intervals)
}
