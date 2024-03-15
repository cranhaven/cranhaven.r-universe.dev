.gchroms <- function(chroms) {
    if (!is.character(chroms)) {
        chroms <- as.character(chroms)
    }

    idx <- substr(chroms, 1, 3) != "chr"
    chroms[idx] <- paste("chr", chroms[idx], sep = "")

    indices <- match(chroms, get("ALLGENOME", envir = .misha)[[1]]$chrom)
    err.chroms <- chroms[is.na(indices)]
    if (length(err.chroms) > 0) {
        stop(sprintf("Chromosome %s does not exist in the database", err.chroms[1]))
    }
    get("ALLGENOME", envir = .misha)[[1]]$chrom[indices] # return factor
}


.gcheckroot <- function() {
    if (!exists("GROOT", envir = .misha) || !exists("ALLGENOME", envir = .misha) || is.null(get("GROOT", envir = .misha)) || is.null(get("ALLGENOME", envir = .misha))) {
        stop("Database root directory is not set. Please call gdb.init().", call. = FALSE)
    }
}


.gdir.cd <- function(dir, rescan) {
    oldwd <- getwd()
    on.exit(setwd(oldwd), add = TRUE)
    setwd(get("GWD", envir = .misha))
    tryCatch(
        {
            t <- .gfindtrackinpath(dir)
            if (!is.null(t)) {
                stop(sprintf("Directory %s belongs to track %s", dir, t), call. = FALSE)
            }

            setwd(dir)
            newwd <- getwd()

            assign("GWD", newwd, envir = .misha)
            setwd(oldwd)
            gdb.reload(rescan)
        },
        interrupt = function(interrupt) {
            setwd(oldwd)
        },
        finally = {
            setwd(oldwd)
        }
    )
}

#' @rdname gdb.init
#' @export
gsetroot <- function(groot = NULL, dir = NULL, rescan = FALSE) {
    if (is.null(groot)) {
        stop("Usage: gsetroot(groot, dir = NULL, rescan = FALSE)", call. = FALSE)
    }

    groot <- normalizePath(groot)

    assign("ALLGENOME", NULL, envir = .misha)
    assign("GROOT", NULL, envir = .misha)

    chromsizes <- read.csv(paste(groot, "chrom_sizes.txt", sep = "/"), sep = "\t", header = FALSE)
    colnames(chromsizes) <- c("chrom", "size")
    intervals <- data.frame(
        chrom = as.factor(paste("chr", as.character(chromsizes$chrom), sep = "")),
        start = 0, end = as.numeric(chromsizes$size)
    )

    if (nrow(intervals) == 0) {
        stop("chrom_sizes.txt file does not contain any chromosomes", call. = FALSE)
    }

    for (chrom in intervals$chrom) {
        if (length(grep(sprintf("^%s$", chrom), intervals$chrom)) > 1) {
            stop(sprintf("Chromosome \"%s\" appears more than once in chrom_sizes.txt", chrom))
        }
    }
    intervals <- intervals[order(intervals$chrom), ]
    rownames(intervals) <- 1:nrow(intervals)

    cartesian <- expand.grid(1:nrow(intervals), 1:nrow(intervals))
    intervals2d <- cbind(intervals[cartesian[, 2], ], intervals[cartesian[, 1], ])
    names(intervals2d) <- c("chrom1", "start1", "end1", "chrom2", "start2", "end2")
    rownames(intervals2d) <- 1:nrow(intervals2d)

    assign("ALLGENOME", list(intervals, intervals2d), envir = .misha)
    assign("GROOT", groot, envir = .misha)
    assign("GWD", groot, envir = .misha)

    success <- FALSE
    tryCatch(
        {
            if (is.null(dir)) {
                .gdir.cd(paste(groot, "tracks", sep = "/"), rescan)
            } else {
                if (nchar(dir) < 1) {
                    stop("dir argument is an empty string")
                }

                c <- substr(dir, 1, 1)
                if (c == "~" || c == "/") {
                    .gdir.cd(dir, rescan)
                } else {
                    .gdir.cd(paste(groot, dir, sep = "/"), rescan)
                }
            }
            success <- TRUE
        },
        finally = {
            if (!success) {
                assign("ALLGENOME", NULL, envir = .misha)
                assign("GROOT", NULL, envir = .misha)
                assign("GWD", NULL, envir = .misha)
            }
        }
    )
}




#' Changes current working directory in Genomic Database
#'
#' Changes current working directory in Genomic Database.
#'
#' This function changes the current working directory in Genomic Database (not
#' to be confused with shell's current working directory). The list of database
#' objects - tracks, intervals, track variables - is rescanned recursively
#' under 'dir'. Object names are updated with the respect to the new current
#' working directory. Example: a track named 'subdir.dense' will be referred as
#' 'dense' once current working directory is set to 'subdir'. All virtual
#' tracks are removed.
#'
#' @param dir directory path
#' @return None.
#' @seealso \code{\link{gdb.init}}, \code{\link{gdir.cwd}},
#' \code{\link{gdir.create}}, \code{\link{gdir.rm}}
#' @keywords ~db ~data ~database ~cd ~dir ~directory ~folder
#' @examples
#' \dontshow{
#' options(gmax.processes = 2)
#' }
#'
#' gdb.init_examples()
#' gdir.cd("subdir")
#' gtrack.ls()
#' gdir.cd("..")
#' gtrack.ls()
#'
#' @export gdir.cd
gdir.cd <- function(dir = NULL) {
    if (is.null(dir)) {
        stop("Usage: gdir.cd(dir)", call. = FALSE)
    }

    success <- FALSE
    oldgwd <- get("GWD", envir = .misha)

    tryCatch(
        {
            .gdir.cd(dir, TRUE)
            success <- TRUE
        },
        finally = {
            if (!success) {
                .gdir.cd(oldgwd, TRUE)
            }
        }
    )
}



#' Creates a new directory in Genomic Database
#'
#' Creates a new directory in Genomic Database.
#'
#' This function creates a new directory in Genomic Database. Creates only the
#' last element in the specified path.
#'
#' @param dir directory path
#' @param showWarnings see 'dir.create'
#' @param mode see 'dir.create'
#' @return None.
#' @note A new directory cannot be created within an existing track directory.
#' @seealso \code{\link{dir.create}}, \code{\link{gdb.init}},
#' \code{\link{gdir.cwd}}, \code{\link{gdir.rm}}
#' @keywords ~db ~data ~database ~dir ~directory ~folder ~create
#' @export gdir.create
gdir.create <- function(dir = NULL, showWarnings = TRUE, mode = "0777") {
    if (is.null(dir)) {
        stop("Usage: gdir.create(dir, showWarnings = TRUE, mode = \"0777\")", call. = FALSE)
    }

    oldwd <- getwd()
    on.exit(setwd(oldwd), add = TRUE)
    setwd(get("GWD", envir = .misha))
    tryCatch(
        {
            d <- dirname(dir)

            if (!file.exists(d)) {
                stop(sprintf("Path %s does not exist.\nNote: recursive directory creation is forbidden.", d), call. = FALSE)
            }

            t <- .gfindtrackinpath(d)
            if (!is.null(t)) {
                stop(sprintf("Cannot create a directory within a track %s", t), call. = FALSE)
            }

            if (length(grep("\\.track$", basename(dir))) > 0) {
                stop("gdir.create cannot create track directories", call. = FALSE)
            }

            dir.create(dir, showWarnings = showWarnings, recursive = FALSE, mode = mode)
        },
        interrupt = function(interrupt) {
            setwd(oldwd)
        },
        finally = {
            setwd(oldwd)
        }
    )
}

#' Create directories needed for track creation
#'
#' @description This function creates the directories needed for track creation.
#' For example, if the track name is 'proj.sample.my_track', this function
#' creates the directories 'proj' and 'sample'. Use this function with caution -
#' a long track name may create a deep directory structure.
#'
#' @param track name of the track
#'
#' @inheritParams gdir.create
#'
#' @return None.
#' @examples
#'
#' gdb.init_examples()
#'
#' # This creates the directories 'proj' and 'sample'
#' gtrack.create_dirs("proj.sample.my_track")
#'
#' @export
gtrack.create_dirs <- function(track, mode = "0777") {
    # split the track name into directories
    dirs <- dirname(gsub("\\.", "/", track))
    dirs <- strsplit(dirs, "/")[[1]]
    dir <- dirs[1]
    for (i in 1:length(dirs)) {
        if (i > 1) {
            dir <- paste(dir, dirs[i], sep = "/")
        }
        gdir.create(dir, mode = mode)
    }
}



#' Returns the current working directory in Genomic Database
#'
#' Returns the absolute path of the current working directory in Genomic
#' Database.
#'
#' This function returns the absolute path of the current working directory in
#' Genomic Database (not to be confused with shell's current working
#' directory).
#'
#' @return A character string of the path.
#' @seealso \code{\link{gdb.init}}, \code{\link{gdir.cd}},
#' \code{\link{gdir.create}}, \code{\link{gdir.rm}}
#' @keywords ~db ~data ~database ~cwd ~pwd ~dir ~directory ~folder
#' @export gdir.cwd
gdir.cwd <- function() {
    .gcheckroot()
    get("GWD", envir = .misha)
}



#' Deletes a directory from Genomic Database
#'
#' Deletes a directory from Genomic Database.
#'
#' This function deletes a directory from Genomic Database. If 'recursive' is
#' 'TRUE', the directory is deleted with all the files/directories it contains.
#' If the directory contains tracks or intervals, the user is prompted to
#' confirm the deletion. Set 'force' to 'TRUE' to suppress the prompt.
#'
#' @param dir directory path
#' @param recursive if 'TRUE', the directory is deleted recursively
#' @param force if 'TRUE', suppresses user confirmation of tracks/intervals
#' removal
#' @return None.
#' @seealso \code{\link{gdb.init}}, \code{\link{gdir.create}},
#' \code{\link{gdir.cd}}, \code{\link{gdir.cwd}}
#' @keywords ~db ~data ~database ~dir ~directory ~folder ~rm
#' @export gdir.rm
gdir.rm <- function(dir = NULL, recursive = FALSE, force = FALSE) {
    if (is.null(dir)) {
        stop("Usage: gdir.rm(dir, recursive = FALSE, force = FALSE)", call. = FALSE)
    }

    oldwd <- getwd()
    on.exit(setwd(oldwd), add = TRUE)
    setwd(get("GWD", envir = .misha))
    tryCatch(
        {
            if (!file.exists(dir)) {
                if (force) {
                    return(invisible())
                }
                stop(sprintf("Directory %s does not exist", dir), call. = FALSE)
            }

            r <- file.info(dir)
            if (r[names(r) == "isdir"] != 1) {
                stop(sprintf("%s is not a directory", dir), call. = FALSE)
            }

            t <- .gfindtrackinpath(dir)
            if (!is.null(t)) {
                stop(sprintf("Directory %s belongs to track %s", dir, t), call. = FALSE)
            }

            answer <- "Y"

            if (recursive && !force) {
                res <- .gcall("gfind_tracks_n_intervals", dir, .misha_env(), silent = TRUE)
                tracks <- res[[1]]
                intervals <- res[[2]]

                if (!force && length(tracks) + length(intervals) > 0) {
                    message(sprintf("Directory %s contains tracks or intervals. Are you still sure you want to delete it (Y/N)? ", dir))
                    answer <- toupper(readLines(n = 1))
                }
            }

            if (answer == "Y" || answer == "YES") {
                if (recursive) {
                    unlink(dir, recursive)
                } else {
                    file.remove(dir)
                }

                if (file.exists(dir)) {
                    stop("Failed to remove the directory", call. = FALSE)
                }
            }
            gdb.reload()
        },
        interrupt = function(interrupt) {
            setwd(oldwd)
        },
        finally = {
            setwd(oldwd)
        }
    )
}




#' Sets read-only track attributes
#'
#' Sets read-only track attributes.
#'
#' This function sets the list of read-only track attributes. The specified
#' attributes may or may not already exist in the tracks.
#'
#' If 'attrs' is 'NULL' the list of read-only attributes is emptied.
#'
#' @param attrs a vector of read-only attributes names or 'NULL'
#' @return None.
#' @seealso \code{\link{gdb.get_readonly_attrs}},
#' \code{\link{gtrack.attr.get}}, \code{\link{gtrack.attr.set}}
#' @keywords ~attr ~attribute
#' @export gdb.set_readonly_attrs
gdb.set_readonly_attrs <- function(attrs) {
    .gcheckroot()

    filename <- paste(get("GROOT", envir = .misha), ".ro_attributes", sep = "/")

    if (is.null(attrs)) {
        unlink(filename)
    } else {
        attrs <- as.character(attrs)

        idx <- which(duplicated(attrs))[1]
        if (!is.na(idx)) {
            stop(sprintf("Attribute %s appears more than once", attrs[idx]), call. = FALSE)
        }

        idx <- which(attrs == "")[1]
        if (!is.na(idx)) {
            stop("Attribute name cannot be an empty string", call. = FALSE)
        }

        f <- file(filename, "wb")
        serialize(attrs, f)
        close(f)
    }
    retv <- 0 # suppress return value
}




#' Creates a new Genomic Database
#'
#' Creates a new Genomic Database.
#'
#' This function creates a new Genomic Database at the location specified by
#' 'groot'. FASTA files are converted to 'Seq' format and appropriate
#' 'chrom_sizes.txt' file is generated (see "User Manual" for more details).
#'
#' If 'genes.file' is not 'NULL' four sets of intervals are created in the
#' database: \code{tss}, \code{exons}, \code{utr3} and \code{utr5}. See
#' \link{gintervals.import_genes} for more details about importing genes
#' intervals.
#'
#' 'fasta', 'genes.file' and 'annots.file' can be either a file path or URL in
#' a form of 'ftp://[address]/[file]'. 'fasta' can also contain wildcards to
#' indicate multiple files. Files that these arguments point to can be zipped
#' or unzipped.
#'
#' See the 'Genomes' vignette for details on how to create a database from common
#' genome sources.
#'
#' @param groot path to newly created database
#' @param fasta an array of names or URLs of FASTA files. Can contain wildcards
#' for multiple files
#' @param genes.file name or URL of file that contains genes. If 'NULL' no
#' genes are imported
#' @param annots.file name of URL file that contains annotations. If 'NULL' no
#' annotations are imported
#' @param annots.names annotations names
#' @return None.
#' @seealso \code{\link{gdb.init}}, \code{\link{gdb.reload}},
#' \code{\link{gintervals.import_genes}}
#' @keywords ~database ~create ~genes
#' @examples
#' \donttest{
#' ftp <- "ftp://hgdownload.soe.ucsc.edu/goldenPath/mm10"
#' mm10_dir <- file.path(tempdir(), "mm10")
#' # only a single chromosome is loaded in this example
#' # see "Genomes" vignette how to downloaded all of them/other genomes
#' gdb.create(
#'     mm10_dir,
#'     paste(ftp, "chromosomes", paste0(
#'         "chr", c("X"),
#'         ".fa.gz"
#'     ), sep = "/"),
#'     paste(ftp, "database/knownGene.txt.gz", sep = "/"),
#'     paste(ftp, "database/kgXref.txt.gz", sep = "/"),
#'     c(
#'         "kgID", "mRNA", "spID", "spDisplayID", "geneSymbol",
#'         "refseq", "protAcc", "description", "rfamAcc",
#'         "tRnaName"
#'     )
#' )
#' gdb.init(mm10_dir)
#' gintervals.ls()
#' gintervals.all()
#' }
#'
#' @export gdb.create
gdb.create <- function(groot = NULL, fasta = NULL, genes.file = NULL, annots.file = NULL, annots.names = NULL) {
    if (is.null(groot) || is.null(fasta)) {
        stop("Usage: gdb.create(groot, fasta, genes.file = NULL, annots.file = NULL, annots.names = NULL)", call. = FALSE)
    }

    if (file.exists(groot)) {
        stop(sprintf("Directory %s already exists", groot), call. = FALSE)
    }

    success <- FALSE
    allgenome.old <- NULL
    groot.old <- NULL
    if (exists("ALLGENOME", envir = .misha)) {
        allgenome.old <- get("ALLGENOME", envir = .misha)
    }
    if (exists("GROOT", envir = .misha)) {
        groot.old <- get("GROOT", envir = .misha)
    }

    tryCatch(
        {
            dir.create(groot, showWarnings = FALSE, recursive = TRUE, mode = "0777")
            dir.create(paste(groot, "pssms", sep = "/"), showWarnings = FALSE, recursive = TRUE, mode = "0777")
            dir.create(paste(groot, "seq", sep = "/"), showWarnings = FALSE, recursive = TRUE, mode = "0777")
            dir.create(paste(groot, "tracks", sep = "/"), showWarnings = FALSE, recursive = TRUE, mode = "0777")

            chroms <- .gseq.import(groot, fasta)

            if (!length(chroms)) {
                stop("No FASTA files were imported", call. = FALSE)
            }

            seq.files <- paste("chr", chroms, ".seq", sep = "")
            seq.files <- paste(paste(groot, "seq", sep = "/"), seq.files, sep = "/")
            chrom.sizes <- data.frame(chrom = chroms, size = file.info(seq.files)$size)
            utils::write.table(chrom.sizes, paste(groot, "chrom_sizes.txt", sep = "/"), quote = FALSE, sep = "\t", col.names = FALSE, row.names = FALSE)

            # before calling gintervals.import_genes new ALLGENOME must be set
            intervals <- data.frame(
                chrom = as.factor(paste("chr", as.character(chrom.sizes$chrom), sep = "")),
                start = 0, end = as.numeric(chrom.sizes$size)
            )
            intervals <- intervals[order(intervals$chrom), ]
            rownames(intervals) <- 1:nrow(intervals)

            cartesian <- expand.grid(1:nrow(intervals), 1:nrow(intervals))
            intervals2d <- cbind(intervals[cartesian[, 2], ], intervals[cartesian[, 1], ])
            names(intervals2d) <- c("chrom1", "start1", "end1", "chrom2", "start2", "end2")
            rownames(intervals2d) <- 1:nrow(intervals2d)

            assign("ALLGENOME", list(intervals, intervals2d), envir = .misha)
            assign("GROOT", groot, envir = .misha)

            if (!is.null(genes.file)) {
                intervs <- gintervals.import_genes(genes.file, annots.file, annots.names)
                if (!is.null(intervs)) {
                    for (i in 1:length(intervs)) {
                        if (!is.null(intervs$tss)) {
                            .gcall_noninteractive(.gintervals.save_file, sprintf("%s/tracks/%s.interv", groot, names(intervs)[i]), intervs[[i]])
                        }
                    }
                }
            }

            # write read-only attributes
            f <- file(paste(groot, ".ro_attributes", sep = "/"), "wb")
            serialize(c("created.by", "created.date"), f)
            close(f)

            message("Database was successfully created")
            success <- TRUE
        },
        finally = {
            assign("ALLGENOME", allgenome.old, envir = .misha)
            assign("GROOT", groot.old, envir = .misha)
            if (!success) {
                unlink(groot, recursive = TRUE)
            }
        }
    )
    retv <- 0 # suppress return value
}



#' Returns a list of read-only track attributes
#'
#' Returns a list of read-only track attributes.
#'
#' This function returns a list of read-only track attributes. These attributes
#' are not allowed to be modified or deleted.
#'
#' If no attributes are marked as read-only a 'NULL' is returned.
#'
#' @return A list of read-only track attributes.
#' @seealso \code{\link{gdb.set_readonly_attrs}},
#' \code{\link{gtrack.attr.get}}, \code{\link{gtrack.attr.set}}
#' @keywords ~attr ~attribute
#' @export gdb.get_readonly_attrs
gdb.get_readonly_attrs <- function() {
    .gcheckroot()

    filename <- paste(get("GROOT", envir = .misha), ".ro_attributes", sep = "/")
    attrs <- NULL
    if (file.exists(filename)) {
        f <- file(filename, "rb")
        attrs <- unserialize(f)
        close(f)
        if (!is.character(attrs)) {
            stop(sprintf("Invalid format of read-only atrributes file %s", filename), call. = FALSE)
        }

        attrs <- unique(attrs)
        attrs <- attrs[attrs != ""]
    }
    attrs
}



#' Initializes connection with Genomic Database
#'
#' Initializes connection with Genomic Database: loads the list of tracks,
#' intervals, etc.
#'
#' 'gdb.init' initializes the connection with the Genomic Database. It is
#' typically called first prior to any other function. When the package is
#' attached it internally calls to 'gdb.init.examples' which opens the
#' connection with the database located at 'PKGDIR/trackdb/test' directory,
#' where 'PKGDIR' is the directory where the package is installed.
#'
#' The current working directory inside the Genomic Database is set to 'dir'.
#' If 'dir' is 'NULL', the current working directory is set to 'GROOT/tracks'.
#'
#' If 'rescan' is 'TRUE', the list of tracks and intervals is achieved by
#' rescanning directory structure under the current current working directory.
#' Otherwise 'gdb.init' attempts to use the cached list that resides in
#' 'groot/.db.cache' file.
#'
#' Upon completion the connection is established with the database. If
#' auto-completion mode is switched on (see 'gset_input_method') the list of
#' tracks and intervals sets is loaded and added as variables to the global
#' environment allowing auto-completion of object names with <TAB> key. Also a
#' few variables are defined at an environment called \code{.misha}, and can be
#' accessed using \code{.misha$variable}, e.g. \code{.misha$ALLGENOME}.
#' These variables should not be modified by user.
#'
#' \tabular{ll}{ GROOT \tab Root directory of Genomic Database\cr GWD \tab
#' Current working directory inside Genomic Database\cr GTRACKS \tab List of
#' all available tracks\cr GINTERVS \tab List of all available intervals\cr
#' GVTRACKS \tab List of all available virtual tracks\cr ALLGENOME \tab List of
#' all chromosomes and their sizes\cr GITERATOR.INTERVALS \tab A set of
#' iterator intervals for which the track expression is evaluated\cr }
#'
#' @aliases gdb.init gdb.init.examples gsetroot
#' @param groot the root directory of the Genomic Database
#' @param dir the current working directory inside the Genomic Database
#' @param rescan indicates whether the file structure should be rescanned
#' @return None.
#' @seealso \code{\link{gdb.reload}}, \code{\link{gdb.create}},
#' \code{\link{gdir.cd}}, \code{\link{gtrack.ls}}, \code{\link{gintervals.ls}},
#' \code{\link{gvtrack.ls}}
#' @keywords ~db ~data ~database
#' @export gdb.init
gdb.init <- function(groot = NULL, dir = NULL, rescan = FALSE) {
    if (is.null(groot)) {
        stop("Usage: gdb.init(groot, dir = NULL, rescan = FALSE)", call. = FALSE)
    }
    gsetroot(groot, dir, rescan)
}

#' @rdname gdb.init
#' @export
gdb.init_examples <- function() {
    db_dir <- tempdir()
    utils::untar(system.file("testdb.tar.gz", package = "misha"), exdir = db_dir)
    gsetroot(file.path(db_dir, "trackdb/test"))
}



#' Reloads database from the disk
#'
#' Reloads database from disk: list of tracks, intervals, etc.
#'
#' Reloads Genomic Database from disk: list of tracks, intervals, etc. Use this
#' function if you manually add tracks or if for any reason the database
#' becomes corrupted. If 'rescan' is 'TRUE', the list of tracks and intervals
#' is achieved by rescanning directory structure under the current current
#' working directory. Otherwise 'gdb.reload' attempts to use the cached list
#' that resides in 'GROOT/.db.cache' file.
#'
#' @param rescan indicates whether the file structure should be rescanned
#' @seealso \code{\link{gdb.init}}, \code{\link{gdb.create}},
#' \code{\link{gdir.cd}},
#' @keywords ~db
#' @return No return value, called for side effects.
#' @export gdb.reload
gdb.reload <- function(rescan = TRUE) {
    if (!exists("GROOT", envir = .misha)) {
        stop("gdb.init() must be called beforehand.", call. = FALSE)
    }

    assign("GTRACKS", NULL, envir = .misha)
    assign("GINTERVS", NULL, envir = .misha)

    dir <- get("GWD", envir = .misha)

    res <- ""

    if (get("GWD", envir = .misha) != paste(get("GROOT", envir = .misha), "tracks", sep = "/")) {
        rescan <- TRUE
    }

    db.filename <- paste(get("GROOT", envir = .misha), ".db.cache", sep = "/")

    suppressWarnings({ # disable warnings since dir() on non dir or non existing dir produces warnings
        if (!rescan) {
            retv <- try(
                {
                    f <- file(db.filename, "rb")
                    res <- unserialize(f)
                    close(f)
                },
                silent = TRUE
            )

            if (inherits(retv, "try-error")) {
                rescan <- TRUE
            }
        }

        if (rescan) {
            res <- .gcall("gfind_tracks_n_intervals", dir, .misha_env(), silent = TRUE)
            if (get("GWD", envir = .misha) == paste(get("GROOT", envir = .misha), "tracks", sep = "/")) {
                try(
                    {
                        f <- file(db.filename, "wb")
                        serialize(res, f)
                        close(f)
                    },
                    silent = TRUE
                )
            } else {
                unlink(db.filename, recursive = TRUE)
            }
        }
    })

    tracks <- res[[1]]
    intervals <- res[[2]]

    tracks <- sort(tracks)
    intervals <- sort(intervals)

    res <- intersect(tracks, intervals)
    if (length(res) > 0) {
        stop("The following tracks exist also as intervals: ", paste(res, collapse = " "))
    }

    assign("GTRACKS", tracks, envir = .misha)
    assign("GINTERVS", intervals, envir = .misha)
}


.gdb.convert_attrs <- function() {
    .gcheckroot()

    ro_attrs <- c("created.by", "created.date")
    .gcall_noninteractive(gdb.set_readonly_attrs, ro_attrs)

    for (track in .misha$GTRACKS) {
        for (attr in ro_attrs) {
            try(
                {
                    if (.gcall_noninteractive(.gtrack.var.exists, track, attr)) {
                        .gcall_noninteractive(.gtrack.attr.set, track, attr, as.character(.gtrack.var.get(track, attr))[1], TRUE)
                        .gcall_noninteractive(gtrack.var.rm, track, attr)
                    }
                },
                silent = TRUE
            )
        }
        message(track)
    }
}

.gdb.convert_tracks <- function() {
    .gcheckroot()

    for (track in .misha$GTRACKS) {
        try(
            {
                retv <- try(.gcall_noninteractive(gtrack.info, track), silent = TRUE)
                if (inherits(retv, "try-error") & length(grep("obsolete", retv)) > 0) {
                    message(sprintf("Converting track %s", track))
                    .gcall_noninteractive(gtrack.convert, track)
                }
            },
            silent = TRUE
        )
    }
}


.gconfirmtrackcreate <- function(track) {
    if (!is.na(match(track, get("GTRACKS", envir = .misha)))) {
        stop(sprintf("Track %s already exists", track), call. = FALSE)
    }

    path <- gsub(".", "/", track, fixed = TRUE)
    dir <- dirname(path)
    fulldir <- paste(get("GWD", envir = .misha), dir, sep = "/")
    fullpath <- sprintf("%s.track", paste(get("GWD", envir = .misha), path, sep = "/"))

    if (!file.exists(fulldir)) {
        stop(sprintf("Directory %s does not exist", dir), call. = FALSE)
    }

    if (file.exists(fullpath)) {
        stop(sprintf("File %s already exists", path), call. = FALSE)
    }

    if (!is.na(match(track, get("GINTERVS", envir = .misha)))) {
        stop(sprintf("Interval %s already exists", track), call. = FALSE)
    }

    if (!is.na(match(track, gvtrack.ls()))) {
        stop(sprintf("Virtual track %s already exists", track), call. = FALSE)
    }

    if (.ggetOption(".gautocompletion", FALSE) && exists(track)) {
        stop(sprintf("Variable \"%s\" shadows the name of the new track.\nPlease remove this variable from the environment or switch off autocompletion mode.", track), call. = FALSE)
    }
}

.gdb.add_track <- function(track) {
    .gcheckroot()

    trackdir <- sprintf("%s.track", paste(get("GWD", envir = .misha), gsub("\\.", "/", track), sep = "/"))
    if (file.exists(trackdir)) {
        tracks <- sort(c(get("GTRACKS", envir = .misha), track))
        intervals <- sort(get("GINTERVS", envir = .misha))

        res <- intersect(tracks, intervals)
        if (length(res) > 0) {
            stop("The following tracks exist also as intervals: ", paste(res, collapse = " "))
        }

        if (.ggetOption(".gautocompletion", FALSE)) {
            if (exists(track, envir = .misha)) {
                stop(sprintf("Variable \"%s\" shadows the name of identically named track.\nPlease remove this variable from the environment or switch off autocompletion mode.", track), call. = FALSE)
            }

            if (.ggetOption(".ginteractive", FALSE)) { # set track to NULL otherwise evaluation of track expression pmin(track, 2) will produce a string "2"
                assign(track, NULL, envir = .misha)
            } else {
                assign(track, track, envir = .misha)
            }
        }

        assign("GTRACKS", tracks, envir = .misha)
    }
}

.gdb.rm_track <- function(track) {
    .gcheckroot()

    trackdir <- sprintf("%s.track", paste(get("GWD", envir = .misha), gsub("\\.", "/", track), sep = "/"))
    if (!file.exists(trackdir)) {
        if (.ggetOption(".gautocompletion", FALSE)) {
            if (exists(track, envir = .misha)) {
                remove(list = track, envir = .misha)
            }
        }

        tracks <- get("GTRACKS", envir = .misha)
        tracks <- tracks[tracks != track]
        assign("GTRACKS", tracks, envir = .misha)
    }
}

.gdb.add_intervals.set <- function(intervals.set) {
    .gcheckroot()

    fname <- sprintf("%s.interv", paste(get("GWD", envir = .misha), gsub("\\.", "/", intervals.set), sep = "/"))
    if (file.exists(fname)) {
        tracks <- get("GTRACKS", envir = .misha)
        intervals <- sort(c(get("GINTERVS", envir = .misha), intervals.set))

        res <- intersect(tracks, intervals)
        if (length(res) > 0) {
            stop("The following tracks exist also as intervals: ", paste(res, collapse = " "))
        }

        if (.ggetOption(".gautocompletion", FALSE)) {
            if (exists(intervals.set, envir = .misha)) {
                stop(sprintf("Variable \"%s\" shadows the name of identically named intervals set.\nPlease remove this variable from the environment or switch off autocompletion mode.", intervals.set), call. = FALSE)
            }

            assign(intervals.set, intervals.set, envir = .misha)
        }

        assign("GINTERVS", intervals, envir = .misha)
    }
}

.gdb.rm_intervals.set <- function(intervals.set) {
    .gcheckroot()

    fname <- sprintf("%s.interv", paste(get("GWD", envir = .misha), gsub("\\.", "/", intervals.set), sep = "/"))
    if (!file.exists(fname)) {
        if (.ggetOption(".gautocompletion", FALSE)) {
            if (exists(intervals.set, envir = .misha)) {
                remove(list = intervals.set, envir = .misha)
            }
        }

        intervals <- get("GINTERVS", envir = .misha)
        intervals <- intervals[intervals != intervals.set]
        assign("GINTERVS", intervals, envir = .misha)
    }
}
