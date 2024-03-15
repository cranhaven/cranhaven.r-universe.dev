.gtrack.var.exists <- function(trackname, varname) {
    trackdir <- sprintf("%s.track", paste(get("GWD", envir = .misha), gsub("\\.", "/", trackname), sep = "/"))
    filename <- paste(trackdir, "vars", varname, sep = "/")
    file.exists(filename)
}

.gtrack.var.get <- function(trackname, varname) {
    if (is.na(match(trackname, get("GTRACKS", envir = .misha)))) {
        stop(sprintf("Track %s does not exist", trackname), call. = FALSE)
    }

    trackdir <- sprintf("%s.track", paste(get("GWD", envir = .misha), gsub("\\.", "/", trackname), sep = "/"))
    filename <- paste(trackdir, "vars", varname, sep = "/")
    if (!file.exists(filename)) {
        stop(sprintf("Track variable %s does not exist", varname), call. = FALSE)
    }
    f <- file(filename, "rb")
    val <- unserialize(f)
    close(f)
    val
}

.gtrack.var.set <- function(trackname, varname, value) {
    if (is.na(match(trackname, get("GTRACKS", envir = .misha)))) {
        stop(sprintf("Track %s does not exist", trackname), call. = FALSE)
    }

    # if vars directory does not exist, create it
    trackdir <- sprintf("%s.track", paste(get("GWD", envir = .misha), gsub("\\.", "/", trackname), sep = "/"))
    dirname <- paste(trackdir, "vars", sep = "/")
    if (!file.exists(dirname)) {
        dir.create(dirname, mode = "0777")
    }

    # save the variable
    filename <- paste(dirname, varname, sep = "/")
    f <- file(filename, "wb")
    serialize(value, f)
    close(f)
}



#' Returns value of a track variable
#'
#' Returns value of a track variable.
#'
#' This function returns the value of a track variable. If the variable does
#' not exist an error is reported.
#'
#' @param track track name
#' @param var track variable name
#' @return Track variable value.
#' @seealso \code{\link{gtrack.var.set}}, \code{\link{gtrack.var.ls}},
#' \code{\link{gtrack.var.rm}}
#' @keywords ~variable
#' @examples
#' \dontshow{
#' options(gmax.processes = 2)
#' }
#'
#' gdb.init_examples()
#' gtrack.var.set("sparse_track", "test_var", 1:10)
#' gtrack.var.get("sparse_track", "test_var")
#' gtrack.var.rm("sparse_track", "test_var")
#'
#' @export gtrack.var.get
gtrack.var.get <- function(track = NULL, var = NULL) {
    if (is.null(substitute(track)) || is.null(var)) {
        stop("Usage: gtrack.var.get(track, var)", call. = FALSE)
    }
    .gcheckroot()

    trackstr <- do.call(.gexpr2str, list(substitute(track)), envir = parent.frame())
    .gtrack.var.get(trackstr, var)
}



#' Returns a list of track variables for a track
#'
#' Returns a list of track variables for a track.
#'
#' This function returns a list of track variables of a track that match the
#' pattern (see 'grep'). If called without any arguments all track variables of
#' a track are returned.
#'
#' @param track track name
#' @param pattern,ignore.case,perl,fixed,useBytes see 'grep'
#' @return An array that contains the names of track variables.
#' @seealso \code{\link{grep}}, \code{\link{gtrack.var.get}},
#' \code{\link{gtrack.var.set}}, \code{\link{gtrack.var.rm}}
#' @keywords ~variable ~ls
#' @examples
#' \dontshow{
#' options(gmax.processes = 2)
#' }
#'
#' gdb.init_examples()
#' gtrack.var.ls("sparse_track")
#' gtrack.var.set("sparse_track", "test_var1", 1:10)
#' gtrack.var.set("sparse_track", "test_var2", "v")
#' gtrack.var.ls("sparse_track")
#' gtrack.var.ls("sparse_track", pattern = "2")
#' gtrack.var.rm("sparse_track", "test_var1")
#' gtrack.var.rm("sparse_track", "test_var2")
#'
#' @export gtrack.var.ls
gtrack.var.ls <- function(track = NULL, pattern = "", ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE) {
    if (length(substitute(track)) != 1) {
        stop("Usage: gtrack.var.ls(track, pattern = \"\", ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)", call. = FALSE)
    }
    .gcheckroot()

    trackstr <- do.call(.gexpr2str, list(substitute(track)), envir = parent.frame())

    if (is.na(match(trackstr, get("GTRACKS", envir = .misha)))) {
        stop(sprintf("Track %s does not exist", trackstr), call. = FALSE)
    }

    trackdir <- sprintf("%s.track", paste(get("GWD", envir = .misha), gsub("\\.", "/", trackstr), sep = "/"))
    dirname <- paste(trackdir, "vars", sep = "/")
    suppressWarnings({ # disable warnings since dir() on non dir or non existing dir produces warnings
        invisible(files <- dir(dirname))
    })
    if (length(files) > 0) {
        grep(pattern, files, value = TRUE, ignore.case = ignore.case, perl = perl, fixed = fixed, useBytes = useBytes)
    } else {
        files
    }
}



#' Deletes a track variable
#'
#' Deletes a track variable.
#'
#' This function deletes a track variable.
#'
#' @param track track name
#' @param var track variable name
#' @return None.
#' @seealso \code{\link{gtrack.var.get}}, \code{\link{gtrack.var.set}},
#' \code{\link{gtrack.var.ls}}
#' @keywords ~variable
#' @examples
#' \dontshow{
#' options(gmax.processes = 2)
#' }
#'
#' gdb.init_examples()
#' gtrack.var.set("sparse_track", "test_var1", 1:10)
#' gtrack.var.set("sparse_track", "test_var2", "v")
#' gtrack.var.ls("sparse_track")
#' gtrack.var.rm("sparse_track", "test_var1")
#' gtrack.var.rm("sparse_track", "test_var2")
#' gtrack.var.ls("sparse_track")
#'
#' @export gtrack.var.rm
gtrack.var.rm <- function(track = NULL, var = NULL) {
    if (is.null(substitute(track)) || is.null(var)) {
        stop("Usage: gtrack.var.rm(track, var)", call. = FALSE)
    }
    .gcheckroot()

    trackname <- do.call(.gexpr2str, list(substitute(track)), envir = parent.frame())
    if (is.na(match(trackname, get("GTRACKS", envir = .misha)))) {
        stop(sprintf("Track %s does not exist", trackname), call. = FALSE)
    }

    trackdir <- sprintf("%s.track", paste(get("GWD", envir = .misha), gsub("\\.", "/", trackname), sep = "/"))
    filename <- paste(trackdir, "vars", var, sep = "/")
    invisible(file.remove(filename))
}



#' Assigns value to a track variable
#'
#' Assigns value to a track variable.
#'
#' This function creates a track variable and assigns 'value' to it. If the
#' track variable already exists its value is overwritten.
#'
#' @param track track name
#' @param var track variable name
#' @param value value
#' @return None.
#' @seealso \code{\link{gtrack.var.get}}, \code{\link{gtrack.var.ls}},
#' \code{\link{gtrack.var.rm}}
#' @keywords ~variable
#' @examples
#' \dontshow{
#' options(gmax.processes = 2)
#' }
#'
#' gdb.init_examples()
#' gtrack.var.set("sparse_track", "test_var", 1:10)
#' gtrack.var.get("sparse_track", "test_var")
#' gtrack.var.rm("sparse_track", "test_var")
#'
#' @export gtrack.var.set
gtrack.var.set <- function(track = NULL, var = NULL, value = NULL) {
    if (is.null(substitute(track)) || is.null(var) || is.null(value)) {
        stop("Usage: gtrack.var.set(track, var, value)", call. = FALSE)
    }
    .gcheckroot()

    trackstr <- do.call(.gexpr2str, list(substitute(track)), envir = parent.frame())
    .gtrack.var.set(trackstr, var, value)
}
