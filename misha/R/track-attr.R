.gtrack.attr.set <- function(trackstr, attr, value, overwrite.readonly) {
    table <- data.frame(value)
    colnames(table) <- attr
    rownames(table) <- trackstr

    .gtrack.attr.import(table, FALSE, overwrite.readonly)
}

.gtrack.attr.import <- function(table, remove.others, overwrite.readonly) {
    table[, 1:ncol(table)] <- sapply(table[, 1:ncol(table)], as.character)

    readonly_attrs <- NULL
    if (!overwrite.readonly) {
        readonly_attrs <- gdb.get_readonly_attrs()
    }

    .gcall("gset_tracks_attrs", table, remove.others, readonly_attrs, .misha_env())
}




#' Returns track attributes values
#'
#' Returns track attributes values.
#'
#' This function returns a data frame that contains track attributes values.
#' Column names of the data frame consist of the attribute names, row names
#' contain the track names.
#'
#' The list of required tracks is specified by 'tracks' argument. If 'tracks'
#' is 'NULL' the attribute values of all existing tracks are returned.
#'
#' Likewise the list of required attributes is controlled by 'attrs' argument.
#' If 'attrs' is 'NULL' all attribute values of the specified tracks are
#' returned. The columns are also sorted then by "popularity" of an attribute,
#' i.e. the number of tracks containing this attribute. This sorting is not
#' applied if 'attrs' is not 'NULL'.
#'
#' Empty character string in a table cell marks a non-existing attribute.
#'
#' @param tracks a vector of track names or 'NULL'
#' @param attrs a vector of attribute names or 'NULL'
#' @return A data frame containing track attributes values.
#' @seealso \code{\link{gtrack.attr.import}}, \code{\link{gtrack.attr.get}},
#' \code{\link{gtrack.attr.set}}
#' @keywords ~attr ~attribute
#' @examples
#' \dontshow{
#' options(gmax.processes = 2)
#' }
#'
#' gdb.init_examples()
#' gtrack.attr.export()
#' gtrack.attr.export(tracks = c("sparse_track", "dense_track"))
#' gtrack.attr.export(attrs = "created.by")
#'
#' @export gtrack.attr.export
gtrack.attr.export <- function(tracks = NULL, attrs = NULL) {
    .gcheckroot()

    if (.ggetOption(".ginteractive", FALSE)) {
        trackstr <- do.call(.gexpr2str, list(substitute(tracks)), envir = parent.frame())
        if (!is.na(match(trackstr, get("GTRACKS", envir = .misha)))) {
            tracks <- trackstr
        }
    }

    if (!is.null(tracks)) {
        tracks <- unique(tracks)
    }
    if (!is.null(attrs)) {
        attrs <- unique(attrs)
    }

    if (is.null(tracks)) {
        tracks <- get("GTRACKS", envir = .misha)
    } else {
        idx <- which(!(tracks %in% get("GTRACKS", envir = .misha)))[1]
        if (!is.na(idx)) {
            stop(sprintf("Track %s does not exist", tracks[idx]), call. = FALSE)
        }
    }

    .gcall("gget_tracks_attrs", tracks, attrs, .misha_env())
}



#' Imports track attributes values
#'
#' Imports track attributes values.
#'
#' This function makes imports attribute values contained in a data frame
#' 'table'. The format of a table is similar to the one returned by
#' 'gtrack.attr.export'. The values of the table must be character strings.
#' Column names of the table should specify the attribute names, while row
#' names should contain the track names.
#'
#' The specified attributes of the specified tracks are modified. If an
#' attribute value is an empty string this attribute is removed from the track.
#'
#' If 'remove.others' is 'TRUE' all non-readonly attributes that do not appear
#' in the table are removed, otherwise they are preserved unchanged.
#'
#' Error is reported on an attempt to modify a value of a read-only attribute.
#'
#' @param table a data frame containing attribute values
#' @param remove.others specifies what to do with the attributes that are not
#' in the table
#' @return None.
#' @seealso \code{\link{gtrack.attr.import}}, \code{\link{gtrack.attr.set}},
#' \code{\link{gtrack.attr.get}}, \code{\link{gdb.get_readonly_attrs}}
#' @keywords ~attr ~attribute
#' @examples
#' \dontshow{
#' options(gmax.processes = 2)
#' }
#'
#' gdb.init_examples()
#' t <- gtrack.attr.export()
#' t$newattr <- as.character(1:dim(t)[1])
#' gtrack.attr.import(t)
#' gtrack.attr.export(attrs = "newattr")
#'
#' # roll-back the changes
#' t$newattr <- ""
#' gtrack.attr.import(t)
#'
#' @export gtrack.attr.import
gtrack.attr.import <- function(table = NULL, remove.others = FALSE) {
    if (is.null(table)) {
        stop("Usage: gtrack.attr.import(table, remove.others = FALSE)", call. = FALSE)
    }
    .gcheckroot()

    tracks <- rownames(table)
    attrs <- colnames(table)

    if (!is.data.frame(table) || any(dim(table) < 1) || !length(tracks) || !length(attrs) || any(is.na(tracks)) || any(is.na(attrs)) || any(attrs == "")) {
        stop("Invalid format of attributes table", call. = FALSE)
    }

    idx <- which(!(tracks %in% get("GTRACKS", envir = .misha)))[1]
    if (!is.na(idx)) {
        stop(sprintf("Track %s does not exist", tracks[idx]), call. = FALSE)
    }

    idx <- which(duplicated(tracks))[1]
    if (!is.na(idx)) {
        stop(sprintf("Track %s appears more than once", tracks[idx]), call. = FALSE)
    }

    idx <- which(duplicated(attrs))[1]
    if (!is.na(idx)) {
        stop(sprintf("Attribute %s appears more than once", attrs[idx]), call. = FALSE)
    }

    .gtrack.attr.import(table, remove.others, FALSE)
    retv <- 0 # suppress return value
}



#' Returns value of a track attribute
#'
#' Returns value of a track attribute.
#'
#' This function returns the value of a track attribute. If the attribute does
#' not exist an empty sting is returned.
#'
#' @param track track name
#' @param attr attribute name
#' @return Track attribute value.
#' @seealso \code{\link{gtrack.attr.import}}, \code{\link{gtrack.attr.set}}
#' @keywords ~attr ~attribute
#' @examples
#' \dontshow{
#' options(gmax.processes = 2)
#' }
#'
#' gdb.init_examples()
#' gtrack.attr.set("sparse_track", "test_attr", "value")
#' gtrack.attr.get("sparse_track", "test_attr")
#' gtrack.attr.set("sparse_track", "test_attr", "")
#'
#' @export gtrack.attr.get
gtrack.attr.get <- function(track = NULL, attr = NULL) {
    if (is.null(substitute(track)) || is.null(attr)) {
        stop("Usage: gtrack.attr.get(track, attr)", call. = FALSE)
    }
    .gcheckroot()

    trackstr <- do.call(.gexpr2str, list(substitute(track)), envir = parent.frame())
    res <- gtrack.attr.export(trackstr, attr)
    res[1, 1]
}



#' Assigns value to a track attribute
#'
#' Assigns value to a track attribute.
#'
#' This function creates a track attribute and assigns 'value' to it. If the
#' attribute already exists its value is overwritten.
#'
#' If 'value' is an empty string the attribute is removed.
#'
#' Error is reported on an attempt to modify a value of a read-only attribute.
#'
#' @param track track name
#' @param attr attribute name
#' @param value value
#' @return None.
#' @seealso \code{\link{gtrack.attr.get}}, \code{\link{gtrack.attr.import}},
#' \code{\link{gtrack.var.set}}, \code{\link{gdb.get_readonly_attrs}}
#' @keywords ~attr ~attribute
#' @examples
#' \dontshow{
#' options(gmax.processes = 2)
#' }
#'
#' gdb.init_examples()
#' gtrack.attr.set("sparse_track", "test_attr", "value")
#' gtrack.attr.get("sparse_track", "test_attr")
#' gtrack.attr.set("sparse_track", "test_attr", "")
#'
#' @export gtrack.attr.set
gtrack.attr.set <- function(track = NULL, attr = NULL, value = NULL) {
    if (is.null(substitute(track)) || is.null(attr) || is.null(value)) {
        stop("Usage: gtrack.attr.set(track, attr, value)", call. = FALSE)
    }
    .gcheckroot()

    trackstr <- do.call(.gexpr2str, list(substitute(track)), envir = parent.frame())
    .gtrack.attr.set(trackstr, attr, value, FALSE)
    retv <- 0 # suppress return value
}
