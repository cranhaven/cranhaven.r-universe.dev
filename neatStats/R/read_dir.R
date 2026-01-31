#' @title Read and Merge Files from Directory
#'
#'@description Reads data files from any given directory as data frames and
#'  merges them into a single data frame (using
#'  \code{\link[data.table:rbindlist]{data.table::rbindlist}}).
#'
#'@param pattern Regular expression ("regex"; as string or \code{NULL}) for
#'  selecting files (passed to the \code{\link{list.files}} function). The
#'  default \code{NULL} means that all files at the specified path will be read
#'  in. To select, for example, a specific extension like ".txt", the pattern
#'  can be given as \code{"\\.txt$"} (for CSV files, \code{"\\.csv$"}, etc.).
#'  Files ending with e.g. "group2.txt" can be specified as
#'  \code{"group2\\.txt$"}. Files starting with "exp3" can be specified as
#'  \code{"^exp3"}. Files starting with "exp3" AND ending with ".txt" extension
#'  can be specified as \code{"^exp3.*\\.txt$"}. To read in a single file,
#'  specify the full filename (e.g. \code{"exp3_subject46_group2.txt"}). (See
#'  \code{?regex} for more details.)
#'@param path Path to the directory from which the files should be selected and
#'  read. The default \code{"."} means the current working directory (as
#'  returned by \code{\link{getwd}()}). Either specify correct working directory
#'  in advance (see \code{\link{setwd}}, \code{\link{path_neat}}), or otherwise
#'  enter relative or full paths (e.g. \code{"C:/research"} or
#'  \code{"/home/projects"}, etc.).
#'@param reader_function A function to be used for reading the files,
#'  \code{\link[data.table:fread]{data.table::fread}} by default.
#'@param ... Any arguments to be passed on to the chosen \code{reader_function}.
#'@param subdirs Logical (\code{FALSE} by default). If \code{TRUE}, searches
#'  files in subdirectories as well (relative to the given \code{path}).
#'@param filt An expression to filter, by column values, each data file after it
#'  is read and before it is merged with the other data. (The expression should
#'  use column names alone; see Examples.)
#'@param hush Logical. If \code{FALSE} (default), prints lists all data file
#'  names as they are being read (along with related warnings).
#'@note This function is very similar to the \code{readbulk::read_bulk}
#'  function. One important difference however is the \code{\link[data.table]{data.table}}
#'  use, which greatly speeds up the process. Another important difference is
#'  the possibility of file selection based on any regex \code{pattern}.
#'  Furthermore, this function allows pre-filtering by file (see \code{filt}).
#'  Data files could include significant amount of unnecessary data, and
#'  filtering prevents these to be merged.
#'
#'@seealso \code{\link[data.table:rbindlist]{data.table::rbindlist}}
#' @examples
#'\donttest{
#'
#'# first, set current working directory
#'# e.g. to script's path with setwd(path_neat())
#'
#'# read all text files in currect working directory
#' merged_df = read_dir("\\.txt$")
#' # merged_df now has all data
#'
#'# to use utils::read.table for reading (slower than fread)
#'# (with some advisable options passed to it)
#' merged_df = read_dir(
#'     '\\.txt$',
#'     reader_function = read.table,
#'     header = TRUE,
#'     fill = TRUE,
#'     quote = "\"",
#'     stringsAsFactors = FALSE
#' )
#' }
#'
#' @export
read_dir = function(pattern = "*[.]",
                    path = ".",
                    reader_function = data.table::fread,
                    ...,
                    subdirs = FALSE,
                    filt = NULL,
                    hush = FALSE) {
    validate_args(match.call(),
                  list(
                      val_arg(pattern, c('null', 'char'), 1),
                      val_arg(path, c('char'), 1),
                      val_arg(reader_function, c('function')),
                      val_arg(subdirs, c('bool'), 1),
                      val_arg(hush, c('bool'), 1)
                  ))
    filt = paste(deparse(substitute(filt)), collapse = "")
    if (filt != "NULL") {
        if (startsWith(filt, "'") | startsWith(filt, '"')) {
            stop('The argument "filt" must be an expression (not string).')
        }
    }
    f_names = setdiff(
        list.files(
            path = path,
            pattern = pattern,
            recursive = subdirs
        ),
        list.dirs(path, recursive = subdirs, full.names = FALSE)
    )
    if (length(f_names) == 0) {
        warning('No files found with these specifications.')
    }
    merged = data.table()
    for (f_nam in enum(f_names, hush = TRUE)) {
        if (hush == FALSE) {
            if (f_nam[1] == "1") {
                message('Began reading ',
                        length(f_names),
                        ' files at path "',
                        path,
                        '".')
            } else {
                cat("; ")
            }
            cat('(', f_nam[1], ') ', f_nam[2], sep = '')
        }
        new_dat = reader_function(paste(path, f_nam[2], sep = "/"), ...)
        setDT(new_dat)
        if (filt != "NULL") {
            filt_vec = eval(parse(text = paste0('new_dat[',
                                                filt,
                                                ']')))
            na_sum = sum(is.na(filt_vec))
            if (na_sum > 0) {
                if (hush != TRUE) {
                    cat(' (NAs replaced with FALSE for filtering)')
                }
                filt_vec[is.na(filt_vec)] = FALSE
            }
            new_dat = new_dat[filt_vec, ]
        }
        if (hush != TRUE && nrow(new_dat) == 0) {
            cat(" (0 row!)")
        }
        merged = rbindlist(list(merged, new_dat), fill = TRUE)
    }
    if (hush == FALSE && length(f_names) != 0) {
        cat(
            '. Done (returning data frame with',
            ncol(merged),
            'columns',
            nrow(merged),
            'rows).',
            fill = TRUE
        )
    }
    invisible(as.data.frame(merged))
}
