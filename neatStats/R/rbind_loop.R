#'@title Merge by Columns in Loops
#'
#'@description Merges rows by columns in a loop using the \code{\link{enum}}
#'  function. On first iteration, indicated by \code{\link{enum}}, initiates a
#'  new \code{\link[data.table]{data.table}} with the data to merge as first row. On all
#'  following iterations, adds data to merge as subsequent rows (using
#'  \code{\link[data.table:rbindlist]{data.table::rbindlist}}).
#'
#'@param merged The name of the \code{\link[data.table]{data.table}} for the merged data
#'  (without quotes).
#'@param ... Any number of data to be merged. Each argument must be one of the
#'  following: a \code{\link{data.frame}} (or \code{\link[data.table]{data.table}}) with
#'  either single row or two column; a named vector or a named list (with single
#'  elements); or a single value with parameter name (e.g. date = 1989 or id =
#'  "jdoe"). Data with two columns will be transposed using first column as
#'  column names and second column as corresponding values. See Details,
#'  Examples.
#'@param hush Logical. If \code{TRUE} (default), prints message when the data
#'  frame for merging is initiated.
#'@details In each call, all data passed to the function (via \code{...}) will
#'  be merged into a single row, and that single row will be added to the
#'  "\code{merged}" data table.
#'
#'  See an extensive example via https://github.com/gasparl/neatstats.
#'@seealso \code{\link{enum}}
#' @examples
#'
#' my_vector = c('aa', 'bb', 'cxyz', 'last')
#' for (elem in enum(my_vector)) {
#'     cat(elem, fill = TRUE)
#'     rbind_loop(
#'         merged_data, # data frame name for merging
#'         item = elem[2],
#'         number = elem[1],
#'         whatever = paste0('number (', elem[1], ')')
#'     )
#' }
#' # merged_data now contains all merged rows
#' print(merged_data)
#' # item number   whatever
#' # 1   aa      1 number (1)
#' # 2   bb      2 number (2)
#' # 3 cxyz      3 number (3)
#' # 4 last      4 number (4)
#'
#'
#' # example with other data types
#' for (elem in enum(my_vector)) {
#'     cat(elem, fill = TRUE)
#'     dframe1 = data.frame(item = elem[2],
#'                          number = elem[1])
#'     print(elem[1])
#'     asnum = as.numeric(elem[1])
#'     dframe2 = data.frame(
#'         my_cols = c('index', 'squared', 'multiple'),
#'         my_vals = c(elem[1], asnum ** 2, asnum * 10)
#'     )
#'     my_list = list(ls_item = elem[2], ls_num = elem[1])
#'     my_vec = c(v_item = elem[2], v_num = elem[1])
#'     rbind_loop(
#'         merged_data,
#'         dframe1, # data frame with single row
#'         dframe2, # data frame with two columns
#'         my_list, # named list
#'         my_vec, # named vector
#'         single_val = elem[2], # single element
#'         constant = "whatever" # other single element
#'     )
#' }
#'
#' # again merged_data contains all merged rows
#' # (previous content, if any, were removed)
#' print(merged_data)
#'
#'
#' # example with differring columns
#' for (elem in enum(my_vector)) {
#'     cat(elem, fill = TRUE)
#'     dframe = data.frame(item = elem[2],
#'                         number = elem[1])
#'     asnum = as.numeric(elem[1])
#'     if (asnum %% 2 == 0) {
#'         dframe$sqr = asnum ** 2
#'     }
#'     rbind_loop(merged_data,
#'                dframe)
#' }
#'
#' # merged_data contains all new merged rows
#' # with NAs where sqr was not added
#' print(merged_data)
#'
#' # example with data.table added
#' library('data.table')
#' for (elem in enum(my_vector)) {
#'     cat(elem, fill = TRUE)
#'     dframe = data.frame(item = elem[2],
#'                         number = elem[1])
#'     asnum = as.numeric(elem[1])
#'     dtable = data.table(item2 = paste('DT', elem[2]),
#'                         number2 = asnum + 9)
#'     if (asnum %% 2 == 0) {
#'         dframe$sqr = asnum ** 2
#'     }
#'     rbind_loop(merged_data,
#'                dframe,
#'                dtable)
#' }
#'
#' print(merged_data)
#'
#' # an extensive example to show how to collect and aggregate raw data is
#' # available via the README file at the repository:
#' # https://github.com/gasparl/neatstats
#'
#' @export
rbind_loop = function(merged,
                      ...,
                      hush = FALSE) {
    merged_str = deparse(substitute(merged))
    if (is.null(pkg.globals$my_unique_first_iter)) {
        stop(
            'First iteration was not indicated via the neatStats::enum() function. ',
            'Please use enum() if you want to use rbind_neat(). See ?rbind_neat for details'
        )
    }
    to_merge = get_row(...)
    if (pkg.globals$my_unique_first_iter  == TRUE) {
        if (hush == FALSE) {
            message('Initiated ',
                    merged_str,
                    ' (with ',
                    ncol(to_merge),
                    ' columns).')
        }
        pkg.globals$my_unique_first_iter = FALSE
        assign(merged_str, to_merge, envir = parent.frame())
    } else if (pkg.globals$my_unique_first_iter  == FALSE) {
        assign(merged_str,
               rbindlist(list(merged, to_merge), fill = TRUE),
               envir = parent.frame())
    }
}
