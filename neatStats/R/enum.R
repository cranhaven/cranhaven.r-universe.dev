#' @title Enumerate
#'
#' @description Aids enumeration and merging (via \code{\link{rbind_loop}}) in
#'   loops: adds numbers to a vector input, and indicates loop start for
#'   \code{\link{rbind_loop}}.
#' @param items The items to be enumerated in the loop.
#' @param hush Logical. If \code{TRUE} (default), prints "Loop started." when
#'   executed.
#' @param enumerate Logical. If \code{TRUE} (default), adds numbering to the
#'   input vector (in item pairs, see Examples). If \code{FALSE}, returns the
#'   original input.
#' @return Vector with numbers added (if so set).
#' @seealso \code{\link{rbind_loop}}
#' @examples
#'
#' my_vector = c('aa', 'bb', 'cxyz', 'last')
#'
#' for (item in enum(my_vector)) {
#'     print(item)
#' }
#'
#' # just to show what enum() returns
#' enum(my_vector)
#'
#' @export
enum = function(items,
                hush = FALSE,
                enumerate = TRUE) {
    validate_args(match.call(),
                  list(val_arg(hush, c('bool'), 1),
                       val_arg(enumerate, c('bool'), 1)))
    if (length(items) > 0) {
        if (hush == FALSE) {
            message('Loop started.')
        }
        pkg.globals$my_unique_first_iter = TRUE
        if (enumerate == TRUE) {
            return(mapply(c, 1:length(items), items, SIMPLIFY = FALSE))
        } else {
            return(items)
        }
    } else {
        message('No items to iterate.')
        return(items)
    }
}
