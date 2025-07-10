#' @title Logistic function
#'
#' @description Implementation of the logistic function
#'
#' @param x value to apply the function to.
#' @param x0 midpoint parameter. Gives the centre of the function.
#' @param k shape parameter. Gives the slope of the function.
#'
#' @return Value of logistic function with given parameters
logistic <- function(x, x0, k) {
    1 / (1 + exp(-k * (x - x0)))
}


#' @title Bring items forward
#'
#' @description Move selected items to the start of a list.
#'
#' @param ll list to adjust item order.
#' @param items vector of items to bring to the front. Any not in the list will
#'        be ignored.
#'
#' @return list with selected items first
#' @importFrom checkmate check_list
bringItemsForward <- function(ll, items) {

    checkmate::check_list(ll, min.len = 1, names = "unique")
    checkmate::check_character(items, any.missing = FALSE, min.len = 1,
                               unique = TRUE)

    items <- items[items %in% names(ll)]

    if (length(items) > 0) {
        ll.front <- ll[items]
        ll.back <- ll[!(names(ll) %in% items)]

        ll <- c(ll.front, ll.back)
    }

    return(ll)
}
