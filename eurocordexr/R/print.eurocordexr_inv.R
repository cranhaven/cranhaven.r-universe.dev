#' Print an inventory
#'
#' Modified
#' \code{\link[data.table:print.data.table]{data.table::print.data.table}} to
#' print an inventory from \code{\link{get_inventory}} and
#' \code{\link{get_inventory_cmip5}} more nicely by removing some columns.
#'
#' @param x data.table to print
#' @param all_cols Boolean (default \code{FALSE}), if \code{TRUE}, will print all
#'   columns available
#' @param ... passed on to \code{\link[data.table:print.data.table]{data.table::print.data.table}}
#'
#' @return x invisibly, used for side effects: prints to console
#'
#' @seealso \code{\link{print.default}}
#' @export
print.eurocordexr_inv <- function(x, all_cols = F, ...){

  # remove "eurocordexr_inv" class, so print falls back to data.table default (internal)
  setattr(x, "class", c("data.table", "data.frame"))

  cols_optional <- c("nn_files",
                     "total_simulation_years",
                     "period_contiguous",
                     "list_files")
  class_abbs <- c("<int>", "<int>", "<lgcl>", "<list>")

  # print less columns
  if(!all_cols){

    avail <- cols_optional %in% colnames(x)
    cols_not_print <- cols_optional[avail]
    n <- length(cols_not_print)
    print(x[, -..cols_not_print], ...)
    # borrowed from data.table:::print.data.table()
    if(n > 0L){
      cat(sprintf(ngettext(n,
                           "%d variable not shown: %s\n",
                           "%d variables not shown: %s\n"),
                  n,
                  paste(cols_not_print, class_abbs[avail], collapse = ", ")))
    }


  } else {
    print(x, ...)
  }

  # add back "eurocordexr_inv" class, since modified by reference
  setattr(x, "class", c("eurocordexr_inv", "data.table", "data.frame"))

  invisible(x)
}

