#' @title Print a crosstab object
#' @description This function prints the results of a calculated two-way
#' frequency table.
#' @param x An object of class \code{crosstab}
#' @param ... not currently used.
#' @importFrom stats addmargins
#' @return
#' No return value, called for side effects
#' @examples
#' mycrosstab <- crosstab(mtcars, cyl, gear, type = "freq", digits = 2)
#' print(mycrosstab)
#'
#' mycrosstab <- crosstab(mtcars, cyl, gear, type = "rowpercent", digits = 3)
#' print(mycrosstab)
#' @rdname print.crosstab
#' @export
print.crosstab <- function(x, ...) {
  if(!inherits(x, "crosstab")) stop("Object must be of type crosstab")
  tb <- x$table
  if (x$type == "freq"){
    if (x$total){
      tb <- addmargins(tb, 1, FUN=list(Total=sum), quiet=TRUE)
      tb <- addmargins(tb, 2, FUN=list(Total=sum), quiet=TRUE)
    }
  }

  if (x$type == "percent"){
    if (x$total){
      tb <- addmargins(tb, 1, FUN=list(Total=sum), quiet=TRUE)
      tb <- addmargins(tb, 2, FUN=list(Total=sum), quiet=TRUE)
    }
    tb <- replace(tb, TRUE, sprintf(paste0("%.", x$digits,"f%%"), tb*100))
  }

  if (x$type == "rowpercent"){
    if (x$total){
      tb <- addmargins(tb, 2, FUN=list(Total=sum), quiet=TRUE)
    }
    tb <- replace(tb, TRUE, sprintf(paste0("%.", x$digits,"f%%"), tb*100))
  }

  if (x$type == "colpercent"){
    if (x$total){
      tb <- addmargins(tb, 1, FUN=list(Total=sum), quiet=TRUE)
    }
    tb <- replace(tb, TRUE, sprintf(paste0("%.", x$digits,"f%%"), tb*100))

  }
  print.table(tb, right=TRUE, justify="right")

  if (!is.null(x$chisquare)){
    cat("\n", x$chisquare, "\n")
  }
}
