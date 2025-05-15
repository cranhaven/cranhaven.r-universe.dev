#' Symbolic Variable
#'
#' @name sym.var
#' @aliases sym.var
#' @author Oldemar Rodriguez Rojas
#' @description This function get a symbolic variable from a symbolic data table.
#' @usage sym.var(sym.data, number.sym.var)
#' @param sym.data The symbolic data table
#' @param number.sym.var The number of the column for the variable (feature) that we want to get.
#'
#' @return
#' Return a symbolic data variable with the following structure: \cr
#'
#' $N\cr
#'
#' [1] 7\cr
#'
#' $var.name\cr
#'
#' [1] 'F6'\cr
#'
#' $var.type\cr
#'
#' [1] '$I'\cr
#'
#' $obj.names\cr
#'
#' [1] 'Case1' 'Case2' 'Case3' 'Case4' 'Case5' 'Case6' 'Case7'\cr
#'
#' $var.data.vector\cr
#'
#' F6  F6.1\cr
#'
#' Case1   0.00 90.00\cr
#'
#' Case2 -90.00 98.00\cr
#'
#' Case3  65.00 90.00\cr
#'
#' Case4  45.00 89.00\cr
#'
#' Case5  20.00 40.00\cr
#'
#' Case6   5.00  8.00\cr
#'
#' Case7   3.14  6.76\cr
#'
#' @references
#' Billard L. and  Diday E. (2006).
#' Symbolic data analysis: Conceptual statistics and data mining. Wiley, Chichester.
#'
#' Bock H-H. and Diday E. (eds.) (2000).
#' Analysis of Symbolic Data. Exploratory methods for extracting statistical information
#' from complex data. Springer, Germany.
#'
#' @seealso sym.obj
#' @keywords Symbolic Variable
#' @export
#'
#'
sym.var <- function(sym.data, number.sym.var) {
  if ((number.sym.var > sym.data$M) || (number.sym.var <= 0)) {
    stop("number.sym.var out of range")
  }
  pos <- sym.data$sym.var.starts[number.sym.var]
  adv <- sym.data$sym.var.length[number.sym.var]
  sym.var <- list(
    N = sym.data$N, var.name = sym.data$sym.var.names[number.sym.var],
    var.type = sym.data$sym.var.types[number.sym.var], obj.names = sym.data$sym.obj.names,
    var.data.vector = sym.data$meta[, pos:(pos + adv - 1)]
  )
  return(sym.var)
}
