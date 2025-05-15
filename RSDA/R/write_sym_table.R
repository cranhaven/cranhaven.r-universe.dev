
#' Write Symbolic Data Table
#'
#' @name write.sym.table
#' @author Oldemar Rodriguez Rojas
#' @aliases write.sym.table
#' @description This function write (save) a symbolic data table from a CSV data file.
#' @usage write.sym.table(sym.data, file, sep, dec, row.names = NULL, col.names = NULL)
#' @param sym.data Symbolic data table
#' @param file The name of the CSV file.
#' @param sep As in R function read.table
#' @param dec As in R function read.table
#' @param row.names As in R function read.table
#' @param col.names As in R function read.table
#' @return Write in CSV file the symbolic data table.
#' @references Bock H-H. and Diday E. (eds.) (2000). Analysis of Symbolic Data.
#' Exploratory methods for extracting statistical information from complex data. Springer, Germany.
#' @seealso read.sym.table
#'
#' @examples
#' \dontrun{
#' data(example1)
#' write.sym.table(example1, file = "temp4.csv", sep = "|",
#'                 dec = ".", row.names = TRUE, col.names = TRUE)
#' ex1 <- read.sym.table("temp4.csv", header = TRUE,
#'                        sep = "|", dec = ".", row.names = 1)
#' }
#' @importFrom utils write.table
#' @export
write.sym.table <- function(sym.data, file, sep, dec, row.names = NULL, col.names = NULL) {
  sym.data <- to.v2(sym.data)
  utils::write.table(sym.data$meta, file,
    sep = as.character(sep), dec = dec, quote = FALSE,
    row.names = c(row.names), col.names = c(col.names)
  )
}
