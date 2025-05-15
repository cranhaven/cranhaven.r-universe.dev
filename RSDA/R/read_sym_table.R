#' Read a Symbolic Table
#' @name read.sym.table
#' @aliases read.sym.table
#' @author Oldemar Rodriguez Rojas
#' @description It reads a symbolic data table from a CSV file.
#' @usage read.sym.table(file, header = TRUE, sep, dec, row.names = NULL)
#' @param file The name of the CSV file.
#' @param header As in R function read.table
#' @param sep As in R function read.table
#' @param dec As in R function read.table
#' @param row.names As in R function read.table
#' @details
#' The labels $C means that follows a continuous variable, $I means an interval variable, $H
#' means a histogram variables and $S means set variable. In the first row each labels should
#' be follow of a name to variable and to the case of histogram a set variables types the names
#' of the modalities (categories) . In data rows for continuous variables we have just one
#' value, for interval variables we have the minimum and the maximum of the interval,
#' for histogram variables we have the number of modalities and then the probability
#' of each modality and for set variables we have the cardinality of the set and next
#' the elements of the set.
#'
#' The format is the CSV file should be like:
#'
#'   $C   F1 $I F2 F2 $H F3  M1  M2  M3 $S F4 E1 E2 E3 E4 \cr
#'
#' Case1 $C  2.8 $I  1  2 $H  3 0.1 0.7 0.2 $S  4  e  g  k  i\cr
#'
#' Case2 $C  1.4 $I  3  9 $H  3 0.6 0.3 0.1 $S  4  a  b  c  d\cr
#'
#' Case3 $C  3.2 $I -1  4 $H  3 0.2 0.2 0.6 $S  4  2  1  b  c\cr
#'
#' Case4 $C -2.1 $I  0  2 $H  3 0.9 0.0 0.1 $S  4  3  4  c  a\cr
#'
#' Case5 $C -3.0 $I -4 -2 $H  3 0.6 0.0 0.4 $S  4  e  i  g  k\cr
#'
#' The internal format is:\cr

#'   $N\cr

#' [1] 5\cr

#' $M\cr

#' [1] 4\cr

#' $sym.obj.names\cr

#' [1] 'Case1' 'Case2' 'Case3' 'Case4' 'Case5'\cr

#' $sym.var.names\cr

#' [1] 'F1' 'F2' 'F3' 'F4'\cr

#' $sym.var.types\cr

#' [1] '$C' '$I' '$H' '$S'\cr

#' $sym.var.length\cr

#' [1] 1 2 3 4\cr

#' $sym.var.starts\cr

#' [1]  2  4  8 13\cr

#' $meta\cr

#' $C   F1 $I F2 F2 $H F3  M1  M2  M3 $S F4 E1 E2 E3 E4\cr

#' Case1 $C  2.8 $I  1  2 $H  3 0.1 0.7 0.2 $S  4  e  g  k  i\cr

#' Case2 $C  1.4 $I  3  9 $H  3 0.6 0.3 0.1 $S  4  a  b  c  d\cr

#' Case3 $C  3.2 $I -1  4 $H  3 0.2 0.2 0.6 $S  4  2  1  b  c\cr

#' Case4 $C -2.1 $I  0  2 $H  3 0.9 0.0 0.1 $S  4  3  4  c  a\cr

#' Case5 $C -3.0 $I -4 -2 $H  3 0.6 0.0 0.4 $S  4  e  i  g  k\cr

#' $data\cr

#' F1 F2 F2.1  M1  M2  M3 E1 E2 E3 E4\cr

#' Case1  2.8  1    2 0.1 0.7 0.2  e  g  k  i\cr

#' Case2  1.4  3    9 0.6 0.3 0.1  a  b  c  d\cr

#' Case3  3.2 -1    4 0.2 0.2 0.6  2  1  b  c\cr

#' Case4 -2.1  0    2 0.9 0.0 0.1  3  4  c  a\cr

#' Case5 -3.0 -4   -2 0.6 0.0 0.4  e  i  g  k\cr
#'
#' @return Return a symbolic data table structure.
#' @references
#' Bock H-H. and Diday E. (eds.) (2000).
#' Analysis of Symbolic Data. Exploratory methods for extracting statistical information
#' from complex data. Springer, Germany.
#' @seealso display.sym.table
#' @examples
#' \dontrun{
#' data(example1)
#' write.sym.table(example1,
#'   file = "temp4.csv", sep = "|", dec = ".", row.names = TRUE,
#'   col.names = TRUE
#' )
#' ex1 <- read.sym.table("temp4.csv", header = TRUE, sep = "|", dec = ".", row.names = 1)
#' }
#' @keywords Symbolic Table
#' @export
#' @importFrom utils read.table
#'
read.sym.table <- function(file, header = TRUE, sep, dec, row.names = NULL) {

  meta.data <- utils::read.table(file, header,
    sep = as.character(sep), dec = as.character(dec),
    row.names = c(row.names), check.names = FALSE
  )
  n.sym.objects <- dim(meta.data)[1]
  meta.M <- dim(meta.data)[2]
  sym.var.types <- list()
  sym.var.length <- rep(0, length(meta.M))
  sym.var.names <- list()
  sym.var.starts <- list()
  sym.obj.names <- rownames(meta.data)
  del.columns <- c()
  del.columns.length <- 0
  if (header == TRUE) {
    meta.types <- colnames(meta.data)
  } else {
    stop("Data file have to have a header")
  }
  meta.types.orig <- meta.types
  for (i in 1:length(meta.types)) {
    meta.types[i] <- substr(meta.types[i], start = 1, stop = 2)
  }
  for (j in 1:length(meta.types)) {
    if ((meta.types[j] == "$C") || (meta.types[j] == "$c")) {
      sym.var.types[j] <- "$C"
      sym.var.length[j] <- 1
      sym.var.names[j] <- meta.types.orig[j + 1]
      sym.var.starts[j] <- j + 1
    } else if ((meta.types[j] == "$I") || (meta.types[j] == "$i")) {
      sym.var.types[j] <- "$I"
      sym.var.length[j] <- 2
      sym.var.names[j] <- meta.types.orig[j + 1]
      sym.var.starts[j] <- j + 1
    } else if ((meta.types[j] == "$H") || (meta.types[j] == "$h")) {
      sym.var.types[j] <- "$H"
      sym.var.length[j] <- as.integer(meta.data[2, j + 1])
      del.columns[del.columns.length + 1] <- j + 1
      del.columns.length <- del.columns.length + 1
      sym.var.names[j] <- meta.types.orig[j + 1]
      sym.var.starts[j] <- j + 2
    } else if ((meta.types[j] == "$M") || (meta.types[j] == "$m")) {
      sym.var.types[j] <- "$M"
      sym.var.length[j] <- as.integer(meta.data[2, j + 1])
      del.columns[del.columns.length + 1] <- j + 1
      del.columns.length <- del.columns.length + 1
      sym.var.names[j] <- meta.types.orig[j + 1]
      sym.var.starts[j] <- j + 2
    } else if ((meta.types[j] == "$S") || (meta.types[j] == "$s")) {
      sym.var.types[j] <- "$S"
      sym.var.length[j] <- as.integer(meta.data[2, j + 1])
      del.columns[del.columns.length + 1] <- j + 1
      del.columns.length <- del.columns.length + 1
      sym.var.names[j] <- meta.types.orig[j + 1]
      sym.var.starts[j] <- j + 2
    } else {
      sym.var.types[j] <- "NA"
    }
  }
  del1 <- match(sym.var.types, c("$C", "$I", "$H", "$S", "$M"), 0)
  for (k in 1:del.columns.length) {
    sym.var.types[del.columns[k]] <- "$H"
  }


  del2 <- match(sym.var.types, c("$C", "$I", "$H", "$S", "$M"), 0)
  sym.var.types <- sym.var.types[del1 > 0]
  sym.var.length <- sym.var.length[del1 > 0]
  n.sym.var <- length(sym.var.length)
  data.matrix <- as.data.frame(meta.data[, del2 == 0])
  sym.data <- list(
    N = n.sym.objects, M = n.sym.var, sym.obj.names = sym.obj.names,
    sym.var.names = unlist(sym.var.names), sym.var.types = unlist(sym.var.types),
    sym.var.length = sym.var.length, sym.var.starts = unlist(sym.var.starts), meta = meta.data,
    data = data.matrix
  )

  class(sym.data) <- c("sym.data.table")
  sym.data <- to.v3(sym.data)
  return(sym.data)
}
