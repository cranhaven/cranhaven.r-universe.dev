read.big.data <- function(path,  sep = ",", header = FALSE) {
  bigmemory::read.big.matrix(path, sep = sep, header = header, type = "double")
}
 