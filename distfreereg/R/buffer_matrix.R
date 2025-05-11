# This function takes a character matrix and "buffers" each entry with blank
# spaces so each column's entries have the same length, equal to the length of
# the longest string in that column.
buffer_matrix <- function(mat){
  stopifnot(is.matrix(mat), is.character(mat))
  min_width <- apply(mat, MARGIN = 2, FUN = function(x) max(nchar(x)))
  output <- vector(mode = "list", length = 4)
  for(i in seq_len(ncol(mat))){
      mat[,i] <- sprintf(paste0("%", min_width[i], "s"), mat[,i])
  }
  return(mat)
}
