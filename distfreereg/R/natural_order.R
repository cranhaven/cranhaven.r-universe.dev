natural_order <- function(data, cols){
  # data should be only a matrix or a data frame at this point
  stopifnot(is.matrix(data) || is.data.frame(data))
  df <- as.data.frame(data)
  if(is.numeric(cols)){
    cols <- paste0("df[[", cols, "]]", collapse = ", ")
  } else {
    cols <- paste0("df[[\"", cols, "\"]]", collapse = ", ")
  }
  eval(str2expression(paste0("order(", cols, ")")))
}
