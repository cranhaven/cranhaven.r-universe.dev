get_obj_sizes <- function(x, obj) {
  
  # convert to vector.
  x <- as.numeric(x)
  
  # remove NA's (if any).
  x <- x[!is.na(x)]
  
  # extract element from object using index.
  x <- obj[[c(x)]]
  
  # compute object size.
  as.numeric(object_size(x))
  
}

#' @importFrom pryr object_size
get_obj_sizes_dt <- function(dt, obj) {
  
  dt <- copy(dt)
  
  # split data.table into candidates - one for each row.
  objs <- split.data.frame(dt, seq_len(nrow(dt)))
  obj_sizes <- vapply(objs, 
                      get_obj_sizes,
                      FUN.VALUE = numeric(1),
                      obj = obj)
  
  # insert in data.table.
  dt$size <- obj_sizes
  
  dt
  
}