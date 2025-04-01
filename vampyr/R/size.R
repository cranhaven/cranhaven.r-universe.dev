size <- function(x=NULL,n=NULL){
  if (is.list(x) & !is.data.frame(x)){
    # x is a list
    return(lapply(x,size))
  } else {
    if (is.null(x)){
      # x is NULL
      if (is.null(n)){
        return(c(0,0))
      } else {
        return(c(0,0)[n])
      }
    } else {
      if (is.null(dim(x))){
        # x is a vector
        if (is.null(n)){
          return(c(1,length(x)))
        } else {
          return(c(1,length(x))[n])
        }
      } else {
        # x is a matrix, an array or a data.frame
        if (is.null(n)){
          return(dim(x))
        } else {
          return(dim(x)[n])
        }
      }
    }
  }
}
