transpose <- function(object=NULL){
    if (is.null(object))
      stop('transpose: input is not a matrix or a vector.',
           call.=FALSE)
    if (!is.matrix(object)){
      if (is.atomic(object)){
        mat <- matrix(object,ncol=length(object),byrow=TRUE)
      } else {
        stop('transpose: input is not a matrix or a vector.',
             call.=FALSE)
      }
    } else {
      mat <- object
    }
    return(t(mat))
  }
