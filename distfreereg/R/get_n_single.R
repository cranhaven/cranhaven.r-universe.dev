get_n_single <- function(x){
  if(!is.null(x)){
    if(is.numeric(x)){
      output <- nrow(as.matrix(x))
    } else {
      if(is.data.frame(x)){
        output <- nrow(x)
      } else {
        if(is(x, "lm")){
          output <- nrow(x[["model"]])
        } else {
          if(is(x, "nls")){
            output <- nrow(eval(x[["data"]]))
          } else {
            if(is.function(x) || is(x, "formula")){
              output <- NULL
            } else {
              stop("Invalid input to get_n_single(): ", class(x))
            }
          }
        }
      }
    }
  } else {
    output <- NULL
  }
  # message("output: ", output)
  return(output)
}
