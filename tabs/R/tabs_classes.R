#' Create an Object of Class 'tabs'
#'
#' Constructs an S3 object of class 'tabs' containing reconstruct output.
#'
#' @param x output of reconstruct
#' 
#' @return An object of class 'tabs'.
#' 
#' @keywords internal
#' 
create_tabs_class <- function(x) {
  structure(
    list(
      recvect = x$recvect,
      recrast = x$recrast, 
      recarea = x$recarea,
      labs = x$labs,
      topo = x$topo,
      curve = x$curve, 
      correction = x$correction,
      metadata = x$metadata
    ),
    class = "tabs"
  )
}

#' @export
#' 
print.tabs <- function(x, ...) {
  cat("An object of class 'tabs':\n")
  
  print(list(recvect=x$recvect,
             recrast=x$recrast,
             recarea=x$recarea,
             labs=x$labs,
             topo=x$topo,
             curve=x$curve,
             correction=x$correction
  ))  # Only print the data
}


