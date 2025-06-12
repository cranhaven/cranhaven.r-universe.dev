#' @title Internal function
#' 
#' @description Internal function to perform replacement names in object returned by \code{\link{matrix.p.null}}.
#' 
#' @encoding UTF-8
#' @param x An object returned by \code{\link{matrix.p.null}}.
#' @param replacement A replacement name to matched in object returned by \code{\link{matrix.p.null}}.
#' @param newname New name to be replaced in object returned by \code{\link{matrix.p.null}}.
#' @export
mutate.names.matrix.p.null <- function(x, replacement, newname){
  f.mut <- function(x, replacement, newname){
    colnames(x) <- sub(replacement, newname, colnames(x))
    return(x)
  }
  if(replacement!=newname){
    if(!is.null(x$pcps.obs)){
      x$pcps.obs <- f.mut(x$pcps.obs, replacement, newname)
    }
    if(!is.null(x$pcps.null.site)){
      x$pcps.null.site <- sapply(x$pcps.null.site, f.mut, replacement = replacement, newname = newname, simplify = FALSE)
    }
    if(!is.null(x$pcps.null.taxa)){
      x$pcps.null.taxa <- sapply(x$pcps.null.taxa, f.mut, replacement = replacement, newname = newname, simplify = FALSE)
    }
    if(!is.null(x$pcps.null.taxa.adj)){
      x$pcps.null.taxa.adj <- sapply(x$pcps.null.taxa.adj, f.mut, replacement = replacement, newname = newname, simplify = FALSE)
    }
  }
  return(x)
}