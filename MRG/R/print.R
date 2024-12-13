#' Prints MRG-objects
#'
#' @param x MRG-object, created by call to \code{\link{createMRGobject}}
#' @param ... Other parameters to underlying print functions
#'
#'
#'@rdname createMRGobject
#' @export
print.MRG = function(x, ...) {
  cat("object of class MRG, containing:\n \n")
  cat("MRGinp - list of", length(x$MRGinp), "grids with ", dim(x$MRGinp[[1]])[2], "columns and",
      paste(lapply(lapply(x$MRGinp, dim), "[[", 1), collapse = ", "), "rows, respectively\n")
  cat("\nResolutions:\n")
  cat(x$ress, "\n\nFirst lines of first grid:\n")
  print(st_geometry(x$MRGinp[[1]]), n = 0)
  print(st_drop_geometry(x$MRGinp[[1]][1:5,]))
  if ("ifg" %in% names(x)) {
    cat("\nifg - sf-object with", dim(x$ifg)[1], "rows and", dim(x$ifg)[2], "columns\n")
    print(st_geometry(x$ifg), n = 0)
    print(st_drop_geometry(x$ifg[1:5,]), ...)
  }
  cat("\nParameters: \n")
  print(unlist(x[which(!names(x) %in% c("MRGinp", "ifg", "ress"))]))
}
