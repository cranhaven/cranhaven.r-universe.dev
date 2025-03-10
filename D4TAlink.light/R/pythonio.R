## =======================================================================
#' Restore R data.frame from Apache Arrow feather file.
#' @param quiet issue warning if file does not exists.
#' @inheritParams D4TAlink-common-args
#' @return Object stored in Apache Arrow feather file, or \code{NULL} if file does not exist.
#' @importFrom feather read_feather
#' @export
readFeather <- function(task,type,subdir=NULL,dirCreate=FALSE,quiet=FALSE) {
  fn <- binaryFn(task,type,ext="ftr",subdir=subdir,dirCreate=dirCreate)
  res <- NULL
  if(!file.exists(fn)&&!quiet) {
    warning(paste0("object '",type,"' not found"))
  } else {
    res <- feather::read_feather(fn)
  }
  res
}

#' Save R object in Apache Arrow feather file.
#' @param object data.frame to serialize.
#' @inheritParams D4TAlink-common-args
#' @inheritParams getTaskEnckey
#' @return the file name invisibly.
#' @importFrom reticulate py_load_object
#' @export
saveFeather <- function(object,task,type,subdir=NULL,dirCreate=TRUE) {
  fn <- binaryFn(task,type,ext="ftr",subdir=subdir,dirCreate=dirCreate)
  feather::write_feather(object,fn)
  invisible(fn)
}

## =======================================================================
#' Restore R data.frame from a Python pickle file.
#' @param quiet issue warning if file does not exists.
#' @inheritParams D4TAlink-common-args
#' @return Object stored in Apache Arrow feather file, or \code{NULL} if file does not exist.
#' @importFrom feather read_feather
#' @export
readPickle <- function(task,type,subdir=NULL,dirCreate=FALSE,quiet=FALSE) {
  fn <- binaryFn(task,type,ext="pkl",subdir=subdir,dirCreate=dirCreate)
  res <- NULL
  if(!file.exists(fn)&&!quiet) {
    warning(paste0("object '",type,"' not found"))
  } else {
    res <- reticulate::py_load_object(fn)
  }
  res
}

#' Save R object in Python pickle file.
#' @param object data.frame to serialize.
#' @inheritParams D4TAlink-common-args
#' @inheritParams getTaskEnckey
#' @return the file name invisibly.
#' @importFrom reticulate py_save_object
#' @export
savePickle <- function(object,task,type,subdir=NULL,dirCreate=TRUE) {
  fn <- binaryFn(task,type,ext="pkl",subdir=subdir,dirCreate=dirCreate)
  reticulate::py_save_object(object,fn)
  invisible(fn)
}