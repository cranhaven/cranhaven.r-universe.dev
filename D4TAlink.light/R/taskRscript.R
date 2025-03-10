## =======================================================================
#' Get path of R script file name.
#' @inheritParams D4TAlink-common-args
#' @return File path.
#' @export
rscriptFn <- function(task,suffix=NA) {
  if(is.na(suffix)) return(file.path(getTaskPaths(task)[["code"]],paste0(task$task,".R")))
  else return(file.path(getTaskPaths(task)[["code"]],paste0(task$task,".",suffix,".R")))
}

## =======================================================================
#' Create task R script.
#' @param overwrite overwrite R file if exists, default FALSE
#' @inheritParams D4TAlink-common-args
#' @inheritParams base::readLines
#' @return the file name invisibly.
#' @export
initTaskRscript <- function(task,overwrite=FALSE,encoding="unknown",suffix=NA) {
  fn <- rscriptFn(task,suffix=suffix)
  if(file.exists(fn)&&!overwrite) stop("The task R script file already exists. Set 'overwrite' to FALSE to overwrite the existing file.")
  tfn <- getTaskRscriptTemplate()
  tin <- readLines(tfn,encoding=encoding,warn=FALSE)
  date <- format(as.Date(gsub("(^[^_]*)_.*", "\\1", task$task),"%Y%m%d"),"%Y-%m-%d")
  tin <- gsub("%DATE%"    ,date,tin,fixed=TRUE)
  tin <- gsub("%TASKID%",taskID(task),tin,fixed=TRUE)
  u <- unlist(task)
  for(n in names(u)) {
    tin <- gsub(sprintf("%%%s%%",toupper(n)),u[[n]],tin,fixed=TRUE)
  }
  writeLines(enc2utf8(tin),fn,useBytes=TRUE)
  invisible(fn)
}

