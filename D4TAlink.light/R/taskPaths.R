## ---------------------------------------------------------------------
#' Get the paths of the task.
#' @inheritParams D4TAlink-common-args
#' @return List of task's paths.
#' @export
getTaskPaths <- function(task) {
  l <- getTaskStructure()(task$project,task$package,task$task,task$sponsor)
  lapply(l,gsub,pattern="%ROOT%",replacement=getTaskRoot(),fixed=TRUE)
}

## ---------------------------------------------------------------------
#' Get the path of a file.
#' @inheritParams D4TAlink-common-args
#' @param dirtype task directory where file is stored, i.e., 'documentation', 'code', 'data', 'data source' or 'binary data'.
#' @return Full path to file.
#' @export
getTaskFilepath <- function(task,type,ext,dirtype,subdir=NULL,dirCreate=TRUE) {
  if(missing(dirtype)) stop("Directory type is missing.")
  pz <- getTaskPaths(task)
  if(dirtype%in%names(pz)) { wdir <- dirtype
  } else {
    wdir <- list(documentation="doc",
                 code="code",
                 data="data",
                 `data source`="datasrc",
                 `binary data`="bin",
                 binary="bin")
  }
  if(!dirtype%in%names(pz)) stop("unknown directory type")
  path <- pz[[dirtype]]
  if (!is.null(subdir)) path <- file.path(path,subdir)
  if (dirCreate && !file.exists(path)) dir.create(path,showWarnings=FALSE,recursive=TRUE)
  filename <- paste0(task$task,"_",paste(type,collapse="-"),".",ext)
  path <- file.path(path, filename)
  return(path)
}

## =======================================================================
#' Get path of documentation file.
#' @inheritParams D4TAlink-common-args
#' @return File path.
#' @export
docFn <- function(task,type,ext,subdir=NULL,dirCreate=TRUE)
  getTaskFilepath(task,type,ext,subdir=subdir,dirCreate=dirCreate,dirtype="doc")
#' Get path of documentation directory.
#' @inheritParams D4TAlink-common-args
#' @return File path.
#' @export
docDir <- function(task,subdir=NULL,dirCreate=TRUE)
  dirname(docFn(task,type="NO",ext="NO",subdir=subdir,dirCreate=dirCreate))

## =======================================================================
#' Get path of output file.
#' @inheritParams D4TAlink-common-args
#' @return File path.
#' @export
reportFn <- function(task,type,ext,subdir=NULL,dirCreate=TRUE)
  getTaskFilepath(task,type,ext,subdir=subdir,dirCreate=dirCreate,dirtype="data")
#' Get path of report directory.
#' @inheritParams D4TAlink-common-args
#' @return File path.
#' @export
reportDir <- function(task,subdir=NULL,dirCreate=TRUE)
  dirname(reportFn(task,type="NO",ext="NO",subdir=subdir,dirCreate=dirCreate))
#' Get path of \code{xlsx} output file.
#' @inheritParams D4TAlink-common-args
#' @return File path.
#' @export
reportXlsFn <- function(task,type,ext="xlsx",subdir=NULL)
  reportFn(task,type,ext=ext,subdir=subdir,dirCreate=FALSE);
#' Get path of \code{jpeg} output file.
#' @inheritParams D4TAlink-common-args
#' @return File path.
#' @export
jpegReportFn <- function(task,type,ext="jpg",subdir=NULL)
  reportFn(task,type,ext,subdir=subdir,dirCreate=FALSE);
#' Get path of \code{pdf} output file.
#' @inheritParams D4TAlink-common-args
#' @return File path.
#' @export
pdfReportFn <- function(task,type,ext="pdf",subdir=NULL)
  reportFn(task,type,ext,subdir=subdir,dirCreate=FALSE);
#' Get path of \code{png} output file.
#' @inheritParams D4TAlink-common-args
#' @return File path.
#' @export
pngReportFn <- function(task,type,ext="png",subdir=NULL)
  reportFn(task,type,ext,subdir=subdir,dirCreate=FALSE);

## =======================================================================
#' Get path of binary file.
#' @inheritParams D4TAlink-common-args
#' @return File path.
#' @export
binaryFn <- function(task,type,ext="rds",subdir=NULL,dirCreate=TRUE)
  getTaskFilepath(task,type,ext,subdir=subdir,dirCreate=dirCreate,dirtype="bin")
#' Get path of binary directory.
#' @inheritParams D4TAlink-common-args
#' @return File path.
#' @export
binaryDir <- function(task,subdir=NULL,dirCreate=TRUE)
  dirname(binaryFn(task,type="NO",ext="NO",subdir=subdir,dirCreate=dirCreate))

## =======================================================================
#' Get path of data source file.
#' @inheritParams D4TAlink-common-args
#' @param filename name of the input file.
#' @return File path.
#' @export
datasourceFn <- function (task, filename, subdir = ".", dirCreate = TRUE) {
  if (is.null(subdir)) {
    te <- file.path(getTaskPaths(task)$datasrc, filename)
  } else {
    te <- file.path(getTaskPaths(task)$datasrc, subdir, filename)
  }
  if (dirCreate && !file.exists(dirname(te)))
    dir.create(dirname(te), showWarnings = FALSE, recursive = TRUE)
  te
}
#' Get path of data source directory.
#' @inheritParams D4TAlink-common-args
#' @return File path.
#' @export
datasourceDir <- function(task,subdir=NULL,dirCreate=TRUE)
  dirname(datasourceFn(task,"NOFILE",subdir=subdir,dirCreate=dirCreate))

## =======================================================================

## =======================================================================
#' Get path of scripts directory.
#' @inheritParams D4TAlink-common-args
#' @return File path.
#' @export
progDir <- function(task,subdir=NULL,dirCreate=TRUE)
  dirname(getTaskFilepath(task,type="NO",ext="NO",subdir=subdir,dirCreate=dirCreate,dirtype="code"))

## =======================================================================

## =======================================================================
#' Generic function.
#' @inheritParams D4TAlink-common-args
#' @return \code{NULL}.
#' @export
DTx <- function(sponsor=getTaskSponsor(),task=NULL) {
  setTaskSponsor(sponsor)
  invisible(NULL)
}
## =======================================================================

