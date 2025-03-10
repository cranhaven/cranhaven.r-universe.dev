#' Create an archive containing the files of a given task.
#' @param file full name of the output zip file
#' @param overwrite overwrite the output zip file is it exists
#' @inheritParams D4TAlink-common-args
#' @inheritParams utils::zip
#' @inheritDotParams utils::zip
#' @importFrom utils zip
#' @return the archive file name invisibly.
#' @export
archiveTask <- function(task,file,overwrite=FALSE,...) {
  if(file.exists(file)&&overwrite) unlink(file)
  else if(file.exists(file)&&!overwrite) stop(paste0("output file already exists, use argument 'ovewrite=TRUE'"))
  pz <- getTaskPaths(task)
  dir.create(zd<-tempfile())
  on.exit(unlink(zd,recursive=TRUE))
  ## =====
  for(ty in c("bin","data","log")) {
    dir.create(cd<-file.path(zd,ty))
    for(f in setdiff(list.files(pz[[ty ]],recursive=FALSE,full.names=TRUE,include.dirs=TRUE)
                     ,pz))
      file.copy(f,cd,recursive=TRUE)
  }
  ## =====
  for(ty in c("doc","code")) {
    dir.create(cd<-file.path(zd,ty))
    for(f in list.files(pz[[ty ]],recursive=FALSE,full.names=TRUE,
                        pattern=sprintf("^%s[._-]",task$task))) if(!grepl("[#~]$",f)) {
      file.copy(f,cd,recursive=TRUE,copy.date=TRUE)
    }
    ## backward compatibility
    if((ty=="doc")&&
       file.exists(file.path(pz[[ty ]],"R")))
      for(f in list.files(file.path(pz[[ty ]],"R"),recursive=FALSE,full.names=TRUE,
                          pattern=sprintf("^%s[-._]",task$task))) if(!grepl("[#~]$",f)) {
        file.copy(f,cd,recursive=TRUE,copy.date=TRUE)
      }
  }
  ## =====
  owd <- getwd()
  on.exit({setwd(owd);unlink(zd,recursive=TRUE)})
  setwd(zd)
  fl <- list.files(".",recursive=TRUE,full.names=TRUE)
  utils::zip(file,fl,extra=c(sprintf("-b %s",shQuote(tempdir()))),...)
  unlink(zd,recursive=TRUE)
  invisible(file)
}

#' Restore an archive containing the files of a given task from a file created with archiveTask.
#' @param file full name of the input zip file
#' @param code restore code, default TRUE
#' @inheritParams D4TAlink-common-args
#' @inheritParams utils::unzip
#' @inheritDotParams utils::unzip
#' @importFrom utils unzip
#' @return if list FALSE, the task imported incisibly, otherwise the list of files in the archive.
#' @export
restoreTask <- function(file,overwrite=FALSE,list=FALSE,code=TRUE,...) {
  dir.create(zd<-tempfile())
  on.exit(unlink(zd,recursive=TRUE))
  ls <- utils::unzip(file,list=list,exdir=zd,...)
  ## ======
  #owd <- getwd()
  #on.exit(setwd(owd))
  #setwd(zd)
  ## ======
  tf <- list.files(file.path(zd,"bin"),"_task.rds",full.names=TRUE)
  if(length(tf)!=1) stop("cannot locate task in zip file")
  task <- readRDS(tf)
  if(!inherits(task,"D4TAlinkTask")) stop("the zip file does not contains a task")
  if(is.null(task$sponsor)) task$sponsor <- task$company
  #task$paths <- getTaskStructure()(project=task$project,package=task$package,taskname=task$task,sponsor=task$sponsor)
  ## ======
  pz <- getTaskPaths(task)
  for(p in unlist(pz)) dir.create(p,recursive=TRUE,showWarnings=FALSE)
  ## ======
  for(n in list.files(zd,recursive=FALSE,full.names=TRUE,include.dirs=TRUE)) {
    ty <- basename(n)
    if((ty=="code")&&!code) {} else {
      if(!ty%in%names(pz)) stop("unknown type")
      for(f in list.files(n,recursive=FALSE,full.names=TRUE,include.dirs=TRUE)) {
        if(!overwrite&&file.exists(file.path(pz[[ty]],basename(f)))) warning(paste0("file '",basename(f),"' already exists and won't be overwritten, please use argument 'overwrite'"))
        file.copy(f,pz[[ty]],recursive=TRUE,copy.date=TRUE,overwrite=overwrite)
      }
    }
  }
  ## ======
  saveBinary(task,task,"task")
  cat(jsonlite::toJSON(unclass(task)),file=file.path(binaryDir(task),paste0(task$task,"_task.json")))
  ## ======
  if(list) invisible(ls)
  invisible(task)
}

