#' List details of all tasks stored in the root directory.
#'
#' @inheritParams D4TAlink-common-args
#' @return \code{\link{data.frame}} with the following information for tasks "sponsor", "project", "package", "task".
#' @importFrom stats setNames
#' @export
listTasks <- function(project=NULL,package=NULL,sponsor=NULL,rootpath=getTaskRoot()) {
  fns <- ""
  pa <- NULL
  u <- gsub("%ROOT%",rootpath,fixed=TRUE,
            getTaskStructure()("%PROJECT%","%PACKAGE%","%TASK%",sponsor="%SPONSOR%")$bin)
  if(!is.null(project)) u <- gsub("%PROJECT%",project,u)
  if(!is.null(package)) u <- gsub("%PACKAGE%",package,u)
  if(!is.null(sponsor)) u <- gsub("%SPONSOR%",package,u)
  u <- unlist(strsplit(u,"%[A-Z]+%"))
  for(i in 1:length(u)) {
    d <- u[[i]]
    if(i==length(u)) pa <- "task[.]json$"
    fnn <- NULL
    for(r in paste0(fns,d)) {
      fnn <- unique(c(fnn,list.files(r,pa,full.names=TRUE)))
    }
    fns <- fnn
  }
  if(length(fns)==0) stop("no task found")
  taz <- stats::setNames(lapply(fns,function(fn)jsonlite::fromJSON(readChar(fn,file.size(fn)))),
                  fns)
  ans <- data.frame(n=fns,
                    sponsor=NA,project=NA,package=NA,task=NA,author=NA,date=NA,
                    row.names = fns)
  for(n in rownames(ans)) {
    for(f in c("sponsor", "project", "package", "task","author","date"))
      if(!is.null(taz[[n]][[f]]))
        ans[n,f] <- taz[[n]][[f]]
    f <- "company"
    if(!is.null(taz[[n]][[f]]))
      ans[n,"sponsor"] <- taz[[n]][[f]]
  }
  ans$n <- NULL
  rownames(ans) <- NULL
  ans
}


#' List the files associated to a task.
##' @inheritParams base::list.files
#' @inheritParams D4TAlink-common-args
#' @param which list of file types to list.
#' @return array of file names.
#' @export
listTaskFiles <- function(task,full.names=FALSE,which=NULL) {
  pz <- getTaskPaths(task)
  fns <- list()
  if(is.null(which)) which <- c("bin","data","code","doc")
  for(ty in intersect(c("bin","data"),which)) {
    fns[[ty]] <- fz <- setdiff(list.files(pz[[ty ]],recursive=FALSE,full.names=TRUE,include.dirs=TRUE),pz)
    if(!full.names) fns[[ty]] <- basename(fns[[ty]])
    for(f in fz)
      fns[[ty]] <- c(fns[[ty]],list.files(f,recursive=TRUE,full.names=full.names))
  }
  ## =====
  for(ty in intersect(c("doc","code"),which)) {
    fns[[ty]] <- list.files(pz[[ty ]],recursive=TRUE,full.names=full.names,
                            pattern=sprintf("^%s[._-].*[^#~]$",task$task))
  }
  ## =====
  fns
}

