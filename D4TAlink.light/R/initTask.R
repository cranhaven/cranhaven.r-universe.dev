#' Initialize a task
#'
#' During the initialization:
#' \itemize{
#' \item The folder structure for the task is created
#' in the data repository.
#' \item The task properties are also
#' saved in rds and json format.
#' }
#' Please note that it is recommended
#' to load packages for your analysis
#' before initializing the task.
#' @inheritParams createTask
#' @param dirCreate logical, if TRUE (by default) the directory structure for the task is created in the repository.
#' @param overwrite logical, if TRUE and the task already exists, overwrite its parameters.
#' @param templateCreate create the prefilled Rmd template for the task, default value: FALSE.
#' @return \code{\link{D4TAlinkTask}} object
#' @importFrom utils packageVersion
#' @export
initTask <- function(project, package, taskname,
                     sponsor=getTaskSponsor(),
                     author=getTaskAuthor(),
                     dirCreate=TRUE,templateCreate=FALSE,
                     overwrite=FALSE) {
  task <- loadTask(project,package,taskname,sponsor,author,quiet=TRUE)
  if(overwrite||is.null(task)) task <- createTask(project,package,taskname,sponsor,author)
  if(dirCreate | templateCreate){
    for(path in getTaskPaths(task)) if(!dir.exists(path)){
      dir.create(path,showWarnings=FALSE,recursive=TRUE)
    }
  }
  if(templateCreate) initTaskRmd(task)
  if(file.exists(dirname(getTaskFilepath(task,"task","rds","bin")))) {
    saveBinary(task,task,"task")
    cat(jsonlite::toJSON(unclass(task)),file=file.path(binaryDir(task),paste0(task$task,"_task.json")))
  }
  invisible(task)
}
