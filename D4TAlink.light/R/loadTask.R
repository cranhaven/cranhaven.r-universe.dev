#' Load a task.
#'
#' @inheritParams D4TAlink-common-args
#' @inheritParams readBinary
#' @return Object of class \code{\link{D4TAlinkTask}} or NULL if the \code{task} does not exists.
#' @export
loadTask <- function(project, package, taskname,
                     sponsor=getTaskSponsor(),
                     author=getTaskAuthor(),
                     quiet=FALSE) {
  tabk <- createTask(project,package,taskname,sponsor,author)
  ta <- readBinary(task=tabk,type="task",quiet=TRUE)
  if(is.null(ta)) {
    fn <- file.path(binaryDir(tabk),paste0(tabk$task,"_task.json"))
    if(file.exists(fn)) {
      v <- jsonlite::read_json(fn)
      ta <- tabk
      for(n in names(v)) ta[[n]] <- unlist(v[[n]])
    }
  }
  if(!quiet&&is.null(ta)) message("Task does not exist.")
  if(!is.null(ta)) ta$paths <- NULL # Back-compatibility 
  ta
}

