## =================================================================
#' Create a task.
#' @inheritParams D4TAlink-common-args
#' @return An \code{\link{D4TAlinkTask}} object
#' @keywords internal
createTask <- function(project, package, taskname,
                       sponsor= getTaskSponsor(),
                       author = getTaskAuthor()) {
  # create paths
  #paths <- getTaskStructure()(project,package,taskname,sponsor)
  # retrieved dependencies loaded or attached
  pkgs <- intersect(loadedNamespaces(),rownames(utils::installed.packages()))
  pkgsVersion <- sapply(pkgs, function(x) as.character(packageVersion(x)))
  pkgsVersionStr <- paste(names(pkgsVersion), pkgsVersion, sep = ":")
  sessionStr <- paste(R.version.string, pkgsVersionStr, collapse = "; ")
  # create a task object
  task <- list(
    task = taskname,
    project = project,
    package = package,
    sponsor = sponsor,
    author = author,
    copyright = sprintf("Copyright (c) %s %s",sponsor,format(Sys.time(),"%Y")),
    date = format(Sys.time(),"%Y-%m-%d"),
    footer = sprintf("Copyright (c) %s %s - CONFIDENTIAL",sponsor,format(Sys.time(),"%Y")),
    version = "0.0",
    dependencies = sessionStr
    #paths = paths
  )
  class(task) <- c("D4TAlinkTask", class(task))
  return(task)
}

