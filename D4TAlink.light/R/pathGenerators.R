## =================================================================
#' Task paths generator.
#'
#' The paths are:
#' datasrc: [ROOT]/[sponsor]/[project]/[package]/raw/datasource
#' data:    [ROOT]/[sponsor]/[project]/[package]/output/[taskname]
#' bin:     [ROOT]/[sponsor]/[project]/[package]/output/[taskname]/bin
#' code:    [ROOT]/[sponsor]/[project]/[package]/progs
#' doc:     [ROOT]/[sponsor]/[project]/[package]/docs
#' log:     [ROOT]/[sponsor]/[project]/[package]/output/log
#' @inheritParams D4TAlink-common-args
#' @return a list of file paths
#' @export
pathsDefault <- function(project,package,taskname,sponsor) {
  basePath <- file.path("%ROOT%",sponsor,project,package)
  list(
    root = "%ROOT%",
    datasrc = file.path(basePath, "raw", "data_source"),
    data = file.path(basePath, "output",taskname),
    bin  = file.path(basePath, "output",taskname,"bin"),
    code = file.path(basePath, "progs"),
    doc  = file.path(basePath, "docs"),
    log  = file.path(basePath, "output","log")
  )
}

## =================================================================
#' Task paths generator.
#'
#' The paths are:
#' datasrc: [ROOT]/[sponsor]/[project]/[package]/raw/datasource
#' data:    [ROOT]/[sponsor]/[project]/[package]/output/adhoc/[taskname]
#' bin:     [ROOT]/[sponsor]/[project]/[package]/output/adhoc/[taskname]/bin
#' code:    [ROOT]/[sponsor]/[project]/[package]/progs
#' doc:     [ROOT]/[sponsor]/[project]/[package]/docs
#' log:     [ROOT]/[sponsor]/[project]/[package]/output/log
#' @inheritParams D4TAlink-common-args
#' @return a list of file paths
#' @export
pathsGLPG <- function(project,package,taskname,sponsor) {
  basePath <- file.path("%ROOT%",sponsor,project,package)
  list(
    root = "%ROOT%",
    datasrc = file.path(basePath, "raw", "data_source"),
    data = file.path(basePath, "output","adhoc",taskname),
    bin  = file.path(basePath, "output","adhoc",taskname,"bin"),
    code = file.path(basePath, "progs"),
    doc  = file.path(basePath, "docs"),
    log  = file.path(basePath, "output","log")
  )
}

## =================================================================
#' Task paths generator.
#'
#' The paths are:
#' datasrc: [ROOT]/[sponsor]/PMS_data/[project]/[package]/datasource
#' data:    [ROOT]/[sponsor]/PMS_data/[project]/[package]/[taskname]
#' bin:     [ROOT]/[sponsor]/PMS_data/[project]/[package]/[taskname]/bin
#' code:    [ROOT]/[sponsor]/PMS_code/[project]/[package]/R
#' doc:     [ROOT]/[sponsor]/PMS_documentation/[project]/[package]/[taskname]
#' log:     [ROOT]/[sponsor]/PMS_data/[project]/[package]/[taskname]/log
#' @inheritParams D4TAlink-common-args
#' @return a list of file paths
#' @export
pathsPMS <- function(project,package,taskname,sponsor) {
  basePath <- file.path("%ROOT%","%TYPE%",project,package)
  list(
    root = "%ROOT%",
    datasrc = file.path(gsub("%TYPE%","PMS_data",basePath),"data_source"),
    data = file.path(gsub("%TYPE%","PMS_data",basePath),taskname),
    bin  = file.path(gsub("%TYPE%","PMS_data",basePath),taskname,"bin"),
    code = file.path(gsub("%TYPE%","PMS_code",basePath), "R"),
    doc  = file.path(gsub("%TYPE%","PMS_documentation",basePath),taskname),
    log  = file.path(gsub("%TYPE%","PMS_data",basePath),taskname,"log")
  )
}

