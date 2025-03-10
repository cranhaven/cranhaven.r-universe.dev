#' Arguments used across the functions of the D4TAlink package.

#' @param project Project name.
#' @param package Package name.
#' @param taskname Task name.
#' @param author Author name, system username by default.
#' @param sponsor Sponsor name, default set by \code{\link{setTaskSponsor}}.

#' @param rootpath Path of the task repository, default set by \code{\link{setTaskRoot}}.

#' @param task Object of class \code{\link{D4TAlinkTask}}, as created by \code{\link{initTask}}.
#' @param type Character vector with type of object. If multiple are specified, these are combined with '-'.

#' @param type Filename type. If the type is an array, the cocatenation of the elements is used with separator"-". Filenames have the form [task name]_[type].[ext]
#' @param ext Filename extension.
#' @param suffix Filename suffix, used to develop scripts for sub-analyses for a given task, default NA.
#' @param dirType Directory type, e.g. 'bin' or 'data' or 'doc'.

#' @param subdir (optional) Subdirectory.
#' @param dirCreate Logical, if TRUE (by default) the directory is created.

#' @param pathgen optional function returning a list of paths, currently \code{\link{pathsGLPG}} or \code{\link{pathsPMS}}.

#' @name D4TAlink-common-args

#' @return No return value, used for the documentation of
#' the functions of the package.
NULL
