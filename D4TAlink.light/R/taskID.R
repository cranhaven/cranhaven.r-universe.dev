#' Get task identifier string.
#' @param sep the field separator character, default: "/".
#' @inheritParams D4TAlink-common-args
#' @return String with task ID as:[sponsor][sep][project][sep][package][sep][task]
#' @export
taskID <- function(task,sep="/")
	paste(task$sponsor,task$project,task$package,task$task,sep=sep)
