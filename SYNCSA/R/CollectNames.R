#' @title Collect names an entire list
#'
#' @description Internal function to collect names an entire list.
#'
#' @encoding UTF-8
#' @param l A list.
#' @param prefix A prefix to nomes.
#' @return The names.
#' @keywords SYNCSA
#' @export
CollectNames <- function(l, prefix = NULL) {
  if (!is.list(l)) return(NULL)
  if (is.data.frame(l)) return(NULL)
  names <- Map(paste, names(l), lapply(l, CollectNames), sep = "$")
  names <- gsub("\\$$", "", unlist(names, use.names = FALSE))
  names <- paste(prefix, names, sep = "")
  return(names)
}
