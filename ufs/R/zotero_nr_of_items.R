#' Get number of items in a public Zotero group
#'
#' @param group The group ID
#'
#' @return The umber of items as a numeric vector.
#' @export
#'
#' @examples zotero_nr_of_items(2425237);
zotero_nr_of_items <- function(group) {
  bibCon <- url(paste0("https://api.zotero.org/groups/", group, "/items?format=keys"));
  keys<-readLines(bibCon, warn=FALSE);
  close(bibCon);
  return(length(keys));
}
