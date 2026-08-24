#' Construct the URL for a Zotero export call
#'
#' This function is just a convenience function to create a simple
#' URL to download references from a public Zotero group. See
#' https://www.zotero.org/support/dev/web_api/v3/start
#' for details.
#'
#' @param group The group ID
#' @param sort On which field to sort
#' @param direction The direction to sort in
#' @param format The format to export
#' @param start The index of the first record to return
#' @param limit The number of records to return
#'
#' @return The URL in a character vector.
#' @export
#'
#' @examples zotero_construct_export_call(2425237);
zotero_construct_export_call <- function(group,
                                         sort = "dateAdded",
                                         direction = "asc",
                                         format = "bibtex",
                                         start=0,
                                         limit=100) {
  return(
    paste0(
      "https://api.zotero.org/groups/", group,
      "/items?sort=", sort,
      "&direction=", direction,
      "&format=", format,
      "&start=", start,
      "&limit=", limit
    )
  );
}
