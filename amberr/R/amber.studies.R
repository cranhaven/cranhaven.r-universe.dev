#' Get the studies
#'
#' @title Get the studies
#' @family studies functions
#' @param amber An Amber object
#' @param query The search query
#' @param skip Number of items to skip
#' @param limit Max number of items
#' @param df Return a data.frame (default is TRUE)
#' @return A data.frame (or a named list of raw results when 'df' is FALSE)
#' @examples
#' \dontrun{
#' a <- amber.login("https://amber-demo.obiba.org")
#' amber.studies(a, skip=0, limit=10)
#' amber.logout(a)
#' }
#' @export
#' @import dplyr
amber.studies <-
  function(amber,
           query = list(),
           skip = 0,
           limit = 100,
           df = TRUE) {
    query$`$skip` <- skip
    query$`$limit` <- limit
    res <- .get(amber, "study", query = query)
    .reportListMetrics(res)

    if (df) {
      vals <- lapply(res$data, function(val) {
        list(
          `_id` = val$`_id`,
          name = val$name,
          description = val$description,
          forms = paste(val$forms, collapse = "|"),
          createdBy = val$createdBy,
          createdAt = val$createdAt,
          updatedAt = val$updatedAt
        )
      })
      dplyr::bind_rows(vals)
    } else {
      res
    }
  }

#' Get a study by name or identifier.
#'
#' @title Get a study
#' @family studies functions
#' @param amber An Amber object
#' @param id Study's name or identifier
#' @return A study object as a named list
#' @examples
#' \dontrun{
#' a <- amber.login("https://amber-demo.obiba.org")
#' amber.study(a, id = "Trauma Registry")
#' amber.study(a, id = "6151b5234268f582926d37f44")
#' amber.logout(a)
#' }
#' @export
amber.study <- function(amber, id) {
  query <- list()
  if (regexpr("^[a-zA-Z]+", id) == 1) {
    query$name <- id
  } else {
    query$`_id` <- id
  }
  res <- .get(amber, "study", query = query)
  if (length(res$data) > 0) {
    if (length(res$data) > 1)
      warning(
        "There are more than one study matching the criteria",
        immediate. = TRUE,
        call. = FALSE
      )
    res$data[[1]]
  } else {
    NULL
  }
}
