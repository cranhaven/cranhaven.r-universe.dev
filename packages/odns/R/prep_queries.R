#' Prepare an API query without SQL.
#'
#' @param resource A character string specifying resource id of the data set
#'  to be returned.
#' @param fields A character vector specifying the names of fields to be
#'  included in the returned data.
#' @param limit A numeric value specifying the maximum number of rows to be
#' returned.
#' @param offset A numeric value specifying the number of rows to skip.
#'  
#' @return A character string containing the prepared query.
prep_nosql_query <- function(resource, fields, limit, offset) {
  
  stopifnot("resource should be a character string on length 1" = length(resource) == 1)
  
  f = glue::glue("&fields={paste0(fields, collapse = \",\")}")
  l = glue::glue("&limit={paste0(limit, collapse = \",\")}")
  o = glue::glue("&offset={paste0(offset, collapse = \",\")}")
  
  query <- utils::URLencode(
    glue::glue(
      "https://www.opendata.nhs.scot/api/3/action/datastore_search?",
      "id={resource}",
      "{if (!is.null(fields)) f else \"\"}",
      "{if (!is.null(limit)) l else \"\"}",
      "{if (!is.null(offset)) o else \"\"}",
      "&sort=_id"
    ))
  
  cap_url(query)
  
  return(query)
}

#' Prepare an API query with SQL.
#'
#' @param resource a character string specifying resource id of the data set
#'  to be returned.
#' @param fields a character vector specifying the names of fields to be
#'  included in the returned data.
#' @param limit A numeric value specifying the maximum number of rows to be
#' returned.
#' @param offset A numeric value specifying the number of rows to skip.
#' @param where A character string containing the 'WHERE' element of a simple
#'  SQL SELECT style query. Field names must be double quoted (\code{"}), non 
#'  numeric values must be single quoted (\code{"}), and both single and double
#'  quotes must be delimited. Example; \code{where = "\"AgeGroup\" = 
#'  \'45-49 years\\'"}.
#'
#' @return A character string containing the prepared query.
prep_sql_query <- function(resource, fields, limit, offset, where) {
  
  stopifnot("resource should be a character string on length 1" = length(resource) == 1)
  
  f = paste0("\"", fields, "\"", collapse = ",")
  l = paste0(" LIMIT ", limit)
  o = paste0(" OFFSET ", offset)
  
  query <- utils::URLencode(
    glue::glue(
      "https://www.opendata.nhs.scot/api/3/action/datastore_search_sql?sql=",
      "SELECT {if (is.null(fields)) \"*\" else f}",
      " FROM \"{resource}\" ",
      "{if (is.null(where)) \"\" else glue::glue(\"WHERE {where}\")}",
      " ORDER BY \"_id\" ASC ",
      "{if (is.null(limit)) \"\" else l}",
      "{if (is.null(offset)) \"\" else o}"
    ))
  
  cap_url(query)
  
  return(query)
}
