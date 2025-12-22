#' Search for a Single Security
#'
#' REST path: `/securities`` (See <http://iss.moex.com/iss/reference/5>)
#'
#' @param query (A part of the) ID, name, ISIN, issuer ID, reg. number of a security.
#' @param ... Further arguments to [query_iss]
#'
#' @return A tibble with a list of matched securities.
#' @export
#'
#' @examples
#' \dontrun{
#' search_security(query = 'SBER')
#' }
search_security <- function(query, ...) {
    iss_response <- query_iss(
        rest_path = 'securities',
        params = list(q = query, iss.only = 'securities'),
        ...
    )

    iss_response$securities
}


#' Get a Security's Info
#'
#' REST path: `securities/[security]` (see <http://iss.moex.com/iss/reference/13>)
#'
#' @param secid A security ID.
#' @param ... Further arguments to [query_iss]
#'
#' @return A list with two tibbles:
#' - `description`: Full instrument profile incl. its type, listing level, etc.
#' - `boards`: The security's boards with `boardid`, `history_from`, ... columns.
#' @export
#'
#' @examples
#' \dontrun{
#' get_security_info(secid = 'SBER')
#' }
get_security_info <- function(secid, ...) {
    query_iss(
        rest_path = glue('securities/{secid}'),
        ...
    )
}