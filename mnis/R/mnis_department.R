
#' mnis_department
#'
#' Request the holders of posts in specific departments by department ID.
#'
#' @param department_id The department to look up. `0` returns the
#' cabinet/shadow cabinet, `-1` returns a list of all ministers.
#' @param bench Flag to return either Government or Opposition information.
#' This parameter is case insensitive, so both `'Opposition'` and
#' `'opposition'` will return the same data.
#' Defaults to `'Government'`.
#' @param former Logical parameter to include both current and
#' former ministers/shadow ministers. If `FALSE`, only includes
#' current ministers/shadow ministers. Defaults to `TRUE`.
#' @inheritParams mnis_basic_details
#' @return A tibble with information on departments and
#' ministers/shadow ministers.
#' @export
#' @examples
#' \dontrun{
#' x <- mnis_department(department_id = 0, bench = "Government", former = TRUE)
#' }
#'
mnis_department <- function(department_id = 0, bench = "Government",
                            former = TRUE, tidy = TRUE,
                            tidy_style = "snake_case") {
  if (former == TRUE) {
    query_former <- "former"
  } else {
    query_former <- "current"
  }

  query <- paste0(base_url, "Department/", as.character(department_id),
                  "/", utils::URLencode(bench), "/", query_former, "/")

  got <- mnis_query(query)

  x <- tibble::as_tibble(as.data.frame(got$Department$Posts))

  if (tidy == TRUE) {
    x <- mnis::mnis_tidy(x, tidy_style)
  }
  x
}
