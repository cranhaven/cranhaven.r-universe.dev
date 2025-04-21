

#' Political and Geographical Interests
#'
#' Returns a tibble with members with the specified interest or interests.
#'
#' @param interest A single interest in the form of a string, or a character
#' vector of interests. If a vector of multiple interests, returns all members
#' who list all of those interests.
#' @param house The house the member belongs to. Accepts 'commons', 'lords'
#' and `NULL`. This variable is not case sensitive. If `NULL`,
#' returns members from both houses. Defaults to `NULL`.
#' @param current Logical. If `TRUE`, returns all current and
#' former members of both houses. Defaults to `TRUE`.
#' @inheritParams mnis_basic_details
#'
#' @return A data frame of members with given interest(s).
#' @export
#'
#' @examples
#' \dontrun{
#' x <- mnis_political_interests(c("Africa", "Jamaica"))
#'
#' y <- mnis_political_interests(c("Africa", "Europe"))
#'
#' z <- mnis_political_interests(c("Africa", "Education"))
#' }
#'
mnis_political_interests <- function(interest, house = NULL, current = TRUE,
                                     tidy = TRUE, tidy_style = "snake_case") {

  if ( is.character(house)) {
    house_query <- paste0("House=", tolower(house), "|")
  } else {
    house_query <-   ""
  }

  if (!current) {
    current_query <-  "membership=all|"
  } else {
    current_query <-   ""
  }

  interest <- as.character(interest)

  # if (length(interest) > 0) { # deals with multiple interests
  #
  #   for (i in 1:length(interest)) {
  #     interest[i] <- paste0("biographyinterest=", interest[i], "|")
  #   }
  # }

  interest_query <- paste0(interest, collapse = "|")

  query <- paste0(base_url, "members/query/", interest_query, house_query)

  got <- mnis_query(query)

  if (length(got$Members$Member$`@Member_Id`) <= 1) { ## fails

    df <- tibble::as_tibble(t(unlist(got$Members$Member)))
  } else {
    df <- tibble::as_tibble(got$Members$Member)
  }

  df <- mnis_tidy(df, tidy_style)

  df
}
