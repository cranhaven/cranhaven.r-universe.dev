#' Get Joined Meetups
#'
#' @description This request is specific to an API key. Provides urlname used
#' in other functions.
#'
#' @param key an API key from https://www.meetup.com/meetup_api/
#' @param fields a charcter vector of the fields to return
#' @param ... a named list where each element is a character vector for additional
#' parameters e.g. `list("omit" = c("member.photo", "member.event_context")`
#'
#' @return returns a data.frame of the meetups associated to an API key
#' @export
#'
#' @examples
#' \dontrun{
#' get_joined_meetups("your_api_key")
#' }
#'
get_joined_meetups <- function(key,
                               fields = c("id", "name", "urlname", "link"),
                               ...) {

  dots <- list(...)
  .meetup_api_GET("self/groups", fields, key, dots)

}

#' Get Meetup Members
#'
#' @param urlname string, URL name for the meetup. e.g 'R-Users-Sydney'
#' @param key an API key from https://www.meetup.com/meetup_api/
#' @param fields a charcter vector of the fields to return
#' @param ... a named list where each element is a character vector for additional
#' parameters e.g. `list("omit" = c("member.photo", "member.event_context")`
#'
#' @return a data.frame of members in a meetup.
#' @export
#'
#' @examples
#' \dontrun{
#' get_meetup_members("R-Users-Sydney", "your_api_key")
#' }
get_meetup_members <- function(urlname,
                               key,
                               fields = c("id", "name"),
                               ...) {

  dots <- list(...)
  method <- paste0(urlname, "/members/")
  .meetup_api_GET(method, fields, key, dots = dots)

}

#' Get Meetup Events
#'
#' @description This function retrieves all meetup events for a meetup.
#' This is forced to be ordered in descending order and show both upcoming and
#' past events, therefore 'status' and 'desc' should not be passed as named
#' arguements to the `...` (dots) arguement.
#'
#' @param urlname string, URL name for the meetup. e.g 'R-Users-Sydney'
#' @param key an API key from https://www.meetup.com/meetup_api/
#' @param fields a charcter vector of the fields to return
#' @param ... a named list where each element is a character vector for additional
#' parameters e.g. `list("omit" = c("member.photo", "member.event_context")`
#'
#' @return data.frame of meetup events for a meetup.
#' @export
#'
#' @examples
#'\dontrun{
#' get_meetup_events("R-Users-Sydney", "your_api_key")
#'}
#'
get_meetup_events <- function(urlname,
                              key,
                              fields = c("status", "id", "name"),
                              ...) {

  dots <- list(...,
               "status" = c("upcoming", "past"),
               "desc" = "true")

  method <- paste0(urlname, "/events")
  .meetup_api_GET(method, fields, key, dots = dots)

}

