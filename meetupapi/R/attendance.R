#' Get RSVPs for a meetup event
#'
#' @description Can be used after calling the `get_joined_meetups()` function
#' and getting a urlname, and calling `get_meetup_events()` for an event_id
#'
#' @param urlname string, URL name for the meetup. e.g 'R-Users-Sydney'.
#' @param event_id string, event id for the meetup event.
#' @param key an API key from https://www.meetup.com/meetup_api/.
#' @param fields a charcter vector of the fields to return.
#' @param ... a named list where each element is a character vector for additional
#' parameters e.g. `list("omit" = c("member.photo", "member.event_context")`.
#'
#' @return data.frame of event RSVPs.
#' @export
#'
#' @examples
#' \dontrun{
#' get_event_rsvps("R-Users-Sydney", "your_event_id", "your_api_key")
#' }
#'
get_event_rsvps <- function(urlname,
                            event_id,
                            key,
                            fields = c("response", "member"),
                            ...) {

  dots <- list(...)
  if (length(dots) == 0) {
    dots <- list("omit" = c("member.photo", "member.event_context"))
  }

  method <- paste0(urlname, "/events/", event_id, "/rsvps")
  .meetup_api_GET(method, fields, key, dots = dots, only_first = T)

}

#' Get attendance for a past event
#'
#' @param urlname string, URL name for the meetup. e.g 'R-Users-Sydney'.
#' @param event_id string, event id for the meetup event.
#' @param key an API key from https://www.meetup.com/meetup_api/.
#' @param fields a charcter vector of the fields to return.
#' @param ... a named list where each element is a character vector for additional
#' parameters e.g. `list("omit" = c("member.photo", "member.event_context")`.
#'
#' @return data.frame of attendance for a past meetup event.
#' @export
#'
#' @examples
#' \dontrun{
#' get_event_attendance("R-Users-Sydney", "your_event_id", "your_api_key")
#' }
#'
get_event_attendance <- function(urlname,
                                 event_id,
                                 key,
                                 fields = c("status", "member", "rsvp.response"),
                                 ...) {

  dots <- list(...)
  if (length(dots) == 0) {
    dots <- list("filter" = "all")
  }

  method <- paste0(urlname, "/events/", event_id, "/attendance")
  .meetup_api_GET(method, fields, key, dots = dots)

}

#' Mark Event Attendance
#'
#' @param urlname string, URL name for the meetup. e.g 'R-Users-Sydney'
#' @param event_id string, event id for the meetup event.
#' @param member_ids vector of member id strings to mark attendance for.
#' @param member_status vector string for member status, positions match those in
#' member_id's.
#' @param key an API key from https://www.meetup.com/meetup_api/.
#'
#' @return the POST request object.
#' @export
#'
#' @examples
#' \dontrun{
#' mark_event_attendance("R-Users-Sydney", "your_event_id",
#'                       "my_member_id", "attended",
#'                       "your_api_key")
#' }
#'
mark_event_attendance <- function(urlname,
                                  event_id,
                                  member_ids,
                                  member_status,
                                  key) {

  # check that member status is length of id's
  if (length(member_status) != length(member_ids)) {
    stop("'member_status' must be the same length as 'member_ids'")
  }
  # check that member_status has only valid values
  chk_valid <- all(member_status %in% c("noshow", "abset", "attended"))
  if (!chk_valid) {
    stop("'member_status' contains invalid values")
  }

  # contstruct request
  method <- paste0(urlname, "/events/", event_id, "/attendance")
  req <- .construct_req(method, "", key, list("member" = member_ids,
                                              "status" = member_status))
  # send request
  httr::POST(req)

}
