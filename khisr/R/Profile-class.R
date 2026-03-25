#' Create a Profile
#'
#' Constructor function for objects of class [Profile].
#'
#' @param id The DHIS2 user profile ID.
#' @param username The DHIS2 user username.
#' @param email The DHIS2 user's email.
#' @param phone_number The DHIS2 user's phone number.
#' @param display_name The DHIS2 user's display name.
#' @param first_name The DHIS2 user's first name.
#' @param last_name The DHIS2 user's last name.
#'
#' @return An object of class [Profile]
#' @noRd

init_Profile <- function(id = NULL,
                         username = NULL,
                         email = NULL,
                         phone_number = NULL,
                         display_name = NULL,
                         first_name = NULL,
                         last_name = NULL) {

    Profile$new(
        id = id,
        username = username,
        email = email,
        phone_number = phone_number,
        display_name = display_name,
        first_name = first_name,
        last_name = last_name
    )

}

#' Create a Profile
#'
#' Constructor function for objects of class [Profile].
#'
#' @param id The DHIS2 user profile ID.
#' @param username The DHIS2 user username.
#' @param email The DHIS2 user's email.
#' @param phone_number The DHIS2 user's phone number.
#' @param display_name The DHIS2 user's display name.
#' @param first_name The DHIS2 user's first name.
#' @param last_name The DHIS2 user's last name.
#'
#' @return An object of class [Profile]
#' @noRd

Profile <- R6::R6Class('Profile', list(
    #' @field id DHIS2 user id
    id = NULL,
    #' @field username DHIS2 user's username
    username = NULL,
    #' @field email DHIS2 user's email
    email = NULL,
    #' @field phone_number DHIS2 user's phone number
    phone_number = NULL,
    #' @field display_name DHIS2 user's display name
    display_name = NULL,
    #' @field first_name DHIS2 user's first name
    first_name = NULL,
    #' @field last_name DHIS2 user's last name
    last_name = NULL,

    #' @description Create a new Profile
    initialize = function(id = NULL,
                          username = NULL,
                          email = NULL,
                          phone_number = NULL,
                          display_name = NULL,
                          first_name = NULL,
                          last_name = NULL) {

        # stopifnot(
        #     is.null(id) || is_scalar_character(id),
        #     is.null(username) || is_scalar_character(username),
        #     is.null(email) || is_scalar_character(email),
        #     is.null(phone_number) || is_scalar_character(phone_number),
        #     is.null(display_name) || is_scalar_character(display_name),
        #     is.null(first_name) || is_scalar_character(first_name),
        #     is.null(last_name) || is_scalar_character(last_name),
        # )

        self$id = id
        self$username = username
        self$email = email
        self$phone_number = phone_number
        self$display_name = display_name
        self$first_name = first_name
        self$last_name = last_name

        self
    },
    #' @description Get the ID
    get_id = function() {
        self$id
    },
    #' @description Get the username
    get_username = function() {
        self$username
    },
    #' @description Get the email
    get_email = function() {
        self$email
    },
    #' @description Get the phone number
    get_phone_number = function() {
        self$phone_number
    },
    #' @description Get the display name
    get_display_name = function() {
        self$display_name
    },
    #' @description Get the first name
    get_first_name = function() {
        self$first_name
    },
    #' @description Get the last name
    get_last_name = function() {
        self$last_name
    },
    #' @description Clear the profile info
    clear = function() {
        self$id <- NULL
        self$username <- NULL
        self$email <- NULL
        self$phone_number <- NULL
        self$display_name <- NULL
        self$first_name <- NULL
        self$last_name <- NULL
    }
))
