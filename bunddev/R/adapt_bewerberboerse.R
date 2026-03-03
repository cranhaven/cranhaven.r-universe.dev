#' Search the Bewerberboerse API
#'
#' @param params List of query parameters.
#' @param flatten Logical; drop nested list columns.
#' @param flatten_mode Flatten strategy for list columns. Use "unnest" to
#'   expand list-columns into multiple rows.
#'
#' @return A tibble containing candidate listings with columns including
#'   reference number, availability, location, skills, and contact flags. When
#'   `flatten = FALSE`, includes nested list columns for education and
#'   experience. Metadata columns include `page`, `size`, and `max_ergebnisse`.
#'
#' @details
#' The Bewerberboerse API provides access to candidate listings. Authentication
#' uses an API key passed as `X-API-Key` (clientId `jobboerse-bewerbersuche-ui`).
#' See https://bundesapi.github.io/bewerberboerse-api/ for official docs.
#'
#' Use [bunddev_auth_set()] to configure the key and [bunddev_parameters()] to
#' inspect available query parameters.
#'
#' @seealso
#' [bewerberboerse_details()] for detailed entries and [bunddev_auth_set()] for
#' authentication setup.
#'
#' @examples
#' \dontrun{
#' Sys.setenv(BEWERBERBOERSE_API_KEY = "jobboerse-bewerbersuche-ui")
#' bunddev_auth_set("bewerberboerse", type = "api_key", env_var = "BEWERBERBOERSE_API_KEY")
#' bewerberboerse_search(params = list(was = "data", size = 10), flatten = TRUE)
#' }
#' @export
bewerberboerse_search <- function(params = list(),
                                  flatten = FALSE,
                                  flatten_mode = "json") {
  bunddev_call_tidy(
    "bewerberboerse",
    "bewerberboerse",
    params = params,
    flatten = flatten,
    flatten_mode = flatten_mode
  )
}

#' Retrieve Bewerberboerse candidate details
#'
#' @param referenznummer Bewerber referenznummer.
#' @param flatten Logical; drop nested list columns.
#' @param flatten_mode Flatten strategy for list columns. Use "unnest" to
#'   expand list-columns into multiple rows.
#'
#' @return A tibble containing detailed information for a single candidate,
#'   including personal details, skills, work history, education, and contact
#'   preferences. Structure is similar to [bewerberboerse_search()] results.
#'
#' @details
#' Fetches details for a single candidate. The `referenznummer` typically comes
#' from [bewerberboerse_search()]. See https://bundesapi.github.io/bewerberboerse-api/.
#'
#' @seealso
#' [bewerberboerse_search()] to find candidates and [bunddev_auth_set()] for auth.
#'
#' @examples
#' \dontrun{
#' Sys.setenv(BEWERBERBOERSE_API_KEY = "jobboerse-bewerbersuche-ui")
#' bunddev_auth_set("bewerberboerse", type = "api_key", env_var = "BEWERBERBOERSE_API_KEY")
#' bewerberboerse_details("12345", flatten = TRUE)
#' }
#' @export
bewerberboerse_details <- function(referenznummer,
                                   flatten = FALSE,
                                   flatten_mode = "json") {
  bunddev_call_tidy(
    "bewerberboerse",
    "bewerberdetails",
    params = list(referenznummer = referenznummer),
    flatten = flatten,
    flatten_mode = flatten_mode
  )
}

bunddev_bewerberboerse_search <- function(params = list(),
                                          flatten = FALSE,
                                          flatten_mode = "json") {
  bewerberboerse_search(params = params, flatten = flatten, flatten_mode = flatten_mode)
}

bunddev_bewerberboerse_details <- function(referenznummer,
                                           flatten = FALSE,
                                           flatten_mode = "json") {
  bewerberboerse_details(
    referenznummer,
    flatten = flatten,
    flatten_mode = flatten_mode
  )
}

bunddev_tidy_bewerberboerse <- function(response, operation_id = NULL,
                                        flatten = FALSE, flatten_mode = "json") {
  bewerber <- response$bewerber
  if (is.null(bewerber) || length(bewerber) == 0) {
    return(tibble::tibble())
  }

  chr_or_na <- function(value) {
    if (is.null(value)) NA_character_ else as.character(value)
  }
  lgl_or_na <- function(value) {
    if (is.null(value)) NA else isTRUE(value)
  }
  collapse_or_na <- function(value) {
    if (is.null(value)) NA_character_ else paste(value, collapse = ", ")
  }
  list_or_empty <- function(value) {
    if (is.null(value)) list() else value
  }

  meta_page <- if (is.null(response$page)) NA_integer_ else as.integer(response$page)
  meta_size <- if (is.null(response$size)) NA_integer_ else as.integer(response$size)
  meta_total <- if (is.null(response$maxErgebnisse)) NA_integer_ else as.integer(response$maxErgebnisse)

  data <- tibble::tibble(
    refnr = purrr::map_chr(bewerber, ~ chr_or_na(.x$refnr)),
    verfuegbarkeit_von = purrr::map_chr(bewerber, ~ chr_or_na(.x$verfuegbarkeitVon)),
    aktualisierungsdatum = purrr::map_chr(bewerber, ~ chr_or_na(.x$aktualisierungsdatum)),
    veroeffentlichungsdatum = purrr::map_chr(bewerber, ~ chr_or_na(.x$veroeffentlichungsdatum)),
    stellenart = purrr::map_chr(bewerber, ~ chr_or_na(.x$stellenart)),
    arbeitszeit_modelle = purrr::map_chr(bewerber, ~ collapse_or_na(.x$arbeitszeitModelle)),
    berufe = purrr::map_chr(bewerber, ~ collapse_or_na(.x$berufe)),
    letzte_taetigkeit_jahr = purrr::map_chr(bewerber, ~ chr_or_na(.x$letzteTaetigkeit$jahr)),
    letzte_taetigkeit_bezeichnung = purrr::map_chr(bewerber, ~ chr_or_na(.x$letzteTaetigkeit$bezeichnung)),
    letzte_taetigkeit_aktuell = purrr::map_lgl(bewerber, ~ lgl_or_na(.x$letzteTaetigkeit$aktuell)),
    hat_email = purrr::map_lgl(bewerber, ~ lgl_or_na(.x$hatEmail)),
    hat_telefon = purrr::map_lgl(bewerber, ~ lgl_or_na(.x$hatTelefon)),
    hat_adresse = purrr::map_lgl(bewerber, ~ lgl_or_na(.x$hatAdresse)),
    ort = purrr::map_chr(bewerber, ~ chr_or_na(.x$lokation$ort)),
    plz = purrr::map_chr(bewerber, ~ chr_or_na(.x$lokation$plz)),
    umkreis = purrr::map_chr(bewerber, ~ chr_or_na(.x$lokation$umkreis)),
    region = purrr::map_chr(bewerber, ~ chr_or_na(.x$lokation$region)),
    land = purrr::map_chr(bewerber, ~ chr_or_na(.x$lokation$land)),
    mehrere_arbeitsorte = purrr::map_lgl(bewerber, ~ lgl_or_na(.x$mehrereArbeitsorte)),
    ausbildungen = purrr::map(bewerber, ~ list_or_empty(.x$ausbildungen)),
    erfahrung = purrr::map(bewerber, ~ list_or_empty(.x$erfahrung)),
    operation_id = operation_id,
    page = meta_page,
    size = meta_size,
    max_ergebnisse = meta_total
  )

  if (flatten) {
    return(bunddev_flatten_list_cols(
      data,
      cols = c("ausbildungen", "erfahrung"),
      mode = flatten_mode
    ))
  }

  data
}
