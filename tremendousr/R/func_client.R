#' Create a new Tremendous API Client
#'
#' @param api_key API key from
#'   [tremendous.com](https://developers.tremendous.com/). Can either pass in
#'   here as a character string or set for repeated use with
#'   \code{\link{trem_set_api_key}}.
#' @param sandbox Logical: `TRUE` and any API requests are performed within the
#'   Tremendous sandbox environment, a free and fully-featured environment for
#'   application developing and testing. `FALSE` and the API requests are
#'   performed within the Tremendous production environment. **This will involve
#'   sending actual money, so be certain you wish to do this!**
#'
#' @return An object of class 'tremClient' that contains the API Key and
#'   Environment information for easily performing Tremendous API requests.
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' # Create a client for testing API calls within the Sandbox environment.
#' test_client <- trem_client_new(api_key = "TEST_YOUR-KEY-HERE",
#'                                   sandbox = TRUE)
#'
#' # Create a client for performing API calls within a production environment.
#' # This client will be able to send actual money!
#' prod_client <- trem_client_new(api_key = "PROD_YOUR-KEY-HERE",
#'                                sandbox = FALSE)
#'
#' }
#'
trem_client_new <- function(api_key = NULL, sandbox = TRUE) {
  tremendous_client$new(api_key = api_key,
                        sandbox = sandbox)
}

#' R6 Class representing a new Tremendous API Client
#'
#' Called by \code{\link{trem_client_new}} to bundle API key and environment.
#' @keywords internal
tremendous_client <- R6::R6Class(
  "tremClient",
  public = list(
    #' @field key (character) an API key
    key = NULL,
    #' @field sandbox (logical) tremClient for Sandbox environment (TRUE) or production (FALSE)?
    sandbox = NULL,
    #' @field httpClient for internal use
    httpClient = NULL,

    #' @description Create a new `tremClient` object
    #' @param api_key API key from
    #'   [tremendous.com](https://developers.tremendous.com/). Can either pass in
    #'   here as a character string or set for repeated use with
    #'   \code{\link{trem_set_api_key}}.
    #' @param sandbox Logical: `TRUE` (default) and any API requests are performed
    #'   within the Tremendous sandbox environment, a free and fully-featured
    #'   environment for application developing and testing. `FALSE` and the API
    #'   requests are performed within the Tremendous production environment. **This
    #'   will involve sending actual money, so be certain you wish to do this!**
    #' @param curl_opts A named list of curl options for the API Client. Defaults to include
    #' useragent info.`
    initialize = function(api_key = NULL, sandbox = TRUE,
                          curl_opts = list(useragent = trem_ua())) {
      self$key <- check_api_key(api_key, sandbox = sandbox)
      self$sandbox <- sandbox

      self$httpClient <- crul::HttpClient$new(
        url = trem_url(sandbox = self$sandbox),
        opts = curl_opts,
        headers = list(
          Accept = "application/json",
          Authorization = paste0("Bearer ", self$key)
        )
      )

    },

    #' @description Printing method for object of class 'tremClient'.
    #' @param ... NA; printing function
    print = function(...) {
      cli::cli({
        cli::cli_div(id = "parent", theme = list(.tremEl = list(`margin-left` = 2)))
        cli::cli_text("<{.emph tremendousClient}>")
        cli::cli_par(id = "api", class = "tremEl")
        if (nzchar(self$key)) cli::cli_text('API key: {.field <private>}') else cli::cli_text('API key: Not set!')
        if (self$sandbox) cli::cli_text('API Environment: {.field Sandbox}') else cli::cli_text('API Environment: {.field Production}')
        cli::cli_end(id = "api")
        cli::cli_end(id = "parent")
      })
    }
  )
)
