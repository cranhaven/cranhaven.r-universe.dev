#' Perform a GET request to Tremendous API
#'
#' @description This function provides lower-level access to perform GET
#'   requests via Tremendous API. Available endpoints can be found on the
#'   official [Tremendous API
#'   documentation](https://developers.tremendous.com/).
#'
#' @inheritParams trem_send_reward
#' @inheritParams trem_post
#'
#' @return If `parse = TRUE` (default), a list containing the
#'   response from the API request. Otherwise, the R6 HttpResponse object
#'   containing API request data.
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' # Create a new Tremendous API Client
#' test_client <- trem_client_new(api_key = "TEST_YOUR-API-KEY-HERE",
#' sandbox = TRUE) # Sandbox environment so no actual money is sent
#'
#' # Perform a GET request to list funding sources available in your Tremendous
#' # Account. Documentation:
#' # https://developers.tremendous.com/reference/core-funding-source-index
#' trem_get(trem_client, "funding_sources")
#'
#' # Perform a GET request to list all invoices on your Tremendous Account.
#' # Documentation:
#' # https://developers.tremendous.com/reference/core-invoices-index
#' trem_get(trem_client, "invoices")
#'
#' # Perform a GET request to list all orders (payment history) on your Tremendous
#' # Account. Documentation:
#' # https://developers.tremendous.com/reference/core-orders-index
#' trem_get(trem_client, "orders")
#'
#' # Perform a GET request to list a specific order's information (payment history)
#' # from your Tremendous Account. Documentation:
#' # https://developers.tremendous.com/reference/core-orders-show
#' trem_get(trem_client, "orders/YOUR-ORDER-ID")
#'
#'   }
#'

trem_get <- function(client, path,
                     query = list(), disk = NULL, stream = NULL,
                     parse = TRUE) {

  if (missing(client)) {
    cli::cli_abort("Tremendous API Client required.
                     Please create one with {.fn trem_client_new} .")
  } else if (!missing(client)) {
    check_client(client)
  }

  res <- client$httpClient$get(path = file.path("api/v2", path),
                               query = query,
                               disk = disk,
                               stream = stream)

  err_catcher(res)

  if (!parse) {
    return(res)
  } else if (parse) {
    jsonlite::fromJSON(res$parse("UTF-8"))
  }
}



