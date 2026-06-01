#' Perform a DELETE request to Tremendous API
#'
#' Tremendous only supports DELETE requests for one endpoint -- deleting an
#' invoice. Per [their
#' documentation](https://developers.tremendous.com/reference/core-invoices-delete),
#' this request "removes an invoice. This has no further consequences but is a
#' rather cosmetic operation." See the examples for a walk-through.
#'
#' @inheritParams trem_post
#'
#' @return If `parse = TRUE` (default), a list containing the response from the
#'   API request. Otherwise, the R6 HttpResponse object containing API request
#'   data.
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#'   # Create a new Tremendous API Client
#'   test_client <- trem_client_new(api_key = "TEST_YOUR-API-KEY-HERE",
#'                                  sandbox = TRUE)
#'
#'   # Perform a POST request for an invoice.
#'   # `po_number` is Reference to the purchase order number within your organization
#'   # `amount` is in USD
#'   trem_post(test_client,
#'             path = "invoices",
#'             body = list(po_number = "unique-invoice-id",
#'                         amount = 50)
#'   )
#'
#'   # Perform a GET request for listing all current (non-deleted) invoices.
#'   current_invoices <- trem_get(test_client, "invoices")
#'
#'   # Get index for the correct ID
#'   unique_id_index <- which(current_invoices$invoices$po_number == "unique-invoice-id")
#'
#'   # Get the invoice ID for 'unique-invoice-id' to delete
#'   my_invoice_id <- current_invoices$invoices[unique_id_index, "id"]
#'
#'   # Perform a DELETE request for the specific invoice.
#'   trem_delete(test_client, paste0("invoices/", my_invoice_id))
#'
#'   # Perform a GET request for listing all current (non-deleted) invoices.
#'   # The one with id po_number 'unique-invoice-id' should no longer be here.
#'   trem_get(test_client, "invoices")
#'
#' }
#'
#'
trem_delete <- function(client, path,
                      query = list(), body = NULL,
                      disk = NULL, stream = NULL, encode = "json",
                      parse = TRUE) {

  if (missing(client)) {
    cli::cli_abort("Tremendous API Client required.
                     Please create one with {.fn trem_client_new} .")
  } else if (!missing(client)) {
    check_client(client)
  }

  res <- client$httpClient$delete(path = file.path("api/v2", path),
                                  query = query,
                                  body = body,
                                  disk = disk,
                                  stream = stream,
                                  encode = encode)

  err_catcher(res)

  if (!parse) {
    return(res)
  } else if (parse) {
    jsonlite::fromJSON(res$parse("UTF-8"))
  }
}
