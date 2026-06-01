#' Perform a POST request to Tremendous API
#'
#' @description This function provides lower-level access to perform POST
#'   requests via Tremendous API. Available endpoints can be found on the
#'   official [Tremendous API
#'   documentation](https://developers.tremendous.com/).
#'
#'   For sending payments, I would recommend using \code{\link{trem_send_reward}} as
#'   it's more intuitive to use. However, this can be done using the
#'   `trem_post()` function (see examples).
#'
#' @inheritParams trem_send_reward
#'
#' @param path The URL path, appended to the base URL, for GET requests such as
#'   listing available payment types, funding sources, account members, and
#'   more. See the [Tremendous API
#'   Documentation](https://developers.tremendous.com/) for examples.
#' @param query Query terms as a named list. See [crul::HttpClient] for more
#'   details.
#' @param body Request body for Tremendous API, as an R List.
#' @param disk A path to write to. `NULL` by default, so info is written to
#'   memory. See [crul::HttpClient] for more details.
#' @param stream An R function to determine how to stream data. `NULL` by
#'   default, so info is streaned with memory. See [crul::HttpClient] for more
#'   details.
#' @param encode "json" by default based on Tremendous API Request format. See
#'   [crul::HttpClient] for more options.
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
#'   # Perform a POST request to invite new members to your Tremendous Account.
#'   # Documentation: https://developers.tremendous.com/reference/post_members
#'     trem_post(trem_client,
#'               path = "members",
#'               body = list(email = "example@website.com",
#'                           name = "Example Person",
#'                           role = "MEMBER"))
#'
#'   # Perform a POST send payments --
#'   I find it ~tremendously~ easier to use the `trem_send_reward()` function.
#'   # Documentation: https://developers.tremendous.com/reference/core-orders-create
#'     trem_post(trem_client,
#'               path = "orders",
#'               body = list(
#'                 external_id = "manual-payment-post", # This is a payment description id
#'                 payment = list(
#'                   funding_source_id = "your-funding-id-from-tremendous"
#'                 ),
#'                 rewards = list(
#'                   value = list(
#'                     denomination = 10,
#'                     currency_code = "USD"
#'                   ),
#'                   delivery = list(
#'                     method = "EMAIL" # "EMAIL", "LINK", or "PHONE",
#'                   ),
#'                   recipient = list(
#'                     name = "first last",
#'                     email = "email@website.com"
#'                   ),
#'                   # IDs for Applebee's Gift Card and Amazon Gift Card
#'                   products = c("2JFKPXBWDC1K", "VW9JLMPRL9N7")
#'                 )
#'               ))
#'
#' }
#'
#'
trem_post <- function(client, path,
                      query = list(), body = NULL,
                      disk = NULL, stream = NULL, encode = "json",
                      parse = TRUE) {

  if (missing(client)) {
    cli::cli_abort("Tremendous API Client required.
                     Please create one with {.fn trem_client_new} .")
  } else if (!missing(client)) {
    check_client(client)
  }


  res <- client$httpClient$post(path = file.path("api/v2", path),
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





