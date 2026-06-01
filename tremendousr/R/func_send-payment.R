#' Send a Reward via Tremendous API
#'
#' The most likely reason to use the tremendousr package is to send rewards
#' This function, `trem_send_reward()`, provides an easy interface to do so. See the
#' examples for more details.
#'
#' @param client A Tremendous API Client object, created with
#'   \code{\link{trem_client_new}}.
#' @param name Name of the recipient.
#' @param email Email address of the recipient.
#' @param phone Phone number of the recipient (US phone numbers only).
#' @param reward_amount Amount of the reward (numeric).
#' @param currency_code Currency of the reward (default to "USD").
#' @param delivery_method Default to "EMAIL", for sending the reward to the
#'   recipient via email. Alternatively, reward can be delivered via a link
#'   ("LINK") or text message ("PHONE").
#' @param payment_description_id Unique ID for specific order. This will appear
#'   as `external_id` on Tremendous Dashboard.
#' @param funding_source_id ID of the funding source linked to your account, to
#'   draw funds from for this order. One of the IDs from
#'   `trem_get("funding_sources")`.
#' @param reward_types A character vector of product ids -- reward options --
#'   for the recipient to choose from. Available options can be found
#'   [here](https://www.tremendous.com/catalog).
#' @param parse Logical: Should the API Response results be parsed into a data
#'   frame?
#'
#' @return If `parse = TRUE` (default), a list containing the response
#'   from payment API request. Otherwise, the R6 HttpResponse object containing
#'   API request data.
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#'   # Create a Tremendous Client
#'   test_client <- trem_client_new(api_key = "TEST_YOUR-KEY-HERE",
#'                                  sandbox = TRUE) # Sandbox environment so no actual money is sent
#'
#'   # To send a payment, you can simply pass in the client
#'   # and specify the necessary fields.
#'   payment1 <- trem_send_reward(client = test_client,
#'                            name = "first last",
#'                            email = "email@website.com",
#'                            reward_amount = 10,
#'                            currency_code = "USD",
#'                            delivery_method = "EMAIL",
#'                            payment_description_id = "payment-from-tremendousr-examples",
#'                            funding_source_id = "your-funding-id-from-tremendous",
#'                            reward_types = "Q24BD9EZ332JT", # ID for virtual visa gift card
#'                            parse = TRUE # Return a parsed API response
#'   )
#'
#' }
#'
#'
trem_send_reward <- function(client,
                         name, email = NULL, phone = NULL,
                         reward_amount, currency_code = "USD", delivery_method = "EMAIL",
                         payment_description_id, funding_source_id, reward_types,
                         parse = TRUE) {

  if (missing(client)) {
    cli::cli_abort("Tremendous API Client required.
                     Please create one with {.fn trem_client_new} .")
  } else if (!missing(client)) {
    check_client(client)
  }

  payment_body <- create_order_body(recipient_name = name, recipient_email = email, recipient_phone = phone,
                                    reward_amount = reward_amount, currency_code = currency_code,
                                    delivery_method = delivery_method, payment_description_id = payment_description_id,
                                    funding_source_id = funding_source_id, reward_types = reward_types)

  res <- client$httpClient$post(path = "api/v2/orders",
                                body = payment_body,
                                encode = "json")

  err_catcher(res)

  if (!parse) {
    return(res)
  } else if (parse) {
    jsonlite::fromJSON(res$parse("UTF-8"))
  }

}


