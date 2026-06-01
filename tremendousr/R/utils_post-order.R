
#' Create order body
#'
#' Utility Function to Create an Order on Tremendous. For full API
#' documentation, see
#' \url{https://developers.tremendous.com/reference/core-orders-create}.
#'
#' @inheritParams trem_send_reward
#' @param recipient_name Name of the recipient.
#' @param recipient_email Email address of the recipient.
#' @param recipient_phone Phone number of the recipient (US phone numbers only).
#'
#' @return A nested list that, when converted to JSON, is accepted by
#'   'Tremendous' API's create order endpoint.
#'
#' @keywords internal
#'
#'
create_order_body <- function(recipient_name, recipient_email = NULL, recipient_phone = NULL,
                              reward_amount, currency_code = "USD", delivery_method = "EMAIL",
                              payment_description_id, funding_source_id, reward_types
                              ) {

  contact_info <- list(
    name = recipient_name,
    email = recipient_email,
    phone = recipient_phone
  )

  if (length(reward_types) == 1) {
    reward_options <- list(reward_types)
  } else {
    reward_options <- reward_types
  }

  body <- list(
    external_id = payment_description_id,
    payment = list(
      funding_source_id = funding_source_id
    ),
    rewards = list(
      value = list(
        denomination = reward_amount,
        currency_code = currency_code
      ),
      delivery = list(
        method = delivery_method
      ),
      recipient = Filter(Negate(is.null), contact_info),
      products = reward_options
      )
    )

  return(body)

}

