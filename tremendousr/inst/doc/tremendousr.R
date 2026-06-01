## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
library(tremendousr)

## ----create-test-client-------------------------------------------------------
test_client <- trem_client_new(api_key = "TEST_YOUR-KEY-HERE",
                               sandbox = TRUE)
# Print Tremendous API Client
test_client

## ----set-api-key, eval = FALSE------------------------------------------------
#  trem_set_api_key("TEST_YOUR-KEY-HERE")
#  #> • You may wish to add your Tremendous Test API key to your '.Renviron' file:
#  #> TREMENDOUS_TEST_KEY=TEST_YOUR-KEY-HERE
#  #> [Copied to clipboard]
#  #> • To edit your '.Renviron' file:
#  #> - Check that usethis is installed.
#  #> - Call `usethis::edit_r_environ()`.
#  #> - Check that '.Renviron' ends with a new line.

## ----send-reward, eval = FALSE------------------------------------------------
#  
#  trem_send_reward(client = test_client,
#               name = "first last",
#               email = "email@website.com",
#               reward_amount = 10,
#               currency_code = "USD",
#               delivery_method = "EMAIL",
#               payment_description_id = "payment-from-tremendousr-examples",
#               funding_source_id = "your-funding-id-from-tremendous",
#               reward_types = "Q24BD9EZ332JT", # ID for virtual visa gift card
#               parse = TRUE # Return a parsed API response
#               )

## ----trem-get-examples, eval = FALSE------------------------------------------
#  
#  # Use a GET request to list funding sources available in your Tremendous Account.  Documentation: https://developers.tremendous.com/reference/core-funding-source-index
#  trem_get(test_client, "funding_sources")
#  
#  # Use a GET request to list all orders (payment history) on your Tremendous Account.   Documentation: https://developers.tremendous.com/reference/core-orders-index
#  trem_get(test_client, "orders")

## ----trem-post-examples, eval = FALSE-----------------------------------------
#  
#  # Use a POST request to invite new members to your Tremendous Account.
#  # Documentation: https://developers.tremendous.com/reference/post_members
#   trem_post(test_client,
#             path = "members",
#             body = list(email = "example@website.com",
#                         name = "Example Person",
#                         role = "MEMBER"))
#  

## ----trem-delete-examples, eval = FALSE---------------------------------------
#  
#    # Perform a POST request for an invoice.
#    # `po_number` is Reference to the purchase order number within your organization
#    # `amount` is in USD
#    trem_post(test_client,
#              path = "invoices",
#              body = list(po_number = "unique-invoice-id",
#                          amount = 50)
#    )
#  
#    # Perform a GET request for listing all current (non-deleted) invoices.
#    current_invoices <- trem_get(test_client, "invoices")
#  
#    # Get the invoice ID for 'unique-invoice-id' to delete
#    my_invoice_id <- current_invoices$invoices[which(current_invoices$invoices$po_number == "unique-invoice-id"), "id"]
#  
#    # Perform a DELETE request for the specific invoice.
#    del <- trem_delete(test_client, paste0("invoices/", my_invoice_id))
#  
#    # Perform a GET request for listing all current (non-deleted) invoices.
#    # The one with id po_number 'unique-invoice-id' should no longer be here.
#    new_invoices <- trem_get(test_client, "invoices")
#  
#    # Check that the invoice is was deleted; this should be FALSE
#    del$invoice$id %in% new_invoices$invoices$id

