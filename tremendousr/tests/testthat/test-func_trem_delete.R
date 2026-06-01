test_that("trem_delete throws errors - no client", {

  expect_error(
    trem_post(path = "members",
              body = list(email = "example@website.com",
                          name = "Example Person",
                          role = "MEMBER"),
    ),
    regexp = "Tremendous API Client required.")

})

test_that("trem_delete works", {

  skip_on_cran()
  skip_on_travis()

  test_client <- trem_client_new(api_key = NULL, # Uses system env API key
                                 sandbox = TRUE)


  trem_post(test_client,
            path = "invoices",
            body = list(po_number = "from-testthat-invoice-id",
                        amount = 50)
  )

  # Perform a GET request for listing all current (non-deleted) invoices.
  current_invoices <- trem_get(test_client, "invoices")

  # Get the invoice ID for 'unique-invoice-id' to delete
  my_invoice_id <- current_invoices$invoices[which(current_invoices$invoices$po_number == "from-testthat-invoice-id"), "id"]

  vcr::use_cassette("trem-delete", {
    # Perform a DELETE request for the specific invoice.
    deleteClient <- trem_delete(test_client,
                                paste0("invoices/", my_invoice_id))
  }, match_requests_on = "method"
  )

  # Perform a GET request for listing all current (non-deleted) invoices.
  # The one with id po_number 'unique-invoice-id' should no longer be here.
  new_invoices <- trem_get(test_client, "invoices")


  expect_type(deleteClient, "list")
  expect_named(deleteClient, "invoice")

  expect_type(deleteClient$invoice, "list")
  expect_named(deleteClient$invoice, c("id", "po_number", "amount", "status"))

  expect_type(deleteClient$invoice$id, "character")
  expect_type(deleteClient$invoice$po_number, "character")
  expect_type(deleteClient$invoice$amount, "double")
  expect_equal(deleteClient$invoice$status, "DELETED")

  expect_false(deleteClient$invoice$id %in% new_invoices$invoices$id)


  on.exit(trem_delete(test_client, paste0("invoices/", my_invoice_id)),
          add = TRUE)

})

