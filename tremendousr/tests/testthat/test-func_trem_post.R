test_that("trem_post throws errors - no client", {

  expect_error(
    trem_post(path = "members",
              body = list(email = "example@website.com",
                          name = "Example Person",
                          role = "MEMBER"),
    ),
    regexp = "Tremendous API Client required.")

})
