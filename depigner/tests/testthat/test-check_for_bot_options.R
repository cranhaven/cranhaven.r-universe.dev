test_that("return correct output", {
  expect_equal(suppressWarnings(check_for_bot_options()), FALSE)

  expect_equal(suppressWarnings(check_for_bot_options(bot = 1L)), FALSE)

  expect_equal(
    suppressWarnings(check_for_bot_options(chat_id = 1L)),
    FALSE
  )

  expect_equal(
    suppressWarnings(check_for_bot_options(bot = 1L, chat_id = 1L)),
    TRUE
  )
})


test_that("return a Warning if false", {
  expect_warning(expect_warning(check_for_bot_options()))
  expect_warning(check_for_bot_options(bot = 1L))
  expect_warning(check_for_bot_options(chat_id = 1L))
})
