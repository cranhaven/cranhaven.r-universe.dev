test_that("returns the message sent", {
  skip_if(!getOption("depigner.dev.test_telegram_bot"))
  skip_if(Sys.getenv("R_telegram_bot_name") != "cl_r_bot")

  start_bot_for_chat("Depigner test")

  expect_equal(
    send_to_telegram("test-send_to_telegram"),
    "test-send_to_telegram"
  )

  options(depigner.bot = NULL)
  options(depigner.chat_id = NULL)
})

test_that("returns the ggplot sent", {
  skip_if(!getOption("depigner.dev.test_telegram_bot"))
  skip_if(Sys.getenv("R_telegram_bot_name") != "cl_r_bot")
  start_bot_for_chat("Depigner test")
  gg <- ggplot2::qplot(data = mtcars, x = cyl, y = hp, main = "Test")

  expect_equal(send_to_telegram(gg), gg)

  options(depigner.bot = NULL)
  options(depigner.chat_id = NULL)
})
