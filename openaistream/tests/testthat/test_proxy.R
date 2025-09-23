test_that("test set_proxy", {
  skip_on_cran()
  handle_openai<-openai$new(Sys.getenv("OPENAI_KEY"))
  expect_silent(handle_openai$set_proxy("10.0.108.36",7890))
  expect_error(handle_openai$set_proxy("127.0sdha", 10890))
  expect_error(handle_openai$set_proxy("127.0.0.1", "sdsd"))
  expect_error(handle_openai$set_proxy("127.0.0.1", 8217321))
  expect_error(handle_openai$set_proxy("999.0.888.1", 10890))
  if(Sys.getenv("USE_PROXY")!="TRUE"){
    handle_openai$set_proxy("127.0.0.1",10891)
    res<-handle_openai$models$retrieve(model = "gpt-3.5-turbo",verbosity = 0)
    expect_true(!res$success)
  }
})
