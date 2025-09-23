#test_moderations
test_that("moderations",{
  skip_on_cran()
  Sys.setenv(TEST_EX_COND = "")
  handle_openai<-openai$new(Sys.getenv("OPENAI_KEY"))
  if(Sys.getenv("USE_PROXY")=="TRUE"){
    handle_openai$set_proxy("10.0.108.36",7890)
  }
  modc<-handle_openai$moderations$create(input = "I'll kill this bug",verbosity = 0)
  expect_true(modc$results$categories$violence)
  modc<-handle_openai$moderations$create(input = "I'll kill this bug",verbosity = 4)
  expect_true(!modc$success)
})
