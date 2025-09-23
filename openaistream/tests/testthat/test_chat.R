#test_chat
test_that("chat",{
  skip_on_cran()
  Sys.setenv(TEST_EX_COND = "")
  handle_openai<-openai$new(Sys.getenv("OPENAI_KEY"))
  if(Sys.getenv("USE_PROXY")=="TRUE"){
    handle_openai$set_proxy("10.0.108.36",7890)
  }
  streamlg <- handle_openai$chat$create(
    messages = data.frame(role = c("system", "user"),
                          content = c("You are a assistant.", "How's the weather today?")),
    model = "gpt-3.5-turbo",
    stream = F,
    max_tokens = 10,n=3
  )
  expect_equal(nrow(streamlg$vres),3)
  streamlg <- handle_openai$chat$create(
    messages = data.frame(role = c("system", "user"),
                          content = c("You are a assistant.", "How's the weather today?")),
    model = "gpt-3.5-turbo",
    stream = F,
    max_tokens = 10,n=3,verbosity = 4
  )
  expect_true(!streamlg$success)

  # chat test stream
  streamlg <- handle_openai$chat$create(
    messages = data.frame(role = c("system", "user"),
                          content = c("You are a assistant.", "How's the weather today?")),
    model = "gpt-3.5-turbo",
    stream = TRUE,
    max_tokens = 10,
    n =3
  )
  expect_equal(class(streamlg)[1],"DataStream")
  expect_equal(streamlg$get_state(),"initialized")
  text<-streamlg$next_value
  expect_equal(length(text$all_resp),2)
  text<-streamlg$next_value
  expect_equal(length(text$all_resp),2)
  ss<-streamlg$close()
  expect_equal(ss,"close success")
  text<-streamlg$next_value
  expect_equal(text,"close")
  stat<-streamlg$get_state()
  expect_equal(stat,"close")
  streamlg <- handle_openai$chat$create(
    messages = data.frame(role = c("system", "user"),
                          content = c("You are a assistant.", "How's the weather today?")),
    model = "gpt-3.5-turbo",
    stream = TRUE,
    max_tokens = 1,
    n =3
  )
  for(i in 1:10){
    text<-streamlg$next_value
    if(length(text)==1){
      break
    }
  }
  expect_equal(text,"complete")

  streamlg <- handle_openai$chat$create(
    messages = data.frame(role = c("system", "user"),
                          content = c("You are a assistant.", "How's the weather today?")),
    model = "gpt-3.5-turbo",
    stream = TRUE,
    stream_options = list(include_usage=T),
    max_tokens = 2,
    n =3,
  )
  for(i in 1:10){
    text<-streamlg$next_value
    if(length(text)==1){
      break
    }
  }
  expect_equal(text,"complete")
  streamlg <- handle_openai$chat$create(
    messages = data.frame(role = c("system", "user"),
                          content = c("You are a assistant.", "How's the weather today?")),
    model = "gpt-3.5-turbo",
    stream = TRUE,
    max_tokens = 1,
    num=1,
    n =1
  )
  for(i in 1:10){
    text<-streamlg$next_value
    if(length(text)==1){
      break
    }
  }
  expect_equal(text,"complete")


  #error test
  streamlg <- handle_openai$chat$create(
    messages = data.frame(role = c("system", "user"),
                          content = c("You are a assistant.", "How's the weather today?")),
    model = "gpt-3.5-turbo",
    stream = TRUE,
    max_tokens = 10,
    n = "sdasd"
  )
  text<-streamlg$next_value
  #expect_equal(text,"httr2_close open is fail")
  expect_true(grepl(pattern = "error",text$message,ignore.case = T))

  streamlg <- handle_openai$chat$create(
    messages = data.frame(role = c("system", "user"),
                          content = c("You are a assistant.", "How's the weather today?")),
    model = "gpt-3.5-turbo",
    stream = T,
    max_tokens = 10,
    n = 2,num="sdas"
  )
  text<-streamlg$next_value
  expect_equal(text$message,"non-numeric argument to binary operator")

  expect_error({
    streamlg <- handle_openai$chat$create(
      messages = data.frame(role = c("system", "user"),
                            content = c("You are a assistant.", "How's the weather today?")),
      model = "gpt-3.5-turbo",
      stream = "sadas",
      max_tokens = 10,
      n = 2
    )
  })
  ####Testing extreme conditions
  Sys.setenv(TEST_EX_COND = "error chatstream active next value")
  streamlg <- handle_openai$chat$create(
    messages = data.frame(role = c("system", "user"),
                          content = c("You are a assistant.", "How's the weather today?")),
    model = "gpt-3.5-turbo",
    stream = TRUE,
    max_tokens = 10,
    n =3
  )
  text<-streamlg$next_value
  expect_equal(text$message,"error chatstream active next value")


  Sys.setenv(TEST_EX_COND = "error chatstream check is not curl")
  streamlg <- handle_openai$chat$create(
    messages = data.frame(role = c("system", "user"),
                          content = c("You are a assistant.", "How's the weather today?")),
    model = "gpt-3.5-turbo",
    stream = TRUE,
    max_tokens = 10,
    n =3
  )
  text<-streamlg$next_value
  expect_equal(text,"is not curl")

  Sys.setenv(TEST_EX_COND = "error chatstream active isOpen")
  streamlg <- handle_openai$chat$create(
    messages = data.frame(role = c("system", "user"),
                          content = c("You are a assistant.", "How's the weather today?")),
    model = "gpt-3.5-turbo",
    stream = TRUE,
    max_tokens = 10,
    n =3
  )
  text<-streamlg$next_value
  expect_equal(text,"httr2_invalid")

  Sys.setenv(TEST_EX_COND = "error chatstream data_source open is fail")
  streamlg <- handle_openai$chat$create(
    messages = data.frame(role = c("system", "user"),
                          content = c("You are a assistant.", "How's the weather today?")),
    model = "gpt-3.5-turbo",
    stream = TRUE,
    max_tokens = 10,
    n =3
  )
  text<-streamlg$next_value
  expect_equal(text,"httr2_close open is fail")

  Sys.setenv(TEST_EX_COND = "error chatstream close")
  streamlg <- handle_openai$chat$create(
    messages = data.frame(role = c("system", "user"),
                          content = c("You are a assistant.", "How's the weather today?")),
    model = "gpt-3.5-turbo",
    stream = TRUE,
    max_tokens = 10,
    n =3
  )
  text<-streamlg$close()
  expect_equal(text$message,"error chatstream close")
})
