test_that("speech",{
  skip_on_cran()
  Sys.setenv(TEST_EX_COND = "")
  handle_openai<-openai$new(Sys.getenv("OPENAI_KEY"))
  if(Sys.getenv("USE_PROXY")=="TRUE"){
    handle_openai$set_proxy("10.0.108.36",7890)
  }
  test_dir = tempdir()

  sss<-handle_openai$audio$speech(input = "Hi, this is a voice transmission test.",response_format="mp3")
  expect_type(sss,"raw")
  test_file1<-paste0(test_dir,"/","test1.mp3")
  expect_silent(writeBin(sss,test_file1))

  streammp3<-handle_openai$audio$speech(input = "Hi, this is a voice transmission test",response_format="mp3",stream = T,num=1000)
  expect_equal(class(streammp3)[1],"DataStream")
  expect_equal(streammp3$get_state(),"initialized")
  test_file2<-paste0(test_dir,"/","test2.mp3")
  fcon <- file(test_file2, "wb")
  for(i in 1:3){
    text<-streammp3$next_value
    expect_type(text,"raw")
    expect_silent(writeBin(text,fcon))
  }
  ss<-streammp3$close()
  expect_equal(ss,"close success")
  text<-streammp3$next_value
  expect_equal(text,"close")
  stat<-streammp3$get_state()
  expect_equal(stat,"close")
  text_E1<-handle_openai$audio$transcription(path = test_file1)
  expect_equal(text_E1$text,"Hi, this is a voice transmission test.")
  text_E2<-handle_openai$audio$translation(path = test_file1)
  expect_equal(text_E2$text,"Hi, this is a voice transmission test.")
  #final test
  streammp3<-handle_openai$audio$speech(input = "Hi, this is a voice transmission test",response_format="mp3",stream = T,num=1000)
  for(i in 1:100){
    text<-streammp3$next_value
    expect_type(text,"raw")
    stat<-streammp3$get_state()
    if(stat=="complete"){
      expect_equal(stat,"complete")
      break
    }
  }
  # error test
  sss<-handle_openai$audio$speech(input = "Hi, this is a voice transmission test.",response_format="mp3",verbosity = 4)
  expect_true(!sss$success)
  text_E1<-handle_openai$audio$transcription(path = test_file1,verbosity = 4)
  expect_true(!text_E1$success)
  text_E2<-handle_openai$audio$translation(path = test_file1,verbosity = 4)
  expect_true(!text_E1$success)
  text_E1<-handle_openai$audio$transcription(path = NULL,verbosity = 3)
  expect_true(!text_E1$success)
  text_E2<-handle_openai$audio$translation(path = NULL,verbosity = 3)
  expect_true(!text_E1$success)
})
