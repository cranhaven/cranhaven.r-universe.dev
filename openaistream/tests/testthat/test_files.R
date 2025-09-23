test_that("test files",{
  skip_on_cran()
  handle_openai<-openai$new(Sys.getenv("OPENAI_KEY"))
  if(Sys.getenv("USE_PROXY")=="TRUE"){
    handle_openai$set_proxy("10.0.108.36",7890)
  }
  train_file_path<-system.file("exdata","train.jsonl", package = "openaistream")

  file_id <- handle_openai$files$upload(path = train_file_path,purpose = "fine-tune")
  expect_equal(file_id$filename,"train.jsonl")
  #error test
  res<-handle_openai$files$upload(path = train_file_path,purpose = "fine-tune",verbosity = 5)
  expect_true(!res$success)

  file_list<-handle_openai$files$list(verbosity = 0)
  expect_null(file_list$success)


  file_mes<-handle_openai$files$retrieve(file_id=file_id$id, verbosity = 0)
  expect_equal(file_mes$id,file_id$id)

  file_context<-handle_openai$files$retrieve_content(file_id=file_id$id, verbosity = 0)
  json_data <- fromJSON(sprintf("[%s]", gsub(",$","",gsub("\n", ",", file_context))))
  expect_equal(length(json_data$messages),10)

  #error test
  file_context<-handle_openai$files$retrieve_content(file_id=file_id$id, verbosity = 5)
  expect_true(!file_context$success)

  #error test
  res<-handle_openai$files$retrieve(file_id=file_id$id, verbosity = 5)
  expect_true(!res$success)

  #error test
  res<-handle_openai$files$list(verbosity = 5)
  expect_true(!res$success)

  #error test
  del_res<-handle_openai$files$delete(file_id$id, verbosity = 0)
  expect_true(del_res$deleted)

  #error test
  res<-handle_openai$files$delete(file_id$id, verbosity = 5)
  expect_true(!res$success)

  #error test
  res<-handle_openai$files$upload(path = NULL,purpose = "fine-tune",verbosity = 3)
  expect_true(!res$success)
  #error test
  res<-handle_openai$files$upload(path = "/tmp/sadasdsdad",purpose = "fine-tune",verbosity = 3)
  expect_true(!res$success)
})
