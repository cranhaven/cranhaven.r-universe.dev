test_that("test fileload and fine train",{
  skip_on_cran()
  handle_openai<-openai$new(Sys.getenv("OPENAI_KEY"))
  if(Sys.getenv("USE_PROXY")=="TRUE"){
    handle_openai$set_proxy("10.0.108.36",7890)
  }
  train_file_path<-system.file("exdata","batch.jsonl", package = "openaistream")
  file_id <- handle_openai$files$upload(path = train_file_path,purpose = "batch")
  
  #job create for cancel job
  batch<-handle_openai$batch$create(input_file_id = file_id$id,endpoint = "/v1/chat/completions",completion_window ="24h")
  expect_equal(batch$input_file_id,file_id$id)
  
  #job create for error test
  res<-handle_openai$batch$create(input_file_id = file_id$id,endpoint = "/v1/chat/completions",completion_window ="24h",verbosity = 4)
  expect_true(!res$success)

  
  batch_retrieve<-handle_openai$batch$retrieve(batch$id, verbosity = 0)
  expect_equal(batch_retrieve$id,batch$id)
  
  #error test
  res<-handle_openai$batch$retrieve(batch$id, verbosity = 4)
  expect_true(!res$success)
  
  batch_list<-handle_openai$batch$list()
  expect_contains(names(batch_list),"data")
  
  batch_list<-handle_openai$batch$list(limit = 2, verbosity = 0)
  expect_contains(names(batch_list),"data")
  
  #error test
  res<-handle_openai$batch$list(verbosity = 4)
  expect_true(!res$success)
  
  batch_status<-handle_openai$batch$cancel(batch_id = batch$id,verbosity = 0)
  expect_equal(batch_status$id,batch$id)
  
  #error test
  res<-handle_openai$batch$cancel(batch_id = batch$id,verbosity = 4)
  expect_true(!res$success)
  
  #error test
  del_res<-handle_openai$files$delete(file_id$id, verbosity = 0)
  expect_true(del_res$deleted)
})
