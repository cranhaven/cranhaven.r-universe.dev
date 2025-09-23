test_that("test fileload and fine train",{
  skip_on_cran()
  handle_openai<-openai$new(Sys.getenv("OPENAI_KEY"))
  if(Sys.getenv("USE_PROXY")=="TRUE"){
    handle_openai$set_proxy("10.0.108.36",7890)
  }
  train_file_path<-system.file("exdata","train.jsonl", package = "openaistream")
  file_id <- handle_openai$files$upload(path = train_file_path,purpose = "fine-tune")

  #job create for cancel job
  job<-handle_openai$fine_tuning$create(model = "gpt-3.5-turbo",training_file = file_id$id)
  expect_equal(job$training_file,file_id$id)

  #job create for error test
  res<-handle_openai$fine_tuning$create(model = "gpt-3.5-turbo",training_file = file_id$id,verbosity = 4)
  expect_true(!res$success)

  job_events<-handle_openai$fine_tuning$events(job$id, verbosity = 0)
  expect_contains(names(job_events$data),"data")

  #error test
  res<-handle_openai$fine_tuning$events(job$id, verbosity = 4)
  expect_true(!res$success)
  
  #test checkpoint
  job_checkpoints<-handle_openai$fine_tuning$checkpoints(job$id, verbosity = 0)
  expect_contains(names(job_events$data),"data")
  
  #error test
  res<-handle_openai$fine_tuning$checkpoints(job$id, verbosity = 4)
  expect_true(!res$success)

  job_retrieve<-handle_openai$fine_tuning$retrieve(job$id, verbosity = 0)
  expect_equal(job_retrieve$id,job$id)

  #error test
  res<-handle_openai$fine_tuning$retrieve(job$id, verbosity = 4)
  expect_true(!res$success)

  job_list<-handle_openai$fine_tuning$list()
  expect_contains(names(job_list),"data")

  job_list<-handle_openai$fine_tuning$list(limit = 2, verbosity = 3)
  expect_contains(names(job_list),"data")

  #error test
  res<-handle_openai$fine_tuning$list(verbosity = 4)
  expect_true(!res$success)

  job_status<-handle_openai$fine_tuning$cancel(job$id)
  expect_equal(job_status$id,job$id)

  #error test
  res<-handle_openai$fine_tuning$cancel("ftjob-VxJMKqY0gTmGPT")
  expect_true(!res$success)

  #error test
  del_res<-handle_openai$files$delete(file_id$id, verbosity = 0)
  expect_true(del_res$deleted)
})
