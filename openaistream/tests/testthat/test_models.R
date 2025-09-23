test_that("long time test",{
  skip_on_cran()
  handle_openai<-openai$new(Sys.getenv("OPENAI_KEY"))
  if(Sys.getenv("USE_PROXY")=="TRUE"){
    handle_openai$set_proxy("10.0.108.36",7890)
  }
  train_file_path<-system.file("exdata","train.jsonl", package = "openaistream")
  expect_error({handle_openai$files<-""})
  file_id <- handle_openai$files$upload(path = train_file_path,purpose = "fine-tune")
  # the pretest
  #job create for delete job
  job2<-handle_openai$fine_tuning$create(model = "gpt-3.5-turbo",training_file = file_id$id)
  #test jobs_delete
  Sys.sleep(200)
  #error test
  for(i in 1:100){
    Sys.sleep(20)
    job_retrieve<-handle_openai$fine_tuning$retrieve(job2$id, verbosity = 0)
    print(paste0("\n",i," retry ",job_retrieve$status))
    if(job_retrieve$status=="succeeded"){
      res<-handle_openai$models$delete(job_retrieve$fine_tuned_model)
      expect_true(res$deleted)
      break
    }
  }
  #error test
  res<-handle_openai$models$delete("gpt-3.5-turbo-sadsada")
  expect_true(!res$success)
  del_res<-handle_openai$files$delete(file_id$id, verbosity = 0)
  del_res<-handle_openai$files$delete(job_retrieve$result_files, verbosity = 0)
  res<-handle_openai$models$retrieve(model = "gpt-3.5-turbo",verbosity = 0)
  expect_equal(res$id,"gpt-3.5-turbo")
  res<-handle_openai$models$retrieve(model = "gpt-3.5-turbo",verbosity = 200)
  expect_true(!res$success)
  res<-handle_openai$models$list(verbosity = 100)
  expect_true(!res$success)
  res<-handle_openai$models$list()
  expect_null(res$success)
})
