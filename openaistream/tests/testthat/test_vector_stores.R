#test5
test_that("run",{
  skip_on_cran()
  Sys.setenv(TEST_EX_COND = "")
  handle_openai<-openai$new(Sys.getenv("OPENAI_KEY"))
  if(Sys.getenv("USE_PROXY")=="TRUE"){
    handle_openai$set_proxy("10.0.108.36",7890)
  }
  goog_path<-system.file("exdata","goog-10k.pdf", package = "openaistream")
  brka_path<-system.file("exdata","brka-10k.txt", package = "openaistream")
  goog_file_id <- handle_openai$files$upload(path = goog_path,purpose = "assistants",verbosity = 0)
  brka_file_id <- handle_openai$files$upload(path = brka_path,purpose = "assistants",verbosity = 0)
  #以上为准备基础文件
  #开始向量数仓基础功能测试
  vector_store = handle_openai$vector_stores$create(name="Financial Statements")
  expect_equal(vector_store$status,"completed")
  e_vector_store = handle_openai$vector_stores$create(name="Financial Statements",verbosity = 4)
  expect_true(!e_vector_store$success)
  vsl <- handle_openai$vector_stores$list()
  expect_contains(vsl$data$name,"Financial Statements")
  e_vsl <- handle_openai$vector_stores$list(verbosity = 4)
  expect_true(!e_vsl$success)
  vsm <- handle_openai$vector_stores$modify(vector_store_id = vector_store$id,name="FS",expires_after=list(anchor="last_active_at",days=1))
  expect_equal(vsm$name,"FS")
  e_vsm <- handle_openai$vector_stores$modify(vector_store_id = vector_store$id,name="FS",expires_after=list(anchor="last_active_at",days=1),verbosity = 4)
  expect_true(!e_vsm$success)
  
  vsr <- handle_openai$vector_stores$retrieve(vector_store_id = vector_store$id)
  expect_equal(vsr$expires_after$days,1)
  Sys.sleep(2)
  e_vsr <- handle_openai$vector_stores$retrieve(vector_store_id = vector_store$id,verbosity = 4)
  expect_true(!e_vsr$success)
  #开始向量数仓文件功能测试
  fvs_g <- handle_openai$vector_stores$file_create(vector_store_id = vector_store$id,file_id = goog_file_id$id)
  expect_equal(fvs_g$id,goog_file_id$id)
  e_fvs_g <- handle_openai$vector_stores$file_create(vector_store_id = vector_store$id,file_id = goog_file_id$id,verbosity = 4)
  expect_true(!e_fvs_g$success)
  
  fvs_l <- handle_openai$vector_stores$file_list(vector_store_id = vector_store$id)
  expect_contains(fvs_l$data$id,goog_file_id$id)
  e_fvs_l <- handle_openai$vector_stores$file_list(vector_store_id = vector_store$id,verbosity = 4)
  expect_true(!e_fvs_l$success)
  
  fvs_r <- handle_openai$vector_stores$file_retrieve(vector_store_id = vector_store$id,file_id = goog_file_id$id)
  expect_equal(fvs_r$id,goog_file_id$id)
  e_fvs_r <- handle_openai$vector_stores$file_retrieve(vector_store_id = vector_store$id,file_id = goog_file_id$id,verbosity = 4)
  expect_true(!e_fvs_r$success)
  
  fvs_d <- handle_openai$vector_stores$file_delete(vector_store_id = vector_store$id,file_id = goog_file_id$id)
  expect_true(fvs_d$deleted)
  Sys.sleep(2)
  e_fvs_d <- handle_openai$vector_stores$file_delete(vector_store_id = vector_store$id,file_id = goog_file_id$id,verbosity = 4)
  expect_true(!e_fvs_d$success)
  
  bvs_g <- handle_openai$vector_stores$file_batche_create(vector_store_id = vector_store$id,file_id = c(goog_file_id$id,brka_file_id$id))
  expect_equal(bvs_g$vector_store_id,vector_store$id)
  e_bvs_g <- handle_openai$vector_stores$file_batche_create(vector_store_id = vector_store$id,file_id = c(goog_file_id$id,brka_file_id$id),verbosity = 4)
  expect_true(!e_bvs_g$success)
  
  
  bvs_r <- handle_openai$vector_stores$file_batche_retrieve(vector_store_id = vector_store$id,batch_id = bvs_g$id,verbosity = 0)
  expect_equal(bvs_r$file_counts$total,2)
  e_bvs_r <- handle_openai$vector_stores$file_batche_retrieve(vector_store_id = vector_store$id,batch_id = bvs_g$id,verbosity = 4)
  expect_true(!e_bvs_r$success)
  
  Sys.sleep(5)
  bvs_l <- handle_openai$vector_stores$file_batche_list(vector_store_id = vector_store$id,batch_id = bvs_g$id,verbosity = 3)
  expect_contains(bvs_l$data$id,goog_file_id$id)
  e_bvs_l <- handle_openai$vector_stores$file_batche_list(vector_store_id = vector_store$id,batch_id = bvs_g$id,verbosity = 4)
  expect_true(!e_bvs_l$success)
  
  bvs_c <- handle_openai$vector_stores$file_batche_cancel(vector_store_id = vector_store$id,batch_id = bvs_g$id)
  expect_equal(bvs_c$vector_store_id,vector_store$id)
  e_bvs_c <- handle_openai$vector_stores$file_batche_cancel(vector_store_id = vector_store$id,batch_id = bvs_g$id,verbosity = 4)
  expect_true(!e_bvs_c$success)
  
  
  vsd <- handle_openai$vector_stores$delete(vector_store_id = vector_store$id,verbosity = 0)
  expect_true(vsd$deleted)
  e_vsd <- handle_openai$vector_stores$delete(vector_store_id = vector_store$id)
  expect_true(!e_vsd$success)
  #以下为删除相关文件
  del_res1<-handle_openai$files$delete(file_id = goog_file_id$id, verbosity = 0)
  del_res2<-handle_openai$files$delete(file_id = brka_file_id$id, verbosity = 0)
})