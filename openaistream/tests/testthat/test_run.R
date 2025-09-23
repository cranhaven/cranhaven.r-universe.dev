#test5
test_that("run",{
  skip_on_cran()
  Sys.setenv(TEST_EX_COND = "")
  handle_openai<-openai$new(Sys.getenv("OPENAI_KEY"))
  if(Sys.getenv("USE_PROXY")=="TRUE"){
    handle_openai$set_proxy("10.0.108.36",7890)
  }
  thc<-handle_openai$threads$create(verbosity = 3)
  expect_contains(names(thc),"id")
  train_file_path<-system.file("exdata","assfile.txt", package = "openaistream")
  file_id <- handle_openai$files$upload(path = train_file_path,purpose = "assistants",verbosity = 0)
  ass<-handle_openai$assistants$create(name="cor_flag",
                                       model="gpt-4o",
                                       instructions="I am HealthNutritionAssistant, designed to provide professional and accurate health and nutrition advice.
                                   My primary function is to answer health or nutrition related questions using the uploaded file “assfile.txt,” which contains common health and nutrition questions and their answers.
                                   When a user asks a health or nutrition question, I'll first consult this file.
                                   If it contains a relevant answer, I will use it to respond.
                                   If the file lacks a direct answer, I'll offer general advice based on my broad knowledge and suggest consulting a professional doctor or nutritionist for specific issues.
                                   My tone is friendly and professional.
                                   I'll always clarify the source of my information, like “According to our health FAQs...” I encourage users to seek professional medical advice for specific health concerns.
                                   I do not collect or store personal health information.
                                   My responses are based on the file's content and current best medical practices. I am regularly updated to reflect the latest medical research and health guidelines.",
                                   tools=list(list(type="code_interpreter")),
                                   tool_resources=list(code_interpreter=list(file_ids=list(file_id$id))),
                                   verbosity = 3
  )
  expect_equal(ass$model,"gpt-4o")
  #这里测试助手1
  #这里是测试非流时助手
  runct<-handle_openai$runs$create_tread(ass$id,thread=list(messages=list(list(role="user",content="What foods are good for heart health?"))),verbosity = 3)
  expect_equal(runct$object,"thread.run")
  runl<-handle_openai$runs$list(runct$thread_id)
  expect_contains(runl$data$thread_id,runct$thread_id)
  Sys.sleep(10)
  runls<-handle_openai$runs$steps_list(runct$thread_id,runct$id,limit=1)
  expect_equal(nrow(runls$data),1)
  runr<-handle_openai$runs$retrieve(runct$thread_id,runct$id)
  for(i in 1:4){
    if(runr$status!="completed"){
      Sys.sleep(5)
      runr<-handle_openai$runs$retrieve(runct$thread_id,runct$id)
    }else{
      break
    }
  }
  expect_equal(runr$object,"thread.run")
  runrs<-handle_openai$runs$steps_retrieve(runct$thread_id,runct$id,runls$data$id[1])
  expect_equal(runrs$object,"thread.run.step")
  Sys.sleep(15)
  runm<-handle_openai$runs$modify(runct$thread_id,runct$id,metadata=list(test="test"),verbosity = 3)
  expect_equal(runm$metadata$test,"test")
  #这里是测试流时助手
  runct_e<-handle_openai$runs$create_tread(ass$id,stream = T,thread=list(messages=list(list(role="user",content="What foods are good for heart health?"))),max_completion_tokens=100,verbosity = 3)
  runct_e$next_value
  runct_e$close()
  
  
  #这里测试助手2
  ass2<-handle_openai$assistants$create(name="cor_flag2",
                                        model="gpt-4o",
                                        instructions="You are a weather bot. Use the provided functions to answer questions.",
                                        tools=list(list(type="function",
                                                        "function" = list(name="getCurrentWeather",
                                                                          description="Get the weather in location",
                                                                          parameters=list(
                                                                            type="object",
                                                                            properties=list(
                                                                              location=list(type="string",description="The city and state e.g. San Francisco, CA"),
                                                                              unit=list(type="string",enum=list("c", "f"))
                                                                            ),
                                                                            required=list("location")
                                                                          ))),
                                                   list(type="function",
                                                        "function" = list(name="getNickname",
                                                                          description="Get the nickname of a city",
                                                                          parameters=list(
                                                                            type="object",
                                                                            properties=list(
                                                                              location=list(type="string",description="The city and state e.g. San Francisco, CA")
                                                                            ),
                                                                            required=list("location")
                                                                          ))
                                                   )),verbosity = 0
  )
  mesc<-handle_openai$messages$create(runct$thread_id,role = "user",content="beijing weather?")
  runmls<-handle_openai$messages$list(runct$thread_id)
  runct<-handle_openai$runs$create(runct$thread_id,ass2$id,verbosity = 0)
  runrs<-handle_openai$runs$retrieve(runct$thread_id,runct$id)
  for(i in 1:10){
    if(runrs$status!="completed"){
      Sys.sleep(5)
      runrs<-handle_openai$runs$retrieve(runct$thread_id,runct$id)
    }else{
      break
    }
  }
  runto<-handle_openai$runs$submit_tool_outputs(runct$thread_id,runct$id,tool_outputs = list(list(tool_call_id=runrs$required_action$submit_tool_outputs$tool_calls$id,output="BJ")),verbosity = 3)
  expect_equal(runto$object,"thread.run")
  
  runct_e<-handle_openai$runs$create(runct$thread_id,ass2$id,stream = T,verbosity = 0)
  runct_e$next_value
  runct_e$close()
  
  runto_e<-handle_openai$runs$submit_tool_outputs(runct$thread_id,runct$id,stream = T,tool_outputs = list(list(tool_call_id=runrs$required_action$submit_tool_outputs$tool_calls$id,output="BJ")),verbosity = 3)
  runto_e$next_value
  runto_e$close()
  
  # runmls<-handle_openai$messages$list(runct$thread_id)
  # runsls<-handle_openai$runs$list(runct$thread_id)
  # runct<-handle_openai$runs$create(runct$thread_id,ass$id,verbosity = 3)
  # runrs<-handle_openai$runs$retrieve(runct$thread_id,runto$id)
  # print(runrs)
  Sys.sleep(1)
  aaa<-handle_openai$runs$cancel(runct$thread_id,runto$id,verbosity = 3)
  expect_equal(aaa$status,"cancelling")
  #error test
  aaa<-handle_openai$runs$cancel(runct$thread_id,runct$id,verbosity = 4)
  expect_true(!aaa$success)
  runct<-handle_openai$runs$create(runct$thread_id,ass$id,verbosity = 4)
  expect_true(!runct$success)
  runrs<-handle_openai$runs$retrieve(runct$thread_id,runct$id,verbosity = 4)
  expect_true(!runrs$success)
  runm<-handle_openai$runs$modify(runct$thread_id,runct$id,metadata=list(test="test"),verbosity = 4)
  expect_true(!runm$success)
  runl<-handle_openai$runs$list(runct$thread_id,verbosity = 4)
  expect_true(!runl$success)
  runto<-handle_openai$runs$submit_tool_outputs(runct$thread_id,runct$id,tool_outputs = list(list(tool_call_id=runrs$required_action$submit_tool_outputs$tool_calls$id,output="BJ")),verbosity = 4)
  expect_true(!runto$success)
  runct<-handle_openai$runs$create_tread(ass$id,thread=list(messages=list(list(role="user",content="What foods are good for heart health?"))),verbosity = 4)
  expect_true(!runct$success)
  runrs<-handle_openai$runs$steps_retrieve(runct$thread_id,runct$id,runls$data$id[1],verbosity = 4)
  expect_true(!runrs$success)
  runls<-handle_openai$runs$steps_list(runct$thread_id,runct$id,limit=1,verbosity = 4)
  expect_true(!runls$success)

  del_res<-handle_openai$files$delete(file_id$id, verbosity = 0)
  expect_true(del_res$deleted)
  thd<-handle_openai$threads$delete(thc$id)
  expect_true(thd$deleted)
  assd<-handle_openai$assistants$delete(ass$id)
  expect_true(assd$deleted)
  assd<-handle_openai$assistants$delete(ass2$id)
  expect_true(assd$deleted)
  
  
})
