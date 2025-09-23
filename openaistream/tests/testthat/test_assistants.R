test_that("assistants",{
  skip_on_cran()
  Sys.setenv(TEST_EX_COND = "")
  handle_openai<-openai$new(Sys.getenv("OPENAI_KEY"))
  if(Sys.getenv("USE_PROXY")=="TRUE"){
    handle_openai$set_proxy("10.0.108.36",7890)
  }
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
                                   My responses are based on the file's content and current best medical practices. I am regularly updated to reflect the latest medical research and health guidelines."
  )
  expect_equal(ass$name,"cor_flag")
  e_ass<-handle_openai$assistants$create(name="cor_flag",verbosity = 4,
                                       model="gpt-4o",
                                       instructions="I am HealthNutritionAssistant, designed to provide professional and accurate health and nutrition advice.
                                   My primary function is to answer health or nutrition related questions using the uploaded file “assfile.txt,” which contains common health and nutrition questions and their answers.
                                   When a user asks a health or nutrition question, I'll first consult this file.
                                   If it contains a relevant answer, I will use it to respond.
                                   If the file lacks a direct answer, I'll offer general advice based on my broad knowledge and suggest consulting a professional doctor or nutritionist for specific issues.
                                   My tone is friendly and professional.
                                   I'll always clarify the source of my information, like “According to our health FAQs...” I encourage users to seek professional medical advice for specific health concerns.
                                   I do not collect or store personal health information.
                                   My responses are based on the file's content and current best medical practices. I am regularly updated to reflect the latest medical research and health guidelines."
  )
  expect_true(!e_ass$success)
  
  assr<-handle_openai$assistants$retrieve(ass$id)
  expect_equal(assr$name,"cor_flag")
  e_assr<-handle_openai$assistants$retrieve(ass$id,verbosity = 4)
  expect_true(!e_assr$success)
  
  assm<-handle_openai$assistants$modify(assr$id,model="gpt-4o",tools=list(list(type="code_interpreter")),verbosity = 3)
  expect_equal(assm$model,"gpt-4o")
  e_assm<-handle_openai$assistants$modify(assr$id,model="gpt-4o",tools=list(list(type="code_interpreter")),verbosity = 4)
  expect_true(!e_assm$success)
  
  assl<-handle_openai$assistants$list()
  expect_contains(assl$data$name,"cor_flag")
  e_assl<-handle_openai$assistants$list(verbosity = 4)
  expect_true(!e_assl$success)
  
  assd<-handle_openai$assistants$delete(ass$id)
  expect_true(assd$deleted)
  e_assd<-handle_openai$assistants$delete(ass$id)
  expect_true(!e_assd$success)
})
