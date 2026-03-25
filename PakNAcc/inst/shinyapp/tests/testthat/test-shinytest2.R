library(shinytest2)

test_that("{shinytest2} recording: shinyapp", {
  app <- AppDriver$new(variant = platform_variant(), name = "shinyapp", height = 688, 
      width = 1147)
  app$expect_values()
  app$expect_screenshot()
  app$set_inputs(dataset = "GNICurrent")
  app$set_inputs(dataset = "NAConstant")
  app$set_inputs(dataset = "GNIConstant")
})


test_that("{shinytest2} recording: test-shinytest2", {
  app <- AppDriver$new(variant = platform_variant(), name = "test-shinytest2", height = 688, 
      width = 1147)
  app$expect_values()
  app$expect_screenshot()
  app$set_inputs(dataset = "GNICurrent")
  app$set_inputs(dataset = "NAConstant")
  app$set_inputs(dataset = "GNIConstant")
})
