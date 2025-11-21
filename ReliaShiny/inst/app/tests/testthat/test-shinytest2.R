library(shinytest2)

test_that("{shinytest2} recording: shiny-app", {
  app <- AppDriver$new(name = "shiny-app", seed = 1243, height = 857, width = 1211)
  app$expect_values()
  app$set_inputs(sidebarItemExpanded = "LifeData")
  app$set_inputs(tabset1 = "Contour Plot")
  app$expect_values()
  app$set_inputs(sidebarItemExpanded = "ReliabilityGrowth")
  app$set_inputs(failures = "failures")
  app$set_inputs(tabset2 = "Duane Plot")
  app$expect_values()
})
