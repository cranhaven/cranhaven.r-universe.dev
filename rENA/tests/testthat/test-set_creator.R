suppressMessages(library(rENA, quietly = T, verbose = F))
context("Test wrapper methods");

library(magrittr)

data(RS.data)


codes = c('Data',
          'Technical.Constraints',
          'Performance.Parameters',
          'Client.and.Consultant.Requests',
          'Design.Reasoning',
          'Collaboration')
units = c("UserName","Condition", "GroupName")
convo = c("Condition","GroupName")
meta = c("CONFIDENCE.Change","CONFIDENCE.Pre","CONFIDENCE.Post","C.Change")
window.type = "MovingStanzaWindow"
window.size = 10

test_that("verify warnings", {
  testthat::expect_warning(ena.set.creator(
    data = RS.data,
    codes = codes,
    units = units,
    conversation = convo,
    window.size.back = window.size,
    groupVar = NULL,
    runTest = TRUE
  ), regexp = "Group variable and groups not specified. Unable to run test")

  testthat::expect_warning(ena.set.creator(
    data = RS.data[RS.data$Condition == "FirstGame", ],
    codes = codes,
    units = units,
    conversation = convo,
    window.size.back = window.size,
    groupVar = "Condition",
    groups = NULL,
    runTest = FALSE
  ), regexp = "Group variable only contains one unique value. ENAset has been created without means rotation")

  testthat::expect_warning(ena.set.creator(
    data = RS.data[RS.data$Condition == "FirstGame", ],
    codes = codes,
    units = units,
    conversation = convo,
    window.size.back = window.size,
    groupVar = "Condition",
    groups = NULL,
    runTest = TRUE
  ), regexp = "Multiple groups not specified. Unable to run test")

  testthat::expect_warning(ena.set.creator(
    data = RS.data,
    codes = codes,
    units = units,
    conversation = convo,
    window.size.back = window.size,
    groupVar = "Condition",
    groups = NULL,
    runTest = TRUE
  ), regexp = "No groups specified. Running test on the first two unique group values of the group variable:")

  testthat::expect_warning(ena.set.creator(
    data = RS.data,
    codes = codes,
    units = units,
    conversation = convo,
    window.size.back = window.size,
    groupVar = "Condition",
    groups = "FirstGame",
    runTest = TRUE
  ), "Multiple groups not specified. Unable to run test")

  testthat::expect_warning(ena.set.creator(
    data = RS.data,
    codes = codes,
    units = units,
    conversation = convo,
    window.size.back = window.size,
    groupVar = "Condition",
    groups = c("FirstGame", "SecondGame", "SecondGame"),
    runTest = FALSE
  ), "Only two groups are allowed for means rotation. ENAset has been created using a means rotation on the first two groups given:")

  testthat::expect_warning(ena.set.creator(
    data = RS.data,
    codes = codes,
    units = units,
    conversation = convo,
    window.size.back = window.size,
    groupVar = "Condition",
    groups = c("FirstGame", "SecondGame", "SecondGame"),
    runTest = TRUE
  ), regexp = "More than two groups specified. Running test on the first two groups:")

  testthat::expect_error(ena.set.creator(
    data = RS.data,
    codes = codes,
    units = units,
    conversation = convo,
    window.size.back = window.size,
    groupVar = "Condition",
    groups = c("FirstGame", "WarningGame"),
    runTest = TRUE
  ), regexp = "Group column does not contain supplied group value")

})

test_that("verify messages", {
  testthat::expect_message(ena.set.creator(
    data = RS.data,
    codes = codes,
    units = units,
    conversation = convo,
    window.size.back = window.size,
    groupVar = "Condition",
    groups = NULL,
    runTest = FALSE
  ), regexp = "No groups specified. Defaulting to means rotation using first two unique group values of group variable:")

  testthat::expect_message(ena.set.creator(
    data = RS.data,
    codes = codes,
    units = units,
    conversation = convo,
    window.size.back = window.size,
    groupVar = "Condition",
    groups = "FirstGame",
    runTest = FALSE
  ), regexp = "Only one group value specified. ENAset has been created without means rotation")
})
