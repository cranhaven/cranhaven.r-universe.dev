suppressMessages(library(rENA, quietly = T, verbose = F))
context("Test convs");

data(RS.data)
codenames <- c(
  "Data", "Technical.Constraints", "Performance.Parameters",
  "Client.and.Consultant.Requests", "Design.Reasoning", "Collaboration"
);

test_that("conv check", {
  rs.dt <- data.table::data.table(RS.data)
  cn <- "FirstGame"
  un <- "joseph k"

  accum <- rENA:::ena.accumulate.data.file(
    RS.data, units.by = c("Condition", "UserName"),
    conversations.by = c("ActivityNumber", "GroupName"),
    codes = codenames
  );
  set <- ena.make.set(accum)

  codes = c( "Technical.Constraints", "Performance.Parameters" )
  noExc = ena.conversations(
    set,
    units=c(paste0(cn,".",un)),
    units.by=c('Condition','UserName'),
    conversation.by = c('ActivityNumber', 'GroupName'),
    codes = codes
  )
  expect_equal(
    length(noExc$unitRows),
    nrow(set$model$raw.input[UserName == un & Condition == cn & (Technical.Constraints == 1 | Performance.Parameters == 1), ])
  )

  noExc2 = ena.conversations(
    set,
    units=c(paste0(cn,".",un)),
    conversation.by = c('ActivityNumber', 'GroupName'),
    codes = codes
  )

  expect_equal(
    length(noExc2$unitRows),
    nrow(set$model$raw.input[UserName == un & Condition == cn & (Technical.Constraints == 1 | Performance.Parameters == 1), ])
  )

  noExc3 = ena.conversations(
    set$model$raw.input,
    units=c(paste0(cn,".",un)),
    units.by=c('Condition','UserName'),
    conversation.by = c('ActivityNumber', 'GroupName'),
    codes = codes
  )

  expect_equal(
    length(noExc3$unitRows),
    nrow(set$model$raw.input[UserName == un & Condition == cn & (Technical.Constraints == 1 | Performance.Parameters == 1), ])
  )

  noExc_err = testthat::expect_error(ena.conversations(
    set$model$raw.input,
    units=c(paste0(cn,".",un)),
    conversation.by = c('ActivityNumber', 'GroupName'),
    codes = codes
  ))
})
