suppressMessages(library(rENA, quietly = T, verbose = F))
context("Test functions accumulating data")

codenames <- c("Data", "Technical.Constraints", "Performance.Parameters",
  "Client.and.Consultant.Requests", "Design.Reasoning", "Collaboration");
test_that("Null data errors", {
  df.whole <- data.frame(
    Name = c("J", "Z"),
    Day = c(1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2),
    c1  = c(1, 1, 1, 1, 1, 0, 0, 1, 1, 0, 0, 1),
    c2  = c(1, 1, 1, 0, 0, 1, 0, 1, 0, 1, 0, 0),
    c3  = c(0, 0, 1, 0, 1, 0, 1, 0, 0, 0, 1, 0),
    c4  = c(1, 1, 1, 0, 0, 1, 0, 1, 0, 1, 0, 0)
  )

  enad <- ENAdata$new(df.whole,
            units.by = c("Name"),
            conversations.by = c("Day"),
            codes = c("c1", "c2", "c3"),
            model = "EndPoint"
          )

  testthat::expect_error(accumulate_data(enad))
  testthat::expect_null(enad$raw)
  tryCatch(suppressWarnings(enad$process()))
  testthat::expect_identical(class(enad$raw), c("data.table", "data.frame"))

  enad2 <- enad$clone(deep = T)
  enad2$raw <- as.data.frame(enad2$raw)
  enad2$adjacency.vectors <- NULL
  enad2 <- rENA:::accumulate_data(enad2)
  testthat::expect_identical(class(enad2$raw), c("data.frame"))

  enad3 <- enad$clone(deep = T)
  enad3$adjacency.vectors <- NULL
  enad3 <- rENA:::accumulate_data(enad3)

  testthat::expect_identical(enad2$adjacency.vectors, enad3$adjacency.vectors)

  enad4 <- enad$clone(deep = T)
  enad4$raw$ENA_UNIT <- NULL
  testthat::expect_false("ENA_UNIT" %in% colnames(enad4$raw))
  enad4 <- rENA:::accumulate_data(enad4)
  testthat::expect_true("ENA_UNIT" %in% colnames(enad4$raw))

  enad5 <- enad$clone(deep = T)
  enad5$adjacency.vectors <- NULL
  enad5$accumulated.adjacency.vectors <- NULL
  enad5$codes <- df.whole[, c("c1", "c2", "c3")]
  enad5 <- rENA:::accumulate_data(enad5)
  testthat::expect_identical(
    enad$accumulated.adjacency.vectors[, 1:3],
    enad5$accumulated.adjacency.vectors[, 1:3]
  )
})

test_that("Simple forwarded metadata", {
  fake_codes_len = 10;
  fake.codes <- function(x) sample(0:1,fake_codes_len, replace=T)

  codes = paste("Codes",LETTERS[1:fake_codes_len],sep="-");

  df.units = data.frame(
    Name=rep(c("J","Z"), 6)
    #Group=c(1,1,1,1,2,2,2,3,3,3,4,4)
  );
  df.conversation = data.frame(
    Day=c(1,1,1,1,1,1,2,2,2,2,2,2)#,
    #ActivityNumber=c(1,1,1,1,2,2,2,2,3,3,3,3)
  );
  df.codes = data.frame(
    c1=c(1,1,1,1,1,0,0,1,1,0,0,1),
    c2=c(1,1,1,0,0,1,0,1,0,1,0,0),
    c3=c(0,0,1,0,1,0,1,0,0,0,1,0)#,
    #c4=c(1,1,1,0,0,1,0,1,0,1,0,0)
  );
  df.meta = data.frame(
    m1=c(1,2,1,2,1,2,1,2,1,2,1,2),
    m2=c(1,2,3,4,9,9,9,9,9,9,9,9)
  )
  df = data.frame(
    Name=c("J","Z"),
    Day=c(1,1,1,1,1,1,2,2,2,2,2,2),
    c1=c(1,1,1,1,1,0,0,1,1,0,0,1),
    c2=c(1,1,1,0,0,1,0,1,0,1,0,0),
    c3=c(0,0,1,0,1,0,1,0,0,0,1,0),
    m1=c(1,2,1,2,1,2,1,2,1,2,1,2),
    m2=c(1,2,3,4,9,9,9,9,9,9,9,9)
  );

  df.accum = rENA:::ena.accumulate.data.file(df, units.by = c("Name"), conversations.by = c("Day"), codes = c("c1","c2","c3"));

  df.accum.sep = ena.accumulate.data(units = df.units, conversation = df.conversation, codes = df.codes, metadata = df.meta)

  # expect_true("m1" %in% colnames(df.accum$metadata));
  expect_true("m1" %in% colnames(df.accum.sep$meta.data));
  # expect_equal(df.accum$metadata, df.accum.sep$metadata);
});

test_that("Test trajectories", {
  fake_codes_len = 10;
  fake.codes <- function(x) sample(0:1,fake_codes_len, replace=T)

  codes = paste("Codes",LETTERS[1:fake_codes_len],sep="-");

  df.units = data.frame(
    Name=rep(c("J","Z"), 6)
  );
  df.conversation = data.frame(
    Day=c(1,1,1,1,1,1,2,2,2,2,2,2),
    ActivityNumber=c(1,1,1,1,2,2,2,2,3,3,3,3)
  );
  df.codes = data.frame(
    c1=c(1,1,1,1,1,0,0,1,1,0,0,1),
    c2=c(1,1,1,0,0,1,0,0,0,0,0,1),
    c3=c(0,0,1,0,1,0,1,0,0,0,1,0)
  );
  df <- data.frame(
    Name = c("J", "Z"),
    Day = c(1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2),
    ActivityNumber = c(1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3),
    c1 = c(1, 1, 1, 1, 1, 0, 0, 1, 1, 0, 0, 1),
    c2 = c(1, 1, 1, 0, 0, 1, 0, 0, 0, 0, 0, 1),
    c3 = c(0, 0, 1, 0, 1, 0, 1, 0, 0, 0, 1, 0)
  );

  df.accum = ena.accumulate.data.file(
    df, units.by = c("Name"), conversations.by = c("Day", "ActivityNumber"),
    codes = c("c1","c2","c3"), model = "AccumulatedTrajectory"
  );
  df.non.accum = ena.accumulate.data.file(
    df, units.by = c("Name"), conversations.by = c("Day", "ActivityNumber"),
    codes = c("c1","c2","c3"), model = "SeparateTrajectory"
  );
  df.accum.sep = ena.accumulate.data(
    units = df.units, conversation = df.conversation,
    codes = df.codes, model = "AccumulatedTrajectory"
  );
  df.non.accum.sep = ena.accumulate.data(
    units = df.units, conversation = df.conversation,
    codes = df.codes, model = "SeparateTrajectory"
  );

  adjacency.code.1 = c("c1 & c2")
  # Test for expected accumulated value
  testthat::expect_true(
    as.matrix(df.accum$connection.counts[df.accum$trajectories$Name =="J" & df.accum$trajectories$ActivityNumber == 3, ..adjacency.code.1]) ==
    as.matrix(df.accum$model$row.connection.counts[Name == "J", sum(.SD), .SDcols = adjacency.code.1])
  );
  testthat::expect_true(
    as.matrix(df.accum.sep$connection.counts[df.accum.sep$trajectories$Name=="J" & df.accum.sep$trajectories$ActivityNumber == 3, ..adjacency.code.1]) ==
    as.matrix(df.accum.sep$model$row.connection.counts[Name == "J", sum(.SD), .SDcols = adjacency.code.1])
  );

  # Test for a value of 1 in the first accumulation of the trajectory of code 1
  testthat::expect_true(
    sum(df.accum$model$row.connection.counts[
      Name == "Z" & ActivityNumber == 1, ..adjacency.code.1
    ]) == 1)
  testthat::expect_true(
    sum(df.accum.sep$model$row.connection.counts[
      Name == "Z" & ActivityNumber == 1, ..adjacency.code.1
    ]) == 1)


  # Test for a value of 0 in the second accumulation of the trajectory of code 1
  testthat::expect_true(all(
    df.accum$model$row.connection.counts[
      Name == "Z" & ActivityNumber == 2, ..adjacency.code.1] == 0
  ))
  testthat::expect_true(all(
    df.accum.sep$model$row.connection.counts[
      Name == "Z" & ActivityNumber == 2, ..adjacency.code.1] == 0
  ))

  # Test that the first summed trajectory is 1
  testthat::expect_equal(as.numeric(
    df.accum$connection.counts[
      df.accum$trajectories$Name == "Z" &
      df.accum$trajectories$ActivityNumber == 1, ..adjacency.code.1
    ]), 1
  )
  testthat::expect_equal(as.numeric(
    df.accum.sep$connection.counts[
      df.accum$trajectories$Name == "Z" &
      df.accum$trajectories$ActivityNumber == 1, ..adjacency.code.1
    ]), 1
  )

  # Test that the second summed trajectory is 1, even thought it had a zero
  # accumulation for it's conversations
  testthat::expect_equal(as.numeric(
    df.accum$connection.counts[
      df.accum$trajectories$Name == "Z" &
      df.accum$trajectories$ActivityNumber == 2 &
      df.accum$trajectories$Day == 1, ..adjacency.code.1
    ]), 1
  )
  testthat::expect_equal(as.numeric(
    df.accum.sep$connection.counts[
      df.accum$trajectories$Name == "Z" &
      df.accum$trajectories$ActivityNumber == 2 &
      df.accum$trajectories$Day == 1, ..adjacency.code.1
    ]), 1
  )

  # Test that non-accumulation is properly leaving second trajectory group 0
  # (different than the previous test)
  testthat::expect_identical(
    c(1, 0, 0, 1),
    as.numeric(as.matrix(df.non.accum$connection.counts[
      df.non.accum$trajectories$Name == "Z", ..adjacency.code.1
    ]))
  )
  testthat::expect_identical(
    c(1, 0, 0, 1),
    as.numeric(as.matrix(df.non.accum.sep$connection.counts[
      df.non.accum$trajectories$Name == "Z", ..adjacency.code.1
    ]))
  )

  enadB <- ENAdata$new(df,
            units.by = c("Name"),
            conversations.by = c("Day", "ActivityNumber"),
            codes = c("c1", "c2", "c3"),
            model = "BogusTrajectory"
  )
  catch <- tryCatch(suppressMessages(enadB$process()), error = paste)

  testthat::expect_error(rENA:::accumulate_data(enadB))
})

test_that("Test accumulation with infinite windows", {
  fake_codes_len <- 10;
  fake.codes <- function(x) sample(0:1, fake_codes_len, replace = T)
  codes <- paste("Codes", LETTERS[1:fake_codes_len], sep = "-")

  df.units <- data.frame(
    Name = rep(c("J", "Z"), 6)
  );
  df.conversation <- data.frame(
    Day = c(1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2)
  )
  df.codes <- data.frame(
    c1 = c(1, 1, 1, 1, 1, 0, 0, 1, 1, 0, 0, 1),
    c2 = c(1, 1, 1, 0, 0, 1, 0, 1, 0, 1, 0, 0),
    c3 = c(0, 0, 1, 0, 1, 0, 1, 0, 0, 0, 1, 0)
  );
  df.whole <- data.frame(
    Name = c("J", "Z"),
    Day = c(1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2),
    c1 = c(1, 1, 1, 1, 1, 0, 0, 1, 1, 0, 0, 1),
    c2 = c(1, 1, 1, 0, 0, 1, 0, 1, 0, 1, 0, 0),
    c3 = c(0, 0, 1, 0, 1, 0, 1, 0, 0, 0, 1, 0),
    c4 = c(1, 1, 1, 0, 0, 1, 0, 1, 0, 1, 0, 0)
  )

  df.accum.sep  <- ena.accumulate.data(
    units = df.units, conversation = df.conversation, codes = df.codes)
  df.accum.inf  <- ena.accumulate.data(
    units = df.units, conversation = df.conversation, codes = df.codes,
    window.size.back = Inf)
  df.accum.inf2 <- ena.accumulate.data(
    units = df.units, conversation = df.conversation, codes = df.codes,
    window.size.back = "Inf")
  df.accum.inf3 <- ena.accumulate.data(
    units = df.units, conversation = df.conversation, codes = df.codes,
    window.size.back = "INF")

  expect_false(all(as.matrix(
    df.accum.sep$model$row.connection.counts) ==
      as.matrix(df.accum.inf$model$row.connection.counts)))
  expect_true(all(as.matrix(
    df.accum.inf$model$row.connection.counts) ==
      as.matrix(df.accum.inf2$model$row.connection.counts)))
})

test_that("Test function params", {
  fake_codes_len <- 10;
  fake.codes <- function(x) sample(0:1, fake_codes_len, replace = T)

  codes <- paste("Codes", LETTERS[1:fake_codes_len], sep = "-");

  df.units <- data.frame(
    Name = rep(c("J", "Z"), 6)
  );
  df.conversation <- data.frame(
    Day = c(1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2)
  );
  df.codes <- data.frame(
    c1 = c(1, 1, 1, 1, 1, 0, 0, 1, 1, 0, 0, 1),
    c2 = c(1, 1, 1, 0, 0, 1, 0, 1, 0, 1, 0, 0),
    c3 = c(0, 0, 1, 0, 1, 0, 1, 0, 0, 0, 1, 0)
  );
  df.whole <- data.frame(
    Name = c("J", "Z"),
    Day = c(1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2),
    c1 = c(1, 1, 1, 1, 1, 0, 0, 1, 1, 0, 0, 1),
    c2 = c(1, 1, 1, 0, 0, 1, 0, 1, 0, 1, 0, 0),
    c3 = c(0, 0, 1, 0, 1, 0, 1, 0, 0, 0, 1, 0),
    c4 = c(1, 1, 1, 0, 0, 1, 0, 1, 0, 1, 0, 0)
  )

  accum <- ena.accumulate.data(
    units = df.units, conversation = df.conversation, codes = df.codes);
  expect_equal("EndPoint", accum$`_function.params`$model)
})

test_that("Test different params", {
  fake_codes_len <- 10;
  fake.codes <- function(x) sample(0:1,fake_codes_len, replace=T)

  codes <- paste("Codes", LETTERS[1:fake_codes_len], sep = "-")
  df.units <- data.frame(
    Name = rep(c("J", "Z"), 6)
  )
  df.conversation <- data.frame(
    Day=c(1,1,1,1,1,1,2,2,2,2,2,2)#,
  )
  df.codes <- data.frame(
    c1=c(1,1,1,1,1,0,0,1,1,0,0,1),
    c2=c(1,1,1,0,0,1,0,1,0,1,0,0),
    c3=c(0,0,1,0,1,0,1,0,0,0,1,0)
  )

  expect_error(ena.accumulate.data(
      units = df.units[1:5,, drop = F],
      conversation = df.conversation[1:6,, drop = F],
      codes = df.codes
    ),
    regexp = "the same number of rows"
  )
  expect_error(ena.accumulate.data(
      units = NULL, conversation = df.conversation, codes = df.codes
    ),
    regexp = "Accumulation requires"
  )
  expect_warning(ena.accumulate.data(
      units = df.units, conversation = df.conversation, codes = df.codes,
      as.list = F
    ),
    regexp = "Usage of R6 data objects is deprecated"
  )
})

test_that("Test forward windows", {
  fake_codes_len <- 10;
  fake.codes <- function(x) sample(0:1, fake_codes_len, replace = T)
  codes <- paste("Codes", LETTERS[1:fake_codes_len], sep = "-")

  df.units <- data.frame(
    Name = rep(c("J", "Z"), 6)
  );
  df.conversation <- data.frame(
    Day = c(1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2)
  );
  df.codes <- data.frame(
    c1 = c(1, 1, 1, 1, 1, 0, 0, 1, 1, 0, 0, 1),
    c2 = c(1, 1, 1, 0, 0, 1, 0, 1, 0, 1, 0, 0),
    c3 = c(0, 0, 1, 0, 1, 0, 1, 0, 0, 0, 1, 0)
  );

  df_accum_inf_forward <- ena.accumulate.data(
    units = df.units, conversation = df.conversation, codes = df.codes,
    window.size.forward = Inf, window.size.back = 0, weight.by = "binary")

  expect_equal(as.numeric(as.matrix(
                df_accum_inf_forward$connection.counts)[1, ]),
                c(4, 6, 4))

  df_accum_forward <- ena.accumulate.data(
    units = df.units, conversation = df.conversation, codes = df.codes,
    window.size.forward = 5, window.size.back = 5, weight.by = "binary"
  );

  expect_equal(as.numeric(as.matrix(
                df_accum_forward$connection.counts)[1, ]),
                c(5, 6, 6))
})
