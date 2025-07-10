suppressMessages(library(rENA, quietly = T, verbose = F))
context("Test making R6 sets");

code_names <- c("Data", "Technical.Constraints","Performance.Parameters",
  "Client.and.Consultant.Requests","Design.Reasoning","Collaboration")

test_that("Accumulate returns an R6", {
  data(RS.data)

  df.file <- RS.data

  df.accum <- suppressWarnings(
    ena.accumulate.data.file(
      df.file, units.by = c("UserName", "Condition"),
      conversations.by = c("ActivityNumber", "GroupName"),
      codes = code_names, as.list = FALSE)
  )

  testthat::expect_is(df.accum, "ENAdata",
    "Accumulation with as.list = FALSE did not return ENAdata"
  )
})

test_that("Function params includes ... args", {
  data(RS.data)

  df.file <- RS.data

  df.accum <- suppressWarnings(
    ena.accumulate.data.file(
      df.file, units.by = c("UserName", "Condition"),
      conversations.by = c("ActivityNumber", "GroupName"),
      codes = code_names, as.list = FALSE)
  )

  df.accum.grain <- suppressWarnings(
    ena.accumulate.data.file(
      df.file, units.by = c("UserName", "Condition"),
      conversations.by = c("ActivityNumber", "GroupName"),
      codes = code_names, as.list = FALSE, grainSize = 10)
  )

  testthat::expect_false("grainSize" %in% names(df.accum$function.params))
  testthat::expect_true("grainSize" %in% names(df.accum.grain$function.params))
})

test_that("Old accum ignored meta.data", {
  data(RS.data)

  df.file <- RS.data

  df.accum <- suppressWarnings(
    ena.accumulate.data.file(
      df.file, units.by = c("UserName", "Condition"),
      conversations.by = c("ActivityNumber", "GroupName"),
      codes = code_names, as.list = FALSE,
      include.meta = FALSE
    )
  )
  testthat::expect_equal(nrow(df.accum$metadata), 0)
})

# test_that("Old accum reads a file string", {
#   data(RS.data)
#
#   df.file.path <- system.file("extdata", "rs.data.csv", package = "rENA")
#   if(!file.exists(df.file.path)) {
#     df.file.path = "../../../rENA/inst/extdata/rs.data.csv"
#   }
#   if(!file.exists(df.file.path)) {
#     df.file.path = "../../../../inst/extdata/rs.data.csv"
#   }
#   cat("WD: ", getwd(), "\n")
#   list.files()
#   cat("PATH: ", df.file.path, "\n")
#   cat("EXISTS: ", file.exists(df.file.path), "\n")
#   df.accum <- suppressWarnings(
#     rENA:::ena.accumulate.data.file(
#       df.file.path, units.by = c("UserName", "Condition"),
#       conversations.by = c("ActivityNumber", "GroupName"),
#       codes = code_names, as.list = FALSE
#     )
#   )
#
#   testthat::expect_is(df.accum, "ENAdata",
#     "Accumulation with file path did not return ENAdata"
#   )
# })

test_that("Make.set returns an R6", {
  data(RS.data)

  df.file <- RS.data

  df.accum <- suppressWarnings(
    rENA:::ena.accumulate.data.file(
      df.file, units.by = c("UserName", "Condition"),
      conversations.by = c("ActivityNumber", "GroupName"),
      codes = code_names, as.list = FALSE
    )
  )
  df.set <- suppressWarnings(
    rENA:::ena.make.set(df.accum, as.list = FALSE)
  )

  testthat::expect_is(df.set, "ENAset",
    "Set with as.list = FALSE did not return ENAset")

  df.accum2 <- rENA:::ena.accumulate.data.file(
    df.file, units.by = c("UserName", "Condition"),
    conversations.by = c("ActivityNumber", "GroupName"),
    codes = code_names, as.list = T
  )
  error_set <- testthat::expect_error(
    suppressWarnings(rENA:::ena.make.set(df.accum2, as.list = F)),
    regexp = "Re-run the accumulation with as.list=FALSE"
  )

  error_set2 <- testthat::expect_warning(
    rENA:::ena.make.set(df.accum, as.list = T),
    regexp = "ENAdata objects will be deprecated"
  )
})

test_that("Old sets are the same as the new ones", {
  data(RS.data)

  units.by <- c("UserName", "Condition")
  conv.by <- c("Condition", "GroupName")

  df.accum <- suppressWarnings(
    rENA:::ena.accumulate.data.file(
      RS.data, units.by = units.by,
      conversations.by = conv.by,
      codes = code_names, as.list = FALSE, window.size.back = 4
    )
  )

  df.set <- suppressWarnings(
    rENA:::ena.make.set(df.accum, as.list = FALSE)
  )

  new.set <- rENA:::ena.accumulate.data(
          units = RS.data[, units.by],
          conversation = RS.data[, conv.by],
          metadata = RS.data[, code_names],
          codes = RS.data[,code_names],
          model = "EndPoint",
          window.size.back = 4
        ) %>%
          rENA:::ena.make.set()

  testthat::expect_equivalent(df.set$points.rotated[1, ],
    as.matrix(new.set$points)[1, ])
  testthat::expect_equivalent(df.set$line.weights[1, ],
    as.matrix(new.set$line.weights)[1, ])
})


test_that("Old R6 w custom rotation", {
  data(RS.data)

  df.file <- RS.data

  df.accum <- suppressWarnings(
    rENA:::ena.accumulate.data.file(
      df.file, units.by = c("UserName", "Condition"),
      conversations.by = c("ActivityNumber", "GroupName"),
      codes = code_names, as.list = FALSE
    )
  )
  df.set <- suppressWarnings(
    rENA:::ena.make.set(df.accum, as.list = FALSE)
  )

  df.accum.2 <- suppressWarnings(
    rENA:::ena.accumulate.data.file(
      df.file, units.by = c("GroupName", "Condition"),
      conversations.by = c("ActivityNumber", "GroupName"),
      codes = code_names, as.list = FALSE
    )
  )
  df.set.2 <- suppressWarnings(
    rENA:::ena.make.set(df.accum.2, as.list = FALSE, rotation.set = df.set$rotation.set)
  )
  testthat::expect_equal(df.set$node.positions, df.set.2$node.positions)

  testthat::expect_error(
    suppressWarnings(rENA:::ena.make.set(df.accum.2, as.list = FALSE, rotation.set = -1)),
    regexp = "Supplied rotation.set is not an instance of ENARotationSet"
  )
  testthat::expect_error(
    suppressWarnings(rENA:::ena.make.set(df.accum.2, as.list = FALSE, rotation.by = "NOTHING")),
    regexp = "Unable to find or create a rotation set"
  )

  testthat::expect_error(
    suppressWarnings(rENA:::ena.make.set(df.accum.2, as.list = FALSE, node.position.method = function(set) {
      return(list("failed" = NULL))
    })),
    regexp = "node position method didn't return back the expected objects"
  )


  # testthat::expect_error(
  #   rENA:::ena.make.set(df.accum.2, as.list = FALSE, rotation.by = function(set, nothing) {
  #     return(
  #       list(
  #         "rotation" = matrix(rep(0, choose(length(code_names),2) ^ 2 ), nrow = choose(length(code_names),2)),
  #         "node.positions" = NULL
  #       )
  #     )
  #   }),
  #   regexp = "node position method didn't return back the expected objects"
  # )


  testthat::expect_error(
    suppressWarnings(rENA:::ena.make.set(df.accum.2, as.list = FALSE, rotation.set = -1)),
    regexp = "Supplied rotation.set is not an instance of ENARotationSet"
  )

  rot.set <- list(
    "rotation" = matrix(rep(0, choose(length(code_names),2) ^ 2 ), nrow = choose(length(code_names),2)),
    "node.positions" = NULL
  )
  class(rot.set) <- c("ENARotationSet")
  testthat::expect_error(
    suppressWarnings(rENA:::ena.make.set(df.accum.2, as.list = FALSE, rotation.set = rot.set)),
    regexp = "Unable to determine the node positions either by calculating"
  )
})

test_that("Verify ENArotation set class", {
   data(RS.data)

  df.file <- RS.data

  df.accum <- suppressWarnings(
    rENA:::ena.accumulate.data.file(
      df.file, units.by = c("UserName", "Condition"),
      conversations.by = c("ActivityNumber", "GroupName"),
      codes = code_names,
      as.list = FALSE
    )
  )
  df.set <- suppressWarnings(
    rENA:::ena.make.set(df.accum, as.list = FALSE)
  )

  nodes <- df.set$node.positions
  rownames(nodes) <- NULL
  rotationSet = ENARotationSet$new(
    rotation = df.set$rotation.set$rotation,
    codes = df.set$codes,
    node.positions = nodes,
    eigenvalues = NULL
  )
  testthat::expect_true(all(rownames(rotationSet$node.positions) == df.set$codes))
})
