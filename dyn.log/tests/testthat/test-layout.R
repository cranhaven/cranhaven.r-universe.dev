test_that("can_create_layout", {

  log_layout <- new_log_layout(
    format = list(
      new_fmt_metric(crayon::green$bold, "sysname"),
      new_fmt_metric(crayon::yellow$bold, "release"),
      new_fmt_line_break(),
      new_fmt_log_level(),
      new_fmt_timestamp(crayon::silver$italic),
      new_fmt_exec_scope(crayon::magenta$bold, "top_call"),
      new_fmt_literal(crayon::blue$italic, "literal text"),
      new_fmt_log_msg(),
      new_fmt_line_break(),
      new_fmt_exec_scope(crayon::cyan$bold, "call_stack")
    )
  )

  expect_true(!is.null(log_layout))
  expect_equal(class(log_layout), "log_layout")
  expect_equal(length(log_layout), 10)
})

test_that("log_layout_format", {

  log_layout <- new_log_layout(
    format = list(
      new_fmt_metric(crayon::green$bold, "sysname"),
      new_fmt_metric(crayon::yellow$bold, "release"),
      new_fmt_line_break(),
      new_fmt_log_level(),
      new_fmt_timestamp(crayon::silver$italic),
      new_fmt_exec_scope(crayon::magenta$bold, "top_call"),
      new_fmt_literal(crayon::blue$italic, "literal text"),
      new_fmt_log_msg(),
      new_fmt_line_break(),
      new_fmt_exec_scope(crayon::cyan$bold, "call_stack")
    ),
    association = "test-layout-format"
  )

  layouts <- log_layouts()

  expect_true(!is.null(log_layout))
  expect_equal(class(log_layout), "log_layout")

  with(log_layout_detail(log_layout), {
    expect_equal(length(formats), 10)
    expect_equal(length(types), 8)
    expect_equal(seperator, " ")
  })
})

test_that("log_layout_format_types", {

  log_layout <- new_log_layout(
    format = list(
      new_fmt_metric(crayon::green$bold, "sysname"),
      new_fmt_metric(crayon::yellow$bold, "release"),
      new_fmt_line_break(),
      new_fmt_log_level(),
      new_fmt_timestamp(crayon::silver$italic),
      new_fmt_exec_scope(crayon::magenta$bold, "top_call"),
      new_fmt_literal(crayon::blue$italic, "literal text"),
      new_fmt_log_msg(),
      new_fmt_line_break(),
      new_fmt_exec_scope(crayon::cyan$bold, "call_stack")
    ),
    seperator = "-",
    association = "test-layout-format"
  )

  with(log_layout_detail(log_layout), {
    expect_gt(which(!is.na(match(types, "fmt_metric"))), 0)
    expect_gt(which(!is.na(match(types, "fmt_layout"))), 0)
    expect_gt(which(!is.na(match(types, "fmt_newline"))), 0)
    expect_gt(which(!is.na(match(types, "fmt_log_level"))), 0)
    expect_gt(which(!is.na(match(types, "fmt_timestamp"))), 0)
    expect_gt(which(!is.na(match(types, "fmt_literal"))), 0)
    expect_gt(which(!is.na(match(types, "fmt_log_msg"))), 0)

    expect_equal(seperator, "-")
  })
})

test_that("no_association_is_null", {

  no_association <- log_layouts("missing_association")

  expect_true(is.null(no_association))
})

test_that("log_layout_evaluates_simple_singleline", {

  log_layout <- new_log_layout(
    format = list(
      new_fmt_timestamp(crayon::silver$italic)
    )
  )

  expect_true(!is.null(log_layout))

  context <- list()

  detail <- log_layout_detail(log_layout)

  expect_true(!is.null(detail))
  expect_equal(length(detail$formats), 1)
  expect_equal(length(detail$types), 2)

  actual <- evaluate_layout(detail, context)

  expect_true(stringr::str_detect(actual, pattern = format(Sys.Date(), "%x")))
})

test_that("log_layout_evaluates_simple_multiline", {

  log_layout <- new_log_layout(
    format = list(
      new_fmt_timestamp(crayon::silver$italic),
      new_fmt_line_break(),
      new_fmt_log_msg()
    )
  )

  expect_true(!is.null(log_layout))

  context <- list()

  detail <- log_layout_detail(log_layout)

  expect_true(!is.null(detail))
  expect_equal(length(detail$formats), 3)
  expect_equal(length(detail$types), 4)

  actual <- evaluate_layout(detail, context)

  expect_true(stringr::str_detect(actual, pattern = format(Sys.Date(), "%x")))
})

test_that("log_layout_evaluates_metrics_singleline", {

  log_layout <- new_log_layout(
    format = list(
      new_fmt_metric(crayon::green$bold, "sysname"),
      new_fmt_metric(crayon::red$bold, "release"),
      new_fmt_timestamp(crayon::silver$italic)
    )
  )

  expect_true(!is.null(log_layout))


  detail <- log_layout_detail(log_layout)

  expect_true(!is.null(detail))
  expect_equal(length(detail$formats), 3)
  expect_equal(length(detail$types), 3)

  context <- list(fmt_metric = sys_context())

  actual <- evaluate_layout(detail, context)

  expect_true(stringr::str_detect(actual, pattern = format(Sys.Date(), "%x")))

  expect_true(stringr::str_detect(actual, pattern = stringr::fixed(Sys.info()[["sysname"]])))
  expect_true(stringr::str_detect(actual, pattern = stringr::fixed(Sys.info()[["release"]])))
})

test_that("log_layout_evaluates_metrics_multiline", {

  log_layout <- new_log_layout(
    format = list(
      new_fmt_metric(crayon::green$bold, "sysname"),
      new_fmt_metric(crayon::red$bold, "release"),
      new_fmt_line_break(),
      new_fmt_log_level(),
      new_fmt_timestamp(crayon::silver$italic),
      new_fmt_log_msg()
    ),
    association = "default"
  )

  expect_true(!is.null(log_layout))

  detail <- log_layout_detail(log_layout)

  expect_true(!is.null(detail))
  expect_equal(length(detail$formats), 6)
  expect_equal(length(detail$types), 6)

  context <- list(fmt_metric = sys_context())

  actual <- evaluate_layout(detail, context)

  expect_true(stringr::str_detect(actual, pattern = format(Sys.Date(), "%x")))

  expect_true(stringr::str_detect(actual, pattern = stringr::fixed(Sys.info()[["sysname"]])))
  expect_true(stringr::str_detect(actual, pattern = stringr::fixed(Sys.info()[["release"]])))
})

test_that("log_layout_evaluates_cls_attributes_multiline", {

  test_obj <- TestObject$new()

  log_layout <- new_log_layout(
    format = list(
      new_fmt_metric(crayon::green$bold, "sysname"),
      new_fmt_metric(crayon::red$bold, "release"),
      new_fmt_line_break(),
      new_fmt_log_level(),
      new_fmt_timestamp(crayon::silver$italic),
      new_fmt_log_msg(),
      new_fmt_line_break(),
      new_fmt_literal(crayon::bgCyan$bold, "Object Id:"),
      new_fmt_cls_field(crayon::cyan$bold, "id")
    )
  )

  expect_true(!is.null(log_layout))

  detail <- log_layout_detail(log_layout)

  expect_true(!is.null(detail))
  expect_equal(length(detail$formats), 9)
  expect_equal(length(detail$types), 8)

  cls_scope <- class_scope(test_obj)

  context <- list(fmt_metric = sys_context(),
                  fmt_cls_field = cls_scope)

  actual <- evaluate_layout(detail, context)

  expect_true(stringr::str_detect(actual, pattern = format(Sys.Date(), "%x")))

  expect_true(stringr::str_detect(actual, pattern = stringr::fixed(Sys.info()[["sysname"]])))
  expect_true(stringr::str_detect(actual, pattern = stringr::fixed(Sys.info()[["release"]])))

  expect_true(stringr::str_detect(actual, pattern = stringr::fixed("Object Id:")))
  expect_true(stringr::str_detect(actual, pattern = stringr::fixed(test_obj$identifier())))
})

test_that("multi_line_fmt_works_2", {

  log_layout <- new_log_layout(
    format = list(
      new_fmt_metric(crayon::red$bold, "sysname"),
      new_fmt_metric(crayon::green$bold, "release"),
      new_fmt_metric(crayon::blue$bold, "version"),
      new_fmt_line_break(),
      new_fmt_literal(crayon::red$italic, "literal1"),
      new_fmt_literal(crayon::green$italic, "literal2"),
      new_fmt_literal(crayon::blue$italic, "literal3")
    ),
    seperator = "-")

  detail <- log_layout_detail(log_layout)

  expect_true(!is.null(detail))
  expect_equal(length(detail$formats), 7)
  expect_equal(length(detail$types), 4)

  context <- list(fmt_metric = sys_context())

  actual <- evaluate_layout(detail, context)

  expected_metrics <- paste(Sys.info()[["sysname"]],
                            Sys.info()[["release"]],
                            Sys.info()[["version"]],
                            sep = "-")

  expect_true(stringr::str_detect(actual, stringr::fixed(expected_metrics)))

  expected_literals <- paste("literal1",
                             "literal2",
                             "literal3",
                             sep = "-")

  expect_true(stringr::str_detect(actual, stringr::fixed(expected_literals)))

  output <- capture_output_lines({
    cat(actual)
  })

  expect_equal(length(output), 2)
})

test_that("multi_line_fmt_works_3", {

  log_layout <- new_log_layout(
    format = list(
      new_fmt_metric(crayon::red$bold, "sysname"),
      new_fmt_metric(crayon::green$bold, "release"),
      new_fmt_metric(crayon::blue$bold, "version"),
      new_fmt_line_break(),
      new_fmt_literal(crayon::red$italic, "literal1"),
      new_fmt_literal(crayon::green$italic, "literal2"),
      new_fmt_literal(crayon::blue$italic, "literal3"),
      new_fmt_line_break(),
      new_fmt_metric(crayon::red$bold, "machine"),
      new_fmt_metric(crayon::green$bold, "nodename"),
      new_fmt_metric(crayon::blue$bold, "user")
    ),
    seperator = "---")

  detail <- log_layout_detail(log_layout)

  expect_true(!is.null(detail))
  expect_equal(length(detail$formats), 11)
  expect_equal(length(detail$types), 4)

  context <- list(fmt_metric = sys_context())
  actual <- evaluate_layout(detail, context)

  expected_metrics <- paste(Sys.info()[["sysname"]],
                            Sys.info()[["release"]],
                            Sys.info()[["version"]],
                            sep = "---")

  expect_true(stringr::str_detect(actual, stringr::fixed(expected_metrics)))

  expected_literals <- paste("literal1",
                             "literal2",
                             "literal3",
                             sep = "---")

  expect_true(stringr::str_detect(actual, stringr::fixed(expected_literals)))

  expected_metrics_2 <- paste(Sys.info()[["machine"]],
                              Sys.info()[["nodename"]],
                              Sys.info()[["user"]],
                              sep = "---")

  expect_true(stringr::str_detect(actual, stringr::fixed(expected_metrics_2)))

  output <- capture_output_lines({
    cat(actual)
  })

  expect_equal(length(output), 3)
})

test_that("multi_line_fmt_works_4", {

  log_layout <- new_log_layout(
    format = list(
      new_fmt_metric(crayon::red$bold, "sysname"),
      new_fmt_metric(crayon::green$bold, "release"),
      new_fmt_metric(crayon::blue$bold, "version"),
      new_fmt_line_break(),
      new_fmt_literal(crayon::red$italic, "literal1"),
      new_fmt_literal(crayon::green$italic, "literal2"),
      new_fmt_literal(crayon::blue$italic, "literal3"),
      new_fmt_line_break(),
      new_fmt_metric(crayon::red$bold, "machine"),
      new_fmt_metric(crayon::green$bold, "nodename"),
      new_fmt_metric(crayon::blue$bold, "user"),
      new_fmt_line_break(),
      new_fmt_literal(crayon::red$italic, "literal4"),
      new_fmt_literal(crayon::green$italic, "literal5"),
      new_fmt_literal(crayon::blue$italic, "literal6")
    ),
    seperator = "---")

  detail <- log_layout_detail(log_layout)

  expect_true(!is.null(detail))
  expect_equal(length(detail$formats), 15)
  expect_equal(length(detail$types), 4)

  context <- list(fmt_metric = sys_context())
  actual <- evaluate_layout(detail, context)

  expected_metrics <- paste(Sys.info()[["sysname"]],
                            Sys.info()[["release"]],
                            Sys.info()[["version"]],
                            sep = "---")

  expect_true(stringr::str_detect(actual, stringr::fixed(expected_metrics)))

  expected_literals <- paste("literal1",
                             "literal2",
                             "literal3",
                             sep = "---")

  expect_true(stringr::str_detect(actual, stringr::fixed(expected_literals)))

  expected_metrics_2 <- paste(Sys.info()[["machine"]],
                              Sys.info()[["nodename"]],
                              Sys.info()[["user"]],
                              sep = "---")

  expect_true(stringr::str_detect(actual, stringr::fixed(expected_metrics_2)))

  expected_literals_2 <- paste("literal4",
                               "literal5",
                               "literal6",
                               sep = "---")

  expect_true(stringr::str_detect(actual, stringr::fixed(expected_literals_2)))

  output <- capture_output_lines({
    cat(actual)
  })

  expect_equal(length(output), 4)
})
