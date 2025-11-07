testthat::test_that(
  desc = "can_associate_log_layout",
  code = {

    testthat::capture_output_lines({
      init_logger()
    })

    new_log_layout(
      format = list(
        new_fmt_literal(crayon::bgCyan$black$bold, "Class:"),
        new_fmt_cls_field(crayon::cyan$bold, "cls_name"),
        new_fmt_literal(crayon::bgCyan$black$bold, "Object Id:"),
        new_fmt_cls_field(crayon::cyan$bold, "id"),
        new_fmt_line_break(),
        new_fmt_log_level(),
        new_fmt_timestamp(crayon::silver$italic),
        new_fmt_log_msg(),
        new_fmt_line_break(),
        new_fmt_metric(crayon::green$bold, "sysname"),
        new_fmt_metric(crayon::red$bold, "nodename"),
        new_fmt_literal(crayon::blue$bold, "R Version:"),
        new_fmt_metric(crayon::blue$italic$bold, "r_ver"),
        new_fmt_line_break()
      ),
      association = "TestObject"
    )

    expect_true(!is.null(log_layouts("TestObject")))

    test_obj <- TestObject$new()

    actual <- testthat::capture_output_lines({
      test_obj$invoke_logger("Logger")
    })

    expect_equal(length(actual), 4)

    expect_true(stringr::str_detect(actual[1], stringr::fixed("Object Id:")))
    expect_true(stringr::str_detect(actual[1], stringr::fixed(test_obj$identifier())))

    expect_true(stringr::str_detect(actual[1], stringr::fixed("Class:")))
    expect_true(stringr::str_detect(actual[1], stringr::fixed(test_obj$class_name())))

    expect_true(stringr::str_detect(actual[2], stringr::fixed("TRACE")))
    expect_true(stringr::str_detect(actual[2], stringr::fixed("test - 123 - 100")))

    expect_true(stringr::str_detect(actual[3], stringr::fixed(Sys.info()[["sysname"]])))
    expect_true(stringr::str_detect(actual[3], stringr::fixed(Sys.info()[["nodename"]])))
  }
)

testthat::test_that(
  desc = "can_associate_derived_log_layout",
  code = {

    testthat::capture_output_lines({
      init_logger()
    })

    new_log_layout(
      format = list(
        new_fmt_literal(crayon::bgCyan$black$bold, "Class:"),
        new_fmt_cls_field(crayon::cyan$bold, "cls_name"),
        new_fmt_literal(crayon::bgCyan$black$bold, "Object Id:"),
        new_fmt_cls_field(crayon::cyan$bold, "id"),
        new_fmt_line_break(),
        new_fmt_log_level(),
        new_fmt_timestamp(crayon::silver$italic),
        new_fmt_log_msg(),
        new_fmt_line_break(),
        new_fmt_metric(crayon::green$bold, "sysname"),
        new_fmt_metric(crayon::red$bold, "nodename"),
        new_fmt_literal(crayon::blue$bold, "R Version:"),
        new_fmt_metric(crayon::blue$italic$bold, "r_ver"),
        new_fmt_line_break()
      ),
      association = "TestObject"
    )

    expect_true(!is.null(log_layouts("TestObject")))
    expect_true(is.null(log_layouts("DerivedTestObject")))

    test_obj <- DerivedTestObject$new()

    actual <- capture_output_lines({
      test_obj$invoke_logger("Logger")
    })

    expect_equal(length(actual), 4)

    expect_true(stringr::str_detect(actual[1], stringr::fixed("Object Id:")))
    expect_true(stringr::str_detect(actual[1], stringr::fixed(test_obj$identifier())))

    expect_true(stringr::str_detect(actual[1], stringr::fixed("Class:")))
    expect_true(stringr::str_detect(actual[1], stringr::fixed(test_obj$class_name())))

    expect_true(stringr::str_detect(actual[2], stringr::fixed("TRACE")))
    expect_true(stringr::str_detect(actual[2], stringr::fixed("derived test - 321 - 200")))

    expect_true(stringr::str_detect(actual[3], stringr::fixed(Sys.info()[["sysname"]])))
    expect_true(stringr::str_detect(actual[3], stringr::fixed(Sys.info()[["nodename"]])))
  }
)
