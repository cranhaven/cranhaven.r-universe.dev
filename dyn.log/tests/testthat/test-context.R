testthat::test_that(
  desc = "basic_case",
  code = {

    func <- ".inner(d, e, f)"
    actual <- extract_func_name(func)

    expect_equal(actual, ".inner")
  }
)

testthat::test_that(
  desc = "obj_init",
  code = {
    func <- "Test$new(d, e, f)"
    actual <- extract_func_name(func)

    expect_equal(actual, "Test$new")
  }
)

testthat::test_that(
  desc = "has_system_metrics",
  code = {
    context <- sys_context()

    expect_named(context["sysname"])
    expect_gt(nchar(context["sysname"]), 0)

    expect_named(context["release"])
    expect_gt(nchar(context["release"]), 0)

    expect_named(context["version"])
    expect_gt(nchar(context["version"]), 0)

    expect_named(context["nodename"])
    expect_gt(nchar(context["nodename"]), 0)

    expect_named(context["machine"])
    expect_gt(nchar(context["machine"]), 0)

    expect_named(context["login"])
    expect_gt(nchar(context["login"]), 0)

    expect_named(context["user"])
    expect_gt(nchar(context["user"]), 0)

    expect_named(context["r_ver"])
    expect_gt(nchar(context["r_ver"]), 0)
  }
)

testthat::test_that(
  desc = "call_stack_output_no_args",
  code = {
    test <- function(a, b, c) {
      wrapper <- function(x, y, z) {
        outer <- function(d, e, f) {
          inner <- function(g, h, i) {
            get_call_stack(keep_args = FALSE,
                           filter_internal = FALSE)
          }

          inner(d, e, f)
        }

        outer(x, y, z)
      }
      wrapper(a, b, c)
    }

    call_stack <- test(1, 2, 3)

    expect_true(stringr::str_detect(call_stack[[1]], "test"))
    expect_true(stringr::str_detect(call_stack[[2]], "wrapper"))
    expect_true(stringr::str_detect(call_stack[[3]], "outer"))
    expect_true(stringr::str_detect(call_stack[[4]], "inner"))
  }
)

testthat::test_that(
  desc = "call_stack_output_with_args",
  code = {
    test <- function(a, b, c) {
      wrapper <- function(x, y, z) {
        outer <- function(d, e, f) {
          inner <- function(g, h, i) {
            get_call_stack(keep_args = TRUE,
                           filter_internal = FALSE)
          }

          inner(d, e, f)
        }

        outer(x, y, z)
      }
      wrapper(a, b, c)
    }

    call_stack <- test(1, 2, 3)

    expect_true(stringr::str_detect(call_stack[[1]], pattern = stringr::fixed("test(1, 2, 3)")))
    expect_true(stringr::str_detect(call_stack[[2]], pattern = stringr::fixed("wrapper(a, b, c)")))
    expect_true(stringr::str_detect(call_stack[[3]], pattern = stringr::fixed("outer(x, y, z)")))
    expect_true(stringr::str_detect(call_stack[[4]], pattern = stringr::fixed("inner(d, e, f)")))
  }
)

testthat::test_that(
  desc = "execution_context_works",
  code = {
    test <- function(a, b, c) {
      wrapper <- function(x, y, z) {
        outer <- function(d, e, f) {
          inner <- function(g, h, i) {
            exec_context(filter_internal = FALSE)
          }

          inner(d, e, f)
        }

        outer(x, y, z)
      }
      wrapper(a, b, c)
    }

    exec_contet <- test(1, 2, 3)

    expect_equal(class(exec_contet), c("exec_context", "context"))

    expect_equal(length(exec_contet$call_stack), 6)

    expect_true(stringr::str_detect(exec_contet$call_stack[[1]], "test"))
    expect_true(stringr::str_detect(exec_contet$call_stack[[2]], "wrapper"))
    expect_true(stringr::str_detect(exec_contet$call_stack[[3]], "outer"))
    expect_true(stringr::str_detect(exec_contet$call_stack[[4]], "inner"))

    expect_equal(exec_contet$ncalls, 6)
  }
)

testthat::test_that(
  desc = "internal_calls_get_filtered",
  code = {
    call_stack <- c(call_1 = "global::test",
                    call_2 = "wrapper",
                    call_3 = "outer",
                    call_4 = "inner",
                    call_5 = "dyn.log::exec_context",
                    call_6 = "dyn.log:::get_call_stack")

    actual <- clean_internal_calls(call_stack)

    expected <- c(call_1 = "global::test",
                    call_2 = "wrapper",
                    call_3 = "outer",
                    call_4 = "inner")

    expect_equal(actual, expected)
  }
)

testthat::test_that(
  desc = "internal_calls_get_filter_correctly",
  code = {
    call_stack <- c(call_1 = "global::test",
                    call_2 = "wrapper",
                    call_3 = "outer",
                    call_4 = "inner")

    actual <- clean_internal_calls(call_stack)

    expected <- c(call_1 = "global::test",
                  call_2 = "wrapper",
                  call_3 = "outer",
                  call_4 = "inner")

    expect_equal(actual, expected)
  }
)

testthat::test_that(
  desc = "derived_class_scope_works",
  code = {
    obj <- DerivedTestObject$new()

    scope <- class_scope(obj)

    expect_equal(length(scope), 2)

    # private field
    expect_equal(scope$id, obj$.__enclos_env__$private$id)

    # public field
    expect_equal(scope$cls_name, obj$cls_name)
  }
)
