test_that("ecm_mars() aborts when the workbook does not exist", {
  missing <- ec_missing_workbook()
  expect_false(file.exists(missing))
  expect_error(
    ecm_mars(data_path = missing,
             circ_vars = ec_circ_dotted,
             prod_vars = ec_prod_dotted,
             parallel_enable = FALSE)
  )
})

test_that("the workbook width check accepts exactly one column per name", {
  names14 <- paste0("v", 1:14)
  table_of <- function(n) as.data.frame(matrix(0, nrow = 2L, ncol = n))
  expect_invisible(EconCausal:::check_workbook_width(table_of(14L), names14))
  expect_error(EconCausal:::check_workbook_width(table_of(13L), names14),
               "must have exactly 14 columns", fixed = TRUE)
  expect_error(EconCausal:::check_workbook_width(table_of(15L), names14),
               "must have exactly 14 columns", fixed = TRUE)
})

test_that("ecm_mars() rejects a workbook of the wrong width before fitting", {
  # The shipped readxl workbook has eleven columns. Without the check the call
  # would run to the end and return a table of missing metrics.
  expect_error(
    ecm_mars(data_path = ec_foreign_workbook(),
             circ_vars = ec_circ_dotted,
             prod_vars = ec_prod_dotted,
             parallel_enable = FALSE),
    "must have exactly 14 columns",
    fixed = TRUE
  )
})

# The test below starts from a state the function never installs by itself, so
# a missing on.exit() is visible: the function leaves a plain sequential plan,
# which differs from the marked starting state.

test_that("ecm_mars() restores the future plan after failing", {
  previous <- future::plan(future::sequential, split = TRUE)
  on.exit(future::plan(previous), add = TRUE)
  before <- future::plan("list")
  expect_error(
    ecm_mars(data_path = ec_missing_workbook(),
             circ_vars = ec_circ_dotted,
             prod_vars = ec_prod_dotted,
             parallel_enable = FALSE)
  )
  expect_equal(future::plan("list"), before)
})

# Thread control. The deterministic tests replace thread_controller(), the only binding that
# reaches RhpcBLASctl, with an in-memory controller, so they exercise the save, limit and
# restore logic on every machine, whatever BLAS and OpenMP it has. BLAS starts at 2 and OpenMP
# at 3, so a restore that swapped the channels would be visible.

test_that("ecm_mars() limits both thread counts and restores them after failing", {
  fake <- ec_fake_thread_controller(blas = 2L, omp = 3L)
  local_mocked_bindings(thread_controller = function() fake$controller)
  expect_error(
    ecm_mars(data_path = ec_missing_workbook(),
             circ_vars = ec_circ_dotted,
             prod_vars = ec_prod_dotted,
             parallel_enable = FALSE)
  )
  expect_identical(fake$calls(), c("get_blas", "set_blas(1)", "get_omp", "set_omp(1)",
                                   "set_omp(3)", "set_blas(2)"))
  expect_identical(fake$state(), c(blas = 2L, omp = 3L))
})

test_that("ecm_mars() runs without touching threads when RhpcBLASctl is absent", {
  local_mocked_bindings(thread_controller = function() NULL)
  expect_error(
    ecm_mars(data_path = ec_missing_workbook(),
             circ_vars = ec_circ_dotted,
             prod_vars = ec_prod_dotted,
             parallel_enable = FALSE)
  )
  expect_null(EconCausal:::limit_threads(NULL)())
})

test_that("a thread count that cannot be read is neither changed nor restored", {
  fake <- ec_fake_thread_controller(blas = 2L, omp = NA_integer_)
  restore <- EconCausal:::limit_threads(fake$controller)
  expect_identical(fake$state(), c(blas = 1L, omp = NA_integer_))
  restore()
  expect_identical(fake$calls(), c("get_blas", "set_blas(1)", "get_omp", "set_blas(2)"))
  expect_identical(fake$state(), c(blas = 2L, omp = NA_integer_))
})

test_that("only positive whole numbers count as restorable thread counts", {
  expect_true(EconCausal:::is_thread_count(1L))
  expect_true(EconCausal:::is_thread_count(8))
  expect_false(EconCausal:::is_thread_count(NA_integer_))
  expect_false(EconCausal:::is_thread_count(0L))
  expect_false(EconCausal:::is_thread_count(1.5))
  expect_false(EconCausal:::is_thread_count(c(1L, 2L)))
  expect_false(EconCausal:::is_thread_count(NULL))
})

test_that("an error while limiting restores every channel it may have changed", {
  fake <- ec_fake_thread_controller(blas = 2L, omp = 3L)
  fake$fail_next("set_omp", applied = TRUE)
  expect_error(EconCausal:::limit_threads(fake$controller), "set_omp failed", fixed = TRUE)
  expect_identical(fake$state(), c(blas = 2L, omp = 3L))
})

test_that("a failed restore of one channel still restores the other", {
  fake <- ec_fake_thread_controller(blas = 2L, omp = 3L)
  restore <- EconCausal:::limit_threads(fake$controller)
  fake$fail_next("set_omp")
  expect_warning(restore(), "Could not restore the OpenMP thread count", fixed = TRUE)
  expect_identical(fake$state(), c(blas = 2L, omp = 1L))
})

# Round trip against the real RhpcBLASctl, one channel per test. Each test first checks that a
# value set through RhpcBLASctl reads back, which fails on the reference BLAS and when
# RhpcBLASctl was built without OpenMP; in that case there is no state to restore and the test
# is skipped. A read-back shows the setting is held, not how many threads a computation uses.

test_that("ecm_mars() restores the real BLAS thread count after failing", {
  skip_if_not_installed("RhpcBLASctl")
  previous <- RhpcBLASctl::blas_get_num_procs()
  on.exit(RhpcBLASctl::blas_set_num_threads(previous), add = TRUE)
  RhpcBLASctl::blas_set_num_threads(2L)
  if (!identical(RhpcBLASctl::blas_get_num_procs(), 2L)) {
    skip("the BLAS in use does not report a thread count set through RhpcBLASctl")
  }
  expect_error(
    ecm_mars(data_path = ec_missing_workbook(),
             circ_vars = ec_circ_dotted,
             prod_vars = ec_prod_dotted,
             parallel_enable = FALSE)
  )
  expect_identical(RhpcBLASctl::blas_get_num_procs(), 2L)
})

test_that("ecm_mars() restores the real OpenMP thread count after failing", {
  skip_if_not_installed("RhpcBLASctl")
  previous <- RhpcBLASctl::omp_get_max_threads()
  if (!EconCausal:::is_thread_count(previous)) {
    skip("RhpcBLASctl was built without OpenMP")
  }
  on.exit(RhpcBLASctl::omp_set_num_threads(previous), add = TRUE)
  RhpcBLASctl::omp_set_num_threads(2L)
  if (!identical(RhpcBLASctl::omp_get_max_threads(), 2L)) {
    skip("the OpenMP thread count set through RhpcBLASctl does not read back")
  }
  expect_error(
    ecm_mars(data_path = ec_missing_workbook(),
             circ_vars = ec_circ_dotted,
             prod_vars = ec_prod_dotted,
             parallel_enable = FALSE)
  )
  expect_identical(RhpcBLASctl::omp_get_max_threads(), 2L)
})
