test_that("get_free_ram_mb() never errors and returns NA or a positive number", {
  result <- get_free_ram_mb()
  expect_true(is.na(result) || (is.numeric(result) && result > 0))
})

test_that("vm_stat page size is read from the header, not assumed", {
  # Captured from an Apple Silicon host, where pages are 16K. Assuming the 4K
  # Intel page size here would under-report available RAM by a factor of four
  # and switch memory_mode = "auto" to lazy far below the intended threshold.
  vm_16k <- c(
    "Mach Virtual Memory Statistics: (page size of 16384 bytes)",
    "Pages free:                                  1222810.",
    "Pages active:                                 915158.",
    "Pages inactive:                               861249.",
    "Pages speculative:                              7678.",
    "Pages wired down:                             163857."
  )
  expect_equal(vm_stat_page_size(vm_16k), 16384)

  vm_4k <- sub("16384", "4096", vm_16k)
  expect_equal(vm_stat_page_size(vm_4k), 4096)

  # Same page counts, four times the page size, four times the memory.
  expect_equal(
    vm_stat_available_mb(vm_16k, 16384),
    4 * vm_stat_available_mb(vm_4k, 4096)
  )
})

test_that("vm_stat available memory counts reclaimable pages", {
  vm <- c(
    "Mach Virtual Memory Statistics: (page size of 16384 bytes)",
    "Pages free:                                     1000.",
    "Pages inactive:                                 2000.",
    "Pages speculative:                               500."
  )
  # (1000 + 2000 + 500) pages * 16384 B = 57.34 MB
  expect_equal(vm_stat_available_mb(vm, 16384), 3500 * 16384 / 1024^2)

  # Absent page classes are treated as zero rather than poisoning the total.
  vm_free_only <- vm[c(1, 2)]
  expect_equal(vm_stat_available_mb(vm_free_only, 16384), 1000 * 16384 / 1024^2)

  # No free-page line at all is "unknown", so callers fall back to the fixed
  # threshold instead of acting on a wrong number.
  expect_true(is.na(vm_stat_available_mb(vm[c(1, 3)], 16384)))
})

test_that("estimate_dense_matrix_mb() uses double arithmetic (no integer overflow)", {
  # 50000 x 50000 as integer multiplication would overflow .Machine$integer.max;
  # as.numeric() before multiplying keeps this correct at any realistic scale.
  mb <- estimate_dense_matrix_mb(50000, 50000)
  expect_true(is.finite(mb))
  expect_gt(mb, 0)
})

test_that("estimate_dense_solve_mb() covers the peak a dense solve was measured at", {
  # The guard decides whether a dense solve fits, so its estimate has to cover
  # the solve and not the matrix. These are the shapes and peaks the paper's
  # memory benchmark measured: one fresh R session per arm, peak resident set
  # read from outside against an idle session that loaded the same packages.
  # Estimating the matrix alone put the 20,000-unit figure at 2,845 MB against
  # a solve that peaked at 6,256 MB, which is the under-warning this guards.
  measured <- data.frame(
    n_left   = c(1667,  3333,  6667),
    n_right  = c(3333,  6667, 13333),
    peak_mb  = c(464.3, 1280.2, 6255.7)
  )
  for (i in seq_len(nrow(measured))) {
    expect_gte(
      estimate_dense_solve_mb(measured$n_left[i], measured$n_right[i]),
      measured$peak_mb[i]
    )
  }

  # A solve costs strictly more than the matrix it runs on, at every shape.
  expect_gt(estimate_dense_solve_mb(6667, 13333),
            estimate_dense_matrix_mb(6667, 13333))
})

test_that("resolve_memory_mode() reads the solve estimate, not the matrix", {
  testthat::local_mocked_bindings(
    get_free_ram_mb = function() 1000,
    .package = "couplr"
  )
  # The switch fires when the estimate passes half the available RAM, which the
  # mock fixes at 500 MB. At 3000 x 4000 the matrix estimate is 384 MB and the
  # solve estimate 1,152 MB, so the threshold falls between them: a guard reading
  # the matrix leaves this dense, and one reading the solve moves it to lazy.
  limit_mb <- 0.5 * 1000
  expect_lt(estimate_dense_matrix_mb(3000, 4000), limit_mb)
  expect_gt(estimate_dense_solve_mb(3000, 4000), limit_mb)

  expect_warning(
    mode <- resolve_memory_mode(3000, 4000, "auto", solver_supports_lazy = TRUE),
    "Switching to memory_mode"
  )
  expect_equal(mode, "lazy")
})

test_that("resolve_memory_mode(): small problems never probe RAM", {
  # A mock that errors if called proves the cheap cell-count short-circuit
  # actually skips the probe for ordinary-sized problems.
  testthat::local_mocked_bindings(
    get_free_ram_mb = function() stop("should not be called for small problems"),
    .package = "couplr"
  )
  expect_equal(resolve_memory_mode(10, 10, "auto", solver_supports_lazy = FALSE), "dense")
  expect_equal(resolve_memory_mode(10, 10, "auto", solver_supports_lazy = TRUE), "dense")
})

test_that("resolve_memory_mode(): \"dense\" always skips the RAM probe", {
  testthat::local_mocked_bindings(
    get_free_ram_mb = function() stop("should not be called when memory_mode = 'dense'"),
    .package = "couplr"
  )
  expect_equal(resolve_memory_mode(1e6, 1e6, "dense", solver_supports_lazy = TRUE), "dense")
})

test_that("resolve_memory_mode(): \"lazy\" errors when unsupported, returns \"lazy\" when supported", {
  expect_error(
    resolve_memory_mode(10, 10, "lazy", solver_supports_lazy = FALSE),
    "not supported"
  )
  expect_equal(resolve_memory_mode(10, 10, "lazy", solver_supports_lazy = TRUE), "lazy")
})

test_that("resolve_memory_mode(): \"auto\" switches to lazy under tight simulated RAM when supported", {
  testthat::local_mocked_bindings(
    get_free_ram_mb = function() 1000,  # pretend only 1GB free
    .package = "couplr"
  )
  expect_warning(
    mode <- resolve_memory_mode(50000, 50000, "auto", solver_supports_lazy = TRUE),
    "Switching to memory_mode"
  )
  expect_equal(mode, "lazy")
})

test_that("resolve_memory_mode(): \"auto\" warns and stays dense under tight RAM when lazy unsupported", {
  testthat::local_mocked_bindings(
    get_free_ram_mb = function() 1000,
    .package = "couplr"
  )
  expect_warning(
    mode <- resolve_memory_mode(50000, 50000, "auto", solver_supports_lazy = FALSE),
    "Proceeding densely"
  )
  expect_equal(mode, "dense")
})
