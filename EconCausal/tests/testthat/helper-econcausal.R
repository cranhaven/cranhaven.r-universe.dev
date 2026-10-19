# Shared fixtures for the EconCausal test suite.
#
# No test in this suite fits a model. Every test exercises argument validation,
# an optional-dependency guard, the restoration of global state, or the package
# interface, so the suite runs on any machine, needs neither Stan nor the Boom
# family, and never pays the cost of an estimation run.
#
# Two conventions keep the suite portable. First, the expected text is fixed
# only for messages that EconCausal itself emits: messages coming from base R or
# from another package are translated or reworded across versions, so those
# tests assert that the call fails, not how it phrases the failure. Second, when
# a message from base R is needed, it is derived in the running session instead
# of being copied as a literal.

# Column names as ecm_mars() renames them: dots preserved.
ec_circ_dotted <- c("ER.SPOT.CAN.US", "ER.SPOT.US.CAN", "ER.SPOT.US.REMB",
                    "CPI", "TreasuryBonds10y", "FedDiscountRate")

ec_prod_dotted <- c("Exports", "RealNetProfit", "RealSocialConsumptionPerWorker2017",
                    "RealWagePPP2017", "CapitalStockPPP2017",
                    "LaborProductivityPPP2017", "InvestmentPerWorkerPPP2017")

# Column names as bsts_model() and bglmar1() see them after simple_name().
ec_circ_simple <- c("TC_SPOT_CAN_US", "TC_SPOT_US_CAN", "TC_SPOT_US_REMB",
                    "IPC", "TdI_LdelT", "TasaDescuento")

ec_prod_simple <- c("ValorExportaciones", "Real_Net_Profit",
                    "RealSocialConsumptionPerWorker2017", "RealWage_PPP2017",
                    "CapitalStock_PPP2017", "LaborProductivity_PPP2017",
                    "InvestmentPerWorker_PPP2017")

# A workbook path that is guaranteed not to exist. tempfile() only builds the
# name, so the reader fails on the first access and nothing is ever created.
ec_missing_workbook <- function() {
  tempfile(pattern = "econcausal-missing-", fileext = ".xlsx")
}

# A real workbook with a shape the package does not accept: eleven numeric
# columns and no date column. It ships with readxl, a hard dependency, so it is
# available wherever EconCausal is.
ec_foreign_workbook <- function() {
  readxl::readxl_example("datasets.xlsx")
}

# A small table with the shape bglmar1() accepts: a date column named Month and
# the thirteen series under their cleaned names. Twenty-four months are far fewer
# than the rows a validation window needs, so a call that got past the input
# checks would skip every direction instead of fitting a model.
ec_valid_bglmar1_table <- function() {
  tab <- data.frame(Month = seq(as.Date("2000-01-01"), by = "month", length.out = 24L))
  for (k in seq_along(c(ec_circ_simple, ec_prod_simple))) {
    tab[[c(ec_circ_simple, ec_prod_simple)[k]]] <- k + seq_len(24L) / 10
  }
  tab
}

# The same table written as a workbook and shipped with the tests.
ec_valid_workbook <- function() {
  test_path("fixture-bglmar1.xlsx")
}

# Evaluate one call as if no package were installed, which is how the guards for
# the optional dependencies are reached on a machine where those packages are
# present.
#
# local_mocked_bindings() cannot be used here: it only replaces bindings the
# package itself defines, and requireNamespace() belongs to base, so testthat
# refuses with "Can't find binding for `requireNamespace`". Pointing the library
# path at an empty directory reaches the same guard through the documented
# behaviour of requireNamespace(), which searches .libPaths().
#
# The library path is narrowed for the duration of a single call and restored on
# exit, including when that call fails, so no other test ever sees it narrowed.
# It works only while the package being hidden is not already loaded, because a
# loaded namespace satisfies requireNamespace() whatever .libPaths() says; each
# caller checks that precondition and skips otherwise.
ec_without_libraries <- function(expr) {
  empty <- tempfile(pattern = "econcausal-empty-lib-")
  dir.create(empty)
  old <- .libPaths()
  on.exit(.libPaths(old), add = TRUE)
  .libPaths(empty)
  force(expr)
}

# The deparsed body of a function, as one string, for the few contracts that
# cannot be reached at run time. Used only where a dynamic test is impossible:
# see test-bsts_model.R, where loading 'bsts' always loads 'BoomSpikeSlab' too.
ec_source_of <- function(fun) {
  paste(deparse(body(fun), width.cutoff = 500L), collapse = " ")
}

# An in-memory stand-in for the list returned by thread_controller(). It holds a
# BLAS and an OpenMP thread count and records every call with its argument.
# fail_next(name, applied) makes the next call of that setter fail once, either
# before changing its count or, with `applied = TRUE`, after changing it, so the
# tests can reach both error paths of limit_threads() deterministically.
ec_fake_thread_controller <- function(blas, omp) {
  state <- c(blas = blas, omp = omp)
  calls <- character()
  pending <- list()
  setter <- function(name, channel) {
    function(threads) {
      calls[length(calls) + 1L] <<- sprintf("%s(%s)", name, threads)
      failure <- pending[[name]]
      pending[[name]] <<- NULL
      if (isTRUE(failure)) {
        state[[channel]] <<- as.integer(threads)
      }
      if (!is.null(failure)) {
        stop(sprintf("%s failed", name), call. = FALSE)
      }
      state[[channel]] <<- as.integer(threads)
      invisible(NULL)
    }
  }
  getter <- function(name, channel) {
    function() {
      calls[length(calls) + 1L] <<- name
      state[[channel]]
    }
  }
  list(
    controller = list(
      get_blas = getter("get_blas", "blas"),
      set_blas = setter("set_blas", "blas"),
      get_omp  = getter("get_omp", "omp"),
      set_omp  = setter("set_omp", "omp")
    ),
    state = function() state,
    calls = function() calls,
    fail_next = function(name, applied = FALSE) pending[[name]] <<- applied
  )
}
