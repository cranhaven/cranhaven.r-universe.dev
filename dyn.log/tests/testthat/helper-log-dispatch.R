LogDispatchTester <- R6::R6Class( # nolint (class name)
  classname = "LogDispatchTester",
  inherit = LogDispatch,
  lock_objects = FALSE,
  lock_class = FALSE,
  cloneable = FALSE,
  portable = FALSE,

  public = list(

    initialize = function() {
      super$initialize()
    }
  ),
  private = list()
)

SingletonTester <- R6::R6Class( # nolint (class name)
  classname = "SingletonTester",
  inherit = LogDispatch,
  lock_objects = FALSE,
  lock_class = FALSE,
  cloneable = FALSE,
  portable = FALSE,

  public = list(

    initialize = function() {

      if (is.null(private$public_bind_env)) {
        private$create_singleton(SingletonTester)
      } else {
        self <- private$instance
        private$set_bindings()
      }

      invisible(self)
    }
  ),
  private = list()
)

test_that("log_single_instance", {

  inst_n <- SingletonTester$new()
  inst_m <- SingletonTester$new()

  expect_true(identical(inst_n, inst_m))
})
