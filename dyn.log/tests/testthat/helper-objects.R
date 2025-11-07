TestObject <- R6::R6Class( # nolint (class definition)
  classname = "TestObject",

  public = list(

    cls_name = NULL,

    initialize = function() {
      private$id <- private$generate_id()
      self$cls_name <- private$get_class_name()
    },

    get_logger = function(name) {
      globalenv()[[eval(name)]]
    },

    identifier = function() {
      invisible(private$id)
    },

    class_name = function() {
      invisible(self$cls_name)
    },

    invoke_logger = function(name) {
      logger <- self$get_logger(name)

      a <- "test"; b <- 123; c <- 100L
      logger$trace("these are some variables: {a} - {b} - {c}")
    }
  ),

  private = list(
    id = NULL,

    generate_id = function(n = 15) {
      paste0(sample(LETTERS, n, TRUE), collapse =  "")
    },

    get_class_name = function() {
      calls <- as.character(sys.calls())
      calls <- calls[max(which(stringr::str_detect(calls, "\\$new\\(.*\\)")))]
      stopifnot(length(calls) == 1)
      invisible(stringr::str_remove(calls, "\\$new\\(.*\\)"))
    }
  )
)

DerivedTestObject <- R6::R6Class( # nolint (class definition)
  classname = "DerivedTestObject",
  inherit = TestObject,
  public = list(

    initialize = function() {
      super$initialize()
    },

    invoke_logger = function(name) {
      logger <- self$get_logger(name)

      a <- "derived test"; b <- 321; c <- 200L

      logger$trace("variables in derived: {a} - {b} - {c}")
    }
  )
)

UnassociatedTestObject <- R6::R6Class( # nolint (class definition)
  classname = "UnassociatedTestObject",

  public = list(

    initialize = function() {

    },

    get_logger = function(name) {
      globalenv()[[name]]
    },

    invoke_logger = function(name) {
      logger <- self$get_logger(name)

      a <- "derived test"; b <- 321; c <- 200L
      logger$trace("variables in derived: {a} - {b} - {c}")
    }
  ),

  active = list(),
  private = list()
)
