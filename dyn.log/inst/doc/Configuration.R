## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(dyn.log)

## ---- comment = "", results = "asis", echo = F--------------------------------
options(crayon.enabled = TRUE)
knitr::opts_chunk$set(collapse = TRUE, comment = "")
old.hooks <- fansi::set_knit_hooks(knitr::knit_hooks)

## ---- eval = F----------------------------------------------------------------
#  configs <- get_configurations()
#  configs

## ---- echo = F----------------------------------------------------------------
configs <- get_configurations()
pander::pander(sapply(configs, basename))

## ---- eval=FALSE--------------------------------------------------------------
#  file.copy(from = configs$default, to = "inst/logging.yaml")

## ---- eval=FALSE--------------------------------------------------------------
#  init_logger(config_file = system.file("logging.yaml", package = "yourpkg"))

## ---- eval=FALSE--------------------------------------------------------------
#  options("dyn.log.config" = system.file("logging.yaml", package = "yourpkg"))

## ---- eval=F------------------------------------------------------------------
#  # use here to get your workspace root
#  file.copy(from = configs$default,
#            to = file.path(here::here(), "logging.yaml"))

## ---- eval=F------------------------------------------------------------------
#  library(dyn.log)
#  
#  init_logger("logging.yaml")

## ---- eval=FALSE--------------------------------------------------------------
#  options("dyn.log.config" = file.path(here::here(), "logging.yaml"))

## -----------------------------------------------------------------------------
TestObject <- R6::R6Class(
  classname = "TestObject",

  public = list(

    cls_name = NULL,

    initialize = function() {
      private$id <- private$generate_id()
      self$cls_name <- private$get_class_name()
    },

    identifier = function() {
      invisible(private$id)
    },

    class_name = function() {
      invisible(self$cls_name)
    },

    invoke_logger = function() {
      a <- "test"; b <- 123; c <- 100L

      Logger$trace("these are some variables: {a} - {b} - {c}")
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

DerivedTestObject <- R6::R6Class(
  classname = "DerivedTestObject",
  inherit = TestObject,
  public = list(

    initialize = function() {
      super$initialize()
    },

    invoke_logger = function() {
      a <- "derived test"; b <- 321; c <- 200L

      Logger$trace("variables in derived: {a} - {b} - {c}")
    }
  )
)

## -----------------------------------------------------------------------------
init_logger(configs$object)

test_obj <- DerivedTestObject$new()

test_obj$invoke_logger()

