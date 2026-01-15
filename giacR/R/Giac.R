#' @title R6 class to access to Giac
#'
#' @description Creates an object allowing to execute Giac commands.
#'
#' @export
#' @importFrom R6 R6Class
#' @import chromote
#' @importFrom processx supervisor_kill
#' @importFrom jsonlite fromJSON
Giac <- R6Class(
  "Giac",

  cloneable = FALSE,

  private = list(
    "session" = NULL
  ),

  public = list(

    #' @description Create a new \code{Giac} instance.
    #' @param chromePath path to the Chrome executable (or Chromium, Brave,
    #'   etc); if \code{find_chrome()} does not work, you can set the
    #'   environment variable \code{CHROMOTE_CHROME} to the path and it will
    #'   work
    #' @return A \code{Giac} object.
    "initialize" = function(chromePath = find_chrome()) {
      if(is.null(chromePath)) {
        stop("Set the path to a Chrome executable.")
      }
      chrm <- Chrome$new(
        path = chromePath,
        args = "--disable-gpu --headless --remote-debugging-port=9222"
      )
      chromote <- Chromote$new(browser = chrm)
      session  <- ChromoteSession$new(parent = chromote)
      ids <- session$Page$navigate("about:blank")
      jsfile <- system.file("Giac", "giacwasm.js", package = "giacR")
      script <- paste0(readLines(jsfile), collapse = "\n")
      . <- session$Runtime$evaluate(script)
      . <- session$Runtime$evaluate("
var UI = {
  Datestart: Date.now(),
  ready: false,
  warnpy: false
};
Module.onRuntimeInitialized = function() {
  UI.ready = true;
;};
")
      . <- session$Runtime$evaluate(
        "var docaseval = Module.cwrap('caseval', 'string', ['string']);"
      )
      ready <- session$Runtime$evaluate("UI.ready")$result$value
      while(!ready) {
        ready <- session$Runtime$evaluate("UI.ready")$result$value
      }
      private[["session"]] <- session
    },


    #' @description Execute a Giac command.
    #' @param command the command to be executed given as a character string
    #' @param timeout timeout in milliseconds
    #' @return The result of the command in a character string.
    #'
    #' @examples
    #' \donttest{if(!is.null(chromote::find_chrome())) {
    #'   giac <- Giac$new()
    #'   giac$execute("2 + 3/7")
    #'   giac$execute("integrate(ln(x))")
    #'   giac$close()
    #' }}
    "execute" = function(command, timeout = 10000) {
      evaluate <- private[["session"]]$Runtime$evaluate(
        sprintf("docaseval('%s')", command), timeout = timeout
      )
      if(evaluate[["result"]][["type"]] != "string") {
        stop("An error occured.")
      }
      evaluate[["result"]][["value"]]
    },


    #' @description Gröbner implicitization (see examples)
    #' @param equations comma-separated equations
    #' @param relations comma-separated relations, or an empty string if there
    #'   is no relation; the relations between the constants must placed first,
    #'   followed by the relations between the variables
    #' @param variables comma-separated variables
    #' @param constants comma-separated constants, or an empty string if there
    #'   is no constant
    #' @param timeout timeout in milliseconds
    #' @return The implicitization of the equations.
    #'
    #' @examples
    #' library(giacR)
    #' \donttest{if(!is.null(chromote::find_chrome())) {
    #'   giac <- Giac$new()
    #'   giac$implicitization(
    #'     equations = "x = a*cost, y = b*sint",
    #'     relations = "cost^2 + sint^2 = 1",
    #'     variables = "cost, sint",
    #'     constants = "a, b"
    #'   )
    #'   giac$close()
    #' }}
    "implicitization" = function(
      equations, relations = "", variables, constants = "", timeout = 10000
    ) {
      equations  <- trimws(strsplit(equations, ",")[[1L]])
      coordinates <- toString(vapply(equations, function(eq) {
        trimws(strsplit(eq, "=")[[1L]][1L])
      }, character(1L)))
      # if(nchar(trimws(constants)) > 0L) {
      #   symbols <- paste0(variables, ", ", coordinates, ", ", constants)
      # } else{
      #   symbols <- paste0(variables, ", ", coordinates)
      # }
      relations  <- trimws(strsplit(relations, ",")[[1L]])
      relations  <- vapply(relations, subtraction, character(1L))
      equations  <- paste0(
        vapply(equations, subtraction, character(1L)), collapse = ", "
      )
      equations <- paste0(c(equations, relations), collapse  = ", ")
      symbols <- variables
      body <- paste0("[", equations, "], [", symbols, "]")
      command <- sprintf("eliminate(%s)", body)
      gbasis <- self$execute(command, timeout = timeout)
      variables <- trimws(strsplit(variables, ",")[[1L]])
      command <- paste0(
        "apply(expr -> ", paste0(vapply(variables, function(s) {
          sprintf("has(expr, %s)==0", s)
        }, character(1L)),
        collapse = " and "), ", ", gbasis, ")")
      free <- fromJSON(self$execute(command))
      gbasis <- strsplit(sub("\\]$", "", sub("^\\[", "", gbasis)), ",")[[1L]]
      gbasis[free]
    },


    #' @description Close a Giac session
    #' @return \code{TRUE} or \code{FALSE}, whether the session has been closed.
    "close" = function() {
      parent <- private[["session"]]$parent
      . <- private[["session"]]$close()
      success <- quiet(parent$close())
      supervisor_kill()
      success
    }

  )
)
