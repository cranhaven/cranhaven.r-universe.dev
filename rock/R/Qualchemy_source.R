#' #' R6 Class representing a {qualchemy} source
#' #'
#' #' A {qualchemy} source: usually a text file that can be (or is) coded.
#' #'
#' #' @examples
#' #'
#' Qualchemy_source <- R6::R6Class(
#'   "Qualchemy_source",
#'
#'   ### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#'   ### Public properties & methods
#'   ### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#'
#'   public = list(
#'
#'     ### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#'     ### Public properties
#'     ### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#'
#'     #' @field contents The source's contents
#'     contents = NULL,
#'
#'     ### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#'     ### Public methods
#'     ### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#'
#'     ### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#'     ### Initialization
#'     ### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#'
#'     #' @description
#'     #' Create a new source.
#'     #' @param x The source contents.
#'     #' @return A new `Qualchemy_source` object.
#'     initialize = function(project = NULL) {
#'
#'       ### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#'       ### Set fields
#'       ### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#'
#'       private$project <- project;
#'
#'     },
#'
#'     ### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#'     ### Add a source
#'     ### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#'
#'     #' @description
#'     #' Import source contents from a file.
#'     #' @return Invisibly, the `source` object itself.
#'     load = function(x) {
#'
#'       if (is.character(x) && (length(x) == 1) && file.exists(x)) {
#'         self$contents <-
#'           rock::load_source(x);
#'       } else if (is.character(x)) {
#'         self$contents <- x;
#'       }
#'
#'       ### Return new source
#'       return(invisible(self));
#'
#'     },
#'
#'     ### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#'     ### Return project containing this source
#'     ### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#'
#'     #' @description
#'     #' Return the project that contains this source
#'     #' @return Invisibly, the `project` containing this source.
#'     parent_project = function() {
#'
#'       return(invisible(private$project));
#'
#'     }
#'
#'
#'
#'   ), ### End of public properties and methods
#'
#'   ### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#'   ### Private properties & methods
#'   ### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#'
#'   private = list(
#'
#'     ### The project, another R6 object, passed by reference
#'     project = NULL
#'
#'   ) ### End of private properties and methods
#'
#' )
