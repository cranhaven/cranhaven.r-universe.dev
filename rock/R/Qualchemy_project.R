#' #' R6 Class representing a {qualchemy} project
#' #'
#' #' A {qualchemy} project mostly just contains other objects.
#' #'
#' #' @examples
#' #'
#' Qualchemy_project <- R6::R6Class(
#'   "Qualchemy_project",
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
#'     #' @field metadata The project's metadata in a list
#'     metadata = list(title = ""),
#'
#'     #' @field classes The project's classes as a character vector with the
#'     #' class identifier of each class (e.g., "participantId" for the class
#'     #' with participants)
#'     classes = c(),
#'
#'     #' @field classInstances A list of character vectors of class instance
#'     #' identifiers (e.g., "participant_1", "question_7"), where each character
#'     #' vector's name is the corresponding class identifier (e.g. "participantId",
#'     #' "questionId", "locationId")
#'     classInstances = list(),
#'
#'     #' @field codebook A qualchemy codebook object
#'     codebook = NULL,
#'
#'     #' @field sources A list of qualchemy source objects
#'     sources = list(),
#'
#'     #' @field codings A list of qualchemy coding objects (a coding is the
#'     #' application of a code to a part of a source)
#'     codings = list(),
#'
#'     #' @field attributes A named list of qualchemy attribute objects, where
#'     #' each list element's name is the corresponding class identifier (e.g.
#'     #' "participantId", "questionId", "locationId")
#'     attributes = "",
#'
#'     #' @field extra A named list of extra information.
#'     extra = list(),
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
#'     #' Create a new group object. Most of this text comes directly
#'     #' from the TSV manual page at
#'     #' <https://www.limesurvey.org/manual/Tab_Separated_Value_survey_structure>, so
#'     #' please see that page for more details.
#'     #' @param metadata A list with the project's metadata.
#'     #' @param classes A character vector with the project's class identifiers.
#'
#'     #' @param ... Any additional options, stored as a named list in the
#'     #' `otherOptions` property by assigning `as.list(...)`.
#'     #' @return A new `Group` object.
#'     initialize = function(metadata = "",
#'                           classes = "",
#'                           classInstances = list(),
#'                           codebook = list(),
#'                           sources = list(),
#'                           codings = list(),
#'                           attributes = list(),
#'                           extra = list()) {
#'
#'       ### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#'       ### Set fields
#'       ### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#'
#'       self$metadata <- metadata;
#'       self$classes <- classes;
#'       self$classInstances <- classInstances;
#'       self$codebook <- codebook;
#'       self$sources <- sources;
#'       self$codings <- codings;
#'       self$attributes <- attributes;
#'       self$extra <- extra;
#'
#'     },
#'
#'     ### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#'     ### Add a source
#'     ### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#'
#'     #' @description
#'     #' Add a source to a project.
#'     #' @param x The path to the file with the source contents, or the source
#'     #' contents as a character vector.
#'     #' @return Invisibly, the `source` object that was just added. Note
#'     #' that you can further modify this, which will modify the source object
#'     #' "in" the project as well. This allows you to pipe the source
#'     #' creation on to, for example, clean it.
#'     add_source = function(x) {
#'
#'       newSource <-
#'         Qualchemy_source$new(project = self);
#'
#'       self$sources <- c(self$sources, newSource);
#'
#'       ### Return new source
#'       return(invisible(newSource));
#'
#'     }
#'
#'   ), ### End of public properties and methods
#'
#'   ### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#'   ### Private properties & methods
#'   ### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#'
#'   private = list(
#'
#'     ### Unique numeric identifiers (for MySQL basically)
#'     somePrivateProperty = 0
#'
#'   ) ### End of private properties and methods
#'
#' )
