
#' Project S4 Class
#'
#' Project S4 class represents a potential project and its slots include a
#' Project.criteria object, with the list of needed factors to the project and
#' their degree of importance. The project has a non-empty name.
#'
#' @slot name character (any other argument will be cast to character)
#'
#' @slot project.criteria Project.criteria
#'
#' @export
#' @include project-criteria.R
setClass(
  "Project",
  representation(
    name = "character",
    project.criteria = "Project.criteria"),

  validity = function(object) {

    if (!methods::is(object@project.criteria, "Project.criteria"))
      stop("@project.criteria must be a Project.criteria S4 object")
    if (length(object@name) > 1) stop("@name cannot have more then 1 value")
    if (object@name == "") stop("@name cannot be void")
    if (grepl("^\\s*$", object@name)) stop("@name cannot be only blanc spaces")
    TRUE
  }
)



setMethod(
  f = "initialize",
  signature = "Project",
  definition = function(.Object,
                        name,
                        project.criteria){
    #cat("~~~ Project: initializator ~~~ \n")
    # Assignment of the slots
    .Object@name <- as.character(name)
    .Object@project.criteria = project.criteria
    methods::validObject(.Object)
    return(.Object)
    # return of the object
  }
)


#' Project Constructor function
#'
#'
#' Constructs a Project S4 object.  ... TODO(Pessoa) VRF e Ampliar
#'
#' @param  name character
#' @param  project.criteria Project.criteria S4 object
#'
#' @return a \code{\link{Project}} S4 object
#'
#' @export
#'
#' @examples
#' \dontrun{Project <- Project(name, project.criteria)}
#'
#'
#'
Project <- function(name, project.criteria){
  new("Project", name, project.criteria)
}



#' getProjectFactorsNames
#'
#' This function returns a sorted vector with all the factors names in a Project
#' S4 object
#'
#' @param project an Project S4 object
#'
#' @return It provides a sorted vector with the names of factors in an project
#' @export
#'
#' @examples
#' \dontrun{getProjectFactorsNames(project)}
#'
getProjectFactorsNames <- function(project){

  #type check
  if (!methods::is(project, "Project"))
    stop("project parameter must be an Project S4 object")

  list.of.factors.names <- list()
  for (project.criterion in
       project@project.criteria@list.of.project.criterion) {
    list.of.factors.names <-
      list(list.of.factors.names, project.criterion@factor@name )
    }
  vector.of.factors.names <- unlist(list.of.factors.names)
  vector.of.factors.names <- sort(vector.of.factors.names, decreasing = FALSE)
  return(vector.of.factors.names)
}


#' getProjectFactorsSpecific
#'
#' This function returns a sorted vector with all the factors names in a Project
#' S4 object which were classified as specific to the project under discussion.
#'
#' @param project an Project S4 object
#'
#' @return It provides a sorted vector with the names of factors in an project
#' which were classified as specific to the project under discussion.
#'
#' @export
#'
#' @examples
#' \dontrun{getProjectFactorsSpecific(project)}
#'
getProjectFactorsSpecific <- function(project){


  #type check
  if (!methods::is(project, "Project"))
    stop("project parameter must be an Project S4 object")


  list.of.factors.names <- list()
  for (project.criterion in
       project@project.criteria@list.of.project.criterion) {
    if (project.criterion@specific == TRUE) {
    list.of.factors.names <-
      list(list.of.factors.names, project.criterion@factor@name )
    }
  }
  vector.of.factors.names <- unlist(list.of.factors.names)
  vector.of.factors.names <- sort(vector.of.factors.names, decreasing = FALSE)
  return(vector.of.factors.names)
}

#' @rdname show
#' @param Project Project
#' @export
setMethod("show", "Project",
          function(object){
            cat(paste("\nProject: ", object@name), "\n")
            print(object@project.criteria)
          }
)

