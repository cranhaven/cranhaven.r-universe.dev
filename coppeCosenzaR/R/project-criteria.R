
#' Project.criteria S4 Class
#'
#' Project.criteria S4 class contains a list of S4 Project.criterion objects.
#' This list is   used to construct Projec objects, and is type checked.
#'
#' @slot list.of.project.criterion list of Project.criterion
#'
#' @export
#'
#' @include project-criterion.R
#'
setClass(
  "Project.criteria",
  representation(
    list.of.project.criterion = "list"),
  validity = function(object) {

    # not null
    if (is.null(object@list.of.project.criterion))
      stop("@list.of.project.criterion cannot be NULL")
    #is.data.frame(df) && nrow(df)==0

    # is list and have elements
    if (!(is.list(object@list.of.project.criterion) &&
          length(object@list.of.project.criterion) > 0))
      stop("list.of.project.criterion must be a list with one or more
           Project.criterion")

    # all elements are Project.criterion
    for (project.criterion in object@list.of.project.criterion) {
      if (!methods::is(project.criterion, "Project.criterion"))
        stop("@list.of.project.criterion must be a list of
             Project.criterion S4 objects")
    }

    # all elements are Project.criterion are related to distinct factors
    factor.names <- c()
    for (project.criterion in object@list.of.project.criterion) {
         factor.names <- c(factor.names, project.criterion@factor@name)}
    #print(factor.names)
    if (anyDuplicated(factor.names) > 0) stop("Only one criterion for each
                                              factor is allowed. Check -> ",
                                              factor.names )

  }
)



setMethod(
  f = "initialize",
  signature = "Project.criteria",
  definition = function(.Object,
                        list.of.project.criterion){
    #cat("~~~ Project.criteria: initializator ~~~ \n")
    # Assignment of the slots
    .Object@list.of.project.criterion <- list.of.project.criterion
    methods::validObject(.Object)
    return(.Object)
    # return of the object
  }
)

#' Project.criteria Constructor
#'
#' Project.criteria is a constructor to Factor S4 objects.
#'
#' @param list.of.project.criterion list of Project.criterion S4 objects. The list is type checked
#' and cannot be empty. The factors of the used project.criterion must be
#' distinct
#'
#' @return a \code{\link{Project.criteria}} S4 object
#'
#' @export
#'
#' @examples
#' \dontrun{Project.criteria(list(project.criterion1,project.criterion2))}
#'
Project.criteria <- function(list.of.project.criterion){
  new("Project.criteria", list.of.project.criterion)
}



#' @rdname show
#' @param Project.criteria Project.criteria
#' @export
setMethod("show", "Project.criteria",
          function(object){

            df <- NULL
            for (i in 1:length(object@list.of.project.criterion)) {
              x <- object@list.of.project.criterion[[i]]
              df <- rbind(
                df,
                data.frame(
                  as.character(x@factor@name),
                  x@importance.degree,
                  x@specific
                  )
                )
            }
            row.names(df) <- df[, 1]
            df <- df[, -1]
            names(df) <- c("importance.degree","specific")
            print(df)
            cat("\n")
          }
)



