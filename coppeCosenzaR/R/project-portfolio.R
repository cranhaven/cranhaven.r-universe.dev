

#' Project.portfolio
#'
#' Project.portfolio S4 class contains a type-checked list of S4
#' Project objects. This project.portfolio is an argument to construct the
#' CoppeCosenza S4 objects, which, in turn, represents the method solution.
#'
#' @slot list.of.project list of Project S4 objects
#'
#' @export
#'
#' @note  Any S4 Project object can be included in the @list.of.project. This
#' means we can have projects with different set of factors. It is possible to
#' export and import Project.portfolio to/from data.frame, allowing to store
#' and edit information externally.
#'
#' @include project.R
#'
setClass(
  "Project.portfolio",
  representation(
    list.of.project = "list"),
  validity = function(object) {
    # not null
    if (is.null(object@list.of.project))
      stop("@list.of.project cannot be NULL")
    # is list and have elements
    if (!(is.list(object@list.of.project) &&
          length(object@list.of.project) > 0))
      stop("list.of.project must be a list with one or more
           Project")
    # all elements are Project
    for (project in object@list.of.project) {
      if (!methods::is(project, "Project"))
        stop("@list.of.project must be a list of Project S4 objects")
    }
    # Project@name are distinct
    project.names <- c()
    for (project in object@list.of.project) {
      project.names <- c(project.names, project@name)}
    #print(project.names)
    if (anyDuplicated(project.names) > 0)
      stop("Project names must be different from each other. Check -> ",
           project.names )
  }
)



setMethod(
  f = "initialize",
  signature = "Project.portfolio",
  definition = function(.Object,
                        list.of.project){
    #cat("~~~ Project.portfolio: initializator ~~~ \n")
    # Assignment of the slots
    .Object@list.of.project <- list.of.project
    methods::validObject(.Object)
    return(.Object)
    # return of the object
  }
)



#' Project.portfolio
#'
#' S4 method to construct Project.portfolio S4 objects. It accepts different
#' sets for parameters types.
#' @param x list A non-empty list with Project S4 objects, or a data frame with
#' factors evauation
#' @param y data.frame with specfic factors, if x is also a data.frame
#'
#'
#' @return a Project.portfolio S4 object
#' @export
#'
setGeneric("Project.portfolio", function(x, y)
  standardGeneric("Project.portfolio"))



#' @rdname Project.portfolio
#' @note Arguments (ANY) \cr
#'  A call to \code{Project.portfolio( )} with no parameters will return
#'  an error message for missing argument.
#'
setMethod("Project.portfolio",
          signature("ANY" ),
          function(x)
            stop("Coppe.cosenza constructor not implemented for provided parameters")
)


#' @rdname Project.portfolio
#'
#' @examples
#' \dontrun{option.portfolio <- Project.portfolio(list.of.project)}
#'
setMethod("Project.portfolio",
          signature("list"),
          function(x){
            list.of.project <- x
            new("Project.portfolio", list.of.project)
          }
)




#' @rdname Project.portfolio
#'
#' @note Arguments (data.frame, data.frame). Data.frame where columns represent
#' factors and rows are the projects. The data.frame is checked for no-columns
#' and no-rows. The firs data.frame contain the factors evaluation and the
#' second, with same rows and columns, contain boolean information about the
#' factor being specific or not to the project.
#' The constructors called subsequently will verify if acceptable values where
#' used to factor evaluation and for distinct names of factors and projects
#'
#' @note It is possible to obtain a dummy table to serve as example by
#' construction a portfolio using  \code{Project.portfolio(list.of.projects)}
#' and, after, converting it in a data.frame using the function
#' \code{as.data.frame(project.portfolio)}.
#'
#' @examples
#' \dontrun{project.portfolio <-
#' (project.portfolio.as.data.frame, project.portfolio.specifics.as.data.frame)}
#'
#'
setMethod("Project.portfolio",
          signature("data.frame", "data.frame"),
          function(x, y){
            project.portfolio.as.data.frame <- x
            project.portfolio.specifics.as.data.frame <- y

            if (!(row.names(project.portfolio.as.data.frame) > 0) )
              stop("there is no project in the portfolio")

            if (!(colnames(project.portfolio.as.data.frame) > 0) )
              stop("there is no factors in the portfolio")

            if (!(row.names(project.portfolio.as.data.frame) ==
                  row.names(project.portfolio.specifics.as.data.frame)))
              stop("both 02 data frames must have the same projects")


            if (!(colnames(project.portfolio.as.data.frame) ==
                  colnames(project.portfolio.specifics.as.data.frame)))
              stop("both 02 data frames must have the same factors ")


            project.names <- row.names(project.portfolio.as.data.frame)
            factors.names <- colnames(project.portfolio.as.data.frame)
            Project.portfolio(
              lapply( i <- 1:length(project.names), function(i) {
                Project(
                  project.names[[i]],
                  Project.criteria(
                    lapply( x <- 1:length(factors.names), function(x) {
                      Project.criterion(
                        Factor(as.character(factors.names[[x]])),
                        as.character(project.portfolio.as.data.frame[i,x]),
                        as.logical(project.portfolio.specifics.as.data.frame[i,x])
                      )
                    }
                    )
                  )
                )
              }
              )
            )
          }
)


#function called by method as.data.frame(project.portfolio, boolean)
getProjectPortfolioAsDataFrame <- function(project.portfolio){

  portfolio.factors <- getProjectPortfolioFactors(project.portfolio)
  project.portfolio.names <- getProjectPortfolioNames(project.portfolio)

  df <- data.frame(matrix(ncol = length(portfolio.factors), nrow = length(project.portfolio.names)))
  colnames(df) <- portfolio.factors
  rownames(df) <- project.portfolio.names
  for (project in project.portfolio@list.of.project) {
    for (project.criterion in project@project.criteria@list.of.project.criterion) {
      df[project@name, project.criterion@factor@name] <- project.criterion@importance.degree
      #print(project.criterion@factor@name, project, project.criterion@importance.degree)
    }
  }
  return(df)
}



#function called by method as.data.frame(project.portfolio, boolean)
getProjectPortfolioSpecificsAsDataFrame <- function(project.portfolio){

  portfolio.factors <- getProjectPortfolioFactors(project.portfolio)
  project.portfolio.names <- getProjectPortfolioNames(project.portfolio)

  df <- data.frame(matrix(ncol = length(portfolio.factors), nrow = length(project.portfolio.names)))
  colnames(df) <- portfolio.factors
  rownames(df) <- project.portfolio.names
  for (project in project.portfolio@list.of.project) {
    for (project.criterion in project@project.criteria@list.of.project.criterion) {
      df[project@name, project.criterion@factor@name] <- project.criterion@specific
      #print(project.criterion@factor@name, project, project.criterion@importance.degree)
    }
  }
  # change any "" or "   "  to NA
  #data.frame <- as.data.frame(apply(data.frame,2,function(x)gsub("^\\s*$", NA,x)))
  return(df)
}




#' getProjectPortfolioFactors
#'
#' function that provides a sorted vector with factors from the project list.
#'
#' @param project.portfolio S4 Project.portfolio object
#'
#' @return vector of character
#'
#' @export
#'
#' @examples
#' \dontrun{getProjectPortfolioFactors(project.portfolio)}
#'
getProjectPortfolioFactors <- function(project.portfolio){


  if (!methods::is(project.portfolio, "Project.portfolio"))
    stop("project.portfolio an instance of project.portfolio S4 objects")

  vector.of.factors <- NULL
  for (project in project.portfolio@list.of.project) {
    vector.of.factors <- c(vector.of.factors, getProjectFactorsNames(project))
  }
  vector.of.factors <- sort(vector.of.factors, decreasing = FALSE)
  vector.of.factors <- unique(vector.of.factors)
  return(vector.of.factors)
}




#' getProjectPortfolioNames
#'
#' function that provides a sorted vector with project names.
#'
#' @param project.portfolio S4 Project.portfolio object
#'
#' @return vector of character
#'
#' @export
#'
#' @examples
#' \dontrun{getProjectPortfolioNames(project.portfolio)}
#'
getProjectPortfolioNames <- function(project.portfolio){

  if (!methods::is(project.portfolio, "Project.portfolio"))
    stop("project.portfolio an instance of project.portfolio S4 objects")

  vector.of.names <- NULL
  for (project in project.portfolio@list.of.project) {
    vector.of.names <- c(vector.of.names, project@name)
  }
  vector.of.names <- sort(vector.of.names, decreasing = FALSE)
  return(vector.of.names)
}




#' @rdname as.data.frame
#' @export
#'
#' @examples
#' \dontrun{as.data.frame(project.portfolio, option = TRUE)}
#' \dontrun{as.data.frame(project.portfolio,  , TRUE)}
#' \dontrun{as.data.frame(project.portfolio, ANY, FALSE)}
#' \dontrun{as.data.frame(project.portfolio, option = FALSE)}
#' \dontrun{as.data.frame(project.portfolio)} This infer option is FALSE, too.
#'
#' @include option-portfolio.R
#'
#  The generic S4 method is in option-portfolio.R
#
setMethod("as.data.frame", signature("Project.portfolio"),
          function(x, row.names = NA, optional  = FALSE) {
            if (missing(optional)) optional <- FALSE
            option.portfolio <- x
            if (optional)
              return(getProjectPortfolioSpecificsAsDataFrame(option.portfolio))
            else return(getProjectPortfolioAsDataFrame(option.portfolio))
          }
)



#' @rdname show
#' @param Project.portfolio Project.portfolio
#' @export
setMethod("show", "Project.portfolio",
          function(object){
            cat("\nProject.portfolio:\n")
            cat(
              unlist(lapply(object@list.of.project, function(x) paste0(x@name, " ")))
            )
            cat("\n\n")
          }
)
