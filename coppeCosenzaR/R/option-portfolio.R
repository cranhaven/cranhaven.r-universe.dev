

#' Option.portfolio S4 Class
#'
#' Option.portfolio S4 class contains a type-checked list of S4 Option objects.
#' This object is an argument to construct the CoppeCosenza S4 objects, which,
#' in turn, represents the method solution.
#'
#' Any S4 Option object can be included in the @list.of.options. This means we
#' can have options with different set of factors. It is possible to export and
#' import Option.portfolio to/from data.frame, allowing to store and edit
#' information externally.
#'
#'
#' @slot list.of.option list of Option S4 objects. The option names are checked
#' and must be distinct.
#'
#' @export
#'
#' @include option.R
#'
setClass(
  "Option.portfolio",
  representation(
    list.of.option = "list"),

  validity = function(object) {
    if (is.null(object@list.of.option) || !(length(object@list.of.option) > 0))
      stop("Option.portfolio cannot be NULL and must have one or more Option")
    for (project in object@list.of.option) {
      if (!methods::is(project, "Option"))
        stop("@list.of.option must be a list of Option S4 objects")
    }
    project.names <- c()
    for (project in object@list.of.option) {
      project.names <- c(project.names, project@name)}
    #print(project.names)
    if (anyDuplicated(project.names) > 0) stop("project names must be unique
                                               Check -> ", project.names )
  }
)



setMethod(
  f = "initialize",
  signature = "Option.portfolio",
  definition = function(.Object,
                        list.of.option){
    #cat("~~~ Option.portfolio: initializator ~~~ \n")
    # Assignment of the slots
    .Object@list.of.option <- list.of.option
    methods::validObject(.Object)
    return(.Object)
    # return of the object
  }
)



#' Option.portfolio
#'
#' S4 method to construct Option.portfolio S4 objects. It accepts different
#' sets for parameters types.
#'
#' @param x list of Option S4 object or a data.frame
#'
#' @return a Option.portfolio S4 object
#' @export
#'
setGeneric("Option.portfolio", function(x)
  standardGeneric("Option.portfolio"))


#' @rdname Option.portfolio
#' @note Arguments (ANY) \cr
#'  A call to \code{Project.portfolio( )} with no parameters will return
#'  an error message for mismatch argument.
#' @export
setMethod("Option.portfolio",
          signature("ANY"),
          function(x)
            stop("Option.portfolio constructor not
                 implemented for provided parameters")
)


#' @rdname Option.portfolio
#'
#' @note Arguments list(). A non-empty list with Option S4 objects.
#' @export
#' @examples
#' \dontrun{option.portfolio <- Option.portfolio(list.of.options)}
#'
setMethod("Option.portfolio",
          signature("list"),
          function(x){
            list.of.option <- x
            new("Option.portfolio", list.of.option)
          }
)


#' @rdname Option.portfolio
#'
#' @note Arguments data.frame. A data.frame where columns represent factors and
#' rows are the options. The data frame is checked for no columns and no rows.
#' The constructors called subsequently will verify if acceptable values where
#' used to factor evaluation and for distinct names of factors and options.
#'
#' @note It is possible to obtain a dummy table to serve as example by
#' construction a potrfolio using  \code{Option.portfolio(list.of.options)} and
#' after converting it in a data.frame using the function
#' \code{as.data.frame(option.portfolio)}.
#'
#' @examples
#' \dontrun{option.portfolio <- Option.portfolio(my.option.portfolio.data.frame)}
#'
#' @rdname Option.portfolio
#' @export
#' @include option-portfolio.R
#'
setMethod("Option.portfolio",
          signature("data.frame"),
          function(x){
            option.portfolio.as.data.frame <- x


            if (!(row.names(option.portfolio.as.data.frame) > 0) )
              stop("there is no options in the portfolio")

            if (!(colnames(option.portfolio.as.data.frame) > 0) )
              stop("there is no factors in the portfolio")

            option.names <- row.names(option.portfolio.as.data.frame)
            factors.names <- colnames(option.portfolio.as.data.frame)
            Option.portfolio(
              lapply( i <- 1:length(option.names), function(i) {
                Option(
                  option.names[[i]],
                  Option.resources(
                    lapply( x <- 1:length(factors.names), function(x) {
                      factor <- Factor(factors.names[[x]])
                      print(factor)
                      factor.availability <- as.character(option.portfolio.as.data.frame[i,x])
                      print(factor.availability)
                      return(new("Option.factor.availability", factor, factor.availability))
                    }
                    )
                  )
                )
              }
              )
            )
          }
)



#'
#' getOptionPortfolioFactors
#'
#' function that provides a list of Factor S4 objects presents in a
#' Option.portfolio S4 object
#'
#' @param option.portfolio S4 Option.portfolio object
#'
#' @return list of Factor S4 objects
#'
#' @export
#'
#' @examples
#' \dontrun{getOptionPortfolioFactors(option.portfolio)}
#'
getOptionPortfolioFactors <- function(option.portfolio){

  if (!methods::is(option.portfolio, "Option.portfolio"))
    stop("option.portfolio an instance of Option.portfolio S4 objects")

  vector.of.factors <- NULL
  for (option in option.portfolio@list.of.option) {
    vector.of.factors <- c(vector.of.factors, getOptionFactorsNames(option))
  }
  vector.of.factors <- sort(vector.of.factors, decreasing = FALSE)
  vector.of.factors <- unique(vector.of.factors)
  return(vector.of.factors)
}






#' getOptionPortfolioNames
#'
#' function that provides a sorted vector with option names.
#'
#' @param option.portfolio S4 Option.portfolio object
#'
#' @return vector of character
#'
#' @export
#'
#' @examples
#' \dontrun{getOptionPortfolioNames(option.portfolio)}
#'
getOptionPortfolioNames <- function(option.portfolio){

  if (!methods::is(option.portfolio, "Option.portfolio"))
    stop("option.portfolio an instance of Option.portfolio S4 objects")

  vector.of.names <- NULL
  for (option in option.portfolio@list.of.option) {
    vector.of.names <- c(vector.of.names, option@name)
  }
  vector.of.names <- sort(vector.of.names, decreasing = FALSE)
  return(vector.of.names)
}





#' as.data.frame
#'
#' This S4 method masks the \code{base::as.data.frame()} S3 function. If a call
#' uses parameters other then the expected by this package, then it will be
#' forward to the S3 function.
#'
#' @param x Option.portfolio or Project.portfolio
#' @param row.names not used. It is inherited from \code{base::as.data.frame()}
#' @param optional logical. To be used with  Project.portfolio. Indicates if the
#'  return is a data.frame with factor evaluations or with the information about
#'   which factors are specific to a project.
#'   The default is \code{optional = FALSE}
#' @param ... not used.
#'
#' @return data.frame
#' @export
setGeneric("as.data.frame", function(x, row.names, optional, ...)
  standardGeneric("as.data.frame"),
  useAsDefault = base::as.data.frame
  )


#' @rdname as.data.frame
#' @export
#' @examples
#' \dontrun{as.data.frame(option.portfolio)}
#'
setMethod("as.data.frame", signature("Option.portfolio"),
          function(x) {
            option.portfolio <- x
            portfolio.factors <- getOptionPortfolioFactors(option.portfolio)
            option.portfolio.names <- getOptionPortfolioNames(option.portfolio)

            df <- data.frame(matrix(ncol = length(portfolio.factors),
                                    nrow = length(option.portfolio.names)))
            colnames(df) <- portfolio.factors
            rownames(df) <- option.portfolio.names

            for (option in option.portfolio@list.of.option) {
              for (option.factor.availability in
                   option@option.resources@list.of.factor.availability) {
                df[option@name, option.factor.availability@factor@name] <-
                  option.factor.availability@availability
              }
            }
            return(df)
          }
)

#' @rdname show
#' @param Option.portfolio Option.portfolio
#' @export
setMethod("show", "Option.portfolio",
          function(object){
            cat("\n")
            cat("Option.portfolio:\n")
            cat(
              unlist(lapply(object@list.of.option, function(x) paste0(x@name, " ")))
            )
            cat("\n")
          }
)

