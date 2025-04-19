


#' Coppe.cosenza S4 Class
#'
#' Coppe.cosenza S4 class represents the solution of the COPPE-Cosenza method.
#' In order to do so, this S4 class contains the final evaluation of the options
#' regarding the studied projects. It presents a data frame presenting the
#' final evaluation of the options regarding each project.
#' If an option does not satisfies project´s specific factors, the option is
#' discarded (a veto operation), with the value NA.  The result also
#' presents relevant  messages list, describing if some evaluation could not be
#' performed due to entry failures or missing evaluations.
#'
#' @slot result data.frame
#' @slot projects.names character
#' @slot options.names character
#' @slot factors.of.interest Factors.of.interest
#' @slot aggregation.matrix Aggregation.matrix
#' @slot messages character
#'
#' @include  aggregation-matrix.R
#' @export
#'
setClass(
  "Coppe.cosenza",
  representation(
    result = "data.frame",
    messages = "character",
    projects.names = "character",
    options.names = "character",
    factors.of.interest = "Factors.of.interest",
    aggregation.matrix = "Aggregation.matrix"),
  validity = function(object) {
    if (!is.data.frame(object@result)) stop("@result must be a data.frame" )
    TRUE
  }

)



setMethod(
  f = "initialize",
  signature = "Coppe.cosenza",
  definition = function(.Object,
                        result,
                        messages,
                        projects.names,
                        options.names,
                        factors.of.interest,
                        aggregation.matrix){
    # cat("~~~ CoppeCosenza: initializator ~~~ \n")
    # Assignment of the slots
    .Object@result <- result
    .Object@messages <- messages
    .Object@projects.names <- projects.names
    .Object@options.names <- options.names
    .Object@factors.of.interest <- factors.of.interest
    .Object@aggregation.matrix <- aggregation.matrix
    methods::validObject(.Object)
    return(.Object)
    # return of the object
  }
)



#' Coppe.cosenza
#'
#' S4 method to construct Coppe.cosenza objects. The package also provides a way
#' to verify the consistency of the entry data. There are 3 different matrices
#' which are considered for the evaluation purposes: The project' s required
#' factors; The project's description of specific factors; and the options'
#' available level of factors. All the factors must be evaluated by each project
#'  and by each option. The program deconstruct each evaluation so as to verify:
#'  if all the factors are evaluated for each project; if all the factors are
#'  evaluated for each option, and besides, if all the linguistic variables are
#'  the prescribed ones. Such verification avoids incomplete or incorrect
#'  evaluations presenting the correspondent error messages.
#'
#' @include project-portfolio.R
#' @include option-portfolio.R
#' @include factors-of-interest.R
#'
#'
#' @param x Project.portfolio or Project S4 object
#' @param y  Option.portfolio or Option S4 object
#' @param factors.of.interest Factors.of.interest S4 object
#' @param aggregation.matrix.name character - the name of Aggregation.matrix to
#'  be used. If not provided the "default" implementation will be used
#' @param normalize logical - if TRUE, the values will be normalized,
#' dividing results by the number of factors.
#'
#' @return Coppe.cosenza S4 object
#'
setGeneric("Coppe.cosenza", function(x, y, factors.of.interest,
                                     aggregation.matrix.name = "default",
                                     normalize = FALSE)
  standardGeneric("Coppe.cosenza"))


#' @rdname Coppe.cosenza
#' @export
setMethod("Coppe.cosenza",
          signature("ANY"),
          function(x)
            stop("Coppe.cosenza constructor not
                 implemented for provided parameters")
)



#' @rdname Coppe.cosenza
#' @export
setMethod("Coppe.cosenza",
          signature("Project.portfolio", "Option.portfolio",
                    "Factors.of.interest", "missing", "missing"),
          function(x, y, factors.of.interest,
                   aggregation.matrix.name, normalize) {

            return(
              Coppe.cosenza(x, y, factors.of.interest,
                            aggregation.matrix.name, normalize)
            )
          }
)

#' @rdname  Coppe.cosenza
#' @export
setMethod("Coppe.cosenza",
          signature("Project.portfolio", "Option.portfolio",
                    "Factors.of.interest", "character", "missing"),
          function(x, y, factors.of.interest,
                   aggregation.matrix.name, normalize) {

            return(
              Coppe.cosenza(x, y, factors.of.interest,
                            aggregation.matrix.name, normalize)
            )
          }
)

#' @rdname  Coppe.cosenza
#' @export
setMethod("Coppe.cosenza",
          signature("Project.portfolio", "Option.portfolio",
                    "Factors.of.interest", "missing", "logical"),
          function(x, y, factors.of.interest,
                   aggregation.matrix.name, normalize) {

            return(
              Coppe.cosenza(x, y, factors.of.interest,
                            aggregation.matrix.name, normalize)
            )
          }
)



#' @rdname Coppe.cosenza
#' @export
setMethod("Coppe.cosenza",
          signature("Project.portfolio", "Option.portfolio",
                    "Factors.of.interest", "character", "logical"),
          function(x, y, factors.of.interest,
                   aggregation.matrix.name, normalize) {
            # change to semantic expressive names
            project.portfolio <- x
            option.portfolio <- y
            aggregation.matrix.name <-
              paste0("Aggregation.matrix.", aggregation.matrix.name)

            # warning.list store informatio to compose S4 CoppeCosenza@messages
            messages.vector <- c("Warning messages:")

            # verify if all factors are eveluated for the selected portfolios
            if (!CheckSelectFactors(project.portfolio, option.portfolio,
                                    factors.of.interest)) {
              stop("The selected factors are incompatible with the portfolios")
            }

            # remove factors out of interest

            project.portfolio.as.data.frame <-
              as.data.frame(project.portfolio)[,
                                               getFactorsOfInterestNames
                                               (factors.of.interest),
                                               drop = FALSE]

            # remove projects with NA values for any factor

            rows.initial <- row.names(project.portfolio.as.data.frame)

            project.portfolio.as.data.frame <-
              na.omit(project.portfolio.as.data.frame)

            rows.without.NA <- row.names(project.portfolio.as.data.frame)

            rows.whith.NA <-  base::setdiff(rows.initial, rows.without.NA)

            if (length(rows.whith.NA) > 0) {
              messages.vector <- c(messages.vector,
                                    paste0(
                                      "Disregarding projects with NA value for any factors:",
                                      sep = " ", as.character(rows.whith.NA)))
            }

            #setting project specifics data.frame
            project.portfolio.specifics.as.data.frame <-
              as.data.frame(project.portfolio, optional = TRUE)

            project.portfolio.specifics.as.data.frame <-
              project.portfolio.specifics.as.data.frame[
                row.names(project.portfolio.as.data.frame),
                colnames(project.portfolio.as.data.frame),
                drop = FALSE]


            # remove factors out of interest
            option.portfolio.as.data.frame <-
              as.data.frame(option.portfolio)[ ,
                                               getFactorsOfInterestNames
                                               (factors.of.interest),
                                               drop = FALSE]


            # remove options with NA values for any factor

            rows.initial <- row.names(option.portfolio.as.data.frame)

            option.portfolio.as.data.frame <-
              na.omit(option.portfolio.as.data.frame)

            rows.without.NA <- row.names(option.portfolio.as.data.frame)

            rows.whith.NA <-  base::setdiff(rows.initial, rows.without.NA)

            if (length(rows.whith.NA) > 0) {
              messages.vector <- c(messages.vector,
                                    paste0(
                                      "Disregarding options with NA value for any factors:",
                                      sep = " ", as.character(rows.whith.NA)))
            }

            # call the Aggregate function for the correct matrix
            aggregation.matrix <- new(aggregation.matrix.name)


            out <- AggregateMatrix(
              aggregation.matrix,
              project.portfolio.as.data.frame,
              project.portfolio.specifics.as.data.frame,
              option.portfolio.as.data.frame)


            if (normalize == TRUE) {
              messages.vector <- c(messages.vector,"CoppeCosenza normalizing result (using 1/nrfactors)")
              out <- out/length(factors.of.interest@list.of.factors)


            }

            coppe.cosenza <- new("Coppe.cosenza",
                                 out,
                                 messages.vector,
                                 getProjectPortfolioNames(project.portfolio),
                                 getOptionPortfolioNames(option.portfolio),
                                 factors.of.interest,
                                 aggregation.matrix )

            return(coppe.cosenza)
          }
)


#' @rdname  Coppe.cosenza
#' @export
setMethod("Coppe.cosenza",
          signature("Project"),
          function(
            x,
            y,
            factors.of.interest,
            aggregation.matrix.name,
            normalize) {

            if (methods::is(y, "Option")) y <- Option.portfolio(list(y))

            Coppe.cosenza(Project.portfolio(list(x)),
                          y,
                          factors.of.interest,
                          aggregation.matrix.name,
                          normalize)
          }
)




#' @rdname  Coppe.cosenza
#' @export
setMethod("Coppe.cosenza",
          signature("Project.portfolio", "Option"),
          function(
            x,
            y,
            factors.of.interest,
            aggregation.matrix.name,
            normalize) {

            Coppe.cosenza(x,
                          Option.portfolio(list(y)),
                          factors.of.interest,
                          aggregation.matrix.name,
                          normalize)
          }
)



# Function to verify if all factors in factors.of.interest are included
# in project.portfolio and option.portfolio
CheckSelectFactors <-
  function(project.portfolio, option.portfolio, factors.of.interest) {
    factors.of.interest.names <-
      getFactorsOfInterestNames(factors.of.interest)
    project.portfolio.as.data.frame <- as.data.frame(project.portfolio, FALSE)
    option.portfolio.as.data.frame <- as.data.frame(option.portfolio)

    factors.not.in.project.portfolio <-
      setdiff(
        factors.of.interest.names,
        colnames(project.portfolio.as.data.frame)
      )
    factors.not.in.option.portfolio <-
      setdiff(
        factors.of.interest.names,
        colnames(option.portfolio.as.data.frame))
    flag <- TRUE
    if (length(factors.not.in.project.portfolio) > 0) {
      flag <- FALSE
      cat("\nThe following factors are not considered in project portfolio: ",
          factors.not.in.project.portfolio, "\nThere is no project that complies
          with the factors.of.interest list")
    }
    if (length(factors.not.in.option.portfolio) > 0 ) {
      flag <- FALSE
      cat("\nThe following factors are not considered in option portfolio: ",
          factors.not.in.option.portfolio, "\nThere is no option that complies
          with the factors.of.interest list")
    }
    return(flag)
  }


#' summary
#'
#' Generic S4 method to \code{\link{summary}}.
#'
#' @param object Coppe.cosenza
#' @param ... not used.
#'
#' @return summary
#'
#' @export
#'
setGeneric("summary", function(object, ...)
  standardGeneric("summary"),
  useAsDefault = base::summary
)


#' @rdname summary
#' @export
setMethod("summary", signature("Coppe.cosenza"),
          function(object) {


            cat("\n\n-----------------Coppe.cosenza method---------------------------\n\n")
            if (length(object@messages) > 1) {
              for (txt in object@messages) cat(paste0(txt, sep = " ", "\n"))
            }


            ########### summary for a single project
            if (length(object@projects.names) == 1) {

                factor.list <- row.names(object@result)
                option.list <- names(object@result)
                data <- object@result

                cat("\n----------------\n")
                cat(paste0("Solutions for ", as.character(object@projects.names),sep = " "))
                cat(paste0("\nUsing  ", object@aggregation.matrix@name,sep = " "))
                cat("\n----------------\n")

                for (i in 1:length(factor.list)) {
                  for (j in 1:length(option.list)) {
                    if (is.na(data[i, j])) {
                      cat(paste0("\nProject ", as.character(object@projects.names), " cannot use option ", option.list[[j]], " since factor ", factor.list[[i]], " is specific and not satisfied" ))
                    }
                  }
                }

                # transpose data
                data <- t(data)

                #removing options with NA (specific factors not satisfied)
                data <- as.data.frame(na.omit(data))

                cat("\n----------------\n")
                cat(paste("Options and factor value analisys - considering project", as.character(object@projects.names), "\n\n",sep = " "))

                data.rows <- row.names(data)
                df <- NULL
                for (i in 1:length(data.rows)) {
                  df <- rbind(df, summary(as.numeric(data[i, ])))
                }

                row.names(df) <- data.rows
                #order by min, max, and median
                df <- df[order( df[ ,1], df[ ,6], df[ ,3], decreasing = TRUE),]

                print(df)
              }


            ########### summary for a single option
            if (length(object@options.names) == 1) {

              factor.list <- row.names(object@result)
              project.list <- names(object@result)
              data <- object@result

              cat("\n----------------\n")
              cat(paste0("Solutions for ", as.character(object@options.names),sep = " "))
              cat(paste0("\nUsing  ", object@aggregation.matrix@name,sep = " "))
              cat("\n----------------\n")

              for (i in 1:length(factor.list)) {
                for (j in 1:length(project.list)) {
                  if (is.na(data[i, j])) {
                    cat(paste0("\nOption ", as.character(object@options.names), " cannot be applied to ", project.list[[j]], " since factor ", factor.list[[i]], " is specific and not satisfied" ))
                  }
                }
              }

              # transpose data
              data <- t(data)

              #removing projectes with NA (specific factors not satisfied)
              data <- as.data.frame(na.omit(data))

              cat("\n----------------\n")
              cat(paste("Projects and factor value analisys - considering option", as.character(object@options.names), "\n\n",sep = " "))

              data.rows <- row.names(data)
              df <- NULL
              for (i in 1:length(data.rows)) {
                df <- rbind(df, summary(as.numeric(data[i, ])))
              }

              row.names(df) <- data.rows
              #order by min, max, and median
              df <- df[order( df[ ,1], df[ ,6], df[ ,3], decreasing = TRUE),]

              print(df)
            }


            ########### summary for a set of projects and options

            if (length(object@projects.names) > 1 &&
                length(object@options.names) > 1) {

              # project and option list may differ from object@projects.names and
              # object@options.names, since some of the originals can be
              #  disregarded because do not have all factors of interest evaluated
              project.list <- row.names(object@result)
              option.list <- names(object@result)

              df1 <- NULL
              df <- NULL
              for (i in 1:length(project.list) ) {
                for (j in 1:length(option.list) ) {
                  if (!is.na(object@result[[i,j]])) {
                    df <- rbind(
                      df,
                      data.frame(project.list[[i]],
                                 option.list[[j]],
                                 object@result[[i,j]]
                      )
                    )
                  }
                  else df1 <- rbind(
                    df1,
                    data.frame(project.list[[i]],
                               option.list[[j]]
                    )
                  )
                }
              }

              if (length(row.names(df)) == 0) cat("No solution found\n")

              names(df) <-  c("Project","Option", "Aggregate.value")
              df$Aggregate.value <- as.numeric(as.character(df$Aggregate.value))
              df <- df[order( df[,3] , decreasing = TRUE),]

              cat("\n----------------\n")
              cat(paste0("Solutions using the ", sep = " ", object@aggregation.matrix@name))
              cat("\n----------------\n")
              print(df)

              if (!is.null(df1)) {
                names(df1) <-  c("Dropped Project","Incompatible Option")
                df <- df[order( df[,1]),]
                cat(paste0("\n-------------------\nIncompatible solutions using the", sep = " ", object@aggregation.matrix@name, ":"))
                cat("\n--------------------\n")
                print(df1)
              }
            }


            cat("\n--------------------------------------------------------\n")

            cat(paste0("Description of the used ", object@aggregation.matrix@name, ":\n\n"))

            #print(new("Aggregation.matrix.default"))
            print(new(object@aggregation.matrix@name))

            #print(citation("coppeCosenzaR"))

            cat("\n--------------------------------------------------------\n")

          }
)


#' @rdname show
#' @param Coppe.cosenza Coppe.cosenza
#' @export
setMethod("show", "Coppe.cosenza",
          function(object){
            print(object@result)
          }
)
