#' @name disto
#' @title Constructor for class 'disto'
#' @description Create mapping to data sources storing distances(symmetric),
#'   dissimilarities(non-symmetric), similarities and so on
#' @details This is a wrapper to create a 'disto' handle over different backends
#'   storing distances, dissimilarities, similarities etc with minimal data
#'   overhead like a database connection. The following named arguments are
#'   required to set-up the backend:
#'
#'   \itemize{
#'
#'   \item \strong{dist}:
#'
#'   \itemize{
#'
#'   \item objectname: Object of the class 'dist' or the name of the object as a
#'   'string'.
#'
#'   \item env: Environment where the object exists. When this is missing, its
#'   assumed to be parent environment.
#'
#'   }
#'
#'   }
#' @param ... Arguments for a backend. See details
#' @param backend (string) Specify a backend. Currently supported: 'dist'
#' @return Object of class 'disto' which is a thin wrapper on a list
#' @examples
#' temp <- stats::dist(iris[,1:4])
#' dio   <- disto(objectname = "temp")
#' dio
#' unclass(dio)
#' @export
disto <- function(...
                  , backend = "dist"
                  ){

  assertthat::assert_that(assertthat::is.string(backend))

  backends  <- c("dist")
  assertthat::assert_that(backend %in% c("dist"))

  arguments <- list(...)

  res <- switch(backend
         , dist = disto_dist(arguments)
         )
  res$backend <- backend
  return(res)

}

#' @name disto_dist
#' @title Constructior of disto with dist backend
#' @description  Constructior of disto with dist backend
#' @param arguments to construct disto object
#' @return returns a list
#' @details to be used by disto constructor function
disto_dist <- function(arguments){

  if(is.null(arguments$env)){
    arguments$env <- .GlobalEnv
  } else {
    assertthat::assert_that(is.environment(arguments$env))
  }
  assertthat::assert_that(assertthat::is.string(arguments$objectname))

  assertthat::assert_that(
    exists(arguments$objectname, envir = arguments$env)
    , msg = "Unable to find the object in the specified environment."
    )
  assertthat::assert_that(
    inherits(get(arguments$objectname, envir = arguments$env)
             , "dist"
             )
    )

  dlist        <- vector("list")
  dlist$name   <- arguments$objectname
  dlist$env    <- arguments$env
  class(dlist) <- "disto"

  return(dlist)
}

#' @name size
#' @title Obtain size of the disto object
#' @description Obtain size of the disto object
#' @param disto object of class disto
#' @param ... currently not in use
#' @return Integer vector of length 1
#' @examples
#' temp <- stats::dist(iris[,1:4])
#' dio   <- disto(objectname = "temp")
#' size(dio)
#' @export
size <- function(disto, ...){

  res <- switch(disto$backend
    , dist = attr(get(disto$name, envir = disto$env), "Size")
    )

  return( res )
}

#' @name names.disto
#' @title Get names/labels
#' @description Get names/labels of the underlying distance storing backend
#' @param x disto object
#' @return A character vector
#' @examples
#' temp <- stats::dist(iris[,1:4])
#' dio <- disto(objectname = "temp")
#' dio
#' names(dio) <- paste0("a", 1:150)
#' @export
names.disto <- function(x){

  res <- switch(x$backend
                , dist = attr(get(x$name, envir = x$env), "Labels")
                )
  return(res)
}

#' @name `names<-.disto``
#' @title Set names/labels
#' @description Set names/labels of the underlying distance storing backend
#' @param x disto object
#' @param value A character vector
#' @return invisible disto object
#' @examples
#' temp <- stats::dist(iris[,1:4])
#' dio <- disto(objectname = "temp")
#' dio
#' names(dio) <- paste0("a", 1:150)
#' @export
`names<-.disto` <- function(x, value){

  assertthat::assert_that(is.character(value))
  assertthat::assert_that(length(value) == size(x))

  res <- switch (x$backend,
    dist = {
      val <- eval(`attr<-`(get(x$name, envir = x$env)
                      , "Labels"
                      , value
                    )
             , envir = x$env
             )

      assign(x$name, val, envir = x$env)

           }
  )

  return(invisible(x))
}

#' @name print.disto
#' @title Print method for dist class
#' @description Print method for dist class
#' @param x object of class disto
#' @param ... currently not in use
#' @return invisible NULL. Function writes backend type and size to terminal as
#'   a message.
#' @examples
#' temp <- stats::dist(iris[,1:4])
#' dio   <- disto(objectname = "temp")
#' print(dio)
#' @export
print.disto <- function(x, ...){
  message("disto with backend: ", x$backend)
  message("size: ", size(x))

  return(invisible(NULL))
}

#' @name summary.disto
#' @title Summary method for dist class
#' @description Summary method for dist class
#' @param object object of class disto
#' @param ... currently not in use
#' @return invisibly returns the tidy output of summary as a dataframe.
#' @examples
#' temp <- stats::dist(iris[,1:4])
#' dio   <- disto(objectname = "temp")
#' dio
#' summary(dio)
#' @export
summary.disto <- function(object, ...){
  res <- switch(object$backend
    , dist = broom::tidy(summary.default(get(object$name
                                 , envir = object$env
                                )
                        )
                    )
    )

  print(object)
  print(knitr::kable(tidyr::gather(res, key = "statistic")))
  return(invisible(res))
}


#' @name as.data.frame.disto
#' @title Convert a disto object to dataframe
#' @description Convert the underlying data of a disto object to a dataframe in
#'   long format (3 columns: item1, item2, distance). This might be a costly
#'   operation and should be used with caution.
#' @param x object of class disto
#' @param ... arguments for \code{\link[broom]{tidy}}
#' @return a dataframe in long format
#' @examples
#' temp <- stats::dist(iris[,1:4])
#' dio  <- disto(objectname = "temp")
#' dio
#' head(as.data.frame(dio))
#' @export
as.data.frame.disto <- function(x, ...){

  arguments <- list(...)
  res <- switch(x$backend
    , dist = do.call(broom::tidy
                     , c(list(get(x$name, envir = x$env)), arguments)
                     )
    )

  return( res )
}


#' @name `[.disto`
#' @title Extract from a disto object in matrix style extraction
#' @description Extract a disto object in matrix style extraction and via direct
#'   indexing. 'product' specification allows both outer (matrix output, default
#'   option) and inner (vector) product type extraction. For dist backend see:
#'   \code{\link{dist_extract}}.
#' @param x object of class 'disto'
#' @param i (integer vector) row indexes
#' @param j (integer vector) column indexes
#' @param k (integer vector) direct indexes
#' @param product (string) One among: "inner", "outer"
#' @return When product is 'outer', returns a matrix. Else, a vector.
#' @examples
#' temp <- stats::dist(iris[,1:4])
#' dio <- disto(objectname = "temp")
#' dio
#' names(dio) <- paste0("a", 1:150)
#'
#' dio[1, 2]
#' dio[2, 1]
#' dio[c("a1", "a10"), c("a5", "a72")]
#' dio[c("a1", "a10"), c("a5", "a72"), product = "inner"]
#' dio[k = c(1,3,5)]
#' @export
`[.disto` <- function(x, i, j, k, product = "outer"){

  res <- switch(x$backend
    , dist = dist_extract(get(x$name, envir = x$env)
                          , i
                          , j
                          , k
                          , product = product
                          )
                )

  return(res)

}

#' @name `[[.disto`
#' @title Extract a single value from disto object
#' @description Extract a single value from disto object in matrix style
#'   extraction and via direct indexing. This does not support using names. This
#'   is faster than \code{link{extract}}. For dist backend see:
#'   \code{\link{dist_extract}}.
#' @param x object of class 'disto'
#' @param i (integer vector) row index
#' @param j (integer vector) column index
#' @param k (integer vector) direct index
#' @return (A real number)  Distance value
#' @examples
#' temp <- stats::dist(iris[,1:4])
#' dio  <- disto(objectname = "temp")
#' dio
#'
#' dio[[1, 2]]
#' dio[[2, 1]]
#' dio[[k = 3]]
#' @export
`[[.disto` <- function(x, i, j, k){

  size <- size(x)

  if(!missing(k)){
    assertthat::assert_that(missing(i) && missing(j))
    assertthat::assert_that(assertthat::is.scalar(k) &&
                            assertthat::is.count(k) &&
                            k <= size * (size - 1)/2
                            )
  } else {
    assertthat::assert_that(assertthat::is.scalar(i) &&
                              assertthat::is.count(i) &&
                              i <= size
                            )
    assertthat::assert_that(assertthat::is.scalar(j) &&
                              assertthat::is.count(j) &&
                              j <= size
                            )
  }

  res <- switch(x$backend
    , dist = {
        if(missing(k)){

          `[`(get(x$name, envir = x$env), dist_ij_k(i, j, size))

        } else {

          `[`(get(x$name, envir = x$env), k)

        }
      }
    )

  return(res)
}

#' @name `[<-.disto`
#' @title In-place replacement of values
#' @description For dist backend see: \code{\link{dist_replace}}.
#' @param x object of class 'disto'
#' @param i (integer vector) row index
#' @param j (integer vector) column index
#' @param k (integer vector) direct index
#' @param value (integer/numeric vector) Values to replace
#' @return Invisible disto object. Note that this function is called for its
#'   side effect.
#' @examples
#' temp       <- stats::dist(iris[,1:4])
#' dio        <- disto(objectname = "temp")
#' names(dio) <- paste0("a", 1:150)
#' dio
#'
#' dio[1, 2] <- 10
#' dio[1,2]
#'
#' dio[1:10, 2:11] <- 100
#' dio[1:10, 2:11, product = "inner"]
#'
#' dio[paste0("a", 1:5), paste0("a", 6:10)] <- 101
#' dio[paste0("a", 1:5), paste0("a", 6:10), product = "inner"]
#' @export
`[<-.disto` <- function(x, i, j, k, value){

  res <- switch(x$backend
    , dist = {
        if(missing(k)){
          val <- dist_replace(object = get(x$name, envir = x$env)
                        , i = i
                        , j = j
                        , value = value
                        )
        } else {
          val <- dist_replace(object = get(x$name, envir = x$env)
                        , k = k
                        , value = value
                        )
        }

        assign(x$name, val, envir = x$env)

      }
    )

  return(invisible(x))
}

#' @name dapply
#' @title Matrix like apply function for disto object
#' @description Apply function for data underlying disto object
#' @param x disto object
#' @param margin (one among 1 or 2) dimension to apply function along
#' @param fun Function to apply over the margin
#' @param subset (integer vector) Row/Column numbers along the margin
#' @param nproc Number of parallel processes (unix only)
#' @return Simplified output of 'sapply' like function
#' temp <- dist(iris[,1:4])
#' dio  <- disto(objectname = "temp")
#'
#' # function to pick indexes of 5 nearest neighbors
#' # an efficient alternative with Rcpp is required
#' udf <- function(x){
#'   dim(x) <- NULL
#'   order(x)[1:6]
#'   }
#' hi <- dapply(dio, 1, udf)[-1, ]
#' dim(hi)
#' @export
dapply <- function(x, margin = 1, fun, subset, nproc = 1){

  assertthat::assert_that(inherits(x, "disto"))
  assertthat::assert_that(assertthat::is.scalar(margin) && margin %in% 1:2)
  assertthat::assert_that(is.function(fun))
  assertthat::assert_that(assertthat::is.count(nproc))

  size <- size(x)
  if(missing(subset)){
    subset <- 1:size
  } else {
    assertthat::assert_that(all(subset %in% 1:size))
  }

  res <- pbapply::pbsapply(subset
                           , function(s) fun(x[s, ])
                           , cl = nproc
                           )
  return(res)
}

#' @name plot.disto
#' @title Plot a disto object
#' @description Various plotting options for subsets of disto objects
#' @param x object of class disto
#' @param ... Additional arguments. See details.
#' @details Among the additional arguments,
#'
#' \itemize{
#'
#' \item 'type: is mandatory. Currently, these options are supported: heatmap, dendrogram.
#'
#' \item sampleSize: A random sample of indexes is drawn from the distance object underlyting the disto mapping. Default value of sampleSize is set to 100.
#'
#' \item seed seed for random sample. Default is 100.
#'
#' }
#' @return ggplot object
#' @examples
#' temp <- stats::dist(iris[,1:4])
#' dio  <- disto(objectname = "temp")
#' plot(dio, type = "heatmap")
#' plot(dio, type = "dendrogram")
#' @export
plot.disto <- function(x, ...){

  arguments <- list(...)

  assertthat::assert_that(assertthat::is.string(arguments$type))
  assertthat::assert_that(arguments$type %in% c("heatmap", "dendrogram"))
  if(!is.null(arguments$sampleSize)){
    assertthat::assert_that(assertthat::is.count(arguments$sampleSize))
  } else {
    arguments$sampleSize <- 100L
  }

  if(!is.null(arguments$seed)){
    assertthat::assert_that(assertthat::is.count(arguments$seed))
  }

  size <- size(x)
  res <- switch(x$backend
    , dist =
      {
        if(size > arguments$sampleSize){

          set.seed(ifelse(is.null(arguments$seed), 1L, arguments$seed))
          si <- sample.int(size, arguments$sampleSize)
          do <- dist_subset(get(x$name, envir = x$env), si)
          if(is.null(attr(do, "Labels"))){
            attr(do, "Labels") <- as.character(si)
          }
          labs <- attr(do, "Labels")

        } else {

          do   <- get(x$name, envir = x$env)
          if(is.null(attr(do, "Labels"))){
            attr(do, "Labels") <- as.character(1:size)
          }
          labs <- attr(do, "Labels")
        }

        plotObject <- switch(arguments$type
          , heatmap = {
            factoextra::fviz_dist(do) +
              ggplot2::ggtitle("Heatmap of distances")
          }
          , dendrogram = {
            factoextra::fviz_dend(fastcluster::hclust(do)
                                  , repel = TRUE
                                  , horiz = TRUE
                                  ) +
              ggplot2::ggtitle("Dendrogram of distances")
          }
          )
      }
    )

  return(res)
}
