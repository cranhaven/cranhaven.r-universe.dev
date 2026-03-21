#' @name dist_ij_k_
#' @title Convert ij index to k index
#' @description Convert ij index to k index for a dist object
#' @param i row index
#' @param j column index
#' @param size value of size attribute of the dist object
#' @return k index
dist_ij_k_ <- compiler::cmpfun(
  function(i, j, size){

    if(i == j){
      NA_integer_
    }
    else if(i < j){

      size*(i-1) - i*(i-1)/2 + j-i

    } else {

      size*(j-1) - j*(j-1)/2 + i-j
    }
  }
  , options = list(optimize = 3)
)

#' @name dist_ij_k
#' @title Vectorized version of dist_ij_k_
#' @description Convert ij indexes to k indexes for a dist object
#' @param i row indexes
#' @param j column indexes
#' @param size value of size attribute of the dist object
#' @return k indexes
dist_ij_k <- compiler::cmpfun(
  Vectorize(dist_ij_k_, vectorize.args = c("i", "j"))
  , options = list(optimize = 3)
)

#' @name dist_k_ij_
#' @title Convert kth index to ij index
#' @description Convert kth index to ij index of a dist object
#' @param k kth index
#' @param size value of size attribute of the dist object
#' @return ij index as a length two integer vector
dist_k_ij_ <- compiler::cmpfun(function(k, size){
  sums <- cumsum(seq(size - 1, 1, -1))
  j    <- Position(function(x) x >= k, sums)
  i    <- size - sums[j] + k

  return(c(i, j))
})

#' @name dist_k_ij
#' @title Vectorized version of dist_k_ij_
#' @description Convert kth indexes to ij indexes of a dist object
#' @param k kth indexes
#' @param size value of size attribute of the dist object
#' @return ij indexes as 2*n matrix where n is length of k vector
dist_k_ij <- compiler::cmpfun(
  Vectorize(dist_k_ij_, vectorize.args = c("k", "size"))
  , options = list(optimize = 3)
)

#' @name dist_extract
#' @title Matrix style extraction from dist object
#' @description Matrix style extraction supports 'inner' and 'outer'(default)
#'   products
#' @param object dist object
#' @param i (integer vector) row positions
#' @param j (integer vector) column positions
#' @param k (integer vector) positions
#' @param product (string) One among: 'inner', 'outer'(default)
#' @return A matrix or vector of distances when product is 'outer' and 'inner'
#'   respectively
#' @details In k-mode, both i and j should be missing and k should not be
#'   missing. In ij-mode, k should be missing and both i and j are optional. If
#'   i or j are missing, they are interpreted as all values of i or j (similar
#'   to matrix or dataframe subsetting). If i and j are of unequal length, the
#'   smaller one is recycled.
#' @examples
#' # examples for dist_extract
#'
#' # create a dist object
#' temp <- dist(iris[,1:4])
#' attr(temp, "Labels") <- outer(letters, letters, paste0)[1:150]
#' head(temp)
#' max(temp)
#' as.matrix(temp)[1:5, 1:5]
#'
#'
#' dist_extract(temp, 1, 1)
#' dist_extract(temp, 1, 2)
#' dist_extract(temp, 2, 1)
#' dist_extract(temp, "aa", "ba")
#'
#' dist_extract(temp, 1:10, 11:20)
#' dim(dist_extract(temp, 1:10, ))
#' dim(dist_extract(temp, , 1:10))
#' dist_extract(temp, 1:10, 11:20, product = "inner")
#' length(dist_extract(temp, 1:10, , product = "inner"))
#' length(dist_extract(temp, , 1:10, product = "inner"))
#'
#' dist_extract(temp, c("aa", "ba", "ca"), c("ca", "da", "fa"))
#' dist_extract(temp, c("aa", "ba", "ca"), c("ca", "da", "fa"), product = "inner")
#'
#' dist_extract(temp, k = 1:3) # product is always inner when k is specified
#' @export
dist_extract <- compiler::cmpfun(
  function(object
           , i
           , j
           , k
           , product = "outer"
  ){

    size   <- attr(object, "Size")
    labels <- attr(object, "Labels")

    if(!missing(k)){
      assertthat::assert_that(missing(i) && missing(j))
      assertthat::assert_that(all(vapply(k, assertthat::is.count, logical(1))))
      assertthat::assert_that(max(k) <= size * (size - 1)/2)
      product <- "inner"

      out <- object[k]

    } else {
      if(missing(i)){
        i <- 1:size
      } else {
        assertthat::assert_that(
          all(
            vapply(
              i
              , function(x) assertthat::is.count(x) || assertthat::is.string(x)
              , logical(1)
            )
          )
        )
        if(inherits(i, "character")){
          if(is.null(labels)){
            stop("dist object does not have names (labels). i, j, k should be integers.")
          } else {
            i <- fastmatch::fmatch(i, labels)
            if(anyNA(i)){
              stop("Unable to resolve some names to integer positions in i")
            }
          }
        }
        assertthat::assert_that(!any(i > size))
      }
      if(missing(j)){
        j <- 1:size
      } else {
        assertthat::assert_that(
          all(
            vapply(
              j
              , function(x) assertthat::is.count(x) || assertthat::is.string(x)
              , logical(1)
            )
          )
        )
        if(inherits(j, "character")){
          if(is.null(labels)){
            stop("dist object does not have names (labels). i, j should be integers.")
          } else {
            j <- fastmatch::fmatch(j, labels)
            if(anyNA(j)){
              stop("Unable to resolve some names to integer positions in j")
            }
          }
        }
        assertthat::assert_that(!any(j > size))
      }
      assertthat::assert_that(assertthat::is.string(product) &&
                                product %in% c("inner", "outer")
      )
      il <- length(i)
      jl <- length(j)

      if(product == "inner"){

        out <- object[dist_ij_k(i, j, size)]

      } else { # outer case

        out             <- object[outer(i, j, function(x, y) dist_ij_k(x, y, size))]
        dim(out)        <- c(il, jl)

        if(!is.null(labels)){
          rownames(out) <- labels[i]
          colnames(out) <- labels[j]
        }
      }
    }

    out[is.na(out)] <- 0
    return(out)
  }
  , options = list(optimize = 3)
)

#' @name dist_replace
#' @title Replacement values in dist
#' @description Replacement values of a dist object with either ij or
#'   position indexing
#' @param object dist object
#' @param i (integer vector) row positions
#' @param j (integer vector) column positions
#' @param k (integer vector) positions
#' @param value (integer/numeric vector) Values to replace
#' @details There are two modes to specify the positions:
#'
#'   \itemize{
#'
#'   \item ij-mode where i and j are specified and k is missing. If i or j are
#'   missing, they are interpreted as all values of i or j (similar to matrix or
#'   dataframe subsetting). Lengths of i, j are required to be same. If 'value'
#'   is singleton, then it is extended to the length of i or j. Else, 'value'
#'   should have same length as i or j.
#'
#'   \item k-mode where  k is present and both i and k are missing. k is the
#'   positions in the dist object. If 'value' is singleton, then it is extended
#'   to the length of k. Else, 'value' should have same length as k.
#'
#'   }
#'
#' @return dist object
#' @examples
# # examples for dist_replace
#'
#' # create a dist object
#' d <- dist(iris[,1:4])
#' attr(d, "Labels") <- outer(letters, letters, paste0)[1:150]
#' head(d)
#' max(d)
#' as.matrix(d)[1:5, 1:5]
#'
#' # replacement in ij-mode
#' d <- dist_replace(d, 1, 2, 100)
#' dist_extract(d, 1, 2, product = "inner")
#' d <- dist_replace(d, "ca", "ba", 102)
#' dist_extract(d, "ca", "ba", product = "inner")
#'
#' d <- dist_replace(d, 1:5, 6:10, 11:15)
#' dist_extract(d, 1:5, 6:10, product = "inner")
#' d <- dist_replace(d, c("ca", "da"), c("aa", "ba"), 102)
#' dist_extract(d, c("ca", "da"), c("aa", "ba"), product = "inner")
#'
#' # replacement in k-mode
#' d <- dist_replace(d, k = 2, value = 101)
#' dist_extract(d, k = 2)
#' dist_extract(d, 3, 1, product = "inner") # extracting k=2 in ij-mode
#' @export
dist_replace <- compiler::cmpfun(
  function(object
           , i
           , j
           , value
           , k
  ){

    size   <- attr(object, "Size")
    labels <- attr(object, "Labels")

    if(!missing(k)){ # k-mode

      assertthat::assert_that(missing(i) && missing(j))
      assertthat::assert_that(
        all(vapply(k, assertthat::is.count, logical(1)))
      )
      assertthat::assert_that(max(k) <= size * (size - 1)/2)
      assertthat::assert_that(is.numeric(value))
      assertthat::assert_that(length(value) %in% c(1, length(k)))

    } else {

      if(missing(i)){
        i <- 1:size
      } else {
        assertthat::assert_that(
          all(
            vapply(
              i
              , function(x) assertthat::is.count(x) || assertthat::is.string(x)
              , logical(1)
            )
          )
        )
        if(inherits(i, "character")){
          if(is.null(labels)){
            stop("dist object does not have names (labels). i, j, k should be integers.")
          } else {
            i <- fastmatch::fmatch(i, labels)
            if(anyNA(i)){
              stop("Unable to resolve some names to integer positions in i")
            }
          }
        }
        assertthat::assert_that(!any(i > size))
      }

      if(missing(j)){
        j <- 1:size
      } else {
        assertthat::assert_that(
          all(
            vapply(
              j
              , function(x) assertthat::is.count(x) || assertthat::is.string(x)
              , logical(1)
            )
          )
        )
        if(inherits(j, "character")){
          if(is.null(labels)){
            stop("dist object does not have names (labels). i, j should be integers.")
          } else {
            j <- fastmatch::fmatch(j, labels)
            if(anyNA(j)){
              stop("Unable to resolve some names to integer positions in j")
            }
          }
        }
        assertthat::assert_that(!any(j > size))
      }

      il <- length(i)
      jl <- length(j)
      if(il != jl){
        stop("Lengths of i and j should be equal")
      }
      assertthat::assert_that(!any(i == j))
      assertthat::assert_that(is.numeric(value))
      assertthat::assert_that(length(value) %in% c(1L, il))

      k <- dist_ij_k(i, j, size)
    }

    object[k] <- value
    return(object)

  }
  , options = list(optimize = 3)
)

#' @name dist_subset
#' @title dist_subset
#' @description Compute subset faster than regular `[[` on a dist object. This
#'   is from \pkg{proxy} package (not exported by proxy).
#' @param x dist object
#' @param subset index of the subset. This has to be unique.
#' @param ... additional arguments
#' @return returns a dist subset
#' @export
dist_subset <- getFromNamespace("subset.dist", ns = "proxy")