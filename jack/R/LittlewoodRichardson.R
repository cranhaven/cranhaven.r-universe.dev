insertWith <- function(f, mp, key, value) {
  if(key %in% names(mp)) {
    mp[[key]] <- f(value, mp[[key]])
  } else {
    mp[[key]] <- value
  }
  mp
}

#' @title Littlewood-Richardson rule for multiplication
#' @description Expression of the product of two Schur polynomials as a linear
#'   combination of Schur polynomials.
#'
#' @param mu,nu integer partitions, given as vectors of decreasing integers
#' @param output the type of the output, \code{"dataframe"} or \code{"list"}
#'
#' @return This computes the expression of the product of the two Schur
#'   polynomials associated to \code{mu} and \code{nu} as a linear combination
#'   of Schur polynomials. If \code{output="dataframe"}, the output is a
#'   dataframe with two columns: the column \code{coeff} gives the coefficients
#'   of this linear combination, these are positive integers, and the column
#'   \code{lambda} gives the partitions defining the Schur polynomials of this
#'   linear combination as character strings, e.g. the partition
#'   \code{c(4, 3, 1)} is encoded by the character string \code{"[4, 3, 1]"}.
#'   If \code{output="list"}, the output is a list
#'   of lists with two elements. Each of these lists with two elements
#'   corresponds to a term of the linear combination: the first element,
#'   named \code{coeff}, is the coefficient, namely the Littlewood-Richardson
#'   coefficient \eqn{c^{\lambda}_{\mu,\nu}}, where \eqn{\lambda} is the
#'   integer partition given in the second element of the list, named
#'   \code{lambda}, which defines the Schur polynomial of the
#'   linear combination.
#' @export
#'
#' @examples
#' library(jack)
#' mu <- c(2, 1)
#' nu <- c(3, 2, 1)
#' LR <- LRmult(mu, nu, output = "list")
#' LRterms <- lapply(LR, function(lr) {
#'   lr[["coeff"]] * SchurPol(3, lr[["lambda"]])
#' })
#' smu_times_snu <- Reduce(`+`, LRterms)
#' smu_times_snu == SchurPol(3, mu) * SchurPol(3, nu) # should be TRUE
LRmult <- function(mu, nu, output = "dataframe") {
  stopifnot(isPartition(mu), isPartition(nu))
  output <- match.arg(output, c("dataframe", "list"))
  add <- function(old, lambda) {
    insertWith(`+`, old, partitionAsString(lambda), 1L)
  }
  v <- Reduce(add, addMu(mu, nu), init = integer(0L))
  if(output == "dataframe") {
    data.frame("coeff" = v, "lambda" = names(v))
  } else {
    lambdasAsStrings <- names(v)
    out <- lapply(lambdasAsStrings, function(lambdaAsString) {
      list(
        "coeff"  = v[[lambdaAsString]],
        "lambda" = fromPartitionAsString(lambdaAsString)
      )
    })
    names(out) <- lambdasAsStrings
    out
  }
}

#' @importFrom utils tail
drop <- function(n, x) {
  tail(x, max(length(x) - n, 0L))
}

addMu <- function(mu, part) {
  go <- function(ubs, mms, dds, part) {
    if(length(mms) == 0L) {
      list(part)
    } else {
      d <- dds[1L]
      ds <- dds[-1L]
      ms <- mms[-1L]
      L <- addRowOf(ubs, part)
      LL <- lapply(L, function(x) {
        ubsprime <- x[[1L]]
        partprime <- x[[2L]]
        go(drop(d, ubsprime), ms, ds, partprime)
      })
      do.call(c, LL)
    }
  }
  ubs0 <- headOrZero(part) + seq_len(headOrZero(mu))
  dmu <- diffSeq(mu)
  go(ubs0, mu, dmu, part)
}

addRowOf <- function(pcols, part) {
  go <- function(lb, ububs, p, ncols) {
    if(length(ububs) == 0L) {
      list(list(rev(ncols), p))
    } else {
      ub <- ububs[1L]
      ubs <- ububs[-1L]
      LL <- lapply(newBoxes(lb + 1L, ub, p), function(ij) {
        col <- ij[2L]
        go(col, ubs, addBox(ij, p), c(col, ncols))
      })
      do.call(c, LL)
    }
  }
  go(0L, pcols, part, integer(0L))
}

newBoxes <- function(lb, ub, part) {
  go <- function(i, jjs, lp) {
    if(length(jjs) == 0L) {
      if(lb <= 1L && 1L <= ub && lp > 0L) {
        list(c(i, 1L))
      } else {
        list()
      }
    } else {
      j <- jjs[1L]
      js <- jjs[-1L]
      j1 <- j + 1L
      if(j1 < lb) {
        list()
      } else if(j1 <= ub && lp > j) {
        c(list(c(i, j1)), go(i + 1L, js, j))
      } else {
        go(i + 1L, js, j)
      }
    }
  }
  rev(go(1L, part, headOrZero(part) + 1L))
}

addBox <- function(kl, part) {
  k <- kl[1L]
  go <- function(i, pps) {
    if(length(pps) == 0L) {
      if(i == k) {
        1L
      } else {
        stop("addBox: shouldn't happen")
      }
    } else {
      p <- pps[1L]
      ps <- pps[-1L]
      if(i == k) {
        c(p + 1L, ps)
      } else {
        c(p, go(i + 1L, ps))
      }
    }
  }
  go(1L, part)
}

headOrZero <- function(xs) {
  if(length(xs) == 0L) {
    0L
  } else {
    xs[1L]
  }
}

diffSeq <- function(x) {
  diff(-c(x, 0L))
}


#~~ Littlewood-Richardson for skew Schur polynomial ~~####

#' @title Littlewood-Richardson rule for skew Schur polynomial
#' @description Expression of a skew Schur polynomial as a linear
#'   combination of Schur polynomials.
#'
#' @param lambda,mu integer partitions defining the skew partition:
#'   \code{lambda} is the outer partition and \code{mu} is the inner partition
#'   (so \code{mu} must be a subpartition of \code{lambda})
#' @param output the type of the output, \code{"dataframe"} or \code{"list"}
#'
#' @return This computes the expression of the skew Schur polynomial
#'   associated to the skew partition defined by \code{lambda} and \code{mu}
#'   as a linear combination of Schur polynomials. Every coefficient of this
#'   linear combination is a positive integer, a so-called
#'   Littlewood-Richardson coefficient.
#'   If \code{output="dataframe"},
#'   the output is a dataframe with two columns: the column \code{coeff} gives
#'   the coefficients of this linear combination, and the column \code{nu}
#'   gives the partitions defining the Schur polynomials of this linear
#'   combination as character strings, e.g. the partition \code{c(4, 3, 1)} is
#'   given by \code{"[4, 3, 1]"}. If \code{output="list"}, the output is a list
#'   of lists with two elements. Each of these lists with two elements
#'   corresponds to a term of the linear combination: the first element, named
#'   \code{coeff}, is the coefficient, namely the Littlewood-Richardson
#'   coefficient \eqn{c^{\lambda}_{\mu,\nu}}, where \eqn{\nu} is the integer
#'   partition given in the second element of the list, named
#'   \code{nu}, which defines the Schur polynomial of the linear
#'   combination.
#' @export
#'
#' @examples
#' library(jack)
#' LRskew(lambda = c(4, 2, 1), mu = c(3, 1))
LRskew <- function(lambda, mu, output = "dataframe") {
  stopifnot(isPartition(lambda), isPartition(mu))
  output <- match.arg(output, c("list", "dataframe"))
  lambda <- as.integer(removeTrailingZeros(lambda))
  mu <- as.integer(removeTrailingZeros(mu))
  ellLambda <- length(lambda)
  ellMu <- length(mu)
  if(ellLambda < ellMu) {
    stop("The partition `mu` is not a subpartition of the partition `lambda`.")
  }
  mu <- c(mu, rep(0L, ellLambda - ellMu))
  if(any(lambda < mu)) {
    stop("The partition `mu` is not a subpartition of the partition `lambda`.")
  }
  n <- sum(lambda - mu)
  if(n == 0L) {
    if(output == "dataframe") {
      return(data.frame("coeff" = 1L, "nu" = "[]"))
    } else {
      return(list(list("coeff" = 1L, "nu" = integer(0L))))
#      return(list("coeff" = 1L, "nu" = list(integer(0L))))
    }
  }
  f <- function(old, nu) {
    insertWith(`+`, old, partitionAsString(nu), 1L)
  }
  Liab <- rev(zip3(seq_len(ellLambda), lambda, mu))
  diagram <- do.call(rbind, do.call(c, lapply(Liab, function(iab) {
    i <- iab[1L]
    a <- iab[2L]
    b <- iab[3L]
    jvec <- if(b < a) (b + 1L):a else integer(0L)
    lapply(jvec, function(j) {
      c(i, j)
    })
  })))
  Lnu <- lapply(fillings(n, diagram), `[[`, 1L)
  v <- Reduce(f, Lnu, init = integer(0L))
  if(output == "dataframe") {
    data.frame("coeff" = v, "nu" = names(v))
  } else {
    nusAsStrings <- names(v)
    out <- lapply(nusAsStrings, function(nuAsString) {
      list(
        "coeff" = v[[nuAsString]],
        "nu" = fromPartitionAsString(nuAsString)
      )
    })
    names(out) <- nusAsStrings
    out
    # partitions <- lapply(names(v), fromPartitionAsString)
    # list("coeff" = unname(v), "nu" = partitions)
  }
}

zip3 <- function(v1, v2, v3) {
  lapply(1L:length(v1), function(i) {
    c(v1[i], v2[i], v3[i])
  })
}

fillings <- function(n, diagram) {
  if(nrow(diagram) == 0L) {
    list(list(integer(0L), integer(0L)))
  } else {
    xy <- diagram[1L, ]
    x <- xy[1L]
    y <- xy[2L]
    rest <- diagram[-1L, , drop = FALSE]
    diagram <- apply(diagram, 1L, partitionAsString)
    upper <-
      n + 1L - match(partitionAsString(c(x, y + 1L)), diagram, nomatch = n + 1L)
    lower <-
      n + 1L - match(partitionAsString(c(x - 1L, y)), diagram, nomatch = n + 1L)
    L <- lapply(fillings(n - 1L, rest), function(filling) {
      nextLetter(lower, upper, filling)
    })
    do.call(c, L)
  }
}

nextLetter <- function(lower, upper, filling) {
  nu <- filling[[1L]]
  lpart <- filling[[2L]]
  shape <- c(nu, 0L)
  lb <- if(lower > 0L) lpart[lower] else 0L
  ub <- if(upper > 0L) min(length(shape), lpart[upper]) else length(shape)
  f <- function(j) {
    if(j == 1L || shape[j-1L] > shape[j]) j else 0L
  }
  v <- vapply((lb+1L):ub, f, integer(1L))
  nlist <- v[v > 0L]
  lapply(nlist, function(i) {
    list(incr(i, shape), c(lpart, i))
  })
}

incr <- function(i, xxs) {
  if(length(xxs) == 0L) {
    integer(0L)
  } else {
    if(i == 0L) {
      finish(xxs)
    } else if(i == 1L) {
      c(xxs[1L] + 1L, finish(xxs[-1L]))
    } else {
      c(xxs[1L], incr(i - 1L, xxs[-1L]))
    }
  }
}

finish <- function(xxs) {
  if(length(xxs) == 0L) {
    integer(0L)
  } else {
    x <- xxs[1L]
    if(x > 0L) {
      c(x, finish(xxs[-1L]))
    } else {
      integer(0L)
    }
  }
}




#' @title Skew Schur polynomial
#'
#' @description Returns the skew Schur polynomial.
#'
#' @param n number of variables, a positive integer
#' @param lambda,mu integer partitions defining the skew partition:
#'   \code{lambda} is the outer partition and \code{mu} is the inner partition
#'   (so \code{mu} must be a subpartition of \code{lambda})
#'
#' @return A \code{qspray} multivariate polynomial, the skew Schur polynomial
#'   associated to the skew partition defined by \code{lambda} and \code{mu}.
#'
#' @details The computation is performed with the help of the
#'   Littlewood-Richardson rule (see \code{\link{LRskew}}).
#'
#' @export
#'
#' @examples
#' SkewSchurPol(3, lambda = c(3, 2, 1), mu = c(1, 1))
SkewSchurPol <- function(n, lambda, mu) {
  stopifnot(isPositiveInteger(n))
  LR <- LRskew(lambda, mu, output = "list")
  LRterms <- lapply(LR, function(coeff_nu) {
    coeff_nu[["coeff"]] * SchurPol(n, coeff_nu[["nu"]])
  })
  # LRcoeffs <- LR[["coeff"]]
  # LRparts <- LR[["nu"]]
  # LRterms <- lapply(1:length(LRcoeffs), function(i) {
  #   LRcoeffs[i] * SchurPol(n, LRparts[[i]])
  # })
  Reduce(`+`, LRterms)
}
