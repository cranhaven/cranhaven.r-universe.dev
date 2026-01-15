# SSYTXwithGivenShapeAndContent <- function(lambda, mu) { # mu partition
#   if(sum(lambda) != sum(mu)) {
#     return(list())
#   }
#   if(!isDominated(mu, lambda)) {
#     return(list())
#   }
#   if(all(lambda == 1L)) {
#     if(all(mu == 1L)) {
#       return(list(as.list(seq_along(lambda))))
#     } else {
#       return(list())
#     }
#   }
#   l <- length(mu)
#   mu[l] <- mu[l] - 1L
#   mu <- removeTrailingZeros(mu)
#   out <- list()
#   for(i in seq_along(lambda)) {
#     kappa <- lambda
#     kappa[i] <- kappa[i] - 1L
#     if(isDecreasing(kappa)) {
#       kappa <- removeTrailingZeros(kappa)
#       ssytx <- SSYTXwithGivenShapeAndContent(kappa, mu)
#       ssytx <- lapply(ssytx, function(ssyt) {
#         copy <- ssyt
#         if(i > length(ssyt)) {
#           row <- integer(0L)
#         } else {
#           row <- ssyt[[i]]
#         }
#         copy[[i]] <- c(row, l)
#         copy
#       })
#       out <- c(out, unique(ssytx))
#     }
#   }
#   unique(out)
# }

#' @importFrom utils tail
#' @noRd
charge <- function(w) {
  l <- length(w)
  if(l == 0L) {
    return(0L)
  }
  n <- max(w)
  if(n == 1L) {
    return(0L)
  }
  pos <- match(1L, w)
  index <- 0L
  indices <- integer(n-1L)
  positions <- integer(n)
  positions[1L] <- pos
  for(r in 1L + seq_len(n-1L)) {
    v <- tail(w, l - pos)
    pos <- match(r, v) + pos
    if(is.na(pos)) {
      pos <- match(r, w)
      index <- index + 1L
    }
    indices[r-1L] <- index
    positions[r] <- pos
  }
  sum(indices) + charge(w[-positions])
}

ssytWord <- function(ssyt) {
  do.call(c, lapply(ssyt, rev))
}

#' @title Kostka-Foulkes polynomial
#' @description Kostka-Foulkes polynomial for two given partitions.
#'
#' @param lambda,mu integer partitions; in order for the Kostka-Foulkes
#'   polynomial to be non-zero, a necessary condition is that \code{lambda}
#'   and \code{mu} have the same weight; more precisely, \code{mu} must
#'   be dominated by \code{lambda}
#'
#' @return The Kostka-Foulkes polynomial associated to \code{lambda} and
#'   \code{mu}. This is a univariate \code{qspray} polynomial whose value
#'   at \code{1} is the Kostka number associated to \code{lambda} and
#'   \code{mu}.
#' @export
#' @importFrom qspray qlone qzero showQsprayOption<- showQsprayXYZ qsprayMaker
#' @importFrom syt ssytx_withGivenShapeAndWeight
KostaFoulkesPolynomial <- function(lambda, mu) {
  stopifnot(isPartition(lambda), isPartition(mu))
  lambda <- removeTrailingZeros(as.integer(lambda))
  mu <- removeTrailingZeros(as.integer(mu))
  if(isDominated(mu, lambda)) {
    # Tx <- SSYTXwithGivenShapeAndContent(lambda, mu)
    Tx <- ssytx_withGivenShapeAndWeight(lambda, mu)
    charges <- lapply(Tx, function(ssyt) {
      charge(ssytWord(ssyt))
    })
    out <- qsprayMaker(powers = charges, coeffs = rep("1", length(charges)))
    # ws <- lapply(Tx, function(t) unlist(lapply(t, rev)))
    # charges <- vapply(ws, charge, integer(1L))
    # t <- qlone(1L)
    # out <- Reduce(`+`, lapply(charges, function(e) t^e))
  } else {
    out <- qzero()
  }
  showQsprayOption(out, "showQspray") <- showQsprayXYZ("t")
  out
}

#' @importFrom utils head
#' @importFrom qspray qone
#' @noRd
invUnitTriMatrix <- function(L) {
  d <- length(L)
  if(d == 1L) {
    return(L)
  } else {
    B <- invUnitTriMatrix(lapply(head(L, -1L), function(row) {
      head(row, -1L)
    }))
    newColumn <- lapply(seq_len(d-1L), function(i) {
      toAdd <- mapply(
        `*`,
        B[[i]], lapply(i:(d-1L), function(j) {
          L[[j]][[d-j+1L]]
        }),
        SIMPLIFY = FALSE, USE.NAMES = FALSE
      )
      -Reduce(`+`, toAdd)
    })
    B <- c(B, list(list()))
    newColumn <- c(newColumn, list(qone()))
    names(B) <- names(newColumn) <- names(L)
    Names <- lapply(L, names)
    mapply(
      function(row, x, nms) {
        out <- c(row, list(x))
        names(out) <- nms
        out
      },
      B, newColumn, Names,
      SIMPLIFY = FALSE, USE.NAMES = TRUE
    )
  }
}

#' @importFrom symbolicQspray Qzero showSymbolicQsprayOption<-
#' @importFrom ratioOfQsprays showRatioOfQspraysXYZ
#' @importFrom methods as
#' @noRd
HallLittlewoodP <- function(n, lambda) {
  weight <- sum(lambda)
  lambdas <- listOfPartitions(weight)
  lambdaStrings <- vapply(lambdas, partitionAsString, character(1L))
  names(lambdas) <- lambdaStrings
  i <- match(partitionAsString(lambda), lambdaStrings)
  lambdas <- lambdas[i:length(lambdas)]
  kfs <- lapply(seq_along(lambdas), function(j) {
    kappas <- lambdas[j:length(lambdas)]
    names(kappas) <- vapply(kappas, partitionAsString, character(1L))
    kappa <- kappas[[1L]]
    lapply(kappas, function(mu) {
      KostaFoulkesPolynomial(kappa, mu)
    })
  })
  names(kfs) <- lambdaStrings[i:length(lambdaStrings)]
  coeffs <- invUnitTriMatrix(kfs)
  coeffs <- coeffs[[lambdaStrings[i]]]
  hlp <- Qzero()
  for(mu in names(coeffs)) {
    c <- coeffs[[mu]]
    mu <- fromPartitionAsString(mu)
    hlp <- hlp + c * as(SchurPol(n, mu), "symbolicQspray")
  }
  showSymbolicQsprayOption(hlp, "showRatioOfQsprays") <-
    showRatioOfQspraysXYZ("t")
  hlp
}

phi_r <- function(r) {
  t <- qlone(1L)
  Reduce(`*`, lapply(seq_len(r), function(i) (1L-t^i)))
}

b <- function(lambda) {
  m <- vapply(unique(lambda), function(i) sum(lambda == i), integer(1L))
  Reduce(`*`, lapply(m, phi_r))
}

#' @title Hall-Littlewood polynomial
#' @description Hall-Littlewood polynomial of a given partition.
#'
#' @param n number of variables
#' @param lambda integer partition
#' @param which which Hall-Littlewood polynomial, \code{"P"} or \code{"Q"}
#'
#' @return The Hall-Littlewood polynomial in \code{n} variables of the
#'   integer partition \code{lambda}. This is a \code{symbolicQspray}
#'   polynomial with a unique parameter usually denoted by \eqn{t} and
#'   its coefficients are polynomial in this parameter. When substituting
#'   \eqn{t} with \eqn{0} in the Hall-Littlewood \eqn{P}-polynomials, one
#'   obtains the Schur polynomials.
#' @export
#' @importFrom symbolicQspray Qzero Qone
HallLittlewoodPol <- function(n, lambda, which = "P") {
  stopifnot(isPositiveInteger(n))
  stopifnot(isPartition(lambda))
  which <- match.arg(which, c("P", "Q"))
  lambda <- as.integer(removeTrailingZeros(lambda))
  if(length(lambda) == 0L) {
    return(Qone())
  }
  if(length(lambda) > n) {
    return(Qzero())
  }
  Qspray <- HallLittlewoodP(n, lambda)
  if(which == "Q") {
    Qspray <- b(lambda) * Qspray
  }
  Qspray
}
