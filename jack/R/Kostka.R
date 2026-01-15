#' @importFrom partitions conjugate
#' @importFrom gmp as.bigq
#' @importFrom utils tail
#' @noRd
.KostkaJackNumbersWithGivenLambda <- function(lambda, alpha, output) {
  mus <- rev(listOfDominatedPartitions(lambda))
  nparts <- length(mus)
  musAsStrings <-
    vapply(mus, partitionAsString, character(1L), USE.NAMES = FALSE)
  kNumbers <- vector("list", nparts)
  names(kNumbers) <- musAsStrings
  kNumbers[[1L]] <- as.bigq(1L)
  if(nparts >= 2L) {
    names(mus) <- musAsStrings
    ellLambda <- length(lambda)
    lambdap <- conjugate(lambda)
    nlambda <- sum(seq_len(ellLambda - 1L) * tail(lambda, -1L))
    nlambdap <- sum(seq_len(lambda[1L] - 1L) * tail(lambdap, -1L))
    elambda <- alpha*nlambdap - nlambda
    for(muAsString in tail(musAsStrings, -1L)) {
      mu <- mus[[muAsString]]
      mup <- conjugate(mu)
      ellMu <- mup[1L]
      i_ <- seq_len(ellMu - 1L)
      nmu <- sum(i_ * tail(mu, -1L))
      nmup <- sum(seq_len(mu[1L] - 1L) * tail(mup, -1L))
      emu <- alpha*nmup - nmu
      ee <- elambda - emu
      x <- 0L
      for(i in i_) {
        mu_i <- mu[i]
        for(j in (i+1L):ellMu) {
          mu_j <- mu[j]
          dmuij <- mu_i - mu_j
          kappa <- mu
          for(t in seq_len(mu_j - 1L)) {
            kappa[i] <- mu_i + t
            kappa[j] <- mu_j - t
            kappaOrd <- sort(kappa, decreasing = TRUE)
            kappaOrdAsString <- partitionAsString(kappaOrd)
            if(kappaOrdAsString %in% musAsStrings){
              x <- x + kNumbers[[kappaOrdAsString]] * (dmuij + 2L*t)
            }
          }
          mu_i_plus_mu_j <- mu_i + mu_j
          kappa[i] <- mu_i_plus_mu_j
          kappaOrd <- sort(kappa[-j], decreasing = TRUE)
          kappaOrdAsString <- partitionAsString(kappaOrd)
          if(kappaOrdAsString %in% musAsStrings){
            x <- x + kNumbers[[kappaOrdAsString]] * mu_i_plus_mu_j
          }
        }
      }
      kNumbers[[muAsString]] <- x / ee
    }
  }
  if(output == "list") {
    mapply(
      function(kNumber, mu) {
        list("mu" = mu, "value" = kNumber)
      },
      kNumbers, mus,
      USE.NAMES = TRUE, SIMPLIFY = FALSE
    )
  } else {
    vapply(kNumbers, as.character, character(1L), USE.NAMES = TRUE)
  }
}

#' @title Kostka-Jack numbers with a given partition \eqn{\lambda}
#'
#' @description Kostka numbers with Jack parameter, or Kostka-Jack numbers
#'   \eqn{K_{\lambda,\mu}(\alpha)} for a given Jack parameter \eqn{\alpha}
#'   and a given integer partition \eqn{\lambda}.
#'
#' @param lambda integer partition
#' @param alpha the Jack parameter, a \code{bigq} number or anything coercible
#'   to a \code{bigq} number
#' @param output the format of the output, either \code{"vector"} or
#'   \code{"list"}
#'
#' @return If \code{output="vector"}, this function returns a named vector.
#'   This vector is made of the non-zero (i.e. positive) Kostka-Jack numbers
#'   \eqn{K_{\lambda,\mu}(\alpha)} given as character strings and its names
#'   encode the partitions \eqn{\mu}.
#'   If \code{ouput="list"}, this function returns a list of lists.
#'   Each of these lists has two
#'   elements. The first one is named \code{mu} and is an integer
#'   partition, and the second one is named \code{value} and is a \code{bigq}
#'   rational number, the Kostka-Jack number \eqn{K_{\lambda,\mu}(\alpha)}.
#' @export
#' @seealso \code{\link{KostkaJackNumbers}},
#'   \code{\link{symbolicKostkaJackNumbersWithGivenLambda}}.
#'
#' @details The Kostka-Jack number \eqn{K_{\lambda,\mu}(\alpha)} is the
#'   coefficient of the monomial symmetric polynomial \eqn{m_\mu} in the
#'   expression of the \eqn{P}-Jack polynomial \eqn{P_\lambda(\alpha)} as a
#'   linear combination of monomial symmetric polynomials. For \eqn{\alpha=1}
#'   it is the ordinary Kostka number.
#'
#' @examples
#' KostkaJackNumbersWithGivenLambda(c(3, 2), alpha = "2")
KostkaJackNumbersWithGivenLambda <- function(lambda, alpha, output = "vector") {
  stopifnot(isPartition(lambda))
  alpha <- as.bigq(alpha)
  if(is.na(alpha)) {
    stop("Invalid `alpha`.")
  }
  output <- match.arg(output, c("vector", "list"))
  lambda <- removeTrailingZeros(as.integer(lambda))
  .KostkaJackNumbersWithGivenLambda(lambda, alpha, output)
}

#' @title Kostka-Jack numbers with a given Jack parameter
#'
#' @description Kostka numbers with Jack parameter, or Kostka-Jack numbers,
#'   for partitions of a given weight and a given Jack parameter.
#'
#' @param n positive integer, the weight of the partitions
#' @param alpha the Jack parameter, a \code{bigq} number or an object coercible
#'   to a \code{bigq} number
#'
#' @return The matrix of the Kostka-Jack numbers \eqn{K_{\lambda,\mu}(\alpha)}
#'   given as character strings representing integers or fractions.
#'   The row names of this matrix encode the partitions \eqn{\lambda} and
#'   the column names encode the partitions \eqn{\mu}
#' @export
#' @seealso \code{\link{KostkaJackNumbersWithGivenLambda}},
#'   \code{\link{symbolicKostkaJackNumbers}},
#'   \code{\link{skewKostkaJackNumbers}}.
#' @importFrom gmp as.bigq c_bigq
#'
#' @details The Kostka-Jack number \eqn{K_{\lambda,\mu}(\alpha)} is the
#'   coefficient of the monomial symmetric polynomial \eqn{m_\mu} in the
#'   expression of the \eqn{P}-Jack polynomial \eqn{P_\lambda(\alpha)} as a
#'   linear combination of monomial symmetric polynomials. For \eqn{\alpha=1}
#'   it is the ordinary Kostka number.
#'
#' @examples
#' KostkaJackNumbers(4)
KostkaJackNumbers <- function(n, alpha = "1") {
  stopifnot(isPositiveInteger(n))
  alpha <- as.bigq(alpha)
  if(is.na(alpha)) {
    stop("Invalid `alpha`.")
  }
  lambdas <- listOfPartitions(n)
  lambdasAsStrings <-
    vapply(lambdas, partitionAsString, character(1L), USE.NAMES = FALSE)
  zeros <- rep("0", length(lambdas))
  names(zeros) <- lambdasAsStrings
  Knumbers <- do.call(
    rbind,
    lapply(lambdas, function(lambda) {
      kNumbersLambda <-
        .KostkaJackNumbersWithGivenLambda(lambda, alpha, "vector")
      zeros[names(kNumbersLambda)] <- kNumbersLambda
      zeros
    })
  )
  colnames(Knumbers) <- rownames(Knumbers) <- lambdasAsStrings
  Knumbers
  # if(n == 0L) {
  #   Knumbers <- as.matrix("1")
  #   colnames(Knumbers) <- rownames(Knumbers) <- "()"
  #   return(Knumbers)
  # }
}

#' @importFrom qspray qone qlone
#' @importFrom partitions conjugate
#' @importFrom utils tail
#' @importFrom ratioOfQsprays as.ratioOfQsprays showRatioOfQspraysXYZ showRatioOfQspraysOption<-
#' @noRd
.symbolicKostkaJackNumbersWithGivenLambda <- function(lambda, n, which) {
  mus <- rev(listOfDominatedPartitions(lambda))
  if(!is.null(n)) {
    mus <- Filter(
      function(mu) {
        length(mu) <= n
      },
      mus
    )
  }
  nparts <- length(mus)
  musAsStrings <-
    vapply(mus, partitionAsString, character(1L), USE.NAMES = FALSE)
  kNumbers <- vector("list", nparts)
  names(kNumbers) <- musAsStrings
  if(which == "P") {
    kN1 <- as.ratioOfQsprays(1L)
  } else {
    invPcoeff <- as.ratioOfQsprays(symbolicJackPcoefficientInverse(lambda))
    if(which == "J") {
      kN1 <- invPcoeff
    } else if(which == "C") {
      kN1 <- invPcoeff * symbolicJackCcoefficient(lambda)
    } else {
      kN1 <- invPcoeff / symbolicJackQcoefficientInverse(lambda)
    }
  }
  showRatioOfQspraysOption(kN1, "showRatioOfQsprays") <-
    showRatioOfQspraysXYZ("alpha")
  kNumbers[[1L]] <- kN1
  if(nparts >= 2L) {
    alpha <- qlone(1L)
    names(mus) <- musAsStrings
    ellLambda <- length(lambda)
    lambdap <- conjugate(lambda)
    nlambda <- sum(seq_len(ellLambda - 1L) * tail(lambda, -1L))
    nlambdap <- sum(seq_len(lambda[1L] - 1L) * tail(lambdap, -1L))
    elambda <- alpha*nlambdap - nlambda
    for(muAsString in tail(musAsStrings, -1L)) {
      mu <- mus[[muAsString]]
      mup <- conjugate(mu)
      ellMu <- mup[1L]
      i_ <- seq_len(ellMu - 1L)
      nmu <- sum(i_ * tail(mu, -1L))
      nmup <- sum(seq_len(mu[1L] - 1L) * tail(mup, -1L))
      emu <- alpha*nmup - nmu
      ee <- elambda - emu
      x <- 0L
      for(i in i_) {
        mu_i <- mu[i]
        for(j in (i+1L):ellMu) {
          mu_j <- mu[j]
          dmuij <- mu_i - mu_j
          kappa <- mu
          for(t in seq_len(mu_j - 1L)) {
            kappa[i] <- mu_i + t
            kappa[j] <- mu_j - t
            kappaOrd <- sort(kappa, decreasing = TRUE)
            kappaOrdAsString <- partitionAsString(kappaOrd)
            if(kappaOrdAsString %in% musAsStrings){
              x <- x + kNumbers[[kappaOrdAsString]] * (dmuij + 2L*t)
            }
          }
          mu_i_plus_mu_j <- mu_i + mu_j
          kappa[i] <- mu_i_plus_mu_j
          kappaOrd <- sort(kappa[-j], decreasing = TRUE)
          kappaOrdAsString <- partitionAsString(kappaOrd)
          if(kappaOrdAsString %in% musAsStrings){
            x <- x + kNumbers[[kappaOrdAsString]] * mu_i_plus_mu_j
          }
        }
      }
      kNumbers[[muAsString]] <- x / ee
    }
  }
  kNumbers
}

#' @title Kostka-Jack numbers with symbolic Jack parameter for a
#'   given \eqn{\lambda}
#'
#' @description Kostka-Jack numbers \eqn{K_{\lambda,\mu}(\alpha)} with a
#'   symbolic Jack parameter \eqn{\alpha} for a given
#'   integer partition \eqn{\lambda}.
#'
#' @param lambda integer partition
#'
#' @return A named list of \code{ratioOfQsprays} objects. The elements of this
#'   list are the Kostka-Jack numbers \eqn{K_{\lambda,\mu}(\alpha)} and
#'   its names correspond to the partitions \eqn{\mu}.
#' @export
#' @seealso \code{\link{KostkaJackNumbersWithGivenLambda}},
#'   \code{\link{symbolicKostkaJackNumbers}}.
#'
#' @examples
#' symbolicKostkaJackNumbersWithGivenLambda(c(3, 1))
symbolicKostkaJackNumbersWithGivenLambda <- function(lambda) {
  stopifnot(isPartition(lambda))
  lambda <- removeTrailingZeros(as.integer(lambda))
  .symbolicKostkaJackNumbersWithGivenLambda(lambda, NULL, "P")
}

#' @importFrom ratioOfQsprays as.ratioOfQsprays showRatioOfQspraysXYZ showRatioOfQspraysOption<-
#' @importFrom utils tail
#' @noRd
.symbolicKostkaNumbers <- function(n, weight, which){
  # lambdas <- apply(
  #   restrictedparts(weight, n), 2L,
  #   removeTrailingZeros, simplify = FALSE
  # )
  # restrictedparts does not correctly order
  lambdas <- Filter(
    function(lambda) {
      length(lambda) <= n
    },
    listOfPartitions(weight)
  )
  lambdasAsStrings <-
    vapply(lambdas, partitionAsString, character(1L), USE.NAMES = FALSE)
  names(lambdas) <- lambdasAsStrings
  nParts <- length(lambdas)
  zeros <- rep(list(as.ratioOfQsprays(0L)), nParts)
  names(zeros) <- lambdasAsStrings
  kNumbers <- lapply(seq_len(nParts), function(i) {
    kNumbersLambda <-
      .symbolicKostkaJackNumbersWithGivenLambda(lambdas[[i]], n, which)
    zeros[names(kNumbersLambda)] <- kNumbersLambda
    tail(zeros, nParts - i + 1L)
  })
  names(kNumbers) <- lambdasAsStrings
  kNumbers
  # stopifnot(n > 0L, isPositiveInteger(n))
  # stopifnot(isPositiveInteger(weight), weight > 0L)
  # zeroRatioOfQsprays <- as.ratioOfQsprays(0L)
  # unitRatioOfQsprays <- as.ratioOfQsprays(1L)
  # if(weight == 1L) {
  #   return(list("[1]" = list("[1]" = unitRatioOfQsprays)))
  # }
  # allParts <- restrictedparts(weight, n)
  # nParts <- ncol(allParts)
  # lambdas <- apply(allParts, 2L, function(part) {
  #   part[part != 0L]
  # }, simplify = FALSE)
  # stringParts <- vapply(lambdas, partitionAsString, character(1L))
  # row <- rep(list(zeroRatioOfQsprays), nParts)
  # names(row) <- stringParts
  # coefs <- lapply(seq_len(nParts), function(i) {
  #   part <- list(unitRatioOfQsprays)
  #   names(part) <- stringParts[i]
  #   parts <- tail(stringParts, nParts - i)
  #   c(part, row[parts])
  # })
  # names(coefs) <- stringParts
  # for(m in seq_len(nParts-1L)){
  #   lambda <- lambdas[[m]]
  #   eSymbolic_lambda <- .eSymbolic(lambda)
  #   for(k in (m+1L):nParts){
  #     mu <- lambdas[[k]]
  #     l <- length(mu)
  #     eSymbolic_mu <- .eSymbolic(mu)
  #     x <- zeroRatioOfQsprays
  #     for(i in 1L:(l-1L)){ # l is always >1
  #       for(j in (i+1L):l){
  #         for(t in seq_len(mu[j])){
  #           mucopy <- mu
  #           mucopy[i] <- mucopy[i] + t
  #           mucopy[j] <- mucopy[j] - t
  #           muOrd <- sort(mucopy[mucopy != 0L], decreasing = TRUE)
  #           if(isDominated(muOrd, lambda)){
  #             x <- x + (mucopy[i] - mucopy[j]) /
  #               (eSymbolic_lambda - eSymbolic_mu) *
  #               coefs[[m]][[partitionAsString(muOrd)]]
  #           }
  #         }
  #       }
  #     }
  #     showRatioOfQspraysOption(x, "showRatioOfQsprays") <-
  #       showRatioOfQspraysXYZ("alpha")
  #     coefs[[m]][[partitionAsString(mu)]] <- x
  #   }
  # }
  # if(which != "P") {
  #   names(lambdas) <- names(coefs)
  #   invPcoeffs <- lapply(lambdas, symbolicJackPcoefficientInverse)
  #   Names <- names(coefs)
  #   names(Names) <- Names
  #   if(which == "J") {
  #     coefs <- lapply(Names, function(lambda) {
  #       mapply(
  #         `*`,
  #         coefs[[lambda]], list(invPcoeffs[[lambda]]),
  #         SIMPLIFY = FALSE, USE.NAMES = TRUE
  #       )
  #     })
  #   } else if(which == "C") {
  #     Ccoefs <- lapply(lambdas, symbolicJackCcoefficient)
  #     factors <-
  #       mapply(`*`, Ccoefs, invPcoeffs, SIMPLIFY = FALSE, USE.NAMES = TRUE)
  #     coefs <- lapply(Names, function(lambda) {
  #       mapply(
  #         `*`,
  #         coefs[[lambda]], list(factors[[lambda]]),
  #         SIMPLIFY = FALSE, USE.NAMES = TRUE
  #       )
  #     })
  #   } else {
  #     invQcoeffs <- lapply(lambdas, symbolicJackQcoefficientInverse)
  #     factors <-
  #       mapply(`/`, invPcoeffs, invQcoeffs, SIMPLIFY = FALSE, USE.NAMES = TRUE)
  #     coefs <- lapply(Names, function(lambda) {
  #       mapply(
  #         `*`,
  #         coefs[[lambda]], list(factors[[lambda]]),
  #         SIMPLIFY = FALSE, USE.NAMES = TRUE
  #       )
  #     })
  #   }
  # }
  # coefs
}

#' @title Kostka-Jack numbers with symbolic Jack parameter
#'
#' @description Kostka-Jack numbers with a symbolic Jack parameter for
#'   integer partitions of a given weight.
#'
#' @param n positive integer, the weight of the partitions
#'
#' @return A named list of named lists of \code{ratioOfQsprays} objects.
#'   Denoting the Kostka-Jack numbers by \eqn{K_{\lambda,\mu}(\alpha)}, the
#'   names of the outer list correspond to the partitions \eqn{\lambda}, and
#'   the names of the inner lists correspond to the partitions \eqn{\mu}.
#' @export
#' @seealso \code{\link{KostkaJackNumbers}},
#'   \code{\link{symbolicKostkaJackNumbersWithGivenLambda}}.
#'
#' @examples
#' symbolicKostkaJackNumbers(3)
symbolicKostkaJackNumbers <- function(n) {
  stopifnot(isPositiveInteger(n))
  lambdas <- listOfPartitions(n)
  names(lambdas) <-
    vapply(lambdas, partitionAsString, character(1L), USE.NAMES = FALSE)
  lapply(lambdas, function(lambda) {
    .symbolicKostkaJackNumbersWithGivenLambda(lambda, NULL, "P")
  })
  # if(n == 0L) {
  #   list("[]" = list("[]" = as.ratioOfQsprays(1L)))
  # } else {
  #   .symbolicKostkaNumbers(n, n, which = "P")
  # }
}

#' @title Skew Kostka-Jack numbers with given Jack parameter
#' @description Skew Kostka-Jack numbers associated to a given skew partition
#'   and a given Jack parameter.
#'
#' @param lambda,mu integer partitions defining the skew partition:
#'   \code{lambda} is the outer partition and \code{mu} is the inner partition
#'   (so \code{mu} must be a subpartition of \code{lambda})
#' @param alpha the Jack parameter, a \code{bigq} number or an object coercible
#'   to a \code{bigq} number; setting \code{alpha=NULL} is equivalent to set
#'   \code{alpha=1}
#' @param output the format of the output, either \code{"vector"} or
#'   \code{"list"}
#'
#' @return If \code{output="vector"}, the function returns a named vector.
#'   This vector is made of the non-zero skew Kostka-Jack numbers
#'   \eqn{K_{\lambda/\mu,\nu}(\alpha)} given as character strings and its names
#'   encode the partitions \eqn{\nu}.
#'   If \code{ouput="list"}, the function returns a list. Each element of this
#'   list is a named list with two elements: an integer partition \eqn{\nu}
#'   in the field named \code{"nu"}, and the corresponding skew Kostka-Jack
#'   number \eqn{K_{\lambda/\mu,\nu}(\alpha)} in the field named \code{"value"}.
#'   Only the non-null skew Kostka-Jack numbers are provided by this list.
#' @export
#' @seealso \code{\link{symbolicSkewKostkaJackNumbers}}.
#'
#' @importFrom gmp as.bigq c_bigq
#' @importFrom utils head
#'
#' @details The skew Kostka-Jack number \eqn{K_{\lambda/\mu,\nu}(\alpha)} is
#'   the coefficient of the monomial symmetric polynomial \eqn{m_\nu} in the
#'   expression of the skew \eqn{P}-Jack polynomial
#'   \eqn{P_{\lambda/\mu}(\alpha)} as a linear combination of monomial
#'   symmetric polynomials. For \eqn{\alpha=1} it is the ordinary skew Kostka
#'   number.
#'
#' @note The skew Kostka-Jack numbers \eqn{K_{\lambda/\mu,\nu}(\alpha)} are
#'   well defined when the Jack parameter \eqn{\alpha} is zero, however this
#'   function does not work with \code{alpha=0}. A possible way to get the
#'   skew Kostka-Jack numbers \eqn{K_{\lambda/\mu,\nu}(0)} is to use the
#'   function \code{\link{symbolicSkewKostkaJackNumbers}} to get the skew
#'   Kostka-Jack numbers with a symbolic Jack parameter \eqn{\alpha}, and then
#'   to substitute \eqn{\alpha} with \eqn{0}.
#'
#' @examples
#' skewKostkaJackNumbers(c(4,2,2), c(2,2))
skewKostkaJackNumbers <- function(lambda, mu, alpha = NULL, output = "vector") {
  stopifnot(isPartition(lambda), isPartition(mu))
  output <- match.arg(output, c("vector", "list"))
  lambda <- as.integer(removeTrailingZeros(lambda))
  mu <- as.integer(removeTrailingZeros(mu))
  ellLambda <- length(lambda)
  ellMu <- length(mu)
  if(ellLambda < ellMu || any(head(lambda, ellMu) < mu)) {
    stop("The partition `mu` is not a subpartition of the partition `lambda`.")
  }
  if(is.null(alpha)) {
    alpha <- "1"
  }
  alpha <- as.bigq(alpha)
  if(is.na(alpha)) {
    stop("Invalid `alpha`.")
  }
  listOfKnumbers <- skewJackInMSPbasis(alpha, "P", lambda, mu)
  if(output == "vector") {
    values <- lapply(listOfKnumbers, `[[`, "coeff")
    kNumbers <- as.character(c_bigq(values))
    names(kNumbers) <- names(listOfKnumbers)
  } else {
    kNumbers <- lapply(
      listOfKnumbers,
      function(lst) {
        list("nu" = lst[["nu"]], "value" = lst[["coeff"]])
      }
    )
  }
  kNumbers
}

#' @title Skew Kostka-Jack numbers with symbolic Jack parameter
#' @description Skew Kostka-Jack numbers associated to a given skew partition
#'   with a symbolic Jack parameter.
#'
#' @param lambda,mu integer partitions defining the skew partition:
#'   \code{lambda} is the outer partition and \code{mu} is the inner partition
#'   (so \code{mu} must be a subpartition of \code{lambda})
#'
#' @return The function returns a list. Each element of this
#'   list is a named list with two elements: an integer partition \eqn{\nu}
#'   in the field named \code{"nu"}, and the corresponding skew Kostka number
#'   \eqn{K_{\lambda/\mu,\nu}(\alpha)} in the field named \code{"value"}, a
#'   \code{ratioOfQsprays} object.
#' @export
#' @importFrom utils head
#' @importFrom ratioOfQsprays showRatioOfQspraysXYZ showRatioOfQspraysOption<-
#'
#' @examples
#' symbolicSkewKostkaJackNumbers(c(4,2,2), c(2,2))
symbolicSkewKostkaJackNumbers <- function(lambda, mu) {
  stopifnot(isPartition(lambda), isPartition(mu))
  lambda <- as.integer(removeTrailingZeros(lambda))
  mu <- as.integer(removeTrailingZeros(mu))
  ellLambda <- length(lambda)
  ellMu <- length(mu)
  if(ellLambda < ellMu || any(head(lambda, ellMu) < mu)) {
    stop("The partition `mu` is not a subpartition of the partition `lambda`.")
  }
  listOfKnumbers <- skewSymbolicJackInMSPbasis("P", lambda, mu)
  lapply(
    listOfKnumbers,
    function(lst) {
      rOS <- lst[["coeff"]]
      showRatioOfQspraysOption(rOS, "showRatioOfQsprays") <-
        showRatioOfQspraysXYZ("alpha")
      list("nu" = lst[["nu"]], "value" = rOS)
    }
  )
}
