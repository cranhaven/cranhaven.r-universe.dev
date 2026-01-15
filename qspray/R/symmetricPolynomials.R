#' @include qspray.R
NULL

#' @title Power sum polynomial
#' @description Returns a power sum function as a polynomial.
#'
#' @param m integer, the number of variables
#' @param lambda an integer partition, given as a vector of decreasing
#'   positive integers
#'
#' @return A \code{qspray} object.
#' @export
#'
#' @examples
#' library(qspray)
#' PSFpoly(3, c(3, 1))
PSFpoly <- function(m, lambda) {
  stopifnot(isNonnegativeInteger(m), isPartition(lambda))
  lambda <- removeTrailingZeros(lambda)
  if(length(lambda) == 0L) {
    return(qone())
  }
  # if(any(lambda > m)) return(as.qspray(0L))
  # if(length(lambda) > m) return(qzero())
  out <- 1L
  for(k in lambda) {
    powers <- lapply(1L:m, function(i) {
      c(rep(0L, i-1L), k)
    })
    pk <- qsprayMaker(powers = powers, coeffs = rep("1", m))
    out <- out * pk
  }
  out
}

#' @title Monomial symmetric function
#' @description Returns a monomial symmetric function as a polynomial.
#'
#' @param m integer, the number of variables
#' @param lambda an integer partition, given as a vector of decreasing
#'   positive integers
#'
#' @return A \code{qspray} object.
#' @importFrom DescTools Permn
#' @export
#'
#' @examples
#' library(qspray)
#' MSFpoly(3, c(3, 1))
MSFpoly <- function(m, lambda) {
  stopifnot(isNonnegativeInteger(m), isPartition(lambda))
  lambda <- removeTrailingZeros(lambda)
  if(length(lambda) > m) return(qzero())
  kappa                    <- numeric(m)
  kappa[seq_along(lambda)] <- lambda
  perms <- Permn(kappa)
  powers <- Rows(perms)
  coeffs <- rep("1", nrow(perms))
  qsprayMaker(powers, coeffs)
}

#' @importFrom partitions parts
#' @noRd
.CSHFpoly <- function(m, k) {
  lambdas <- parts(k)
  qsprays <- apply(lambdas, 2L, function(lambda) {
    MSFpoly(m, lambda)
  }, simplify = FALSE)
  Reduce(`+`, qsprays)
}

#' @title Complete homogeneous symmetric function
#' @description Returns a complete homogeneous symmetric function as a 
#'   \code{qspray} polynomial.
#'
#' @param m integer, the number of variables
#' @param lambda an integer partition, given as a vector of decreasing
#'   positive integers
#'
#' @return A \code{qspray} object.
#' @export
#'
#' @examples
#' library(qspray)
#' CSHFpoly(3, c(3, 1))
CSHFpoly <- function(m, lambda) {
  stopifnot(isNonnegativeInteger(m), isPartition(lambda))
  lambda <- removeTrailingZeros(lambda)
  if(length(lambda) > m) return(qzero())
  if(length(lambda) == 0L) return(qone())
  qsprays <- lapply(lambda, function(k) {
    .CSHFpoly(m, k)
  })
  Reduce(`*`, qsprays)
}

#' @title Symmetric polynomial in terms of the monomial symmetric polynomials
#' @description Expression of a symmetric polynomial as a linear combination 
#'   of the monomial symmetric polynomials.
#'
#' @param qspray a \code{qspray} object defining a symmetric polynomial 
#' @param check Boolean, whether to check the symmetry
#'
#' @return A list defining the combination. Each element of this list is a 
#'   list with two elements: \code{coeff}, a \code{bigq} number, and 
#'   \code{lambda}, an integer partition; then this list corresponds to the 
#'   term \code{coeff * MSFpoly(n, lambda)}, where \code{n} is the number of 
#'   variables in the symmetric polynomial.
#' @export
#' @importFrom methods as canCoerce
#'
#' @examples
#' qspray <- PSFpoly(4, c(3, 1)) + ESFpoly(4, c(2, 2)) + 4L
#' MSPcombination(qspray)
MSPcombination <- function(qspray, check = TRUE) {
  constantTerm <- getConstantTerm(qspray)
  if(isConstant(qspray)) {
    if(isQzero(qspray)) {
      return(list())
    } else {
      return(
        list(list("coeff" = constantTerm, "lambda" = integer(0L)))
      )
    }
  }
  # M <- powersMatrix(qspray - constantTerm)
  # M <- M[lexorder(M), , drop = FALSE]
  # # plutôt filtrer:
  # lambdas <- unique(apply(M, 1L, function(expnts) { 
  #   toString(sort(expnts[expnts != 0L], decreasing = TRUE))
  # }))
  lambdas <- Filter(isDecreasing, orderedQspray(qspray - constantTerm)@powers)
  out <- lapply(lambdas, function(lambda) {
    # lambda <- fromString(lambda)
    list(
      "coeff"  = getCoefficient(qspray, lambda),
      "lambda" = lambda 
    )
  })
  names(out) <- vapply(lambdas, partitionAsString, character(1L))
  if(constantTerm != 0L) {
    out <- c(
      out, list("[]" = list("coeff" = constantTerm, "lambda" = integer(0L)))
    )
  }
  if(check) {
    test <- checkSymmetry(qspray, out)
    if(!test) {
      stop("The polynomial is not symmetric.")
    }
  }
  out
}

checkSymmetry <- function(qspray, mspCombination) {
  cl <- class(qspray)[1L]
  if(!canCoerce(qzero(), cl)) {
    stop(
      "Cannot check symmetry for this type of polynomials."
    )
  }
  n <- numberOfVariables(qspray)
  check <- as(qzero(), cl)
  for(t in mspCombination) {
    coeff  <- t[["coeff"]]
    lambda <- t[["lambda"]]
    check  <- check + coeff * as(MSFpoly(n, lambda), cl)
  }
  check == qspray
}

setGeneric(
  "compactSymmetricQspray",
  function(qspray, check) {
    NULL
  }
)

#' @name compactSymmetricQspray
#' @aliases compactSymmetricQspray,qspray,logical-method compactSymmetricQspray,qspray-method
#' @docType methods
#' @title Compact symmetric qspray
#' @description Prints a symmetric qspray polynomial as a linear combination of 
#'   the monomial symmetric polynomials. 
#'
#' @param qspray a \code{qspray} object, which should correspond to a 
#'   symmetric polynomial
#' @param check Boolean, whether to check the symmetry (default \code{TRUE})
#'
#' @return A character string.
#' @export
#' @importFrom gmp c_bigq
#' 
#' @seealso \code{\link{MSPcombination}}
#'
#' @examples
#' library(qspray)
#' ( qspray <- PSFpoly(4, c(3, 1)) - ESFpoly(4, c(2, 2)) + 4L )
#' compactSymmetricQspray(qspray, check = TRUE)
setMethod(
  "compactSymmetricQspray", c("qspray", "logical"),
  function(qspray, check) {
    combo <- MSPcombination(qspray, check = check)
    powers <- lapply(combo, `[[`, "lambda")
    coeffs <- lapply(combo, `[[`, "coeff")
    coeffs <- as.character(c_bigq(coeffs))
    msp <- new("qspray", powers = powers, coeffs = coeffs)
    #passShowAttributes(qspray, msp)
    showMonomial <- function(exponents) {
      sprintf("M[%s]", toString(exponents))
    }
    showQsprayOption(msp, "showMonomial") <- showMonomial
    f <- getShowQspray(msp)
    f(msp)
  }
)

#' @rdname compactSymmetricQspray
setMethod(
  "compactSymmetricQspray", c("qspray"),
  function(qspray) {
    compactSymmetricQspray(qspray, check = FALSE)
  }
)

#' @title Elementary symmetric polynomial
#' @description Returns an elementary symmetric function as a polynomial.
#'
#' @param m integer, the number of variables
#' @param lambda an integer partition, given as a vector of decreasing
#'   positive integers
#'
#' @return A \code{qspray} object.
#' @importFrom DescTools Permn
#' @export
#'
#' @examples
#' library(qspray)
#' ESFpoly(3, c(3, 1))
ESFpoly <- function(m, lambda) {
  stopifnot(isNonnegativeInteger(m), isPartition(lambda))
  lambda <- lambda[lambda > 0]
  if(any(lambda > m)) return(as.qspray(0L))
  out <- 1L
  for(k in seq_along(lambda)) {
    kappa <- integer(m)
    kappa[seq_len(lambda[k])] <- rep(1L, lambda[k])
    perms <- Permn(kappa)
    powers <- Rows(perms)
    ek <- qsprayMaker(powers = powers, coeffs = rep("1", nrow(perms)))
    out <- out * ek
  }
  out
}

#' @title Check symmetry of a polynomial
#' @description Check whether a \code{qspray} polynomial is symmetric.
#'
#' @param qspray a \code{qspray} polynomial
#'
#' @return A Boolean value indicating whether the polynomial defined by 
#'   \code{qspray} is symmetric. 
#' @export
#' 
#' @seealso \code{\link{MSPcombination}}, \code{\link{compactSymmetricQspray}}
#'
#' @examples
#' e1 <- ESFpoly(3, 1)
#' e2 <- ESFpoly(3, 2)
#' e3 <- ESFpoly(3, 3)
#' q <- e1 + 2*e2 + 3*e3 + 4*e1*e3
#' isSymmetricQspray(q)
isSymmetricQspray <- function(qspray) {
  mspCombination <- MSPcombination(qspray, check = FALSE)
  checkSymmetry(qspray, mspCombination)
  # n <- arity(qspray)
  # i_ <- seq_len(n)
  # E <- lapply(i_, function(i) ESFpoly(n, i))
  # Y <- lapply(i_, function(i) qlone(n + i))
  # G <- lapply(i_, function(i) E[[i]] - Y[[i]])
  # B <- groebner(G, TRUE, FALSE)
  # constantTerm <- getCoefficient(qspray, integer(0L))
  # g <- qdivision(qspray - constantTerm, B)
  # check <- all(vapply(g@powers, function(pwr) {
  #   length(pwr) > n && all(pwr[1L:n] == 0L)
  # }, logical(1L)))
  # if(!check) {
  #   return(FALSE)
  # }
  # powers <- lapply(g@powers, function(pwr) {
  #   pwr[-(1L:n)]
  # })
  # P <- qsprayMaker(powers, g@coeffs) + constantTerm
  # out <- TRUE
  # attr(out, "poly") <- P
  # out
}


#### ~ Hall inner product ~ ####

#' @importFrom partitions compositions
#' @importFrom gmp as.bigq
#' @importFrom DescTools Permn
#' @noRd
E_lambda_mu <- function(lambda, mu) {
  ell_lambda <- length(lambda)
  ell_mu     <- length(mu)
  if(ell_mu > ell_lambda) {
    return(0L)
  }
  # chaque composition donne les longueurs des nu_i 
  compos <- compositions(ell_lambda, ell_mu, include.zero = FALSE)
  compos <- Columns(compos)
  lambdas <- Permn(lambda)
  L <- do.call(c, lapply(Rows(lambdas), function(lambdaPerm) {
    Filter(Negate(is.null), lapply(compos, function(compo) {
      partitionSequences(lambdaPerm, mu, compo)
    }))
  }))
  if(length(L) == 0L) {
    return(as.bigq(0L))
  }
  out <- Reduce(`+`, lapply(L, function(nus) {
    E_lambda_mu_term(mu, nus)
  }))
  if((ell_lambda - ell_mu) %% 2L == 1L) {
    out <- -out
  } 
  return(out)
}

#' @importFrom utils head
partitionSequences <- function(lambda, mu, compo) {
  starts <- cumsum(c(0L, head(compo, -1L))) + 1L
  ends   <- cumsum(c(0L, head(compo, -1L))) + compo
  nus <- lapply(seq_along(compo), function(i) {
    lambda[(starts[i]):(ends[i])]
  })
  weights <- vapply(nus, function(nu) {
    as.integer(sum(nu))
  }, integer(1L))
  test <- all(mu == weights) && all(vapply(nus, function(nu) {
    all(diff(nu) <= 0L)
  }, logical(1L)))
  if(test) {
    nus
  } else {
    NULL
  }
}

#' @importFrom gmp factorialZ
#' @noRd
E_lambda_mu_term <- function(mu, nus) {
  toMultiply <- lapply(seq_along(nus), function(i) {
    nu  <- nus[[i]]
    mjs <- vapply(as.integer(unique(nu)), function(j) {
      sum(nu == j)
    }, integer(1L))
    mu[i] * factorialZ(length(nu)-1L) / prod(factorialZ(mjs))
  })
  Reduce(`*`, toMultiply)
}

#' Monomial symmetric polynomial as linear combination of power sum polynomials
#' @importFrom gmp as.bigq as.bigz c_bigq
#' @importFrom partitions parts
#' @noRd
MSPinPSbasis <- function(mu) {
  if(length(mu) == 0L) {
    return(
      list(
        "coeff"  = as.bigq(1L),
        "lambda" = integer(0L)
      )
    )
  }
  mu <- as.integer(mu)
  partitions <- lapply(Columns(parts(sum(mu))), removeTrailingZeros)
  coeffs <- vector("list", length(partitions))
  k <- 1L
  for(lambda in partitions) {
    coeffs[[k]] <- E_lambda_mu(mu, lambda)
    k <- k + 1L    
  }
  qspray <- qsprayMaker(powers = partitions, coeffs = c_bigq(coeffs))
  lambdas <- qspray@powers
  weights <- as.bigq(qspray@coeffs)
  lapply(seq_along(weights), function(i) {
    lambda <- lambdas[[i]]
    list(
      "coeff"  = weights[i] / as.bigz(zlambda(lambda, alpha = 1L)),
      "lambda" = lambda
    )
  })
}

# also used in the Hall inner product
zlambda <- function(lambda, alpha) {
  parts <- unique(lambda)
  mjs   <- vapply(parts, function(j) {
    sum(lambda == j)
  }, integer(1L))
  out <- prod(factorial(mjs) * parts^mjs) 
  if(alpha != 1L) {
    out <- out * alpha^length(lambda)
  }
  out
}

#' @title Symmetric polynomial in terms of the power sum polynomials
#' @description Expression of a symmetric \code{qspray} polynomial as a 
#'   polynomial in the power sum polynomials.
#'
#' @param qspray a symmetric \code{qspray} polynomial; symmetry is not checked 
#'
#' @return A \code{qspray} polynomial, say \eqn{P}, such that 
#'   \eqn{P(p_1, ..., p_n)} equals the input symmetric polynomial, 
#'   where \eqn{p_i} is the i-th power sum polynomial (\code{PSFpoly(n, i)}).
#' @export
#' @importFrom gmp c_bigq
#' @seealso \code{\link{PSPcombination}}
#' 
#' @examples
#' # take a symmetric polynomial
#' ( qspray <- ESFpoly(4, c(2, 1)) + ESFpoly(4, c(2, 2)) )
#' # compute the power sum expression
#' ( pspExpr <- PSPexpression(qspray) )
#' # take the involved power sum polynomials
#' psPolys <- lapply(1:numberOfVariables(pspExpr), function(i) PSFpoly(4, i))
#' # then this should be TRUE:
#' qspray == changeVariables(pspExpr, psPolys)
PSPexpression <- function(qspray) {
  constantTerm <- getConstantTerm(qspray)
  mspdecomposition <- MSPcombination(qspray - constantTerm, check = FALSE)
  pspexpression <- qzero()
  for(t in mspdecomposition) {
    xs     <- MSPinPSbasis(t[["lambda"]])
    coeffs <- t[["coeff"]] * c_bigq(lapply(xs, `[[`, "coeff"))
    powers <- lapply(xs, function(x) {
      lambda <- x[["lambda"]]
      vapply(seq_len(lambda[1L]), function(j) {
        sum(lambda == j)
      }, integer(1L))
    })
    p             <- qsprayMaker(powers, coeffs)
    pspexpression <- pspexpression + p
  }
  out <- pspexpression + constantTerm
  
  # n <- arity(qspray)
  # i_ <- seq_len(n)
  # P <- lapply(i_, function(i) PSFpoly(n, i))
  # Y <- lapply(i_, function(i) qlone(n + i))
  # G <- lapply(i_, function(i) P[[i]] - Y[[i]])
  # B <- groebner(G, TRUE, FALSE)
  # constantTerm <- getCoefficient(qspray, integer(0L))
  # g            <- qdivision(qspray - constantTerm, B)
  # check <- all(vapply(g@powers, function(pwr) {
  #   length(pwr) > n && all(pwr[1L:n] == 0L)
  # }, logical(1L)))
  # if(!check) {
  #   stop("PSPpolyExpr: the polynomial is not symmetric.")
  # }
  # powers <- lapply(g@powers, function(pwr) {
  #   pwr[-(1L:n)]
  # })
  # out <- qsprayMaker(powers, g@coeffs) + constantTerm
  attr(out, "PSPexpression") <- TRUE
  out
}

#' helper function for PSPcombination
#' returns a qspray representing the linear combination of power sum 
#' polynomials: the powers represent the integer partitions of the power sum 
#' polynomials
#' @noRd
.PSPcombination <- function(qspray) {
  cl <- class(qspray)[1L]
  if(!canCoerce(qzero(), cl)) {
    stop(
      "Invalid object `qspray`."
    )
  }
  mspAssocsList <- MSPcombination(qspray, check = FALSE)
  psPolysAsQspray <- as(qzero(), cl)
  if(cl == "qspray") {
    for(t in mspAssocsList) {
      pairs <- MSPinPSbasis(t[["lambda"]])
      coeffs <- t[["coeff"]] * c_bigq(lapply(pairs, `[[`, "coeff"))
      lambdas <- lapply(pairs, `[[`, "lambda")
      psPolysAsQspray <- psPolysAsQspray + 
        new(
          "qspray", powers = lambdas, coeffs = as.character(coeffs)
        )
    }
  } else {
    for(t in mspAssocsList) {
      pairs <- MSPinPSbasis(t[["lambda"]])
      coeffs <- mapply(
        `*`, list(t[["coeff"]]), lapply(pairs, `[[`, "coeff"),
        SIMPLIFY = FALSE, USE.NAMES = FALSE
      )
      lambdas <- lapply(pairs, `[[`, "lambda")
      psPolysAsQspray <- psPolysAsQspray + 
        new(
          cl, powers = lambdas, coeffs = coeffs
        )
    }
  }
  psPolysAsQspray
}

#' @title Symmetric polynomial as a linear combination of some power sum 
#'   polynomials
#' @description Expression of a symmetric \code{qspray} polynomial as a 
#'   linear combination of some power sum polynomials.
#'
#' @param qspray a symmetric \code{qspray} polynomial; symmetry is not checked 
#'
#' @return A list of pairs. Each pair is made of a \code{bigq} number, the 
#'   coefficient of the term of the linear combination, and an integer 
#'   partition, corresponding to a power sum polynomial.
#' @export
#' @importFrom gmp c_bigq
#' @seealso \code{\link{PSPexpression}}.
#' 
#' @examples
#' # take a symmetric polynomial
#' ( qspray <- ESFpoly(4, c(2, 1)) + ESFpoly(4, c(2, 2)) )
#' # compute the power sum combination
#' ( pspCombo <- PSPcombination(qspray) )
#' # then the polynomial can be reconstructed as follows:
#' Reduce(`+`, lapply(pspCombo, function(term) {
#'   term[["coeff"]] * PSFpoly(4, term[["lambda"]])
#' }))
PSPcombination <- function(qspray) {
  psPolysAsQspray <- orderedQspray(.PSPcombination(qspray))
  lambdas <- psPolysAsQspray@powers
  # we extract the coefficients as follows to keep the show options 
  # in case of a symbolic qspray, because they are lost if we do @coeffs:
  coeffs <- lapply(lambdas, function(exponents) {
    getCoefficient(psPolysAsQspray, exponents)
  })
  lambdaStrings <- vapply(lambdas, partitionAsString, character(1L))
  out <- mapply(
    function(x, y) `names<-`(list(x, y), c("coeff", "lambda")),
    coeffs, lambdas, SIMPLIFY = FALSE, USE.NAMES = FALSE
  )
  names(out) <- lambdaStrings
  out
  # mspAssocsList <- MSPcombination(qspray, check = FALSE)
  # pspCombinations <- lapply(mspAssocsList, function(t) {
  #   xs     <- MSPinPSbasis(t[["lambda"]])
  #   coeffs <- t[["coeff"]] * c_bigq(lapply(xs, `[[`, "coeff"))
  #   lambdas <- lapply(xs, `[[`, "lambda")
  #   lambdaStrings <- vapply(lambdas, partitionAsString, character(1L))
  #   out <- mapply(
  #     function(x, y) `names<-`(list(x, y), c("coeff", "lambda")), 
  #     coeffs, lambdas, SIMPLIFY = FALSE, USE.NAMES = FALSE
  #   )
  #   names(out) <- lambdaStrings
  #   out
  # })
  # accum <- function(assocs1, assocs2) {
  #   lambdas1 <- names(assocs1) 
  #   lambdas2 <- names(assocs2)
  #   intersection <- intersect(lambdas1, lambdas2)
  #   merged <- mapply(
  #     function(pair1, pair2) {
  #       list(
  #         "coeff" = pair1[["coeff"]] + pair2[["coeff"]],
  #         "lambda" = pair1[["lambda"]]
  #       )
  #     },
  #     assocs1[intersection], assocs2[intersection],
  #     SIMPLIFY = FALSE, USE.NAMES = TRUE
  #   )
  #   c(
  #     merged, 
  #     assocs1[setdiff(lambdas1, intersection)], 
  #     assocs2[setdiff(lambdas2, intersection)]
  #   )
  # }
  # pspCombination <- Reduce(accum, x = pspCombinations)
  # pspCombination <- 
  #   Filter(function(term) term[["coeff"]] != 0L, pspCombination)
  # if(length(pspCombination) <= 1L) {
  #   return(pspCombination)
  # }
  # lambdas <- lapply(pspCombination, `[[`, "lambda")
  # n <- max(lengths(lambdas))
  # lambdasMatrix <- t(vapply(lambdas, function(lambda) {
  #   grow(lambda, n)
  # }, integer(n)))
  # i_ <- do.call(
  #   order, c(Columns(lambdasMatrix), decreasing = TRUE)
  # )
  # pspCombination[i_]
}

#' @title Hall inner product
#' @description Hall inner product of two symmetric polynomials. It has a 
#'   parameter \code{alpha} and the standard Hall inner product is the case 
#'   when \code{alpha=1}. It is possible to get the Hall inner product with 
#'   a symbolic \code{alpha} parameter.
#'
#' @param qspray1,qspray2 two symmetric \code{qspray} polynomials
#' @param alpha parameter equal to \code{1} for the usual Hall inner product, 
#'   otherwise this is the "Jack parameter"; it must be either a single value 
#'   coercible to a \code{bigq} number, e.g. \code{"2/5"}, or \code{NULL} to 
#'   get the Hall product with a symbolic \code{alpha}
#'
#' @return A \code{bigq} number if \code{alpha} is not \code{NULL}, otherwise 
#'   a univariate \code{qspray} polynomial.
#' @export
#' @importFrom gmp as.bigq
#' @importFrom methods canCoerce as
HallInnerProduct <- function(qspray1, qspray2, alpha = 1) {
  cl <- class(qspray1)[1L]
  if(!canCoerce(qzero(), cl) || !inherits(qspray2, cl)) {
    stop(
      "Invalid `qspray` objects."
    )
  }
  zeroQspray <- qspray1 - qspray1 # instead of as(qzero(), cl), to keep the show options
  symbolic <- is.null(alpha)
  if(symbolic) {
    if(cl == "qspray") {
      showQsprayOption(zeroQspray, "x") <- "alpha"
    } else {
      showSymbolicQsprayOption(zeroQspray, "showMonomial") <- 
        showMonomialXYZ("alpha")
    }
    if(isQzero(qspray1) || isQzero(qspray2)) {
      return(zeroQspray)
    }
    alpha <- as(qlone(1L), cl)
  } else {
    stopifnot(length(alpha) == 1L)
    alpha <- as.bigq(alpha)
    if(is.na(alpha)) {
      stop("Invalid `alpha`.")
    }
    if(isQzero(qspray1) || isQzero(qspray2)) {
      return(getConstantTerm(zeroQspray))
    }
  }
  # if(!symbolic && isTRUE(attr(qspray1, "PSPexpression"))) {
  #   PSspray1 <- qspray1
  #   PSspray2 <- PSPexpression(qspray2)
  # } else {
  #   PSspray1 <- PSPexpression(qspray1)
  #   if(qspray2 == qspray1) {
  #     PSspray2 <- PSspray1
  #   } else {
  #     PSspray2 <- PSPexpression(qspray2)
  #   }
  # }
  # powers1 <- PSspray1@powers
  # coeffs1 <- PSspray1@coeffs
  # out <- as.bigq(0L)
  # for(k in seq_along(powers1)) {
  #   pows <- powers1[[k]]
  #   coeff2 <- getCoefficient(PSspray2, pows)
  #   if(coeff2 != 0L) {
  #     lambda <- 
  #       unlist(lapply(rev(seq_along(pows)), function(i) rep(i, pows[i])))
  #     out <- out + as.bigq(coeffs1[k]) * coeff2 * zlambda(lambda, alpha)
  #   }
  # }
  PSspray1 <- .PSPcombination(qspray1)
  PSspray2 <- .PSPcombination(qspray2)
  lambdas1 <- PSspray1@powers
  coeffs1 <- PSspray1@coeffs
  lambdas2 <- PSspray2@powers
  coeffs2 <- PSspray2@coeffs
  if(cl == "qspray") {
    coeffs1 <- as.bigq(coeffs1)
    coeffs2 <- as.list(as.bigq(coeffs2)) # need list to set names
  }
  lambdaStrings <- vapply(lambdas2, partitionAsString, character(1L))
  names(coeffs2) <- lambdaStrings
  if(symbolic) {
    out <- zeroQspray
  } else {
    out <- getConstantTerm(zeroQspray) # the zero ratioOfQsprays
  }
  for(k in seq_along(lambdas1)) {
    lambda <- lambdas1[[k]]
    s <- partitionAsString(lambda) 
    if(s %in% lambdaStrings) {
      coeff2 <- coeffs2[[s]]
      out <- out + coeffs1[[k]] * coeff2 * zlambda(lambda, alpha)
    }
  }
  out
}
