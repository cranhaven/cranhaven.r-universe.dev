# domegaSq <- function(x, df1, df2, populationOmegaSq = 0) {
#   if (populationOmegaSq != 0) {
#     cat0("Noncentrality parameters not implemented yet, sorry!\n");
#   }
#   ### Return density for given omega squared
#   return(df(convert.omegasq.to.f(x, df1, df2), df1, df2));
# }


#' The distribution of Omega Squared
#'
#' These functions use some conversion to and from the *F* distribution to
#' provide the Omega Squared distribution.
#'
#' The functions use [convert.omegasq.to.f()] and
#' [convert.f.to.omegasq()] to provide the Omega Squared
#' distribution.
#'
#' @aliases domegaSq pomegaSq qomegaSq romegaSq
#' @param x,q Vector of quantiles, or, in other words, the value(s) of Omega
#' Squared.
#' @param p Vector of probabilites (*p*-values).
#' @param df1,df2 Degrees of freedom for the numerator and the denominator,
#' respectively.
#' @param n Desired number of Omega Squared values.
#' @param populationOmegaSq The value of Omega Squared in the population; this
#' determines the center of the Omega Squared distribution. This has not been
#' implemented yet in this version of `ufs`. If anybody
#' has the inverse of [convert.ncf.to.omegasq()] for me, I'll happily
#' integrate this.
#' @param lower.tail logical; if TRUE (default), probabilities are the
#' likelihood of finding an Omega Squared smaller than the specified value;
#' otherwise, the likelihood of finding an Omega Squared larger than the
#' specified value.
#' @return `domegaSq` gives the density, `pomegaSq` gives the
#' distribution function, `qomegaSq` gives the quantile function, and
#' `romegaSq` generates random deviates.
#' @author Gjalt-Jorn Peters
#'
#' Maintainer: Gjalt-Jorn Peters <gjalt-jorn@@userfriendlyscience.com>
#' @seealso [convert.omegasq.to.f()],
#' [convert.f.to.omegasq()], [df()], [pf()],
#' [qf()], [rf()]
#' @keywords univar
#' @examples
#'
#' ### Generate 10 random Omega Squared values
#' romegaSq(10, 66, 3);
#'
#' ### Probability of findings an Omega Squared
#' ### value smaller than .06 if it's 0 in the population
#' pomegaSq(.06, 66, 3);
#'
#' @rdname omegasq
#' @export
pomegaSq <- function(q, df1, df2, populationOmegaSq = 0, lower.tail=TRUE) {
  if (populationOmegaSq != 0) {
    cat0("Noncentrality parameters not implemented yet, sorry!\n");
  }
  ### Return p-value for given omega squared
  return(stats::pf(convert.omegasq.to.f(q, df1, df2), df1, df2, lower.tail=lower.tail) );
}

#' @rdname omegasq
#' @export
qomegaSq <- function(p, df1, df2, populationOmegaSq = 0, lower.tail=TRUE) {
  if (populationOmegaSq != 0) {
    cat0("Noncentrality parameters not implemented yet, sorry!\n");
  }
  ### Return omega squared for given p-value
  return(convert.f.to.omegasq(stats::qf(p, df1, df2, lower.tail=lower.tail), df1, df2));
}

#' @rdname omegasq
#' @export
romegaSq <- function(n, df1, df2, populationOmegaSq = 0) {
  if (populationOmegaSq != 0) {
    cat0("Noncentrality parameters not implemented yet, sorry!\n");
  }
  ### Return random omega squared value(s)
  return(convert.f.to.omegasq(stats::rf(n, df1, df2), df1, df2));
}

#' @rdname omegasq
#' @export
domegaSq <- function(x, df1, df2, populationOmegaSq = 0) {
  return(stats::df(convert.omegasq.to.f(x, df1, df2), df1, df2,
                   ncp = convert.omegasq.to.f(populationOmegaSq, df1, df2)));
}

# pomegaSq <- function(q, df1, df2, populationOmegaSq = 0, lower.tail=TRUE) {
#   if (populationOmegaSq != 0) {
#     cat0("Noncentrality parameters not implemented yet, sorry!\n");
#   }
#   ### Return p-value for given omega squared
#   return( pf(convert.omegasq.to.f(q, df1, df2), df1, df2,
#              lower.tail=lower.tail,
#              ncp = convert.omegasq.to.f(populationOmegaSq, df1, df2)) );
# }
#
# qomegaSq <- function(p, df1, df2, populationOmegaSq = 0, lower.tail=TRUE) {
#   if (populationOmegaSq != 0) {
#     cat0("Noncentrality parameters not implemented yet, sorry!\n");
#   }
#   ### Return omega squared for given p-value
#   return(convert.f.to.omegasq(qf(p, df1, df2,
#                                  lower.tail=lower.tail,
#                                  ncp = convert.omegasq.to.f(populationOmegaSq, df1, df2)),
#                               df1, df2));
# }
#
# romegaSq <- function(n, df1, df2, populationOmegaSq = 0) {
#   if (populationOmegaSq != 0) {
#     cat0("Noncentrality parameters not implemented yet, sorry!\n");
#   }
#   ### Return random omega squared value(s)
#   return(convert.f.to.omegasq(rf(n, df1, df2,
#                                  ncp = convert.omegasq.to.f(populationOmegaSq, df1, df2)),
#                               df1, df2));
# }

