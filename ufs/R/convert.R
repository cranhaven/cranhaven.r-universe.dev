#' conversion functions
#'
#' These are a number of functions to convert statistics and effect size
#' measures from/to each other.
#'
#' Note that by default, the behavior of `convert.d.to.r` depends on the
#' sample size (see Bruce, Kromrey & Ferron, 1998).
#'
#' @name convert
#' @aliases convert convert.r.to.t convert.t.to.r convert.b.to.t convert.t.to.p
#' convert.f.to.p convert.f.to.d convert.chisq.to.p convert.chisq.to.V
#' convert.d.to.logodds convert.d.to.r convert.d.to.t convert.d.to.variance
#' convert.etasq.to.cohensf convert.etasq.to.r convert.f.to.etasq
#' convert.f.to.omegasq convert.fisherz.to.r convert.logodds.to.d
#' convert.logodds.to.r convert.or.to.d convert.or.to.r convert.r.to.d
#' convert.r.to.fisherz convert.r.to.p convert.t.to.d convert.V.to.r
#' convert.cohensf.to.omegasq convert.cohensfsq.to.omegasq convert.means.to.d
#' convert.ncf.to.omegasq convert.omegasq.to.cohensf
#' convert.omegasq.to.cohensfsq convert.omegasq.to.f convert.percentage.to.se
#' @param chisq,cohensf,cohensfsq,d,etasq,f,logodds,means,omegasq,or,p,r,t,z
#' The value of the relevant statistic or effect size.
#' @param ncf The value of a noncentrality parameter of the F distribution.
#' @param n,n1,n2,N,ns The number of observations that the r or t value is
#' based on, or the number of observations in each of the two groups for an
#' anova, or the total number of participants when specifying a noncentrality
#' parameter.
#' @param df,df1,df2 The degrees of freedrom for that statistic (for F, the
#' first one is the numerator (i.e. the effect), and the second one the
#' denominator (i.e. the error term).
#' @param proportion The proportion of participants in each of the two groups
#' in a t-test or anova. This is used to compute the sample size in each group
#' if the group sizes are unknown.  Thus, if you only provide df1 and df2 when
#' converting an F value to a Cohen's d value, equal group sizes are assumed.
#' @param b The value of a regression coefficient.
#' @param se,sds The standard error of standard errors of the relevant
#' statistic (e.g. of a regression coefficient) or variables.
#' @param minDim The smallest of the number of columns and the number of rows
#' of the crosstable for which the chisquare is translated to a Cramer's V
#' value.
#' @param lower.tail For the F and chisquare distributions, whether to get the
#' probability of the lower or upper tail.
#' @param akfEq8 When converting Cohen's *d* to *r*, for small sample
#' sizes, bias is introduced when the commonly suggested formula is used
#' (Aaron, Kromrey & Ferron, 1998). Therefore, by default, this function uses
#' different equations depending on the sample size (for n < 50 and for n >
#' 50). When `akfEq8` is set to TRUE or FALSE, the corresponding action is
#' taken; when `akfEq8` is not logical (i.e. TRUE or FALSE), the function
#' depends on the sample size.
#' @param var.equal Whether to compute the value of *t* or Cohen's
#' *d* assuming equal variances ('yes'), unequal variances ('no'), or
#' whether to test for the difference ('test').
#' @return
#'
#' The converted value as a numeric value.
#' @author Gjalt-Jorn Peters and Peter Verboon
#'
#' Maintainer: Gjalt-Jorn Peters <ufs@@opens.science>
#' @references Aaron, B. Kromrey J. D. & Ferron, J. (1998) *Equating
#' "r"-based and "d"-based Effect Size Indices: Problems with a Commonly
#' Recommended Formula.* Paper presented at the Annual Meeting of the Florida
#' Educational Research Association (43rd, Orlando, FL, November 2-4, 1998).
#' @keywords utilities
#' @examples
#'
#' convert.t.to.r(t=-6.46, n=200);
#' convert.r.to.t(r=-.41, n=200);
#'
#' ### Compute some p-values
#' convert.t.to.p(4.2, 197);
#' convert.chisq.to.p(5.2, 3);
#' convert.f.to.p(8.93, 3, 644);
#'
#' ### Convert d to r using both equations
#' convert.d.to.r(d=.2, n1=5, n2=5, akfEq8 = FALSE);
#' convert.d.to.r(d=.2, n1=5, n2=5, akfEq8 = TRUE);
#'
NULL

### See https://statistiki.eu/wiki/Converting_Effect_Sizes for some formulae

###########################################################################
### Converting from: Cohen's d
###########################################################################

#' @export
convert.d.to.r <- function(d, n1 = NULL, n2 = NULL, akfEq8='if (n1 + n2) < 50') {
  if (!is.logical(akfEq8) && !is.null(n1) && !is.null(n2) && ((n1 + n2) < 50)) {
    akfEq8 <- TRUE;
  }
  if (is.logical(akfEq8) && akfEq8) {
    if (is.null(n1) || is.null(n2)) stop("Cannot use the equation by Aaron, Kromrey & Ferron (1998) if you do not specify both sample sizes!");
    N <- n2 + n2;
    return(sqrt( d^2 / ( d^2 + ((N^2 - 2 * N) / (n1 * n2)) ) ));
  } else {
    if (is.null(n1) || is.null(n2)) {
      a <- 4;
    } else {
      a <- (n1 + n2) ^ 2 / (n1 * n2)
    }
    return(d / sqrt(d^2 + a));
  }
}

#' @export
convert.d.to.t <- function(d, df=NULL, n1=NULL, n2=NULL, proportion=.5) {
  ### Obsolete; not basing computation on
  ### reversal of formula used in e.g.
  ### https://journal.frontiersin.org/article/10.3389/fpsyg.2013.00863/full
  #   return(ifelse(d < 0,
  #                 -1 * sqrt(sqrt(n) * abs(d)),
  #                 sqrt(sqrt(n) * abs(d))));

  if (is.null(df) && !is.null(n1) && !is.null(n2)) {
    groupSize1 <- n1;
    groupSize2 <- n2;
  }
  else if (!is.null(df) && is.null(n1) && is.null(n2)) {
    groupSize1 <-      proportion  * (df + 2);
    groupSize2 <- (1 - proportion) * (df + 2);
  }
  else {
    warning("Specify either df (and ideally proportion) or n1 and n2! Returning NA.");
    return(NA);
  }

  ### MBESS uses "ncp <- smd * sqrt((n.1 * n.2)/(n.1 + n.2))", but this
  ### gives the same result

  multiplier <- sqrt((1 / groupSize1) + (1 / groupSize2));

  t <- d / multiplier;

  return(t);

}

#' @export
convert.d.to.logodds <- function(d) {
  if (!is.numeric(d) || (length(d) > 1)) {
    stop("The 'd' argument is not a single numeric value!");
  }
  return(d * (pi / sqrt(3)));
}

#' @export
convert.d.to.variance <- function(d, n1, n2) {
  return( (((n1+n2) / (n1*n2)) + ((d^2) / (2*(n1+n2-2)))) * ((n1+n2) / (n1+n2-2)) );
}

###########################################################################
### Converting from: Log Odds
###########################################################################

#' @export
convert.logodds.to.d <- function(logodds) {
  return(logodds * (sqrt(3) / pi));
}

#' @export
convert.logodds.to.r <- function(logodds) {
  return(convert.d.to.r(convert.logodds.to.d(logodds)));
}

###########################################################################
### Converting from: Odds Ratio
###########################################################################

#' @export
convert.or.to.d <- function(or) {
  return(log(or) * (sqrt(3) / pi));
}

#' @export
convert.or.to.r <- function(or) {
  return(convert.d.to.r(convert.logodds.to.d(log(or))));
}

###########################################################################
### Converting from: Proportions (percentages)
###########################################################################

#' @export
convert.percentage.to.se <- function(p, n) {
  return(sqrt((p * (1-p)) / n));
}

###########################################################################
### Converting from: Pearson's r
###########################################################################

#' @export
convert.r.to.t <- function(r, n) {
  return(r * sqrt((n - 2) / (1-r^2)));
}

#' @export
convert.r.to.d <- function(r) {
  return( (r*2) / sqrt(1 - r^2));
}

#' @export
convert.r.to.p <- function(r, n) {
  t <- convert.r.to.t(r, n);
  return(convert.t.to.p(t, n - 2));
}

#' @export
convert.r.to.fisherz <- function(r) {
  return(.5 * log((1+r) / (1-r)));
}

###########################################################################
### Converting from: Student's t
###########################################################################

#' @export
convert.t.to.r <- function(t, n) {
  return(t / (sqrt(n-2+t^2)));
}

#' @export
convert.t.to.p <- function(t, df) {
  return(2*stats::pt(-abs(t),df));
}

#' @export
convert.t.to.d <- function(t, df=NULL, n1=NULL, n2=NULL, proportion=.5) {

  if (is.null(df) && !is.null(n1) && !is.null(n2)) {
    groupSize1 <- n1;
    groupSize2 <- n2;
  }
  else if (!is.null(df) && is.null(n1) && is.null(n2)) {
    groupSize1 <-      proportion  * (df + 2);
    groupSize2 <- (1 - proportion) * (df + 2);
  }
  else {
    warning("Specify either df (and ideally proportion) or n1 and n2! Returning NA.");
    return(NA);
  }

  ### Updated to reflect https://journal.frontiersin.org/article/10.3389/fpsyg.2013.00863/full
  # multiplier <- sqrt(((groupSize1 + groupSize2) / (groupSize1 * groupSize2)) *
  #                      ((groupSize1 + groupSize2) / (groupSize1 + groupSize2 - 2)));
  multiplier <- sqrt((1 / groupSize1) + (1 / groupSize2));

  d <- t * multiplier;

  return(d);
}


###########################################################################
### Converting from: Chi Square
###########################################################################

#' @export
convert.chisq.to.V <- function(chisq, n, minDim) {
  if (!is.numeric(chisq) || (length(chisq) > 1)) {
    stop("The 'chisq' argument is not a single numeric value!");
  }
  if (!is.numeric(n) || (length(n) > 1)) {
    stop("The 'n' argument is not a single numeric value!");
  }
  if (!is.numeric(minDim) || (length(minDim) > 1)) {
    stop("The 'minDim' argument is not a single numeric value!");
  }
  res <- as.numeric(sqrt(chisq/(n*(minDim - 1))));
  return(ifelse(is.finite(res), res, NA));
}

#' @export
convert.chisq.to.p <- function(chisq, df, lower.tail=FALSE) {
  return(2*stats::pchisq(chisq, df, lower.tail=lower.tail));
}

#' @export
convert.V.to.r <- function(V) {
  return(V);
}

###########################################################################
### Converting from: F
###########################################################################

#' @export
convert.f.to.p <- function(f, df1, df2, lower.tail=FALSE) {
  return(2*stats::pf(f, df1, df2, lower.tail=lower.tail));
}

#' @export
convert.f.to.d <- function(f, df1, df2 = NULL, n1=NULL, n2=NULL, proportion=.5) {
  if (df1 != 1) {
    warning("You can only convert an F value for the comparison of two groups to Cohen's d, ",
            "and you specified a df1 of ", df1, ", which means this F value concerns the comparison ",
            "of ", df1 + 1, " groups. Returning NA.");
    return(NA);
  }
  else if (is.null(df2) && !is.null(n1) && !is.null(n2)) {
    groupSize1 <- n1;
    groupSize2 <- n2;
  }
  else if (!is.null(df2) && is.null(n1) && is.null(n2)) {
    groupSize1 <- proportion * (df1 + df2 + 1);
    groupSize2 <- (1 - proportion) * (df1 + df2 + 1);
  }
  else {
    warning("Specify either df2 (and ideally proportion) or n1 and n2! Returning NA.");
    return(NA);
  }

  d <- sqrt(f * ((groupSize1 + groupSize2) / (groupSize1 * groupSize2)) *
              ((groupSize1 + groupSize2) / (groupSize1 + groupSize2 - 2)));

  return(d);
}

#' @export
convert.f.to.etasq <- function(f, df1, df2) {
  return( (f * df1) / ((f * df1) + df2) );
}

#' @export
convert.f.to.omegasq <- function(f, df1, df2) {
  return( (f - 1) / (f + (df2 + 1) / (df1)) );
}

###########################################################################
### Converting from: Eta square
###########################################################################

#' @export
convert.etasq.to.cohensf <- function(etasq) {
  return(sqrt(etasq / (1-etasq)));
}

#' @export
convert.etasq.to.r <- function(etasq) {
  return(sqrt(etasq));
}

###########################################################################
### Converting from: Cohen's f^2
###########################################################################

### Equation 16 in Steiger's (2004) 'Beyond the F Test' paper

#' @export
convert.cohensf.to.omegasq <- function(cohensf) {
  return(cohensf^2 / (1 + cohensf^2));
}

#' @export
convert.cohensfsq.to.omegasq <- function(cohensfsq) {
  return(cohensfsq / (1 + cohensfsq));
}

###########################################################################
### Converting from: Omega^2
###########################################################################

#' @export
convert.omegasq.to.f <- function(omegasq, df1, df2) {
  return( (omegasq * ((df2 + 1) / df1) + 1) / (1 - omegasq) );
}

### Equation 15 in Steiger's (2004) 'Beyond the F Test' paper

#' @export
convert.omegasq.to.cohensfsq <- function(omegasq) {
  return(omegasq / (1 - omegasq));
}

#' @export
convert.omegasq.to.cohensf <- function(omegasq) {
  return(sqrt(omegasq / (1 - omegasq)));
}

###########################################################################
### Converting from: Beta (regression weight)
###########################################################################

#' @export
convert.b.to.t <- function(b, se) {
  return(b/se);
}

###########################################################################
### Converting from: Fisher's z
###########################################################################

#' @export
convert.fisherz.to.r <- function(z) {
  return((exp(2 * z) - 1) / (exp(2*z)+1));
}

#########################################################################
### Converting from: Noncentrality parameter of the F distribution
#########################################################################

### Using formula 16 in Beyond The F Test, Steiger's 2004 paper

#' @export
convert.ncf.to.omegasq <- function(ncf, N) {
  return(ncf / (ncf + N));
}

###########################################################################
### Converting from: Means and standard deviations
###########################################################################

#' @export
convert.means.to.d <- function(means, sds, ns = NULL, var.equal=NULL) {
  if (is.null(ns)) {
    var <- mean(sds);
  } else {
    if (is.null(var.equal)) {
      ### Try to establish equality ourselves
      if (max(sds) < (3 * min(sds))) {
        ### Consider then equal
        var.equal <- TRUE;
      } else {
        ### Consider them different
        var.equal <- FALSE;
      }
    }
    if (var.equal) {
      ss1 <- sds[1]^2 * (ns[1] - 1);
      ss2 <- sds[2]^2 * (ns[2] - 1);
      var <- (ss1 + ss2) / (ns[1] + ns[2] - 2);
    } else {
      ### Take variance of smallest group
      var <- sds[ns == min(ns)] ^ 2;
    }
  }
  ### Compute difference between means and divide
  ### by standard deviation
  return((means[2] - means[1]) / sqrt(var));
}
