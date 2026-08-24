#' The distribution of Cohen's *d*
#'
#' These functions use some conversion to and from the *t* distribution to
#' provide the Cohen's *d* distribution. There are four versions that act
#' similar to the standard distribution functions (the `d.`, `p.`,
#' `q.`, and `r.` functions, and their longer aliases
#' `.Cohensd`), three convenience functions (`pdExtreme`,
#' `pdMild`, and `pdInterval`), a function to compute the confidence
#' interval for a Cohen's *d* estimate `cohensdCI`, and a function to
#' compute the sample size required to obtain a confidence interval around a
#' Cohen's *d* estimate with a specified accuracy (`pwr.cohensdCI`
#' and its alias `pwr.confIntd`).
#'
#' The functions use [convert.d.to.t()] and
#' [convert.t.to.d()] to provide the Cohen's *d* distribution.
#'
#' The confidence interval functions, `cohensdCI` and `pwr.cohensdCI`,
#' now use the same method as MBESS (a slightly adapted version of
#' the `MBESS` function `conf.limits.nct` is used).
#'
#' More details about `cohensdCI` and `pwr.cohensdCI` are provided in
#' Peters & Crutzen (2017).
#'
#' @aliases dCohensd pCohensd qCohensd rCohensd dd pd qd rd pdExtreme pdMild
#' pdInterval cohensdCI confIntD pwr.cohensdCI pwr.confIntd
#' @param x,q,d Vector of quantiles, or, in other words, the value(s) of
#' Cohen's *d*.
#' @param ds A vector with two Cohen's *d* values.
#' @param p Vector of probabilites (*p*-values).
#' @param df Degrees of freedom.
#' @param n,n1,n2 Desired number of Cohen's *d* values for `rCohensd` and
#' `rd` (`n`), and the number of participants/datapoints in total (`n`) or in each
#' group (`n1` and `n2`) for `dd`, `dCohensd`, `pdExtreme`,
#' `pdMild`, `pdInterval`, and `cohensdCI`.
#' @param populationD The value of Cohen's *d* in the population; this
#' determines the center of the Cohen's *d* distribution. I suppose this
#' is the noncentrality parameter.
#' @param lower.tail logical; if TRUE (default), probabilities are the
#' likelihood of finding a Cohen's *d* smaller than the specified value;
#' otherwise, the likelihood of finding a Cohen's *d* larger than the
#' specified value.
#' @param conf.level The level of confidence of the confidence interval.
#' @param plot Whether to show a plot of the sampling distribution of Cohen's
#' *d* and the confidence interval. This can only be used if specifying
#' one value for `d`, `n`, and `conf.level`.
#' @param w The desired maximum 'half-width' or margin of error of the confidence
#' interval.
#' @param extensive Whether to only return the required sample size, or more
#' extensive results.
#' @param silent Whether to provide `FALSE` or suppress (`TRUE`)
#' warnings.  This is useful because function 'qt', which is used under the
#' hood (see [qt()] for more information), warns that 'full precision
#' may not have been achieved' when the density of the distribution is very
#' close to zero. This is normally no cause for concern, because with sample
#' sizes this big, small deviations have little impact.
#' @return `dCohensd` (or `dd`) gives the density, `pCohensd`
#' (or `pd`) gives the distribution function, `qCohensd` (or
#' `qd`) gives the quantile function, and `rCohensd` (or `rd`)
#' generates random deviates.
#'
#' `pdExtreme` returns the probability (or probabilities) of finding a
#' Cohen's *d* equal to or more extreme than the specified value(s).
#'
#' `pdMild` returns the probability (or probabilities) of finding a
#' Cohen's *d* equal to or *less* extreme than the specified
#' value(s).
#'
#' `pdInterval` returns the probability of finding a Cohen's *d* that
#' lies in between the two specified values of Cohen's *d*.
#'
#' `cohensdCI` provides the confidence interval(s) for a given Cohen's
#' *d* value.
#'
#' `pwr.cohensdCI` provides the sample size required to obtain a
#' confidence interval for Cohen's *d* with a desired width.
#' @author Gjalt-Jorn Peters (Open University of the Netherlands), with
#' the exported MBESS function conf.limits.nct written by Ken Kelley
#' (University of Notre Dame), and with an error noticed by Guy Prochilo
#' (University of Melbourne).
#'
#' Maintainer: Gjalt-Jorn Peters <ufs@@opens.science>
#' @seealso [convert.d.to.t()], [convert.t.to.d()],
#' [dt()], [pt()], [qt()], [rt()]
#' @references
#'
#' Peters, G. J. Y. & Crutzen, R. (2017) Knowing exactly how effective an
#' intervention, treatment, or manipulation is and ensuring that a study
#' replicates: accuracy in parameter estimation as a partial solution to the
#' replication crisis. https://dx.doi.org/
#'
#' Maxwell, S. E., Kelley, K., & Rausch, J. R. (2008). Sample size planning for
#' statistical power and accuracy in parameter estimation. Annual Review of
#' Psychology, 59, 537-63.
#' https://doi.org/10.1146/annurev.psych.59.103006.093735
#'
#' Cumming, G. (2013). The New Statistics: Why and How. Psychological Science,
#' (November). https://doi.org/10.1177/0956797613504966
#' @keywords univar
#' @examples
#'
#' ### Confidence interval for Cohen's d of .5
#' ### from a sample of 200 participants, also
#' ### showing this visually: this clearly shows
#' ### how wildly our Cohen's d value can vary
#' ### from sample to sample.
#' cohensdCI(.5, n=200, plot=TRUE);
#'
#' ### How many participants would we need if we
#' ### would want a more accurate estimate, say
#' ### with a maximum confidence interval width
#' ### of .2?
#' pwr.cohensdCI(.5, w=.1);
#'
#' ### Show that 'sampling distribution':
#' cohensdCI(.5,
#'           n=pwr.cohensdCI(.5, w=.1),
#'           plot=TRUE);
#'
#' ### Generate 10 random Cohen's d values
#' rCohensd(10, 20, populationD = .5);
#'
#' ### Probability of findings a Cohen's d smaller than
#' ### .5 if it's 0 in the population (i.e. under the
#' ### null hypothesis)
#' pCohensd(.5, 64);
#'
#' ### Probability of findings a Cohen's d larger than
#' ### .5 if it's 0 in the population (i.e. under the
#' ### null hypothesis)
#' 1 - pCohensd(.5, 64);
#'
#' ### Probability of findings a Cohen's d more extreme
#' ### than .5 if it's 0 in the population (i.e. under
#' ### the null hypothesis)
#' pdExtreme(.5, 64);
#'
#' ### Probability of findings a Cohen's d more extreme
#' ### than .5 if it's 0.2 in the population.
#' pdExtreme(.5, 64, populationD = .2);
#'
#' @name cohensDdistribution
#' @rdname cohensDdistribution
#' @export dCohensd
#' @export dd
dCohensd <- dd <- function(x, df=NULL,
                           populationD = 0,
                           n=NULL,
                           n1=NULL,
                           n2=NULL,
                           silent=FALSE) {
  if (!is.null(n1) && !is.null(n2)) {
    ### We have both n's; we don't need to do anything
  } else if (!is.null(n)) {
    ### We have the total n
    n1 <- floor(n/2);
    n2 <- n-n1;
    if (!silent) {
      cat0("Using total sample size (n=", n, "); ",
           "assuming equal group sizes (n1=", n1,
           ", n2=", n2, "). Specify n1 and n2 ",
           "(instead of n) to override this.\n");
    }
  } else if (!is.null(df)) {
    ### We have the degrees of freedom
    n <- df + 2;
    n1 <- floor(n/2);
    n2 <- n-n1;
    if (!silent) {
      cat0("Using degrees of freedom (df=", df,
           ", n=", n, "); ",
           "assuming equal group sizes (n1=", n1,
           ", n2=", n2, "). Specify n1 and n2 ",
           "(instead of n) to override this.\n");
    }
  } else {
    stop("I need at least the degrees of freedom (df), ",
         "the total sample size (n; df+2), or the ",
         "sample size in each group (n1 & n2).\n");
  }
  multiplier <- sqrt(n1*n2/(n1+n2));
  ### Return density for given Cohen's d
  return(multiplier*
           stats::dt(x=ufs::convert.d.to.t(x, n1=n1, n2=n2),
                     df=(n1+n2-2),
                     ncp=ufs::convert.d.to.t(populationD, n1=n1, n2=n2)));
}

#' @export pCohensd
#' @export pd
#' @rdname cohensDdistribution
pCohensd <- pd <- function(q, df, populationD = 0, lower.tail=TRUE) {
  ### Return p-value for given Cohen's d
  return(stats::pt(convert.d.to.t(q, df=df), df,
                   ncp=convert.d.to.t(populationD, df=df),
                   lower.tail=lower.tail));
}

#' @export qCohensd
#' @export qd
#' @rdname cohensDdistribution
qCohensd <- qd <- function(p, df, populationD = 0, lower.tail=TRUE) {
  ### Return Cohen's d for given p-value
  return(convert.t.to.d(stats::qt(p=p, df=df,
                                  ncp=convert.d.to.t(d=populationD,
                                                     df=df),
                                  lower.tail=lower.tail),
                       df=df));
}

#' @export rCohensd
#' @export rd
#' @rdname cohensDdistribution
rCohensd <- rd <- function(n, df, populationD = 0) {
  ### Return random Cohen's d value(s)
  return(convert.t.to.d(stats::rt(n, df=df,
                                  ncp=convert.d.to.t(d=populationD,
                                                     df=df)),
                        df=df));
}

#' @export
#' @rdname cohensDdistribution
pdInterval <- function(ds, n, populationD = 0) {
  return(pd(max(ds), df=n - 2, populationD=populationD) -
           pd(min(ds), df=n - 2, populationD=populationD));
}

#' @export
#' @rdname cohensDdistribution
pdExtreme <- function(d, n, populationD = 0) {
  return(2 * pd(d, df=n - 2, populationD=populationD,
                lower.tail = (d <= populationD)));
}

#' @export
#' @rdname cohensDdistribution
pdMild <- function(d, n, populationD = 0) {
  return(1 - pdExtreme(d, n, populationD=populationD));
}
