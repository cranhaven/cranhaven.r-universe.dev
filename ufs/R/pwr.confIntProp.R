#' Estimate required sample size for accuracy in parameter estimation of a proportion
#'
#' This function uses [confIntProp()] to compute the required sample size for
#' estimating a proportion with a given accuracy.
#'
#' @param prop The proportion you expect to find, or a vector of proportions to
#' enable easy sensitivity analyses.
#' @param conf.level The confidence level of the desired confidence interval.
#' @param w The desired 'halfwidth' of the confidence interval.
#' @param silent Whether to provide a lot of information about progress
#' ('FALSE') or not ('TRUE').
#'
#' @return A single numeric value (the sample size).
#'
#' @export
#'
#' @examples ### Required sample size to estimate a prevalence of .03 in the
#' ### population with a confidence interval of a maximum half-width of .01
#' pwr.confIntProp(.03, w=.01);
#'
#' ### Vectorized over prop, so you can easily see how the required sample
#' ### size varies as a function of the proportion
#' pwr.confIntProp(c(.03, .05, .10), w=.01);
pwr.confIntProp <- function(prop,
                            conf.level=.95,
                            w=.1,
                            silent=TRUE) {

  if (length(w) != 1) {
    warning("Multiple widths not supported (yet); only the first one is used!\n",
            "You can use sapply to approximate this vectorization, for example,\n\n",
            "sapply(c(", vecTxt(w, lastElements = 0), "), pwr.cohensdCI, d=.5)",
            "\n");
    w <- w[1];
  }

  ### From a post at the R-help mailig list by Luke Tierney, see
  ### http://stackoverflow.com/questions/3903157/how-can-i-check-whether-a-function-call-results-in-a-warning
  wHandler <- function(w) {
    myWarnings <<- c(myWarnings, list(w));
    invokeRestart("muffleWarning");
  }
  eHandler <- function(e) {
    myErrors <<- c(myErrors, list(e));
  }
  myWarnings <- NULL;
  myErrors <- NULL;

  propVector <- prop;
  nVector <- numeric();

  for (prop in propVector) {

    n <- 4;
    if (!silent) {
      cat0("Estimating required sample size for a confidence interval with maximum halfwidth ",
           w, " for a population proportion of ", prop, ". Setting n to 4 to start.\n");
    }

    for (steps in c(1000, 100, 10, 1)) {
      ciWidth <- 3*w;
      while (ciWidth > 2*w) {
        n <- n + steps;
        if (!silent) {
          if (!silent) {
            cat0("Adding ",
                 steps,
                 " to n.\n");
          }
          cat0("Computing obtained confidence interval for n = ", n , ".\n");
        }
        x <- prop * n;
        obtainedCI <- confIntProp(x, n, conf.level);
        ciWidth <- abs(diff(as.numeric(obtainedCI)));
        if (!silent) {
          cat0("Obtained CI of ",
               formatCI(obtainedCI),
               "; width=",
               round(ciWidth, 2),
               ".\n");
          if (ciWidth < w*2) {
            cat0(" This is smaller than the margin of error (2*w, or ",
                 2*w,
                 ").\n");
          } else {
            cat0(" This is larger than the margin of error (2*w, or ",
                 2*w,
                 ").\n");
          }
        }
      }
      if (!silent) {
        cat0("Done with this cycle; subtracting ",
             steps,
             " from n.\n");
      }
      n <- n - steps;
    }

    nVector <- c(nVector,
                 n);

  }

  names(nVector) <- propVector;

  return(nVector);
}


