#' Estimate required sample size for accuracy in parameter estimation using bootES
#'
#' This function uses [bootES::bootES()] to compute
#'
#' @param data The dataset, as you would normally supply to [bootES::bootES()];
#' you will probably have to simulate this.
#' @param ci.type The estimation method; by default, the default of
#' [bootES::bootES()] is used ('bca'), but this is changed to 'basic' if it
#' encounters problems.
#' @param ... Other options for [bootES::bootES()] (see that help page).
#' @param w The desired 'halfwidth' of the confidence interval.
#' @param silent Whether to provide a lot of information about progress
#' ('FALSE') or not ('TRUE').
#'
#' @return A single numeric value (the sample size).
#'
#' @references
#' Kirby, K. N., & Gerlanc, D. (2013). BootES: An R package for bootstrap confidence
#' intervals on effect sizes. *Behavior Research Methods, 45*, 905–927. \doi{10.3758/s13428-013-0330-5}
#'
#' @export
#'
#' @examples ### This requires the bootES package
#'   if (requireNamespace("bootES", quietly = TRUE)) {
#'
#'   ### To estimate a mean
#'   x <- rnorm(500, mean=8, sd=3);
#'   pwr.bootES(data.frame(x=x),
#'              R=500,
#'              w=.5);
#'
#'   ### To estimate a correlation (the 'effect.type' parameter is
#'   ### redundant here; with two columns in the data frame, computing
#'   ### the confidence interval for the Pearson correlation is the default
#'   ### ehavior of bootES)
#'   y <- x+rnorm(500, mean=0, sd=5);
#'   cor(x, y);
#'   requiredN <-
#'     pwr.bootES(data.frame(x=x,
#'                           y=y),
#'                effect.type='r',
#'                R=500,
#'                w=.2);
#'   print(requiredN);
#'   ### Compare to parametric confidence interval
#'   ### based on the computed required sample size
#'   confIntR(r = cor(x, y),
#'            N = requiredN);
#'   ### Width of obtained confidence interval
#'   print(round(diff(as.numeric(confIntR(r = cor(x, y),
#'                               N = requiredN))), 2));
#' }
pwr.bootES <- function(data=data,
                       ci.type="bca",
                       ...,
                       w=.1,
                       silent=TRUE) {

  if (!requireNamespace("bootES", quietly = TRUE)) {
    message("To build a tree, the \"bootES\" package is required. ",
            "Please install it using `install.packages('bootES');`.");
    return(invisible(FALSE));
  }

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

  n <- 4;
  if (!silent) {
    cat0("Setting n to 4 to start.\n");
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
        cat0("Taking ",
             n,
             " samples from `data` and running `bootES::bootES`: ");
      }
      sampledData <-
        data[sample(1:nrow(data),
                    size=n,
                    replace=TRUE), ];
      tryCatch({
          bootESres <-
            bootES::bootES(data = sampledData,
                           ci.type = ci.type,
                           ...);
        },
        error=eHandler,
        warning=wHandler);
      if (!is.null(myErrors) && ci.type=="bca") {
        ci.type <- "basic";
        n <- n - steps;
        bcaMsg <-
          paste0("You specified the 'bca' type of bootstrapped confidence ",
                 "intervals (the default, so maybe you didn't specify anything). ",
                 "However, `bootES::bootES` ran into an error, so resetting the ",
                 "bootstrapping method to 'basic' and trying to continue.");
        if (!silent) {
          cat0(bcaMsg, "\n");
        }
        warning(bcaMsg);
      }
      if (exists('bootESres')) {
        obtainedCI <- bootESres$bounds;
        ciWidth <- abs(diff(obtainedCI));
        if (!silent) {
          cat0("Obtained CI of ",
               formatCI(obtainedCI),
               "; width=",
               round(ciWidth, 2),
               ".");
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
    }
    if (!silent) {
      cat0("Done with this cycle; subtracting ",
           steps,
           " from n.\n");
    }
    n <- n - steps;
  }

  return(n);
}


