#' @rdname cohensDdistribution
#' @export cohensdCI
#' @export confIntD
cohensdCI <- confIntD <- function(d, n, conf.level = .95, plot=FALSE, silent=TRUE) {

  if (length(conf.level) != 1) {
    stop("Only specify one value for argument 'conf.level'!");
  }

  ci.bound.lo <- (1 - conf.level) / 2;
  ci.bound.hi <- 1 - (1 - conf.level) / 2;

  ### From a post at the R-help mailing list by Luke Tierney, see
  ### https://stackoverflow.com/questions/3903157/how-can-i-check-whether-a-function-call-results-in-a-warning
  wHandler <- function(w) {
    myWarnings <<- c(myWarnings, list(w));
    invokeRestart("muffleWarning");
  }
  myWarnings <- NULL;


  ### Based on @GuyProchilo's message on Twitter at 2019-02-11

  ### Now following https://home.cc.umanitoba.ca/~kesel/CIl_Kelly.pdf

  ### Here, the CI isn't found by using Cohen's d's distribution, for the relevant
  ### NCP in the t distribution (as converted from d), but by finding the two
  ### NCPs that yield two noncentral t distribution where for the first, the
  ### p=.025 corresponds to the relevant NCP, and for the second, the p=.975
  ### corresponds to the relevant NCP.

  ### The MBESS method uses both nlm and optimize, probably because there are
  ### situations where either doesn't work quite as expected. I'll simply export
  ### that method.




  if (length(d) == length(n)) {
    res <- t(sapply(1:length(d), function(i) {
      return(convert.t.to.d(from_MBESS_conf.limits.nct(ncp=convert.d.to.t(d[i],
                                                                          df=n[i]-2),
                                                       df=n[i]-2,
                                                       conf.level=conf.level),
                            df=n[i]-2));
      # return(withCallingHandlers(c(qCohensd(p=ci.bound.lo,
      #                                       df=n[i]-2,
      #                                       populationD=d[i]),
      #                              qCohensd(p=ci.bound.hi,
      #                                       df=n[i]-2,
      #                                       populationD=d[i])),
      #                            warning = wHandler));
    }));
  } else if ((length(d) == 1) || (length(n) == 1)) {
    if (length(d) == 1) d <- rep(d, length(n));
    if (length(n) == 1) n <- rep(n, length(d));
    res <- t(sapply(1:length(d), function(i) {
      return(convert.t.to.d(from_MBESS_conf.limits.nct(ncp=convert.d.to.t(d[i],
                                                                          df=n[i]-2),
                                                       df=n[i]-2,
                                                       conf.level=conf.level),
                            df=n[i]-2));
    }));
    # res <- withCallingHandlers(matrix(c(qCohensd(p=ci.bound.lo,
    #                                              df=n-2,
    #                                              populationD=d),
    #                                     qCohensd(p=ci.bound.hi,
    #                                              df=n-2,
    #                                              populationD=d)), ncol=2),
    #                            warning = wHandler);
  } else {
    stop("Either specify vectors of equal length as 'd' and 'n', or a ",
         "single value for one and a vector for the other.");
  }

  colnames(res) <- c('lo', 'hi');

  if (plot) {
    if ((length(d) > 1) || (length(n) > 1) || (length(conf.level) > 1)) {
      warning("I can only produce a plot if you supply only one value for ",
              "arguments d, n, and conf.level!");
    } else {
      df <- data.frame(d = seq(min(res) - .5,
                               max(res) + .5, .001));
      df$density <- withCallingHandlers(ufs::dd(df$d,
                                                df = n-2,
                                                populationD = d,
                                                silent=TRUE),
                                        warning = wHandler);

      cilo <- min(res);
      cihi <- max(res);
      dValue <- d;

      plot <-
        ggplot2::ggplot(df,
                        ggplot2::aes_string(x='d', y='density')) +
        ggplot2::theme_bw() +
        ggplot2::theme(axis.title.x.top = ggplot2::element_blank()) +
        ggplot2::scale_x_continuous(sec.axis = ggplot2::dup_axis(breaks=c(cilo,
                                                                          dValue,
                                                                          cihi),
                                                                 labels=round(c(cilo,
                                                                                dValue,
                                                                                cihi), 2))) +
        ggplot2::geom_vline(ggplot2::aes(xintercept=cilo), linetype='dashed') +
        ggplot2::geom_vline(ggplot2::aes(xintercept=dValue), linetype='dashed') +
        ggplot2::geom_vline(ggplot2::aes(xintercept=cihi), linetype='dashed') +
        ggplot2::geom_ribbon(data=df[df$d >= min(res) & df$d <= max(res), ],
                             ggplot2::aes_string(ymin = 0, ymax='density'),
                             fill='#cadded') +
        ggplot2::geom_segment(x = min(res),
                              xend = min(res),
                              y = 0,
                              yend = ufs::dd(min(res), df = n-2,
                                             populationD = d,
                                             silent=TRUE),
                              color = '#2a5581', size=1.5) +
        ggplot2::geom_segment(x = max(res),
                              xend = max(res),
                              y = 0,
                              yend = ufs::dd(max(res), df = n-2,
                                             populationD = d,
                                             silent=TRUE),
                              color = '#2a5581', size=1.5) +
        ggplot2::geom_line(size=1.5);
      attr(res, "plot") <- plot;
      class(res) <- 'cohensdCI';
    }
  }

  d <- paste0('d=', d);
  n <- paste0('n=', n);

  rownames(res) <- paste0(d, ", ", n);

  if ((!silent) && (length(myWarnings) > 0)) {
    precisionWarnings <- grepl("full precision may not have been achieved in 'pnt{final}'",
                               myWarnings, fixed = TRUE);
    if (any(precisionWarnings)) {
      cat0("Function 'qt', which is used under the hood of this function (see ?qt for more information), ",
           "warned that 'full precision may not have been achieved'. ",
           "This is normally no cause for concern, because with sufficiently large sample sizes, small deviations ",
           "have little impact, but informing you seemed appropriate nonetheless.\n\n");
    }
    if (!all(precisionWarnings)) {
      cat0("One or more ", ifelse(any(precisionWarnings), "additional", ""),
           " warnings were encountered:\n");
      lapply(myWarnings[!precisionWarnings], function(x) cat0(x$message, "\n"));
      cat("\n");
    }
  }

  return(res);
}

#' @export
print.cohensdCI <- function(x, ...) {
  ### Basically a trick because we're passing the plot as an attribute.
  if (!is.null(attr(x, 'plot'))) {
    grid::grid.draw(attr(x, 'plot'));
    ### So remove the plot
    attr(x, 'plot') <- NULL;
  }
  ### And then remove the class to print
  print(unclass(x));
}

# ggplot(data.frame(x = seq(-3, 3, by=.1),
#                   d = dCohensd(seq(-3, 3, by=.1), populationD = .5, 18),
#                   d2 = dCohensd(seq(-3, 3, by=.1), populationD = .5, 180),
#                   t = dt(seq(-3, 3, by=.1), 18)),
#        aes(x=x)) +
#   geom_line(aes(y=d), color='red') +
#   geom_line(aes(y=d2), color='green') +
#   geom_line(aes(y=t), color='blue') +
#   theme_bw();
