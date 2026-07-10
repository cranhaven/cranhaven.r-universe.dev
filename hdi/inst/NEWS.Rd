% Check from R:
% news(db= tools:::.build_news_db_from_package_NEWS_Rd("~/R/Pkgs/hdi/pkg/inst/NEWS.Rd"))
\name{NEWS}
\title{hdi News}
\encoding{UTF-8}

\section{CHANGES IN VERSION 0.1-7 (latest update 2016-06-24, svn 155)}{
  \itemize{
    \item fixed issues with RNG.
  }
}

\section{CHANGES IN VERSION 0.1-6a (latest update 2016-06-24, svn 152)}{
  \itemize{
    \item added bootstrapping functionality for the de-sparsified Lasso
    \item added the missing Z & Z functionality for the lasso projection method
  }
}

\section{CHANGES IN VERSION 0.1-6 (2016-03-21, svn r147)}{
  \itemize{
    \item changed condition for warning message for confidence intervals
    with \code{multi.split}.
    \item reduced computing time for examples in \code{lasso.proj},
    \code{clusterGroupBound} and \code{plot.clusterGroupBound}.
    \item fixed tests using parallel option on Windows.
    \item temporary removed tests for \code{groupBound} due to problem
    with \code{mvrnorm} (reproducability on different platforms).
  }
}
  
\section{CHANGES IN VERSION 0.1-5 (2016-03-11, svn r144)}{
  \itemize{
    \item new argument \code{suppress.grouptesting} in \code{ridge.proj}
    and \code{lasso.proj} to speed up computation.
    \item argument \code{verbose} instead of \code{trace} in
    \code{multi.split} and \code{stability}.
    \item new functions to generate reference data sets, see
    \code{rXb}, \code{rX} and \code{rb}.
    \item replaced print and cat statement with message statements that
    can be suppressed.
    \item fixed a bug of some estimators (e.g. lasso.proj) setting seeds
    \item added NEWS and CITATION file.
  }
}
