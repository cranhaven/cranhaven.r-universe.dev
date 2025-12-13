\name{NEWS}

\title{NEWS file for the \pkg{model4you} package}


\section{Changes in Version 0.9-6}{
  \itemize{
    \item set all.equal(...) to all.equal(..., check.environment = FALSE) as needed for R version, see https://stat.ethz.ch/pipermail/r-devel/2020-December/080172.html
  }
}

\section{Changes in Version 0.9-6}{
  \itemize{
    \item Fix issues with multi-core computing.
    \item allow plotting of more than two treatment groups for cox model
    \item update score calculation in coxph() models:\\
    old: as.matrix(cbind(residuals(mod, "martingale"), estfun(mod)))
    new: cbind(mod$residuals, mod$residuals * model.matrix(mod))
  }
}



\section{Changes in Version 0.9-5}{
  \itemize{
    \item avoid using class()==, instead use is(), especially for matrix
  }
}


\section{Changes in Version 0.9-4}{
  \itemize{
    \item allow for custom coefficient tables in node_pmterminal()
    \item custom coefficient table for survreg models.
  }
}


\section{Changes in Version 0.9-2}{
  \itemize{
    \item allow plot to have no table
    \item improve viewport settings for plot
  }
}

\section{Changes in Version 0.9-1 (2018-02-02)}{
  \subsection{Bugfixes}{
  \itemize{
    \item{Fixed bug in test_heterogeneity(). Added tests for test_heterogeneity().}
    \item{Fixed bug in binomial_glm_plot().}
  }
  }
  \subsection{Other}{
  \itemize{
    \item{Added check if the model entered has a single factor covariate.}
  }
  }

}


