\name{NEWS}
\title{NEWS file for the omics package}
\section{Changes in version 0.1-5 (2016-11-27)}{
    \itemize{
        \item{\code{mlm} and \code{mlmer} now return `nobs` with names.}
        \item{Updated \code{mlmer} to reflect changes in \code{lmer}.}
    }
}
\section{Changes in version 0.1-4 (2016-02-08)}{
    \itemize{
        \item{\code{mlm} and \code{mlmer} can now handle `.` in model formulae.}
        \item{\code{mlmer} can now handle interaction terms and nested random
              effects.}
    }
}
\section{Changes in version 0.1-3 (2016-01-27)}{
    \itemize{
        \item{Fixed computation of rank distributions in \code{mlmer} when
              random effect estimates are missing for some observations.}
    }
}
\section{Changes in version 0.1-2 (2016-01-25)}{
    \itemize{
        \item{\code{mlm} and \code{mlmer} can now handle missing values.}
        \item{Added \code{testthat} unit tests for \code{mlm} and \code{mlmer}.}
    }
}
\section{Changes in version 0.1-1 (2015-12-05)}{
    \itemize{
        \item{Fixed computation of rank distributions in \code{mlmer}.}
        \item{\code{mlm} and \code{mlmer} now allow \code{vars} to be empty when
              \code{save.residuals} is \code{TRUE}.}
    }
}
