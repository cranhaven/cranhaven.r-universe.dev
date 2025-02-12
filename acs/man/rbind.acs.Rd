\name{rbind.acs}
\alias{rbind.acs}
\alias{rbind}

\title{Combine acs Objects by Rows
}

\description{ Take a pair of acs objects and combine by rows.
}

\usage{
          \method{rbind}{acs}(e1, e2, ...)
          }

\arguments{
  \item{e1, e2}{two acs-class objects}
  \item{...}{provided for consistency with cbind S3 method}
}

\details{ When passed two acs-class objects, rbind will first check to
confirm whether the objects contain compatible data: same endyear and
span; same column names.  If not, it will issue a warning, but will
still proceed.

After this check, the function will return a new acs object that has
resulted from combining the two arguments row-wise.  The effect is
essentially the same as rbind on the underlying estimate and
standard.error matrices, with all the additional acs metadata tended
to.}

\value{ Returns a single new acs object with all of the data contained
  in the two arguments.  }

\author{
Ezra Haber Glenn \email{eglenn@mit.edu}
}

