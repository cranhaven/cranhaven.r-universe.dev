\name{views}
\alias{views}
\title{List Oracle Views}
\description{
  List all views in the database belonging to a specific owner.
}
\usage{
views(owner="\%", view="\%", tolower=TRUE, \dots)
}
\arguments{
  \item{owner}{owner name.}
  \item{view}{view name.}
  \item{tolower}{whether \code{owner} and \code{view} output entries
    should be lowercased.}
  \item{\dots}{passed to \code{dbConnect}.}
}
\details{
  The arguments \code{owner} and \code{view}, are passed as patterns
  that are matched by \samp{LIKE}, where \code{\%} means any number of
  characters and \code{_} means exactly one character. Literal \code{\%}
  and \code{_} are escaped using two backslashes: \code{"\\\\\%"} and
  \code{"\\\\_"}.

  In other words, the default value \code{"\%"} matches all strings.

  The \code{\dots} argument can be used to set \code{username},
  \code{password}, and/or \code{dbname} (see \code{\link{dbConnect}}).
  Abbrevations like \code{user} and \samp{pass} are allowed. The default
  database name is determined by the environment variable
  \env{ORACLE_SID}, which can be redefined within an \R session using
  \code{Sys.setenv(ORACLE_SID="foo")}.
}
\value{Data frame containing two columns:
  \item{owner}{owner name.}
  \item{view}{view name.}
}
\note{
  The output is a subset of \samp{ALL_VIEWS} in Oracle, with simplified
  column names:
  \tabular{ll}{
    \samp{OWNER}     \tab \samp{-> owner}\cr
    \samp{VIEW_NAME} \tab \samp{-> view}
  }
}
\seealso{
  \code{views} is to Oracle as \code{ll} (in package \pkg{gdata}) is to
  \R environments.

  \code{\link{ora}} gives an overview of the package.
}
\examples{
\dontrun{

# Views belonging to owner "sys":
views("sys")

# Views whose name contains "all", followed by at least one character:
views(,"\%all_\%")

# Views whose name begins with "all_":
views(,"all\\\\_\%")

}
}
% Programming
\keyword{database}
\keyword{utilities}
