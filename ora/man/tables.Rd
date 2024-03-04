\name{tables}
\alias{tables}
\title{List Oracle Tables}
\description{
  List all tables in the database belonging to a specific owner or table
  space.
}
\usage{
tables(owner="\%", table="\%", space="\%", tolower=TRUE, \dots)
}
\arguments{
  \item{owner}{owner name.}
  \item{table}{table name.}
  \item{space}{table space name.}
  \item{tolower}{whether \code{owner}, \code{table}, and \code{space}
    output entries should be lowercased.}
  \item{\dots}{passed to \code{dbConnect}.}
}
\details{
  The arguments \code{owner}, \code{table}, and \code{space} are passed
  as patterns that are matched by \samp{LIKE}, where \code{\%} means any
  number of characters and \code{_} means exactly one character. Literal
  \code{\%} and \code{_} are escaped using two backslashes:
  \code{"\\\\\%"} and \code{"\\\\_"}.

  In other words, the default value \code{"\%"} matches all strings.

  The \code{\dots} argument can be used to set \code{username},
  \code{password}, and/or \code{dbname} (see \code{\link{dbConnect}}).
  Abbrevations like \code{user} and \samp{pass} are allowed. The default
  database name is determined by the environment variable
  \env{ORACLE_SID}, which can be redefined within an \R session using
  \code{Sys.setenv(ORACLE_SID="foo")}.
}
\value{Data frame containing five columns:
  \item{owner}{owner name.}
  \item{table}{table name.}
  \item{space}{table space name.}
  \item{rows}{number of rows when the table was last analyzed.}
  \item{analyzed}{when the table was last analyzed.}
}
\note{
  The output is a subset of \samp{ALL_TABLES} in Oracle, with simplified
  column names:
  \tabular{ll}{
    \samp{OWNER}           \tab \samp{-> owner}   \cr
    \samp{TABLE_NAME}      \tab \samp{-> table}   \cr
    \samp{TABLESPACE_NAME} \tab \samp{-> space}   \cr
    \samp{NUM_ROWS}        \tab \samp{-> rows}    \cr
    \samp{LAST_ANALYZED}   \tab \samp{-> analyzed}
  }
}
\seealso{
  \code{tables} is to Oracle as \code{ll} (in package \pkg{gdata}) is to
  \R environments.

  \code{\link{ora}} gives an overview of the package.
}
\examples{
\dontrun{

# Tables belonging to owner "sys":
tables("sys")

# Tables whose name contains "map", preceded by at least one character:
tables(,"\%_map\%")

# Tables whose name ends with "_map":
tables(,"\%\\\\_map")

}
}
% Programming
\keyword{database}
\keyword{utilities}
