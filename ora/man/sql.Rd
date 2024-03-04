\name{sql}
\alias{sql}
\title{Import Data from Oracle}
\description{Run SQL query returning an \R data frame.}
\usage{
sql(query, tolower=TRUE, dots=TRUE, encoding="unknown", useBytes=TRUE,
    stringsAsFactors=FALSE, warn=-1, debug=FALSE, \dots)
}
\arguments{
  \item{query}{string containing SQL query or the name of a file
    containing a query.}
  \item{tolower}{whether column names should be lowercased.}
  \item{dots}{whether underscores in column names should be replaced
    with dots, converting \samp{col_name} to \samp{col.name}.}
  \item{encoding}{passed to \code{readLines}.}
  \item{useBytes}{passed to \code{gsub}.}
  \item{stringsAsFactors}{whether to convert string columns to factors.}
  \item{warn}{sets the handling of warning messages, e.g. when Oracle
    columns are of type \samp{LONG}.}
  \item{debug}{whether to return the finalized SQL query string, instead
    of submitting it to Oracle.}
  \item{\dots}{passed to \code{dbConnect}.}
}
\details{
  The \code{query} is not required to end with a semicolon. In fact,
  semicolons are removed internally before submitting the query to
  Oracle.

  The arguments \code{encoding} and \code{useBytes} enable the user to
  solve character encoding problems within the SQL query. If the query
  contains non-ASCII characters, \code{\link{readLines}} and
  \code{\link{gsub}} (called by \code{sql}) may convert the query to a
  different encoding than the Oracle database expects.

  The arguments \code{stringsAsFactors} and \code{warn} correspond to
  \code{\link{options}} with the same names, but the session options are
  not used as default values. Therefore, it is necessary to pass
  \code{stringsAsFactors=TRUE} directly to \code{sql} in order to import
  string columns as \code{factor}. This option-overriding is designed to
  make results more predictable and facilitate collaboration between
  database users.

  \code{debug=TRUE} is helpful for solving problems, and also to save
  complex queries (possibly to a file) for later use.

  The \code{\dots} argument can be used to set \code{username},
  \code{password}, and/or \code{dbname} (see \code{\link{dbConnect}}).
  Abbrevations like \code{user} and \samp{pass} are allowed. The default
  database name is determined by the environment variable
  \env{ORACLE_SID}, which can be redefined within an \R session using
  \code{Sys.setenv(ORACLE_SID="foo")}.
}
\value{
  Data frame containing the imported data, or a simple string if
  \code{debug=TRUE}.
}
\seealso{
  \code{sql} is to Oracle tables as \code{\link{read.table}} is to text
  files.

  \code{\link{ora}} gives an overview of the package.
}
\examples{
\dontrun{


## 1 Basic queries

# Pass query as a simple string
sql("SELECT username,created FROM all_users WHERE rownum<=10")

# Pass query as a multiline string
sql("SELECT extract(year from created) AS year,
            count(username) AS users
       FROM all_users
   GROUP BY extract(year from created)")

# Pass query as a file
write(c("SELECT username, created",
        "FROM all_users",
        "WHERE rownum <= 10;"), "query.sql")
sql("query.sql")


## 2 Review query string, before sending it to Oracle

sql(paste0("SELECT username,created FROM all_users WHERE rownum<=",5+5),
    debug=TRUE)

}
}
% Programming
\keyword{database}
\keyword{utilities}
