\name{desc}
\alias{desc}
\title{Describe Oracle Table}
\description{
  Show the column names of an Oracle table (or view) and various column
  properties, not unlike the Oracle SQL*Plus \command{DESC} command.
  Also show the number of rows when the table was last analyzed.
}
\usage{
desc(table, tolower=TRUE, dots=FALSE, \dots)
}
\arguments{
  \item{table}{Oracle table name, often in the \samp{owner.table}
    format.}
  \item{tolower}{whether output table strings should be lowercased.}
  \item{dots}{whether underscores in column names should be replaced
    with dots, converting \samp{col_name} to \samp{col.name}.}
  \item{\dots}{passed to \code{dbConnect}.}
}
\details{
  The \code{\dots} argument can be used to set \code{username},
  \code{password}, and/or \code{dbname} (see \code{\link{dbConnect}}).
  Abbrevations like \code{user} and \samp{pass} are allowed. The default
  database name is determined by the environment variable
  \env{ORACLE_SID}, which can be redefined within an \R session using
  \code{Sys.setenv(ORACLE_SID="foo")}.
}
\value{
  A data frame with named rows and the following columns:
  \item{name}{Oracle column name.}
  \item{Sclass}{storage mode in \R.}
  \item{type}{Oracle type.}
  \item{len}{Oracle length.}
  \item{precision}{Oracle precision.}
  \item{scale}{Oracle scale.}
  \item{isVarLength}{whether the variable has varying length in Oracle.}
  \item{nullOK}{whether the variable can be null.}
  Furthermore, the data frame contains two attributes: \code{rows} (the
  number of rows when the table was last analyzed) and \code{analyzed}
  (when the table was last analyzed). These attributes are not available
  for all Oracle tables, but are more likely to be available when the
  main argument \code{table} has the full owner.table format.
}
\note{
  See the Oracle manuals for details about type, length, precision,
  scale, and nulls.
}
\seealso{
  \code{desc} is to Oracle tables as \code{ll} (in package \pkg{gdata})
  is to \R data frames.

  \code{\link{ora}} gives an overview of the package.
}
\examples{
\dontrun{

desc("dual", tolower=FALSE)

desc("all_users")
}
}
% Basics
\keyword{classes}
% Programming
\keyword{database}
\keyword{utilities}
