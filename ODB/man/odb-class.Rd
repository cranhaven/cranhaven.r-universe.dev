\name{ODB-class}
\Rdversion{1.1}
\docType{class}
\alias{ODB-class}
\alias{show,ODB-method}

\title{Class "ODB"}
\description{
  A connection to an Open Document Base (.odb) embedded HSQL database.
  
  This class extends \linkS4class{JDBCConnection}, and objects from it can be accessed via the \code{DBI} interface. Objects from this class also store file paths used for other ODB manipulation (such as queries or comments, stored in the embedded content.xml file rather than the HSQL database).
}

\section{Objects from the Class}{
  Objects can be created by calls to the \code{\link{odb.open}} function, and saved into the original .odb file by \code{\link{odb.close}}.
  
  Several objects can but should not refer to the same .odb file: as they deal with distinct copies of this file, updates in one connection will not propagate to others. Moreover, when one of these connections will be closed, it will override changes in the .odb file made by formerly closed connections.
}

\section{Slots}{
  \describe{
    \item{\code{jc}:}{Inherited from \code{"\linkS4class{JDBCConnection}"}}
    \item{\code{identifier.quote}:}{Inherited from \code{"\linkS4class{JDBCConnection}"}}
    \item{\code{directory}:}{ Single character value, the path to the temp directory storing the database files. }
    \item{\code{odbFile}:}{ Single character value, the path to the .odb file handled. }
    \item{\code{odbVersion}:}{ Single character value, HSQLDB version of the .odb file. }
    \item{\code{jarFile}:}{ Single character value, the path to the HSQLDB .jar library used. }
    \item{\code{jarVersion}:}{ Single character value, HSQLDB version of the .jar library. }
  }
}

\section{Extends}{
  \itemize{
    \item Class \code{"\linkS4class{JDBCConnection}"}, directly.
    \item Class \code{"\linkS4class{DBIConnection}"}, by class "JDBCConnection", distance 2.
    \item Class \code{"\linkS4class{DBIObject}"}, by class "JDBCConnection", distance 3.
  }
}

\section{Methods}{
  \describe{
    \item{show}{ \code{signature(object = "ODB")} }
	}
  See also inherited methods.
}

\author{
  Sylvain Mareschal
}

\note{
  Functions from the \code{DBI} package can be used directly on the object, if you prefer to manage SQL transactions yourself. Notice however that common issues (charset, factors and column names) are handled by \code{\link{odb.read}} and \code{\link{odb.write}}, which can deal with any SQL query.
}

\seealso{
  \code{\link{odb.open}}
  
  See \code{\link{ODB-package}} for a complete example.
}
