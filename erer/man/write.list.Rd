\name{write.list}
\alias{write.list}
\title{Output and Write a List Object}
\description{Print or write its required argument \code{z} to a file}
\usage{write.list(z, file, t.name = NULL, row.names = FALSE, ...) }
\arguments{
  \item{z }{ A list object to be written. Each item in the list is perferably a data frame. If not, it is converted into a data frame. All the contents are coerced into characters to avoid loss of information (e.g., a loss of zero in 5.130.}
  \item{file }{ a character string naming a file.}
  \item{t.name }{ table names. This can be given explicitly, or given by the list name, or by default, named as "table 1" for the first item in \code{z}.}       
  \item{row.names }{ whether the row names in each table should be written (default is FALSE). If TRUE, a new column of the row names is added to each table.}
  \item{... }{ Other arguments that can be passed to \code{write.table}}
}
\details{This function is a wrap-up of \code{write.table}. It is convenient to write a set of tables to C drive.}

\seealso{\code{\link{write.table}}.}

\examples{

h1 <- ts(data=cbind(1:24), start=c(2001, 1), frequency=12)
h2 <- ts(data=cbind(1:24, 25:48), start=c(2001, 1), frequency=12)
h3 <- ts(data=cbind(1:4, 5:8, 9:12), start=c(2001, 1), frequency=4)
colnames(h2) <- c("aa", "bb")
colnames(h3) <- c("cc", "dd", "ee")
h1; h2; h3

test <- list(t1 = h1, t2 = h2, t3 = h3)

\dontrun{
  # test.csv can be saved at a specific working directory
  getwd(); setwd("c:/aERER"); getwd()
  write.list(z = test, file = "test.csv")
}
}
\keyword{manip}