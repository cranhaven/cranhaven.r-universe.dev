\name{Ttcols}

\alias{Ttcols}

\title{Text-to-columns}

\description{Splits character vector into columns of the matrix based on specified separator}

\usage{

Ttcols(text, missed=NA, ...)

}

\arguments{

\item{text}{Character vector}

\item{missed}{How to fill empty cells of the result, default is NA}

\item{...}{Arguments to split() function}

}

\details{

Text-to-columns operation is common in spreadsheets. In R, demands for
this functionality are likely also high because there are numerous
solutions. Below are some most simple, flexible and extendable:

do.call(rbind, strsplit(..., split)) -- fast and easy but it recycles
short rows

read.table(text=..., sep=split, fill=TRUE, colClasses="character",
header=FALSE) -- does not know how many columns you want; it uses only 5
first lines and requires to specify column names otherwise

strcapture() -- needs both rows of equal length _and_ number of future
columns

stringr::str_split_fixed() -- needs number of columns to make

reshape2::colsplit() -- needs column names beforehand

tidyr::separate() -- cannot take vectors and also wants explicit column
names

Ttcols() is fast, simple and flexible solution which does not have
problems from above. It handles only one vector at time but it is easy to
overcome because it is simple to extend and combine.

}

\value{

Matrix of splitted strings without separators, empty cells filled with
'missed'.

}

% \references{}

\author{Alexey Shipunov}

\seealso{\code{\link{Fill}}}

\examples{

aa <- c("one,I,i", "two,II", "three", NA, "", Inf, "2 ,3, 4,_5", 15, ",a,,b")
Ttcols(aa, split=" *, *")

bb <- c("one,I,i", "two,II", "three")
Ttcols(bb, split=",")

Ttcols(row.names(mtcars), split=" ", missed="")

}

\keyword{manip}
