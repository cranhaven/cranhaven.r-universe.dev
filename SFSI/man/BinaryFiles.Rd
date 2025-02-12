
\name{1. Save/read binary files}
\alias{readBinary}
\alias{saveBinary}
\title{Save/read binary files}
\usage{
saveBinary(X, file = paste0(tempdir(), "/file.bin"), 
           precision.format = c("double","single"), 
           verbose = TRUE)
  
readBinary(file = paste0(tempdir(), "/file.bin"), 
           rows = NULL, cols = NULL, 
           drop = TRUE, verbose = TRUE)
}
\arguments{
\item{X}{(numeric matrix) Data to save}

\item{file}{(character) Name of the binary file to save/read}

\item{precision.format}{(character) Either 'single' or 'double' for numeric precision and memory occupancy (4 bytes/32-bit or 8 bytes/64-bit, respectively) of the matrix to save}

\item{rows}{(integer vector) Which rows are to be read from the file. Default \code{rows=NULL} will read all the rows}

\item{cols}{(integer vector) Which columns are to be read from the file. Default \code{cols=NULL} will read all the columns}

\item{drop}{Either \code{TRUE} or \code{FALSE} to whether return a uni-dimensional vector when data is a matrix with either 1 row or 1 column}

\item{verbose}{\code{TRUE} or \code{FALSE} to whether printing file information}
}
\value{
Function 'saveBinary' does not return any value but print a description of the file saved. 

Function 'readBinary' returns the data that was read.
}
\description{
Save/read a numeric data as a fortran-formatted binary file at a defined precision (single or double).
}
\examples{
  require(SFSI)
  
  # A numeric matrix
  X = matrix(rnorm(500*100), ncol=100)
  
  # Save matrix as double-precision
  filename1 = paste0(tempdir(),"/Matrix1.bin")
  saveBinary(X, filename1, precision.format="double")
  
  # Save matrix as single-precision
  filename2 = paste0(tempdir(),"/Matrix2.bin")
  saveBinary(X, filename2, precision.format="single")

  # Read the double-precision matrix
  X2 = readBinary(filename1)
  max(abs(X-X2))             # No loss of precision
  file.info(filename1)$size  # Size of the file

  # Read the single-precision matrix
  X3 = readBinary(filename2)
  max(abs(X-X3))             # Loss of precision
  file.info(filename2)$size  # But smaller-size file

  # Read specific rows and columns
  rows = c(2,4,5,8,10)
  cols = c(1,2,5)
  (X2 = readBinary(filename1, rows=rows, cols=cols))
  # Equal to: 
  X[rows,cols]

}

