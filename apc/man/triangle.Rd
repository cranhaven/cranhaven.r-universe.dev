\name{triangle}
\alias{is.triangle}
\alias{triangle.cumulative}
\alias{triangle.incremental}
\alias{vector.2.triangle}
\title{Triangular matrices used in reserving}
\description{Triangular matrices are used for reserving in general insurance. 
A matrix is triangular if it is square and it has NAs in lower triangle where row+col>dim.
The \code{apc} package uses incremental triangles. 

The function \code{is.triangle} tests if an object is a triangular matrix.

The function \code{triangle.cumulative} forms the cumulative version of an incremental matrix
by taking partial sums in each row.

The function \code{triangle.incremental} forms the incremental version of an cumulative matrix
by taking differences in each row.

The function \code{vector.2.triangle} turns a k*(k+1)/2 vector into a triangular matrix of
dimension k.}
\usage{is.triangle(m)
triangle.cumulative(m)
triangle.incremental(m)
vector.2.triangle(v,k)}
\arguments{
	\item{v}{vector. Length k*(k+1)/2}
	\item{k}{integer. Dimension}
	\item{m}{matrix.  Square matrix}
}
\author{Bent Nielsen <bent.nielsen@nuffield.ox.ac.uk> 21 Nov 2019 (7 Feb 2015)}
\examples{
#########################

m <- vector.2.triangle(1:10,4)
m
is.triangle(m)
triangle.cumulative(m)
triangle.incremental(m)
}
