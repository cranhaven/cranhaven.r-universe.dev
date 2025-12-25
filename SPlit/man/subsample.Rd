\name{subsample}
\alias{subsample}
\title{Nearest neighbor subsampling}
\description{
	\code{subsample()} finds the nearest data points in a dataset to a given set of points as described in Joseph and Vakayil (2021). It uses an efficient kd-tree based algorithm that allows for lazy deletion of a data point from the kd-tree, thereby avoiding the need to rebuild the tree after each query. Please see Blanco and Rai (2014) for details.
}
\usage{
	subsample(data, points)
}
\arguments{
	\item{data}{The dataset; should be numeric.}
	\item{points}{The set of query points of the same dimension as the dataset.}
}
\value{
	Indices of the nearest neighbors in the dataset.
}
\references{
	Blanco, J. L. & Rai, P. K. (2014). nanoflann: a C++ header-only fork of FLANN, a library for nearest neighbor (NN) with kd-trees. https://github.com/jlblancoc/nanoflann.

	Joseph, V. R., & Vakayil, A. (2021). SPlit: An Optimal Method for Data Splitting. Technometrics, 1-11. doi:10.1080/00401706.2021.1921037.
}