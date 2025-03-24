\name{spen_egcv}
\alias{spen_egcv}
\title{
C code for smoothing splines with randomized GCV computation.
}
\description{
This function is only intended for an internal use.
}
\usage{
spen_egcv(dim, x, y, w, h, QtyPerm, lambda, m, nforApp,
		 EGCVflag, agcv)
}

\arguments{
	\item{dim}{vector of sample size.}
	\item{x}{x-vector in smooth.pen.reg.}
	\item{y}{y-vector in smooth.pen.reg.}
	\item{w}{w-vector in smooth.pen.reg.}
	\item{h}{difference vector for x for internal use.}
	\item{QtyPerm}{Second order difference for x for internal use.}
	\item{lambda}{smoothing parameter input for smooth.pen.reg.}
	\item{m}{vector to store the prediction vector.}
	\item{nforApp}{Number of iterations for approximate GCV.}
	\item{EGCVflag}{Logical when GCV is needed.}
	\item{agcv}{Internal scalar. Set to 0. Stores the approximate GCV.}
}

\details{
This is same as smooth.spline except for small changes.
}
\value{
Does not return anything. Changes the inputs according to the iterations.
}
\seealso{
smooth.spline
}


\author{Arun Kumar Kuchibhotla, arunku@wharton.upenn.edu.}

\keyword{Smoothing Spline}