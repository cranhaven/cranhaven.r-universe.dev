\name{derivcvxpec}
\alias{derivcvxpec}
\title{
C code for prediction using cvx.lse.reg, cvx.lip.reg and cvx.lse.con.reg.
}
\description{
This function is only intended for an internal use.
}
\usage{
derivcvxpec(dim, t, zhat, D, kk)
}

\arguments{
	\item{dim}{vector of sample size, size of newdata and which derivative to compute.}
	\item{t}{x-vector in cvx.lse.reg and others.}
	\item{zhat}{prediction obtained from cvx.lse.reg and others.}
	\item{D}{derivative vector obtained from cvx.lse.reg and others.}
	\item{kk}{vector storing the final prediction.}
}

\details{
The estimate is a linear interpolator and the algorithm implements this.
}
\value{
Does not return anything. Changes the inputs according to the algorithm.
}

\author{Arun Kumar Kuchibhotla, arunku@wharton.upenn.edu.}

\keyword{Convex Least Squares Prediction}