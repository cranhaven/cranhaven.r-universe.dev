\name{predcvxpen}
\alias{predcvxpen}
\title{
C code for prediction using cvx.lse.reg, cvx.lip.reg and cvx.lse.con.reg for function and its derivatives.
}
\description{
This function is only intended for an internal use.
}
\usage{
predcvxpen(dim, x, t, zhat, deriv, L, U, fun, P, Q, R)
}

\arguments{
	\item{dim}{vector of sample size, size of newdata.}
	\item{x}{Newdata.}
	\item{t}{x-vector in cvx.pen.reg}
	\item{zhat}{prediction obtained from cvx.pen.reg}
	\item{deriv}{derivative vector obtained from cvx.pen.reg}
	\item{L}{Internal vector obtained from cpen function.}
	\item{U}{Internal vector obtained from cpen function.}
	\item{fun}{vector containing the function estimate.}
	\item{P}{Internal vector set to zero.}
	\item{Q}{Internal vector set to zero.}
	\item{R}{Internal vector set to zero.}
}

\details{
The estimate is characterized by a fixed point equation which gives the algorithm for prediction.
}

\value{
Does not return anything. Changes the inputs according to the algorithm.
}

\author{Arun Kumar Kuchibhotla, arunku@wharton.upenn.edu.}

\keyword{Convex Penalized Least Squares Prediction}