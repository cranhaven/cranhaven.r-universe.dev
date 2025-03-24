\name{cpen}
\alias{cpen}
\title{
C code for convex penalized least squares regression.
}
\description{
This function is only intended for an internal use.
}
\usage{
cpen(dim, t_input, z_input, w_input, a0_input,
	lambda_input, Ky_input, L_input, U_input,
	fun_input, res_input, flag, tol_input, 
	zhat_input, iter, Deriv_input)
}

\arguments{
	\item{dim}{vector of sample size and maximum iteration.}
	\item{t_input}{x-vector in cvx.pen.reg.}
	\item{z_input}{y-vector in cvx.pen.reg.}
	\item{w_input}{w-vector in cvx.pen.reg.}
	\item{a0_input}{initial vector for iterative algorithm.}
	\item{lambda_input}{lambda-value in cvx.pen.reg.}
	\item{Ky_input}{Internal vector used for algorithm.}
	\item{L_input}{Internal vector. Set to 0.}
	\item{U_input}{Internal vector. Set to 0.}
	\item{fun_input}{Internal vector. Set to 0.}
	\item{res_input}{Internal vector. Set to 0.}
	\item{flag}{Logical for stop criterion.}
	\item{tol_input}{tolerance level used in cvx.pen.reg.}
	\item{zhat_input}{Internal vector. Set to zero. Stores the final output.}
	\item{iter}{Iteration number inside the algorithm.}
	\item{Deriv_input}{Internal vector. Set to zero. Stores the derivative vector.}
}

\details{
See the source for more details about the algorithm.
}
\value{
Does not return anything. Changes the inputs according to the iterations.
}
\source{
Dontchev, A. L., Qi, H. and Qi, L. (2003). Quadratic Convergence of Newton's Method for Convex Interpolation and Smoothing. Constructive Approximation, 19(1):123-143.
}


\author{Arun Kumar Kuchibhotla, arunku@wharton.upenn.edu.}

\keyword{Convex Penalized Least Squares}