\name{make.TukeyC.test}
\alias{make.TukeyC.test}

\title{
	Make Tukey Test
}

\description{
	This function implements the Tukey test for balanced or unbalanced designs and schemes.
}

\usage{
make.TukeyC.test(obj,
		 MSE,
		 sig.level,
		 dfr,
		 round,
		 adjusted.pvalue)
}

\arguments{
	\item{obj}{A data.frame with the means and replicate of the factors.}
	\item{MSE}{A vector of length 1 giving the mean squared error.}
	\item{sig.level}{A vector of length 1 giving the level of significance of the test.} 
	\item{dfr}{A vector of length 1 giving the degrees of freedom of \samp{MSE}.}
	\item{round}{Integer indicating the number of decimal places.}
	\item{adjusted.pvalue}{A vector of pvalues obtained of the ptukey function.}
}

\value{
	A list with 5 slots containing the most important results of the test performed:
	\item{Result}{The result of the Tukey test}
	\item{Sig.Level}{The significance of the test}
	\item{Diff_Prob}{A \code{matrix} with the observed means differences (\code{upper.tri}) and respective probalities (\code{lower.tri})}
	\item{MSD}{The minimum significative difference}
	\item{Replicates}{The replicate number for each factor}
}

\note{
	This function is mainly for internal use in the \pkg{TukeyC} package.
}

\author{
	Jose Claudio Faria (\email{joseclaudio.faria@gmail.com})\cr
	Enio Jelihovschi (\email{eniojelihovs@gmail.com})\cr
	Ivan Bezerra Allaman (\email{ivanalaman@gmail.com})
}

\keyword{package}
