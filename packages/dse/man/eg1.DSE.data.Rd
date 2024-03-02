\name{eg1.DSE.data}
\docType{data}
\alias{eg1.DSE.data}
\alias{eg1.DSE.data.diff}
\alias{eg1.dat}

\title{Four Time Series used in Gilbert (1993)}

\description{

	Data is for Canada. 
	The series start in March 1961 (April 1961 for \code{eg1.DSE.data.diff})
	and end in June 1991, giving 364  
	observations on each variable (363 for \code{eg1.DSE.data.diff}).

	The input series is 90-day interest rates (R90) in both 
	\code{eg1.DSE.data} and \code{eg1.DSE.data.diff}.

	The output series are M1, GDP lagged two months, and CPI. 
	M1, GDP and CPI were all seasonally adjusted data. 
	These are not transformed in \code{eg1.DSE.data} and are first 
	difference of logs in \code{eg1.DSE.data.diff}.

	GDP is lagged because it is not available on as timely a basis. 
	(The data was used in an example where the intent was to build 
	a model for timely monitoring.)

	The Statistics Canada series identifiers are B14017, B1627, I37026,
	and B820200.
	
	The data for M1 (B1627) were taken prior to revisions made in 
	December 1993.
	
	The file \code{eg1.dat} contains the same data as \code{eg1.DSE.data}
	in a simple  ASCII file. 
}


\usage{
	data(eg1.DSE.data)
	data(eg1.DSE.data.diff)
}

\format{
	The objects \code{eg1.DSE.data} and \code{eg1.DSE.data.diff} 
	are \code{TSdata objects}. The file \code{eg1.dat} is an ASCII 
	file with 5 columns, the first enumerating
	the observations, the second giving the input series, and the third to
	fifth giving the output series. 
	The input series name  is  "R90" and the output series names are
	"M1", "GDPl2" and "CPI". GDPl2 is GDP lagged two months
}

\source{
	\emph{Statistics Canada,} 
	\emph{Bank of Canada.}
}

\references{
	Gilbert, P.D. (1993) State Space and ARMA Models: An Overview of the 
	Equivalence. Bank of Canada Working Paper 93-4. Available at 
	\url{http://www.bankofcanada.ca/1993/03/publications/research/working-paper-199/}.

}

\seealso{
	\code{\link{TSdata}}
}
\keyword{datasets}
