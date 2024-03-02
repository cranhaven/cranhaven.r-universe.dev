\name{egJofF.1dec93.data}
\docType{data}
\alias{egJofF.1dec93.data}
\title{Eleven Time Series used in Gilbert (1995)}

\usage{
	data(egJofF.1dec93.data)
}

\format{
	This data is a TSdata object.
	The input series name  is  "R90" and the output series names are
	"CPI", "GDP", "M1", "RL", "TSE300", "employment", "PFX", 
	"commod.price index", "US ind.prod." and  "US CPI"
}
\description{

	Data is for Canada unless otherwise indicated. 
	The series start in February 1974 and end in September 1993 (236 
	observations on each variable).

	The input series is 90 day interest rates (R90) and the
	ten output variables are CPI, GDP, M1, long run interest rates (RL), 
	the Toronto stock exchange 300 index (TSE300), employment, 
	the Canada/US exchange rate (PFX), a commodity price index in US dollars, 
	US industrial production, and US CPI.

	R90, RL and TSE are differenced. All other variables are in terms of 
	percent change.

	R90 is the 3 month prime corporate paper rate. While it is not set 
	directly by the Bank of Canada, Bank policy influences it directly 
	and it is often thought of as a proxy "policy variable."
	
	The Statistics Canada identifiers are B14017 (R90), P484549 (CPI), 
	I37026 (GDP), B1627 (M1), B14013 (RL), B4237 (TSE300), 
	D767608 (employment), B3400 (PFX). 
	
	M.BCPI (commodity price index) is published by the Bank of Canada. 
	JQIND (US industrial production), and CUSA0 (US CPI) 
	are DRI identifiers.
	
	The data for M1 (B1627) were taken prior to revisions made in 
	December 1993. 	
}


\source{
	\emph{Statistics Canada,} 
	\emph{Bank of Canada,}
	\emph{DRI.}
}

\references{
	Gilbert, P.D. 1995 "Combining VAR Estimation and State Space Model 
	Reduction for Simple Good Predictions" \emph{J. of Forecasting: Special 
	Issue on VAR Modelling.} 14:229-250
}

\seealso{
	\code{\link{TSdata}}
}
\keyword{datasets}
