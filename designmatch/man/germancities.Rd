\name{germancities} 

\docType{data} 

\alias{germancities} 

\title{Data from German cities before and after the Second World War} 

\description{
	This is part of the data used by Redding and Sturm (2008) to study the impact of market access on economic development in West German cities after the division of Germany after the Second World War.
	There are 119 rows corresponding to different cities and 21 columns that stand for different variables.
	These variables are: one treatment indicator, 15 baseline covariates, and five outcomes.
	Treated cities are those West German cities within 75 kilometers of the border between East and West Germany after the Second World War (see Redding and Sturm (2008) for details).
	The complete dataset is available at \url{http://www.aeaweb.org/articles.php?doi=10.1257/aer.98.5.1766}.
} 

\usage{data(germancities)} 

\format{A data frame with 122 observations corresponding to 20 treated and 102 control cities.  
		The treatment assignment indicator is the first column of the data frame: treat (1 = treated; 0 = control).
		The next 15 columns are the covariates:
		
		\itemize{
			\item log2pop, logarithm base 2 of the population in each city in 1939;
			
			\item popgrowth1939, population growth in each city from 1919 to 1939;
			
			\item popgrowth3339, population growth in each city from 1919 to 1939;
			
			\item emprate, employment rates in each city in 1939;
			
			\item indrate, industry rates in each city in 1939;
			
			\item rubble, amount of rubble in cubic meters per capita in each city in 1939;
			
			\item rubblemiss, missing data indicator for rubble; the missing values were imputed with the mean;
			
			\item flats, number of destroyed dwellings in each city in 1939 as a percentage of the stock of dwelling; 
			
			\item flatsmiss, missing data indicator for flats; the missing values were imputed with the mean;
			
			\item refugees, proportion of each city's population that identified themselves as refugees in 1939;
			
			\item educat, categories for the employment rates in the educational sector in each city in 1939;
			
			\item publicat, categories for the employment rates in the public administration sector in each city in 1939;
			
			\item busiservcat, categories for the employment rates in the bussiness services sector in each city in 1939;
			
			\item mineralcat, categories for the employment rates in the minerals sector in each city in 1939;
			
			\item transcat, categories for the employment rates in the transport sector in each city in 1939.
		}
				
		The last five columns of the data frame are outcomes: pop50, pop60, pop70, pop80 and pop88, the populations in each city in 1950, 1960, 1970, 1980 and 1988, respectively.
		}

\source{\url{http://www.aeaweb.org/articles.php?doi=10.1257/aer.98.5.1766}} 

\references{
	Redding, S. J., and Daniel M. S. (2008), "The Costs of Remoteness: Evidence from German Division and Reunification," \emph{American Economic Review}, 98, 1766-1797.
}

\keyword{datasets}