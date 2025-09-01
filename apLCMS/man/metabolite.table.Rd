\name{metabolite.table}
\alias{metabolite.table}
\docType{data}
\title{
A known metabolite table based on HMDB.
}
\description{
This table was compiled from HMDB metabolites. It contains only the basic information of known metabolites. It can be used to produce feature tables with ion forms of the users' choice. 
}
\usage{data(metabolite.table)}
\format{
A data frame containing the known metabolites. It contains 4 columns: 
	"chemical_formula": the chemical formula of the known table; 
	"HMDB_ID": HMDB ID; 
	"KEGG_compound_ID": KEGG compound ID if known; 
	"mass": the neutral mass; 
}
\details{It is to be used in combination with the object "adduct.table", to create feature table with ion forms of the user's choice. Which ion form to choose should be based on the LC/MS system.}
\source{
Wishart, D. S., et al. (2009). HMDB: a knowledgebase for the human metabolome. Nucleic Acids Res 37, D603-10.}
\examples{
data(metabolite.table)
data(adduct.table)
known.table.example<-make.known.table(metabolite.table[1001:1020,], adduct.table[1:4,])
}
\keyword{datasets}