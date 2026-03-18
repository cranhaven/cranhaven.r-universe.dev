\name{univariate_table_example}
\docType{data}
\alias{univariatetable.csv}
\title{Univariate frequency table example}
\description{

In 2011, a Nepal Living Standards Survey provided multiple censored freqency tables containing household size (percent). Here we show the urban household size table.

univariatetable.csv is a preloaded example that is correctly formatted for the revengc package.  Below shows how a user should read in a csv file to have the same format:

univariatetable.csv<-read.csv("filename.csv", row.names = NULL, header= FALSE, check.names=FALSE)

}
\usage{univariatetable.csv}


\references{
National Planning Commissions Secretariat, Government of Nepal. (2011). \emph{Nepal Living Standards Survey}. Page 28. Retrieved from: http://siteresources.worldbank.org/INTLSMS/Resources/3358986-1181743055198/3877319-1329489437402/Statistical_Report_Vol1.pdf
}