\name{contingency_table_example}
\docType{data}
\alias{contingencytable.csv}
\title{Contingency table example}
\description{

In 2010, the Population Census Data - Statistics Indonesia provided over 60 censored contingency tables containing Floor Area of Dwelling Unit (square meter) by Household Member Size.  The tables are separated by province, urban, and rural. Here we show the household size by area contingency table for Indonesia 's rural Aceh Province. 

contingencytable.csv is a preloaded example that is correctly formatted for the revengc package.  Below shows how a user should read in a csv file to have the same format:

contingencytable.csv<-read.csv("filename.csv", row.names = NULL, header= TRUE, check.names=FALSE)

}

\usage{contingencytable.csv}

\references{
Population Census Data - Statistics Indonesia. (2010). \emph{Household by Floor Area of Dwelling Unit and Households Member Size}.  Retrieved from: http://sp2010.bps.go.id/index.php/site/tabel?wid=1100000000&tid=334&fi1=586&fi2=
}
