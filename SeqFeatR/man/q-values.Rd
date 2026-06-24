\name{q-values}
\alias{qvalues}
\title{Calculate q-values from p-values}

\description{Calculates and adds a column of q-values to a list of given p-values.}

\usage{qvalues(path_to_file_csv = NULL, save_name_csv)}

\arguments{
	\item{path_to_file_csv}{file with a column with p-values. Has to contain a column called "p_value" (without the double
  quotes). For reference see example file.}
	\item{save_name_csv}{name of file to which results are saved in csv format.}
}
\details{
  Takes a csv file with a column which is called 'p_values' and uses the qvalues
  package to calculate from this column the corresponding q-values. 
}

\author{Bettina Budeus}

\examples{
#Input file
\dontrun{
file_input <- system.file("extdata", "assocpair_results.csv", package="SeqFeatR")

#Usage
qvalues(
	path_to_file_csv=file_input, 
	save_name_csv="q-values_result.csv")
}}



