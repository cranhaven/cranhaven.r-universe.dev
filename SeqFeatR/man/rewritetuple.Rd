\name{rewritetuple}
\alias{rewritetuple}
\title{Rewrite result from SeqFeatRs assoctuple}

\description{Function to reqrite output from SeqFeatRs assoctuple into an usable form for SeqFeatRs tartan.}

\usage{rewritetuple(path_to_file_assoctuple_csv_result = NULL, save_name_csv, 
first_position, second_position, value_position, separator, threshold)}

\arguments{
  \item{path_to_file_assoctuple_csv_result}{csv file with results from SeqFeatRs assoctuple function. For reference
    see example file.}
  \item{save_name_csv}{name of file to which results are saved in csv format.}
  \item{first_position}{column position of first sequence position.}
  \item{second_position}{column position of second sequence position.}
  \item{value_position}{column position of p-value.}
  \item{separator}{separator of csv input file (usually ";", ",", "\\t").}
  \item{threshold}{p-value threshold, below which data is to be included in result.}
}
\details{Extracts the two sequence positions and p-value if it is below the
 threshold and generates output csv file for SeqFeatRs tartan with these values.}

\author{Bettina Budeus}

\seealso{\code{\link{assoctuple}}}

\examples{
#Input file
\dontrun{
assoctuple_result <- system.file("extdata", "assoctuple_result.csv", package="SeqFeatR")

#Usage
rewritetuple(
	path_to_file_assoctuple_csv_result=assoctuple_result, 
	save_name_csv="rewritetuple_result.csv", 
	first_position=3, 
	second_position=4, 
	value_position=10, 
	separator="\t", 
	threshold=0.1)
}}
