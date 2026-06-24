\name{visualizepair}
\alias{visualizepair}
\title{Visualize pairs of alignment positions that jointly mutate}

\description{Creates a pdf output with a visualization of the results from the analysis from SeqFeatRs assocpair function.}

\usage{visualizepair(path_to_file_assocpair_csv_result = NULL, 
    save_name_pdf, significance_level = 0.01)}

\arguments{
  \item{path_to_file_assocpair_csv_result}{csv file with results from SeqFeatRs assocpair function. For reference see example file.}
  \item{save_name_pdf}{name of file to which results are saved in pdf format.}
  \item{significance_level}{significance value below which the results of the analysis are considered to be relevant enough to be plotted.}
}
\details{
  An output is created with a sequence position versus sequence position
  graphic. Every dot color(one position with the other position)
  is based on its p-value. The color code is on the right side. In contrast to
  visualizepairfeat, no feature information will be used.}


\author{Bettina Budeus}
\note{Only use files generated from SeqFeatRs assocpair function.}


\seealso{\code{\link{assocpair}}}

\examples{
#Input file
\dontrun{
assocpair_result <- system.file("extdata", "assocpair_results.csv", package="SeqFeatR")

#Usage
visualizepair(
	path_to_file_assocpair_csv_result=assocpair_result,
	save_name_pdf="vispair_plot.pdf",
	significance_level=0.05)
}}
