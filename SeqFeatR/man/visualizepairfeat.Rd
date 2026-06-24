\name{visualizepairfeat}
\alias{visualizepairfeat}
\title{Visualize pairs of sequence alignment positions associated with sequence feature.}
\description{Creates a pdf output with a visualization of the results from the analysis from SeqFeatRs assocpairfeat function.}

\usage{visualizepairfeat(path_to_file_assocpairfeat_csv_result = NULL, save_name_pdf, 
    significance_level = 0.01)}
    
\arguments{
  \item{path_to_file_assocpairfeat_csv_result}{csv file with results from SeqFeatRs assocpairfeat function. For reference see example file.}
  \item{save_name_pdf}{name of file to which results are saved in pdf format.}
  \item{significance_level}{significance value below which the results of the analysis are considered to be relevant enough to be plotted.}
}
\details{
  For every feature (if only one p-value is below given threshold ('significance_level'))
  an output page (pdf page) is created with a sequence position versus
  sequence position graphic. 
  Every dot color(one position with the other position) is based on its p-value. 
  The color code is on the right side.}


\author{Bettina Budeus}
\note{Only use files generated from SeqFeatRs assocpairfeat function.}


\seealso{\code{\link{assocpairfeat}}}

\examples{
#Input file
\dontrun{
assocpairfeat_result <- system.file("extdata", "assocpairfeat_results.csv", package="SeqFeatR")

#Usage
visualizepairfeat(
	path_to_file_assocpairfeat_csv_result=assocpairfeat_result,
	save_name_pdf="vispairfeat_plot.pdf", 
	significance_level=0.05)
}}
