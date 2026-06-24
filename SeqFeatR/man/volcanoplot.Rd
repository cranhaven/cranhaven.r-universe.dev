\name{volcanoplot}
\alias{volcanoplot}
\title{Create volcano plot}
\description{Creates a pdf output file with a volcano plot out of the results from SeqFeatRs assocpoint.}

\usage{volcanoplot(path_to_file_assocpoint_csv_result,
 save_name_pdf, p_values_pos, odds_pos, pos_pos,
 level_of_sig_p, level_of_sig_odds)}
 
\arguments{
  \item{path_to_file_assocpoint_csv_result}{csv file with results from SeqFeatRs assocpoint. For reference see example file.}
  \item{save_name_pdf}{name of file to which results are saved in pdf format.}
  \item{p_values_pos}{column number with p-values.}
  \item{odds_pos}{column number with odds ratios.}
  \item{pos_pos}{column number with positions.}
  \item{level_of_sig_p}{level of significance p values.}
  \item{level_of_sig_odds}{level of significance odds ratios.}
}
\details{Plots log$_10$ Odds ratios versus -log$_10$ p-values and annotates the dots with sequence positions.}


\author{Bettina Budeus}


\examples{
#Input file
\dontrun{
assocpoint_result <- system.file("extdata", "assocpoint_results.csv", package="SeqFeatR")

#Usage
volcanoplot(
	path_to_file_assocpoint_csv_result=assocpoint_result,
	save_name_pdf="volcano_plot.pdf",
	p_values_pos = 3,
	odds_pos = 6,
	pos_pos = 1,
	level_of_sig_p = 0.05,
	level_of_sig_odds = 1)
}}
