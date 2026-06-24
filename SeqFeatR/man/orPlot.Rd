\name{orPlot}
\alias{orPlot}
\title{Visualize odds ratios and p-values from SeqFeatRs assocpoint}

\description{Creates odds ratio/p-value plots for every feature and sequence alignment position.}

\usage{orPlot(path_to_file_assocpoint_results = NULL,
	save_name_pdf, separator = ";",
	number_of_cases = 2, odds_column_position = c(6),
	p_value_column_position = c(2), name_column_position = c(2), freq = 7,
	sequence_column_position = NULL, max_y_axis = 50, interval = 10,
        has_color = 0, bias = 5, p_or_od = "P")}
        
\arguments{
  \item{path_to_file_assocpoint_results}{csv file with results from SeqFeatRs assocpoint function. For reference
    see example file.}
  \item{save_name_pdf}{name of file to which results are saved in csv format.}
  \item{separator}{letter with which csv is separated (usually ";" or ",", or "\\t").}
  \item{number_of_cases}{number of different features in input file.}
  \item{odds_column_position}{column number of first odds ratios.}
  \item{p_value_column_position}{column number of first p-value.}
  \item{name_column_position}{column number of feature name in first row.}
  \item{freq}{frequency of repeat in columns with more than two cases.}
  \item{sequence_column_position}{column number of first amino acid/nucleotide sequence.}
  \item{max_y_axis}{estimated guess of the best p-value/ highest Odds ratio.
  Usuall the number behind the e as positive number.}
  \item{interval}{interval on the y axis.}
  \item{has_color}{if there should be presented another kind of information as
  a color feature. See details.}
  \item{bias}{bias of colors. Select with integer values if you want the first
  color further down. See details.}
  \item{p_or_od}{if p-values or OR should be plotted as height. "P" for p-value,
  "OR" for Odds ratios.}
}
\details{Creates from SeqFeatRs assocpoint results a graphical output where
  each position in the sequence alignment is shown as bar. The height of this
  bar can be the log10 of p-value at this position or the odds ratio, the
  direction information (up or down) based upon odds ratio (below one is down,
  above one is up). If the user adds a column number for an amino acid or DNA
  base, this sequence is added above each bar.
  
  The bar plot can be additionally colored by another type of information
  (e.g. display p-values as height, odds ratio as direction and sequence
  entropy as color). The source of this color information has to be added
  in 'has_color' as a column number. This column can be added manually before
  creating this plot. The column should contain only numbers between zero and
  one. A bias can be given to change the color distribution
  (see R-function colorRampPalette). }

\author{Bettina Budeus}

\seealso{\code{\link{assocpoint}}}

\examples{
#Input file
\dontrun{
assocpoint_result <- system.file("extdata", "assocpoint_results.csv", package="SeqFeatR")
	
#Usage
orPlot(
	path_to_file_assocpoint_results=assocpoint_result, 
	save_name_pdf="orPlot.pdf",
	separator=";",
	number_of_cases=8,
	odds_column_position=6,
	p_value_column_position=2,
	name_column_position=2,
	freq=7,
	sequence_column_position=NULL,
	max_y_axis=4,
	interval=0.2,
	has_color=0,
	bias=5,
	p_or_od="P")
}}
