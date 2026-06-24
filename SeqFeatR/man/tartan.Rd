\name{tartan}
\alias{tartan}
\title{Visualize association of mutation pairs and features from two (comparable) analyses.}

\description{Combines the results of two of SeqFeatRs assocpair or assoctuple analyses in one plot.}

\usage{tartan(path_to_file_assocpair_csv_result=NULL,
 path_to_file_assocpair_csv_result2=NULL, save_name_pdf, space,
 colors, name_positions, names, ticks, first_position_1,
 second_position_1, value_1, first_position_2, second_position_2,
 value_2, with_distance_matrix, path_to_distance_matrix)}
 
\arguments{
  \item{path_to_file_assocpair_csv_result}{csv file with results from SeqFeatRs assocpair or assoctuple function. For reference
    see example file.}
  \item{path_to_file_assocpair_csv_result2}{csv file with results from SeqFeatRs assocpair or assoctuple function. For reference
    see example file.}
  \item{save_name_pdf}{name of file to which results are saved in pdf format.}
  \item{space}{space between blocks. See details.}
  \item{colors}{the colors of the tartan plot. See details.}
  \item{name_positions}{Positions on x and y axis where the labels should be inserted.}
  \item{names}{labels to be inserted on x and y axis. 
  Beware! name_position and names must have the same length!}
  \item{ticks}{ticks which should mark interesting spots and are also the
   breakpoints on which the blocks are separated. See details.}
  \item{first_position_1}{column in which first alignment position is
  located in the first csv file.}
  \item{second_position_1}{column in which second alignment position is
  located in the first csv file.}
  \item{value_1}{column in which value to be shown is located in first csv file.}
  \item{first_position_2}{column in which first alignment position is
  located in the second csv file.}
  \item{second_position_2}{column in which second alignment position is
  located in the second csv file.}
  \item{value_2}{column in which value to be shown is located in csv second file.}
  \item{with_distance_matrix}{if there is a distance matrix available and should be used.}
  \item{path_to_distance_matrix}{path to the distance matrix. Can be zero or empty.}
}

\details{
  For each sequence x sequence position for one feature the corresponding
  (p-)values are added as a colored dot.

  The input files may be the results from SeqFeatRs assocpair or assoctuple
  analyses, but any csv file with two different columns of sequence positions
  and one column with values is sufficient. The values should be in a
  comparable range, but they can be other information as p-values
  (e.g. mutual or direct information or distance matrix given as an extra
  input file).

  The displayed colors are chosen by the user. The format for 'colors' must
  be color names in quotes (e.g. "red", "blue", etc.) separated by ","
  (without the double quotes). More colors will give a better distinction.

  To distinguish different parts of the sequence (e.g proteins, markers, etc.)
  the user may add sequence positions as ticks ('ticks') at which the graphical
  display is split and a little space ('space') is inserted. The user can also
  add names to be displayed on the x-axis/y-axis ('name_positions', 'names').
}

\author{Bettina Budeus}

\seealso{\code{\link{assocpair}, \link{assoctuple}}}

\examples{
#Input files
\dontrun{
assocpair_result <- system.file(
                    "extdata", "shared_mutations_result_for_tartan.csv", package="SeqFeatR")
assocpair_result2 <- system.file(
                    "extdata", "assocpair_results.csv", package="SeqFeatR")

#Usage
tartan(
	path_to_file_assocpair_csv_result=assocpair_result,
	path_to_file_assocpair_csv_result2=assocpair_result2,
	save_name_pdf="tartan_plot.pdf",
	space=5,
	colors=c("wheat", "darkblue", "black", "green"),
	name_positions=c(1,30),
	names=c("S","F"),
	ticks=c(10,30),
	first_position_1=2,
	second_position_1=3,
	value_1=4,
	first_position_2=2,
	second_position_2=3,
	value_2=11,
	with_distance_matrix=FALSE,
	path_to_distance_matrix=NULL)
}}
