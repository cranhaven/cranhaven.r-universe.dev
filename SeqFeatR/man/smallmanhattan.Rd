\name{smallmanhattan}
\alias{smallmanhattan}
\title{A small version of manhattan plot}

\description{Creates a small version of the manhattan plot from assocpoint with annotated
  alignment positions.}

\usage{smallmanhattan(path_to_file_csv, save_name_png, save_name_svg, feature,
       corrected=FALSE, x_axis_breaks)}
    
\arguments{
  \item{path_to_file_csv}{csv file with results from assocpoint. For reference
    see example file.}
  \item{save_name_png}{name of file to which graphic is saved in png format.}
  \item{save_name_svg}{name of file to which graphic is saved in svg format.}
  \item{feature}{column number in input csv file with the values. See details.}
  \item{corrected}{if the corrected p-values should be used.}
  \item{x_axis_breaks}{x axis breaks to be displayed. See details.}
}

\details{
  The input file should be a result file from SeqFeatRs assocpoint.

  The feature, that should be displayed as an extra manhattan plot, must be given
  just like it is displayed in the csv file, e.g for HLA-A2, insert "A2" as
  value for feature. If you want to show the corrected p-values, change
  corrected to TRUE.

  The x_axis_breaks will mark ticks on the x axis, so that a special
  alignment position can be displayed. Those axis breaks have to be given
  in the following format only:\cr
  - starting an ending with quotes\cr
  - between those quotes comma separated ascending alignemnt position numbers
    (pleas keep in mind, that the plot will only display a little more than 
    the highest alignment number, whatever big number you have given here)\cr
  - no blanks\cr
  - can be left empty (""), then breaks with 50 positions will be displayed
}

\value{A small version of the manhattan plot as png/svg picture file. svg files
  can be changed more easily than png files. 
}

\seealso{\code{\link{assocpoint}}}

\author{Bettina Budeus}

\examples{
#Input files
\dontrun{
assocpoint_result <- system.file("extdata", "assocpoint_results.csv", package="SeqFeatR")

#Usage
smallmanhattan(
	assocpoint_result,
        save_name_png = "small_manhattan.png", 
        save_name_svg = "small_manhattan.svg", 
        feature = "A2",
        corrected = FALSE, 
        x_axis_breaks = "1,10,16,20,30,40")
}
}