

\name{merge_histos}
\alias{merge_histos}
\alias{rebin_histos}

\title{
merge and/or rebin TAD/TAT-frequency data
}
\description{
The joint analysis of archival tagging data from different tagging programs is often hampered by differences in the tags' setups, e.g. by the user-specified temporal resolution of time series data or the definition of summary data products. The latter particuarly concerns different selected bin breaks of Time-at-Depth (TAD) and Time-at-Temperature (TAT) frequency data from archival tags by \href{https://wildlifecomputers.com/}{Wildlife Computers}.\cr

The purpose of this function is to allow:\cr\cr1) a grouping of TAD and TAT data from multiple tags based on similiar bin breaks (For this, run the function with default statements, i.e. \code{force_merge} is \code{FALSE}),\cr\cr
2) merging (rebinning) of TAD and TAT data from multiple tags based on the bin breaks that all tags have in common (To do so, run the function with \code{force_merge} set \code{TRUE}).\cr\cr
3) merging (rebinning) of TAD and TAT data from multiple tags based on new user-specified \code{tad_breaks} and/or \code{tat_breaks}. In this case, the \code{force_merge}-statements \code{TRUE} and \code{FALSE} will omit or seperately group tags that do not share all user-specified bin breaks, respectively. \cr\cr 
To combine of TAD/TAT data of several \code{hist_lists}, see \link{combine_histos}.


To visualize Time-at-Temperature (TaT) and Time-at-Depth (TaD) data, please see \link{hist_tat} and \link{hist_tad}, respectively.
}

\usage{
merge_histos(hist_list, tad_breaks=NULL, tat_breaks=NULL, force_merge=FALSE)
rebin_histos(hist_list, tad_breaks=NULL, tat_breaks=NULL, force_merge=FALSE)

}
 

\arguments{
\item{hist_list}{
A list-of-lists containing the TAD and TAT frequency data and the corresponding \code{bin_breaks} from one or several tags.
}

\item{tad_breaks}{
a numeric vector defining the \code{bin_breaks} for the merging (rebinning) of the TAD frequency data.\cr
In case that the additional argument \code{force_merge} is set \code{TRUE}, only tags whose original TAD bin breaks included all of the user-specified \code{tad_breaks} will be merged in a single group ('merged') based on the new bin breaks, while other tags will be omitted in the output. By contrast, if \code{force_merge} is set \code{FALSE}, tags that do not contain all specified \code{tad_breaks} will be merged in seperate groups (group2, group3, etc.), based on similar \code{bin_breaks}.
}

\item{tat_breaks}{
a numeric vector defining the \code{bin_breaks} for the merging (rebinning) of the TAT frequency data.\cr
In case that the additional argument \code{force_merge} is set \code{TRUE}, only tags whose original TAT bin breaks included all of the user-specified \code{tat_breaks} will be merged in a single group ('merged') based on the new bin breaks, while other tags will be omitted in the output. By contrast, if \code{force_merge} is set \code{FALSE}, tags that do not contain all specified \code{tat_breaks} will be merged in seperate groups (group2, group3, etc.), based on similar \code{bin_breaks}.
}


\item{force_merge}{
If \code{FALSE} (default), groups of tags with similar TAD and TAT-\code{bin_breaks} will be combined (no merging on new bin breaks) and identifier labels renamed as group1, group2, etc.\cr If set \code{TRUE}, TAD and TAT frequency data will be merged on user-specified \code{tad/tat_breaks} or, if those arguments are missing, on the \code{bin_breaks} that all tags have in common. In both latter cases, identifier labels will be renamed "merged".
}

}



\value{A list-of-lists of grouped or merged TAD and TAT frequency data.

$ TAD:List\cr
  ..$ group1  : List of 2\cr
  .. ..$ bin_breaks: num\cr 
  .. ..$ df        : data.frame \cr
$ TAT:List\cr 
  ..$ group1  : List of 2\cr
  .. ..$ bin_breaks: num\cr 
  .. ..$ df        : data.frame \cr 
  ..$ group2  : List of 2\cr
  ...

}

\seealso{\link{unmerge_histos}, \link{combine_histos}, \link{hist_tad}} 


\author{
Robert K. Bauer
}

\examples{
## example 1) read, merge and plot TAD frequency data from several files:
## part I - read histogram data from two files:
hist_dat_1 <- read_histos(system.file("example_files/104659-Histos.csv",package="RchivalTag"))
hist_dat_2 <- read_histos(system.file("example_files/104659b-Histos.csv",package="RchivalTag"))
## note the second list is based on the same data (tag), but on different bin_breaks

## part II - combine TAD/TAT frecuency data from seperate files in one list:
hist_dat_combined <- combine_histos(hist_dat_1, hist_dat_2)
par(mfrow=c(2,1))
hist_tad(hist_dat_combined)
hist_tat(hist_dat_combined)

## part III - force merge TAD/TAT frecuency data from seperate files 
# in one list, by applying common bin_breaks:
hist_dat_merged <- merge_histos(hist_dat_combined,force_merge = TRUE)
hist_tad(hist_dat_merged)
hist_tat(hist_dat_merged)

## part IV - plot merged data:
hist_tad(hist_dat_merged) # of all tags
unique(hist_dat_merged$TAD$merged$df$DeployID) ## list unique tags in merged list
hist_tad(hist_dat_merged, select_id = "15P1019b", select_from = 'DeployID') # of one tag

## part V - unmerge data:
unmerge_histos(hist_dat_merged)
}

