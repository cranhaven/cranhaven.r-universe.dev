

\name{combine_histos}
\alias{combine_histos}
\title{
combine lists of TAD/TAT frequency data
}
\description{
This function allows to combine seperate lists of TAD/TAT frequency data from archival tags (i.e. by \href{https://wildlifecomputers.com/}{Wildlife Computers}). The function requires ungrouped/unmerged TAD/TAT lists to avoid merging duplicate records (e.g. multiple TAD/TAT lists from the same individual). However, grouped/merged lists with TAD/TAT from multiple individuals and even duplicate records can be provided, as the function includes an internal call of \link{unmerge_histos} to meet this requirement.
}
\usage{
combine_histos(hist_list1, hist_list2)
}
 

\arguments{
\item{hist_list1, hist_list2}{
Two list-of-lists to be combined, each containing TAD and TAT frequency data and the corresponding \code{bin_breaks} from one or several archival tags.
}



}



\value{A list-of-lists of ungrouped/unmerged TAD and TAT frequency data.

$ TAD:List\cr
  ..$ ID1  : List of 2\cr
  .. ..$ bin_breaks: num\cr 
  .. ..$ df        : data.frame \cr
$ TAT:List\cr 
  ..$ ID1  : List of 2\cr
  .. ..$ bin_breaks: num\cr 
  .. ..$ df        : data.frame \cr 
  ..$ ID2  : List of 2\cr
  ...

}

\seealso{\link{unmerge_histos}, \link{merge_histos}, \link{hist_tad}, \link{hist_tat}} 


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

