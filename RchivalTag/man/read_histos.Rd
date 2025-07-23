

\name{read_histos}
\alias{read_histos}
\title{
reads a TAD/TAT-histogram file from archival tags
}
\description{
reads or posttreats a manually loaded standard histogram data file, containing Time-at-Depth (TAD) and Time-at-Temperature (TAT) frequency data, from archival tags by \href{https://wildlifecomputers.com/}{Wildlife Computers}.
}
\usage{

read_histos(hist_file, date_format, lang_format="en", tz="UTC", dep.end, Serial, 
            force_24h=TRUE, min_perc, omit_negatives=TRUE, right_truncate=TRUE)
}
 

\arguments{
\item{hist_file}{
character string indicating the name of a standard \href{https://wildlifecomputers.com/}{Wildlife Computers} file to read or the \link{data.frame} of a manually loaded histogram data file. The combination of the columns \code{DeployID}, \code{Ptt} and \code{Serial} is assumed to provide an unique key to distinguish data from indiviudal tags.
}

\item{force_24h}{
whether histogram data with a time step of less than 24h should be merged to 24h (default is \code{TRUE}).
Note that the current version of \link{hist_tad} and \link{hist_tat} was written for 24h data!
}

\item{date_format, lang_format, tz}{
character strings indicating the date format, language format and the corresponding time zone, defined by the vectors Date and Time (by default: date_format="\%H:\%M:\%S \%d-\%b-\%Y", lang_format="en", tz='UTC')
If formatting fails, please check as well the input language format, defined by \code{lang_format} (and use abbrviations such as "en" for English,"es" for Spanish, "fr" for French, etc.) as well.
}

\item{dep.end}{
\link{Date} specifying the deployment end of the tag.
}

\item{Serial}{
character-string indicating the Serial number of the tag to be selected. (in case of multi-tag histogram files.)
}

\item{min_perc}{
optional number, defining the minimum data coverage (in percent) of histogram entries obtained from depth time series data.
}

\item{omit_negatives}{
merge negative depth and temperature bins with next positive bin (>= 0; default is \code{TRUE}). 
}

\item{right_truncate}{
truncate the values of the last tat- and tad-bin to 45 degrees and 2000 m, respectively (default is \code{TRUE}). 
}

}

\details{
This function reads or posttreats a manually loaded standard Wildlife Computers histogram file including Time-at-Depth (TAD) and Time-at-Temperature (TAT) frequency data. In the post-treatment, the histogram data is split in lists of TAD and TAT per individual (see below). Thus processed data from several histogram files (or similarly processed time series data) can be combined using the function \link{combine_histos}. Merging of histogram data from several tags, based on similar or user-specified TAD and TAT-\code{bin_breaks}, can be done by applying function \link{merge_histos}. To generate TAD/TAT histogram data from depth and temperature time series data, see \link{ts2histos}.
}

\value{A list-of-lists containing the loaded histogram data. Lists of TAD and TAT data are distinguished at the first nesting level. Further sublists include the \code{bin_breaks} and \link{data.frame}s of the histogram data per tag (ID).
Tag IDs are constructed based on the columns \code{DeployID}, \code{Ptt} and \code{Serial} keys (e.g. DeployID.101_Ptt.102525). \strong{The data.frames of the histogram data also contain average (avg) and standard deviation (SD) of depth and temperature values that are estimated internally from the TAD and TAT data sets (not measured!). The accuracy of these estimates thus depends on the number and selection of bin breaks}, unlike \link{ts2histos}-generated values that are directly estimated from time series data. See statistics-example below.

$ TAD:List\cr
  ..$ ID1  : List of 2\cr
  .. ..$ bin_breaks: num\cr 
  .. ..$ df        : data.frame \cr
  .. .. ..$ DeployID \cr
  .. .. ..$ Ptt \cr
  .. .. ..$ datetime \cr
  .. .. ..$ date \cr
  .. .. ..$ Bin1 \cr
  ..
  .. .. ..$ Bin? (up to number of bin breaks)\cr
  .. .. ..$ avg (average depth estimated!! from histogram data)\cr
  .. .. ..$ SD (average depth estimated!! from histogram data) \cr


$ TAT:List\cr 
  ..$ ID1  : List of 2\cr
  .. ..$ bin_breaks: num\cr 
  .. ..$ df        : data.frame (with columns as above)\cr 
  ..$ ID2  : List of 2\cr
  ...

}

\seealso{\link{ts2histos}, \link{combine_histos}, \link{merge_histos}, \link{hist_tad}, \link{hist_tat}} 


\author{
Robert K. Bauer
}

\examples{
## read and merge 12h histogram data:
# 12h_hist_file <- system.file("example_files/67851-12h-Histos.csv",package="RchivalTag")
# hist_dat_0 <- read_histos(12h_hist_file,min_perc=100) # omit incomplete days
# hist_tad(hist_dat_0)
#hist_tat(hist_dat_0)

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

## part VI - statistics:
# get histogram data with histogram-derived average depth and temperature values
hist_dat_1 <- read_histos(system.file("example_files/104659-Histos.csv",package="RchivalTag"))
avg1 <- hist_dat_1$TAD$DeployID.15P1019_Ptt.104659$df$avg # infered from the histogram data

# generate histogram data and average/sd-estimates from depth time series data of the same tag.
# attention! unlike for histogram files, the average/sd-estimates are calculated
# directly from depth time series data and not from the binned histogram data
ts_file <- system.file("example_files/104659-Series.csv",package="RchivalTag")
ts_df <- read_TS(ts_file)

tad_breaks <- c(0, 2, 5, 10, 20, 50, 100, 200, 300, 400, 600, 2000)
hist_dat_2 <- ts2histos(ts_df, tad_breaks = tad_breaks)
avg2 <- hist_dat_2$TAD$merged$df$avg # directly estimated from the depth time series data

# check accuracy of average depth values:
plot(avg1, avg2) 
avg1-avg2
abline(0,b = 1,lty="dotted")

## crosscheck!
# library(plyr)
# ts_stats <- ddply(ts_df,c("date"),function(x) c(avg=mean(x$Depth,na.rm=T),SD=sd(x$Depth,na.rm=T)))
# avg2==ts_stats$avg

# path <- system.file("example_files",package="RchivalTag")
# PDT <- read_PDT("104659-PDTs.csv",folder=path)
# head(PDT)
# image_TempDepthProfiles(interpolate_PDTs(PDT)[[1]])

## add information
# lines(ts_stats$date+.5,ts_stats$avg)
# add <- hist_dat_2$TAD$merged$df
# lines(add$date+.5,add$avg)
# axis(2,at=50,las=1)
# abline(h=20,lty="dashed",col="violet",lwd=3)


}

