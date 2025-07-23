

\name{hist_tad}
\alias{hist_tad}

\title{
Time-at-Depth histogram
}
\description{
generates daily or back-to-back (e.g. Day-vs-Night-) Time-at-Depth histograms from binned depth or depth time series data
}

\details{
Time-at-Temperature (Tat) and Time-at-Depth (TaD) fequencies are a standard data product of archival tags (incl. tag models TDR-Mk9, PAT-Mk10 and miniPAT by \href{https://wildlifecomputers.com/}{Wildlife Computers}) that allow to assess habitat preferences of tagged animals (see function \link{read_histos}).  It can be likewise generated from transmitted or recovered time series data sets using function \link{ts2histos}. 

However, different depth and temperature bin breaks are often used during different deployment programs, which makes a later comparitive analysis of TaT and TaT data difficult. For such cases, the function \link{combine_histos} and \link{merge_histos} can be applied to merge TaT and TaD frequencies based on common bin breaks of different tags.

The purpose of this function is the visualization of Time-at-Depth (TaD) histograms, whereas \link{hist_tad} is the related function for Time-at-Temperature (TaT) data.
}

\usage{
hist_tad(df, 
            bin_breaks=NULL, bin_prefix="Bin", 
            select_id, select_from='Ptt', aggregate_by='Ptt',
            date, min_perc,
            main, xlab='Time at Depth (\%)', ylab="Depth (m)", labeling=TRUE,
            xlim=c(0, 100), adaptive.xlim=FALSE, 
            split_by=NULL, split_levels, xlab2=split_levels, 
            ylab.side=2, ylab.line, ylab.font=1,
            xlab.side=3, xlab.line=2.5, xlab.font=1,
            xlab2.side=1, xlab2.line=1, xlab2.font=2,
            main.side=3, main.line=3.8, main.font=1,
            col=c("darkgrey", "white"),
            xticks, ylabels,
            do_mid.ticks=TRUE, yaxis.pos=0, 
            mars, space=0,
            plot_sd=TRUE, plot_se, plot_nrec=TRUE, plot_ntags=TRUE,
            cex=1.2, cex.main=cex, cex.lab=cex, cex.inf=cex.axis,cex.axis=1, 
            return.sm=FALSE, 
            subplot=FALSE, inside=FALSE,Type="TAD")
} 

\arguments{
\item{df}{dataframe that either contains depth time series data (as a vector "Depth") or several vectors of Time-at-Depth frequencies. In the latter case, column names composed of a common \code{bin_prefix} (default is "Bin.") hold the pre-binned Time-at-Depth frequencies whose depth limits are defined in \code{bin_breaks}.
}

\item{bin_breaks, bin_prefix}{
\code{bin_breaks} is a numeric vector of depth bin breaks for the histogram data. In case of binned data (e.g. from standard wildlife computer histogram files), column names with a \code{bin_prefix} are expected to contain the preprossesed data (by default: Bin1, Bin2, Bin3, etc.). Alternatively, depth time series data will be directly converted using function \link{ts2histos}.
}

\item{select_id, select_from}{
 these arguments allow to take a direct subset of the input dataframe. \code{select_from} defines the vector whereas \code{select_id} defines the identification key(s) that should be selected.
}

\item{aggregate_by}{
character vector defining the columns by which the tagging data should be aggregated. Should contain columns that identify tags (e.g. Serial, Ptt, DeployID) the date and/or day time period (to seperate records from night, day, dawn and dusk see \link{classify_DayTime}). Default values are: date, Day and Ptt.
}

\item{date}{
An optional vector to select depth data of a specified date/-range.
}

\item{min_perc}{
optional number, defining the minimum data coverage (in percent) of histogram entries obtained from depth time series data.
}

\item{main, xlab, ylab, labeling}{
 The titles for the plot, x- and y-axes to be plotted if \code{labeling} is set \code{TRUE} (default).
}

\item{xlim, adaptive.xlim}{
a vector defining the limits (x1,x2) of the x-axis, by default c(0,100). However, if \code{adaptive.xlim} is set \code{TRUE}, these limits will be overwritten, and the maximum value (xlim[2]) will be chosen from the histogram data.
}

\item{split_by}{
Name of the logical vector by which TaD data should be splitted (e.g. daytime; see \link{classify_DayTime}).
}

\item{split_levels, xlab2}{
Character vector defining the name and order of the levels of the split_by vector (e.g. c("Night", "Day") for split_by vector 'day.time'. The same groups are plotted as a second x-axis label if not defined otherwise (\code{xlab2}=\code{split_levels}). 
}

\item{ylab.side, ylab.line, ylab.font}{
side, line and font of second y-axis label.
}

\item{xlab.side, xlab.line, xlab.font}{
side, line and font of first x-axis label.
}

\item{xlab2.side, xlab2.line, xlab2.font}{
side, line and font of second x-axis labels.
}

\item{main.side, main.line, main.font}{
side, line and font of plot title.
}

\item{col}{
colours to be used for the TaD-histogram, by default 'grey' and 'white' (corresponding to the values of split_by/split_levels).
}

\item{xticks, ylabels}{
tick labels of the x-axis and ylabels of the y-axis to show in the plot.
}

\item{do_mid.ticks}{
whether centered tick-labels, indicating the depth range of histogram cells, shall be plotted (by default \code{FALSE}). Alternatively, tick labels will be indicated at the breakpoints of the histrogram cells.
}

\item{yaxis.pos}{
x-axis coordinate at which the y-axis should be plotted (by default xlim[1], and thus 0).
}

\item{mars}{
a numerical vector of the form \code{c(bottom, left, top, right)}, decribing the number of margin lines to be specified on the each side of the plot. The default is \code{c(2.1, 4.1, 6.1, 2.1)}. In case that \code{do_mid.ticks} is \code{TRUE} margins are: \code{c(2.1, 8, 6.1, 2.1)}.
}

\item{space}{
the space between the histogram bars.
}

\item{plot_sd, plot_se, plot_nrec , plot_ntags}{
whether standard deviation or standard error bars, the number of records and tags shall be plotted (default is \code{TRUE}) inside the TaD/TaT histogram. (if plot_se is \code{TRUE}, plot_sd will be overwritten!).
}

\item{cex, cex.main, cex.lab, cex.inf, cex.axis}{
font size of the title (\code{cex.main}), x- and y-axes labels (\code{cex.lab}), other labels, like the number of records (\code{cex.inf}) as well as of the tick marks.
}

\item{return.sm}{
whether summary information of the TaD histograms, including the number of records per summary period, the relative frequencies per bin and corresponding standard deviation, should be plotted (default is \code{TRUE}).
}

\item{subplot, inside}{
whether the TaD histogram is a subplot or an inner plot of a figure (default is \code{FALSE}). 
If \code{subplot} or \code{inside} are set \code{TRUE}, graphic margins will not be set by \code{hist_tad}.
In case that \code{inside} is \code{TRUE}, no axis-labels and titels wil be plotted.
}

\item{Type}{
The Type of data to be plotted (\code{TAD}: Time-at-Depth histograms; \code{TAT}: Time-at-Temperature histograms)
}

}



\seealso{\link{ts2histos}, \link{combine_histos}, \link{merge_histos}, \link{hist_tat}} 


\author{
Robert K. Bauer
}

\examples{
ts_file <- system.file("example_files/104659-Series.csv",package="RchivalTag")
ts_df <- read_TS(ts_file)
head(ts_df)

tad_breaks <- c(0, 2, 5, 10, 20, 50, 100, 200, 300, 400, 600, 2000)
tat_breaks <- c(10,12,15,17,18,19,20,21,22,23,24,27)

## example 1a) convert only DepthTS data to daily TAD frequencies:
ts2histos(ts_df, tad_breaks = tad_breaks)
hist_tad(ts_df, bin_breaks = tad_breaks)
hist_tad(ts_df, bin_breaks = tad_breaks, do_mid.ticks = FALSE)

## convert 1b) only TemperatureTS data to daily TAT frequencies:
tat <- ts2histos(ts_df, tat_breaks = tat_breaks)
hist_tat(ts_df, bin_breaks = tat_breaks, do_mid.ticks = FALSE)
hist_tat(tat$TAT$merged, do_mid.ticks = FALSE)

## convert 1c) DepthTS & TemperatureTS data to daily TAD & TAT frequencies:
ts2histos(ts_df, tad_breaks = tad_breaks, tat_breaks = tat_breaks)

## convert 1d) back-to-back histogram showing day vs night TAD frequencies:
ts_df$Lat <- 4; ts_df$Lon=42.5 ## required geolocations to estimate daytime
head(ts_df)
ts_df2 <- classify_DayTime(get_DayTimeLimits(ts_df)) # estimate daytime
head(ts_df2)

ts2histos(ts_df2, tad_breaks = tad_breaks,split_by = "daytime")
hist_tad(ts_df2, bin_breaks = tad_breaks,split_by = "daytime", do_mid.ticks = FALSE)


## example 2) rebin daily TAD frequencies:
tad <- ts2histos(ts_df, tad_breaks = tad_breaks)
tad2 <- rebin_histos(hist_list = tad, tad_breaks = tad_breaks[c(1:3,6:12)])
par(mfrow=c(2,2))
hist_tad(tad, do_mid.ticks = FALSE) ## example for multiple individuals
hist_tad(tad$TAD$merged, do_mid.ticks = FALSE)
hist_tad(tad$TAD$merged, bin_breaks = tad_breaks[c(1:3,6:12)]) ## from inside hist_tad


## example 3) read, merge and plot TAD frequency data from several files:
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

