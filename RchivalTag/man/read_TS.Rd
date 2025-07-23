

\name{read_TS}
\alias{read_TS}
\title{
reads Time Series Data from Archival Tags
}

\description{
reads Time Series Data (e.g. Depth and Temperature) from Archival Tags (Supported Models: MiniPAT, sPAT, recovered mk10, mk9 from \href{https://wildlifecomputers.com/}{Wildlife Computers} as well as LOTEK PSAT Models \href{https://www.lotek.com/products/psat-series/}{LOTEK}. Models from other Manufactorers might be supported as well.
}

\usage{
read_TS(ts_file, header=TRUE, sep=",", skip = 0, 
        date_format, lang_format = "en", tz = "UTC")
}
 

\arguments{
\item{ts_file}{
character string indicating the name of a standard \href{https://wildlifecomputers.com/}{Wildlife Computers} file to read or the \link{data.frame} of a manually loaded histogram data file. The file is assumed to include the columns \code{Day}, \code{Time} (or a preformatted date-time vector termed \code{datetime} in "UTC" format.) as well as at one of the subsequent columns \code{DeployID}, \code{Ptt} and \code{Serial} to distinguish data from indiviudal tags.
}

\item{header}{
a logical value indicating whether the file contains the names of the variables as its first line. If missing, the value is determined from the file format: header is set to TRUE if and only if the first row contains one fewer field than the number of columns.
}

\item{sep}{
the field separator character. Values on each line of the file are separated by this character. If sep = "" the separator is 'white space', that is one or more spaces, tabs, newlines or carriage returns.
}

\item{skip}{
integer: the number of lines of the data file to skip before beginning to read data.
}

\item{date_format, lang_format, tz}{
character strings indicating the date format, language format and the corresponding time zone, defined by the vectors Date and Time (by default: date_format="\%d-\%b-\%Y \%H:\%M:\%S", lang_format="en", tz='UTC')
If formatting fails, please check as well the input language format, defined by \code{lang_format} (and use abbrviations such as "en" for English,"es" for Spanish, "fr" for French, etc.) as well.
}
}



\details{
This function reads a time series data file from archival tags. Data sets are "completed" to facilitate an assessment of the data coverage (i.e. by \link{ts2histos} or \link{hist_tad}).
}

\value{A data frame (\link{data.frame}) containing a representation of the data in the file.

}

\seealso{\link{ts2histos}, \link{hist_tad}, \link{plot_TS}} 


\author{
Robert K. Bauer
}

\examples{
### load sample depth and temperature time series data from miniPAT:
ts_file <- system.file("example_files/104659-Series.csv",package="RchivalTag")
ts_df <- read_TS(ts_file)
head(ts_df)

## other date_format:
ts_file2 <- system.file("example_files/104659-Series_date_format2.csv",package="RchivalTag")
# ts_miniPAT2 <- read_TS(ts_file2) # run to see error message
ts_miniPAT2 <- read_TS(ts_file2,date_format = "\%d-\%m-\%Y \%H:\%M:\%S")
head(ts_miniPAT2)

## other date_format and lang_format:
ts_file_ES <- system.file("example_files/104659-Series_date_format_ES.csv",package="RchivalTag")
# ts_miniPAT_ES <- read_TS(ts_file_ES) # run to see error message
ts_miniPAT_ES <- read_TS(ts_file_ES,skip=1,sep=";",header = TRUE, 
                         date_format = "\%d/\%b/\%y \%H:\%M:\%S",lang_format = "es")
head(ts_miniPAT_ES)


## load same data in LOTEK format
ts_file <- system.file("example_files/104659_PSAT_Dive_Log.csv",package="RchivalTag")
ts_df <- read_TS(ts_file,date_format="\%m/\%d/\%Y \%H:\%M:\%S")
head(ts_df) ## attention no identifier (Ptt, Serial, DeployID) included!
ts_df$DeployID <- ts_df$Ptt <- "104659"

## example 1) convert only DepthTS data to daily TaD frequencies:
tad_breaks <- c(0, 2, 5, 10, 20, 50, 100, 200, 300, 400, 600, 2000)
tat_breaks <- c(10,12,15,17,18,19,20,21,22,23,24,27)

histos <- ts2histos(ts_df, tad_breaks = tad_breaks, tat_breaks = tat_breaks)
histos$TAD$merged$df$nperc ## check completeness of TAD data sets
histos$TAT$merged$df$nperc ## check completeness of TAT data sets
# histos <- ts2histos(ts_df, tad_breaks = tad_breaks, tat_breaks = tat_breaks,min_perc = 90)


### example 2) add daytime (Day vs Night) information and plot results
# add daytime periods during plot-function call and return extended data set
# ts_df$Lon <- 5; ts_df$Lat <- 43
# plot_DepthTS(ts_df, plot_DayTimePeriods = TRUE, xlim = unique(ts_df$date)[2:3])
# ts_df2 <- plot_DepthTS(ts_df, plot_DayTimePeriods = TRUE, Return = TRUE) 
# names(ts_df)
# names(ts_df2)

### add daytime periods before function call
# ts_df_extended <- get_DayTimeLimits(ts_df)
# plot_DepthTS(ts_df_extended, plot_DayTimePeriods = TRUE)
# plot_DepthTS(ts_df_extended, plot_DayTimePeriods = TRUE, twilight.set = "naut")


}

