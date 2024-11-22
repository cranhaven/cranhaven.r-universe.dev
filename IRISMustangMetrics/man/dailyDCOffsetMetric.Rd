\name{dailyDCOffsetMetric}

\alias{dailyDCOffsetMetric}


\title{DC Offset Detection}

\description{
The dailyDCOffsetMetric() function identifies days with a jump in the signal mean.
}

\usage{
dailyDCOffsetMetric(df, 
                    offsetDays=5,
                    outlierWindow=7,
                    outlierThreshold=3.0,
                    outputType=1)
}

\arguments{
  \item{df}{a dataframe containing \code{sample_mean} values obtained with getSingleValueMetrics()}
  \item{offsetDays}{number of days used in calculating weighting factors}
  \item{outlierWindow}{window size passed to findOutliers() function in the \pkg{seismicRoll} package}
  \item{outlierThreshold}{detection threshold passed to findOutliers() function in the \pkg{seismicRoll} package}
  \item{outputType}{if 1,  return last day of valid values (index= length(index)-floor(outlierWindow/2)); if 0, return all valid values (indices= max(offsetDays, floor(outlierWindow/2): length(index)-floor(outlierWindow/2)) }
}

\details{
This algorithm calculates lagged differences of the daily mean timeseries over a window of \code{offsetDays} days.
Shifts in the mean that are persistent and  larger than the typical standard deviation of daily means will
generate higher metric values.

%% max preformatted length ---------------------------------------------------
Details of the algorithm are as follows
\preformatted{
# data0 = download requested daily means (in the 'df' dataframe), must be greater than max(offsetDays,outlierWindow)+floor(outlierWindow/2)
# data1 = remove outliers using MAD outlier detection with the 'outlier' arguments specified
# data2 = replace outliers with rolling median values using a default 7 day window, remove last floor(outlierWindow/2) number of samples.
# weights = calculate absolute lagged differences with 1-N day lags, default N=5
# metric0 = multiply the lagged differences together and take the N'th root
# stddev0 = calculate the rolling standard deviation of data2 with a N-day window
# METRIC = divide metric0 by the median value of stddev0
}
}

\value{
A list is returned with a \code{SingleValueMetric} object for the last day-floor(outlierWindow/2) (default 3rd from last day) in the incoming dataframe if outputType=1 (one list element), otherwise the first+offsetDays to last day-floor(outlierWindow/2) (multiple list elements, one per day) if outputType=0.
}

%% \references{ }

\author{
Jonathan Callahan \email{jonathan@mazamascience.com}
}

\note{
Prefer 60+ days of sample_mean values to get a good estimate of the long term sample_mean standard deviation.
After initial testing on stations in the IU network, a metric value > 10 appears to be indicative of a DC offset shift (this may vary across stations or networks and larger values may be preferred as indications of a potential station issue).
}

\seealso{
\code{\link{getSingleValueMetrics}}
}


\keyword{metrics}
