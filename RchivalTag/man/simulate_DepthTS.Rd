

\name{simulate_DepthTS}
\alias{simulate_DepthTS}

\title{
simulate depth time series data
}
\description{
function to simulate depth series data from a template data set.
}

\usage{
simulate_DepthTS(ts_df, ndays=10, gaps=TRUE, trate=90, 
                 random_Depth=TRUE, ref_Depth_lim=300, random_Depth_lim=c(100, 700))
}

\arguments{
\item{ts_df}{\link{data.frame} holding the template depth time series data.
}

\item{ndays}{number of days of depth time series data that should be simulated).
}

\item{gaps, trate}{Whether gaps should be introduced into the dataset according to the transmission rate (\code{trate}), in addition to any potential gaps in the template data set.
}

\item{random_Depth, ref_Depth_lim, random_Depth_lim}{
Whether depth records from the template data set should be additonaly randomized. In this case, depth records >= \code{ref_Depth_lim} will be replaced by values between the limits of \code{random_Depth_lim} (i.e. in the default settings a record of >= 300 will be replaced by a value between 100 and 700 m).
}

}

\seealso{\link{dy_DepthTS}, \link{plot_DepthTS}, \link{plot_data_coverage}} 


\author{
Robert K. Bauer
}


\examples{
### load sample depth and temperature time series data from miniPAT:
# ts_file <- system.file("example_files/104659-Series.csv",package="RchivalTag")
# ts_df <- read_TS(ts_file)
# ts_df$Serial <- ts_df$DeployID
# head(ts_df)
# dy_DepthTS(ts_df) # plot original data
# 
# ts_df_sim <- simulate_DepthTS(ts_df) # simulate data
# 
# dy_DepthTS(ts_df_sim) # plot simulated data
# 
# library(dplyr)
# meta <- rbind(ts_df[,names(ts_df_sim)],ts_df_sim) %>% 
#   group_by(DeployID, Serial, Ptt) %>%
#   summarise(dep.date=min(date),pop.date=max(date)) %>% 
#   as.data.frame()
# 
# ts_list <- list(ts_df,ts_df_sim)
# names(ts_list) <- meta$Serial
# 
# # plot data coverage
# plot_data_coverage(x = ts_list,type="ts", meta = meta)
}
