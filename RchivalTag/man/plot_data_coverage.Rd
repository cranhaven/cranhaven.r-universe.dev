

\name{plot_data_coverage}
\alias{plot_data_coverage}
\alias{abacus_plot}


\title{
Abacus plot to illustrate the data coverage of different data products per tag throughout their deployment period.
}
\description{
Abacus plot to illustrate the data coverage of different data products per tag throughout their deployment period.

}

\details{
Abacus plot to illustrate the data coverage of different data products per tag throughout their deployment period as shown in Figure 3 of Bauer et al. (2020).
}

\usage{
plot_data_coverage(x, type, type2, meta, 
                   Identifier="Serial", fields=c("Serial","Ptt"),
                   date_range_std, show_fullmonths=TRUE, 
                   zlim, mars, na.omit=TRUE,
                   do.arrows=TRUE, xpos.arrows=-.25, xpos.years=-.27, 
                   xpos.fields=c(-.01,-.12), ypos.fields,
                   main, cex.main=1.2,
                   cb.xlab, cex.cb.xlab=1,
                   cb.ticks, cex.cb.ticks=.9,
                   pal="jet", bg="grey")
                   
abacus_plot(x, type, type2, meta,
            Identifier="Serial", fields=c("Serial","Ptt"),
            date_range_std, show_fullmonths=TRUE, 
            zlim, mars, na.omit=TRUE,
            do.arrows=TRUE, xpos.arrows=-.25, xpos.years=-.27, 
            xpos.fields=c(-.01,-.12), ypos.fields,
            main, cex.main=1.2,
            cb.xlab, cex.cb.xlab=1,
            cb.ticks, cex.cb.ticks=.9,
            pal="jet", bg="grey")
} 

\arguments{
\item{x}{a list with the tagging data whose data covereage will be illustrated as abacus plot
}

\item{type, type2}{
the data type of x ("ts", "lightlocs","tad","tat") and the name of the variable to be plotted: "perc" in case of lightlocation and time series data, "perc_dat" in case of histogram data ("tad", "tat").
}

\item{meta}{
a \code{data.frame} containing the meta data of the tagging program. Required column names include: 'dep.date', 'pop.date' as well as the definitions of the Identifier, fields vectors (see below).
}

\item{Identifier}{
unit vector to identify the tags within the tagging data list (must appear in names of sublist) and meta table (column name). By default 'Serial' of the tag.
}

\item{date_range_std, show_fullmonths}{
standardized date range to be plotted (deplyoment years are reset to 0; e.g 0-10-03 and 01-02-17 for a fish that was in the water from 2017-10-03 until 2018-02-17. If missing, the date range will be estimated from the tagging data. If show_fullmonths=\code{TRUE}, the estimated date range will be rounded to full months (e.g. 0-10-01 and 01-03-01). However, show_fullmonths is ignored if date_range_std is specified.
}

\item{mars}{
a numerical vector of the form \code{c(bottom, left, top, right)}, decribing the number of margin lines to be specified on the each side of the plot. The default is \code{c(5,12,4,8)}. In case of length(fields) > 2 you may need to increase the space to the left as well as related arguments (i.e. xpos.arrows, xpos.years, xpos.fields), in order to provide the neccessary space for the columns of the meta data to be shown.
}

\item{na.omit}{
whether missing data points within the time series shall be converted to 0. If FALSE, such missing points will be shaded in grey.
}

\item{zlim}{
the minimum and maximum z values for which colors should be plotted. By default c(0,100).
}

\item{do.arrows, xpos.arrows, xpos.years}{
whether arrows shall be shown next to the deployment years as well as the horizontal position of arrows and deployment years.
}

\item{fields, xpos.fields, ypos.fields}{
vectors to define the column(s) in meta data to be illustrated to the left of the data coverage abacus plot as well as related horizontal and vertical positions.
}

\item{main, cex.main}{
the title and it's font size. if missing, title will be set to "type".
}

\item{cb.xlab, cex.cb.xlab}{
the title of the colorbar and it's font size.
}

\item{cb.ticks, cex.cb.ticks}{
the tick labels of the colorbar and it's font size.
}

\item{pal, bg}{
the color palette of the colorbar (either a vector or a single keyword referring to a colorbar from the colorbars of the \link{oceanmap} package) as well as the background color of the data coverage abacus plot.
Please note also, that if the argument 'na.omit' is set to FALSE, missing data points will be shaded in grey.
}

}

\references{
Bauer, R., F. Forget, JM. Fromentin and M. Capello (2020) Surfacing and vertical behaviour of Atlantic bluefin tuna (Thunnus thynnus) in the Mediterranean Sea: implications for aerial surveys. ICES Journal of Marine Science. \doi{10.1093/icesjms/fsaa083}
}


\seealso{\link{read_histos}, \link{ts2histos}, \link{read_TS}} 


\author{
Robert K. Bauer
}

\examples{
# sample_file <- system.file("example_files/abacus_sample_data.rd",package="RchivalTag")
# load(sample_file, verbose=T)
#
# ## Please note: the sample data is contains only the columns required to produce the figures. 
# ## Other fields (e.g. Bins and bin breaks in the histos data are missing). 
# ## The basic structure has been mantained and needs to be adopted 
# ## to apply the plot_data_coverage-function.
#
# str(meta) # meta data example with all required columns
# str(lightlocs) ## not yet implemented, but can structure be adapted for other data sets.
# str(histos) ## combined but not merged histogram data. 
              ## Please compare with read_histos or ts2histos output and examples
# str(ts_list) ## list of depth time series data. compare with read_TS output
#
# plot_data_coverage(lightlocs, type="lightlocs", meta=meta)
# plot_data_coverage(histos, type="tad", meta=meta)
# plot_data_coverage(ts_list, type="ts", meta=meta)

}

