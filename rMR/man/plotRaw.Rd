\name{plotRaw}
\alias{plotRaw}

\title{
Plotting Data from Witrox
}
\description{
A good way to visualize your respiro data to get an idea of where to set up the time intervals in functions like \code{MR.loops()} or \code{get.pcrit()}.
}
\usage{
plotRaw(data, DO.var.name, time.var.name = "std.time",
             start.time = data$x[1],
             end.time = data$x[length(data$x)],...)
}

\arguments{
 \item{data}{
data object for plotting
}
 \item{DO.var.name}{
A character string matching the column header for the DO variable column.
}
  \item{time.var.name}{
Column name of time (or x) axis in character class. 
}
  \item{start.time}{
Character string specifying left bound x limit in \code{strptime} compatable format.
}
  \item{end.time}{
Character string specifying right bound x limit in \code{strptime} compatable format.
}
  \item{\dots}{
Arguments passed on to internal functions.
}
}
\details{
\code{start.time} and \code{end.time} arguments must match the \code{time.var.name} column's format for date time.
}
\value{
Plot showing the overall metabolic data
}
\author{
Tyler L. Moulton
}

\seealso{
\code{\link{plot}},
\code{\link{strptime}},
\code{\link{get.pcrit}},
\code{\link{MR.loops}},

}
\examples{
## load data ##

data(fishMR)
## create time variable in POSIXct format ##
fishMR$std.time <- as.POSIXct(fishMR$Date.time,
                    format = "\%d/\%m/\%Y \%I:\%M:\%S \%p")

plotRaw(data = fishMR, DO.var.name = "DO.mgL",
         start.time = "2015-07-03 06:15:00",
         end.time = "2015-07-03 08:05:00")
         
plotRaw(fishMR, DO.var.name = "DO.mgL",
         start.time = "2015-07-03 01:00:00",
         end.time = "2015-07-03 05:12:00")

}
