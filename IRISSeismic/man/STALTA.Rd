\name{STALTA}
\docType{methods}
\alias{STALTA}
\alias{STALTA,Trace,missing,missing,missing,missing,missing,missing,missing-method}
\alias{STALTA,Trace,numeric,numeric,character,logical,logical,numeric,numeric-method}
\alias{STALTA,Trace,numeric,numeric,character,missing,missing,missing,missing-method}
\alias{STALTA,Trace,numeric,numeric,missing,missing,missing,missing,missing-method}

\title{STA/LTA}

\description{
The \code{STALTA} method of \code{Trace} objects applies one of several STA/LTA
"first break picking" algorithms to \code{Trace} data in order to automatically
detect seismic events.
}

\usage{
STALTA(x, staSecs, ltaSecs, algorithm, demean, detrend, taper, increment)
}

\arguments{
  \item{x}{a \code{Trace} object}
  \item{staSecs}{length of the \bold{S}hort averaging window in secs (default=3)}
  \item{ltaSecs}{length of the \bold{L}ong averaging windowin secs (default=30)}
  \item{algorithm}{algorithm to be used (default="classic_LR")}
  \item{demean}{boolean flag determining whether to demean the data before applying the algorithm (default=\code{TRUE})}
  \item{detrend}{boolean flag determining whether to detrend the data before applying the algorithm (default=\code{TRUE})}
  \item{taper}{proportion of the signal to be tapered at each end before applying the algorithm (default=0.0)}
  \item{increment}{the increment to use when sliding the averaging windows to the next location (default=1).}
}

\details{
By default, this method uses the "classic_LR" algorithm which calculates the average power in the \code{Trace} 
data over a short window (STA) and a long window (LTA). With this algorithm, windows are "left/right aligned" meaning 
that the point for which STA/LTA is calculated is at the lefttmost edge of the STA window
and the rightmost edge of the LTA window.
The resulting STA/LTA ratio thus has the same number of points as the original data.  This is a standard method
of "first break picking" and can be used to identify the onset of a seismic event.

Three different algorithms are currently available:

\bold{1) \code{algorithm="classic_RR"}} This is the original STA/LTA algorithm with "right alignment".

\deqn{ STA(x_i) = \frac{1}{ns}\sum_{j=i-ns}^{i}{x_i^2} }
\deqn{ LTA(x_i) = \frac{1}{nl}\sum_{j=i-nl}^{i}{x_i^2} }
\deqn{ r_i = \frac{STA_i}{LTA_i} }

\preformatted{
[---------- LTA ---------*]
                [-- STA -*]
}

\bold{2) \code{algorithm="classic_LR"}} (default) This algorithm has the index at the left edge of the STA window
and the right edge of the LTA window

\deqn{ STA(x_i) = \frac{1}{ns}\sum_{j=i}^{i+ns}{x_i^2} }
\deqn{ LTA(x_i) = \frac{1}{nl}\sum_{j=i-nl}^{i}{x_i^2} }
\deqn{ r_i = \frac{STA_i}{LTA_i} }

\preformatted{
[---------- LTA --------*]
                       [*- STA --]
}

\bold{3) \code{algorithm="EarleAndShearer_envelope"}}

\deqn{ STA(x_i) = \frac{1}{ns} \sum_{j=i}^{i+ns}{Mod(H(x))_i} }
\deqn{ LTA(x_i)= \frac{1}{nl} \sum_{j=i-nl}^{i}{Mod(H(x))_i} }
\deqn{ r_i = \frac{STA_i}{LTA_i} }

\preformatted{
[---------- LTA ---------*]
                        [*- STA --]
}

where \eqn{H(x)} is the Hilbert transform of the data and \eqn{Mod(H(x))} is the 'envelope' of the seismic signal.
\emph{Note that because the Hilbert transform involves performing an FFT of the data it can take significantly longer
than the "classic" algorithms for longer seismic signals (>500K pts).}


%%\bold 4) \code{algorithm="Wong_MER"}} This is known as the "Modified Energy Ratios" algorithm.
%%
%%\deqn{ er_i = \sum_{j=i-ne}^{i}{x_j^2} / \sum_{j=i}^{i+ne}{x_j^2} }
%%\deqn{ mer_i = (abs(x_i) * er_i)^3 }
%%
%%\preformatted{
%%[---------- LTA ---------]*
%%                          *[-- STA --]
%%}

}

\value{
A vector of values is returned of the same length as the data in the \code{Trace}.
}

\references{
\href{https://en.wikipedia.org/wiki/First_break_picking}{First break picking} (Wikipedia)

\href{https://www.crewes.org/Documents/ResearchReports/2014/CRR201476.pdf}{Automatic time-picking of first arrivals on large seismic datasets}

\href{http://www.fcaglp.unlp.edu.ar/~velis/papers/PickingGeop10.pdf}{Automatic first-breaks picking: New strategies and algorithms} (Sabbione and Velis 2010)

\href{https://geoconvention.com/wp-content/uploads/abstracts/2012/279_GC2012_Adaptive_Microseismic_Event_Detection.pdf}{Adaptive microseismic event detection and automatic time picking} (Akram and Eaton 2012)

"Characterization of Global Seismograms Using an Automatic-Picking Algorithm"
Bulletin of the Seismological Society of America, Vol. 84, No. 2, pp. 366-376, April 1994 (Earle and Shearer)
}

\author{
Jonathan Callahan \email{jonathan@mazamascience.com}
}

\note{
%%  ~~further notes~~
The returned vector will contain \code{NA} near the edges of the trace where insufficient data are available to fill the windows.
Additional \code{NA} values will appear for every index that is \emph{skipped over} when the \code{increment} parameter is greater than one.

For higher resolution channels, picking an increment of \code{2/sampling_rate} can greatly speed up processing times and still generate reasonable results.
}

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
\code{\link{triggerOnset}}
}

\examples{
\dontrun{
# Open a connection to IRIS DMC webservices
iris <- new("IrisClient")

starttime <- as.POSIXct("2010-02-27",tz="GMT")
endtime <- as.POSIXct("2010-02-28",tz="GMT")
  
# Get the waveform
st <- getDataselect(iris,"IU","ANMO","00","BHZ",starttime,endtime)
tr <- st@traces[[1]]
picker <- STALTA(tr,3,30)

# Plot the trace and overlay the picker
plot(tr)
par(new=TRUE)
plot(picker, type='l', col='red', axes=FALSE, xlab="", ylab="")
mtext("Picker", side=1, line=-8, adj=0.05, col='red')
par(new=FALSE)
}
}

\keyword{methods}
