\name{close_fig}
\alias{close_fig}
\title{
function to close current graphic device
}
\description{
function to close current graphic device, complement to \link{figure}-function that generates graphic devices in flexible fileformats.
}

\usage{
close_fig(do.close=F, do.save=do.close)

}
\arguments{
  
 \item{do.close, do.save}{
 whether file should be saved or not (default is TRUE). 
 if FALSE, new graphic device will be opened inside R.
 }
}

\seealso{ \code{\link{figure}}}

\author{
Robert K. Bauer
}


\examples{
# do.save <- TRUE
# figure("Gulf_of_Lions", do.save=do.save, width=5, height=5, type="pdf")
# plotmap("lion")
# close_fig(do.save)

do.save <- TRUE
plotmap("lion")
close_fig(do.save)

do.save <- FALSE
figure("Gulf_of_Lions", do.save=do.save, width=5, height=5, type="pdf")
plotmap("lion")
close_fig(do.save)

}