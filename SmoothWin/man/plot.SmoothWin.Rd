\name{plot.SmoothWin}
\alias{plot.SmoothWin}

\title{
 Plot function for the SmoothWin object
}
\description{
  This function plots a SmoothWin object
}
\usage{
  \method{plot}{SmoothWin}(x, 
                           ylab   = 'Response'              , 
                           xlab   = 'Time (continuous)'     ,
                           sub    = NULL                    ,
                           col    = NULL                    , 
                           digits = 2                       , 
                           ...
                           )
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
  SmoothWin object
}
  \item{ylab}{
  Label on the y axis. Default `Response`
}
  \item{xlab}{
  Label on the x axis. Default `Time (continuous)`
}  
\item{sub}{
  See the `sub` parameter in `plot()` function. If left NULL then some information about the final window will be shown. Default NULL
}
  \item{col}{
   Colour parameter for the points. Set to NULL to use the default colouring (spectrum colouring). Default NULL
  }
  \item{digits}{
   The number of visible digits for l, k and SWS. Default 2
  }
  \item{\dots}{
  Optional parameters that can be passed to the `plot`/`qqPlot` function. See `car` package for the qqPlot function
}
}
\author{
	Hamed Haselimashhadi <hamedhm@ebi.ac.uk>
}

\seealso{
	\code{\link{SmoothWin}}
}

\examples{
	example(SmoothWin)
}
