\name{Gridmoon}
\alias{Gridmoon}
\title{Draw with 'R'}
\description{Sraw with 'R'}

\usage{Gridmoon(Skyres=50, Nightsky=TRUE, Daysky="deepskyblue", Moon=TRUE,
 Moonsize=0.05, Stars=TRUE, Hillcol="black", Text=c("Once upon a time..."),
 Textsize=22, Textpos=c(.15, .51), Textcol="white")}

\arguments{
\item{Skyres}{Sky resoluiton}
\item{Nightsky}{If TRUE, ther eis a night}
\item{Daysky}{Color of day sky}
\item{Moon}{If TRUE, there is a moon}
\item{Moonsize}{Moon size}
\item{Stars}{If TRUE, there are stars}
\item{Hillcol}{Hill color}
\item{Text}{Text to print}
\item{Textsize}{Text size}
\item{Textpos}{Text position}
\item{Textcol}{Text color}
}

\details{
'Gridmoon()' is an example how to paint (draw) with 'R'. Just for fun. From Murrell (2006) "R Graphics", with modifications.

Author's comments:

An example of a one-off image drawn using the grid system.

The code is somewhat modular and general, with functions for producing different shapes, but the sizes and locations used in this particular image assume a 2:1 aspect ratio.

The gradient-fill background (dark at the top to lighter at the bottom) is achieved by filling multiple overlapping polygons with slowly changing shades of grey.
}

% \value{}

\references{Murrell P. 2006. R Graphics.}

\author{Alexey Shipunov}

% \seealso{}

\examples{
## Examples best viewed with 2:1 aspect ratio, use something like
## dev.new(width=10, height=5)
Gridmoon(Skyres=75)
Gridmoon(Nightsky=FALSE, Moon=FALSE, Stars=FALSE, Hillcol="forestgreen",
 Text="Use R!", Textcol="yellow", Textpos=c(.25, .85), Textsize=96)
}

\keyword{hplot}
