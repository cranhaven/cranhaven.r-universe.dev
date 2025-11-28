\name{Dotcharts}
\alias{Dotchart}
\alias{Dotchart1}
\alias{Dotchart3}

\title{Improved dotcharts}
\description{Dotcharts, improved and extended}

\usage{

Dotchart1(x, labels=NULL, groups=NULL, gdata=NULL, offset=1/8,
 ann=par("ann"), xaxt=par("xaxt"),  frame.plot=TRUE, log="",
 cex=par("cex"), pt.cex=cex, pch=21, gpch=21, bg=par("bg"),
 color=par("fg"),  gcolor=par("fg"), lcolor="gray", xlim=
 range(x[is.finite(x)]), main=NULL, xlab=NULL, ylab=NULL, ...)

Dotchart(x, ...)

Dotchart3(values, left, right, pch=21, bg="white", pt.cex=1.2,
 lty=1, lwd=2, gridcol="grey", ...)

}

\arguments{
\item{x}{Either a vector or matrix of numeric values. Inputs are coerced by 'as.numeric()', with a message.}
\item{labels}{A vector of labels for each point.}
\item{groups}{An optional factor indicating how the elements of 'x' are grouped.}
\item{gdata}{Data values for the groups. This is typically a summary such as the median or mean of each group.}
\item{offset}{Offset in inches of 'ylab' and 'labels'.}
\item{ann}{Logical value indicating whether title and x and y axis labels should appear on the plot.}
\item{xaxt}{String indicating the x-axis style; use 'n' to suppress and see also par("xaxt").}
\item{frame.plot}{Logical indicating whether a box should be drawn around the plot.}
\item{log}{Character string indicating if one or the other axis should be logarithmic, see ?plot.default.}
\item{cex}{The character size to be used.}
\item{pt.cex}{The 'cex' to be applied to plotting symbols.}
\item{pch}{The plotting character or symbol to be used.}
\item{gpch}{The plotting character or symbol to be used for group values.}
\item{bg}{The background color of plotting characters.}
\item{color}{The color(s) to be used for points and labels.}
\item{gcolor}{The single color to be used for group labels and values.}
\item{lcolor}{The color(s) to be used for the horizontal lines.}
\item{xlim}{Horizontal range for the plot.}
\item{main}{Overall title for the plot, see 'title'.}
\item{xlab, ylab}{Axis annotations as in 'title'.}
\item{values}{Centers for 'Dotchart3()'}
\item{left}{Left margins for 'Dotchart3()'}
\item{right}{Right margins for 'Dotchart3()'}
\item{lty}{Line type for 'Dotchart3()'}
\item{lwd}{Line width for 'Dotchart3()'}
\item{gridcol}{Grid color for 'Dotchart3()'}
\item{...}{Additional arguments}
}

\details{

For better explanations of options, see 'help(dotchart)'.

Dotchart1() is a dotchart() corrected for use with 1-dimensional tables.
If the argument is a 1-dimensional table, Dotchart() converts it into
numeric vector first and instead of warning, outputs the message. This is
helpful to the beginners with R, and especially on macOS GUI where
warnings are in red. It also allows dotcharts to show 'ylab' (this was
not available in R < 4.0.3 but corrected later).

Dotchart() is a prettified dotchart with the following defaults:
'lcolor="black", bg="white", pt.cex=1.2'.

Dotchart3() is the dotchart extension which shows values together with
ranges. Somewhat similar to Linechart() but more general (and does not
work with grouped data).

}

% \value{}

% \references{}

\author{Alexey Shipunov}

\seealso{\code{\link{dotchart}}, \code{\link{Linechart}}}

\examples{

## Compare:
aa <- table(c(1, 1, 1, 2, 2, 3))
dotchart(aa, ylab="Ylab") # produces warning; does not show 'ylab' if R version < 4.0.3
Dotchart1(aa, ylab="Ylab") # outputs message instead of warning; always shows 'ylab'
Dotchart(aa, ylab="Ylab") # in addition to Dotchart1(), dots and grid are more visible

iris1 <- aggregate(iris[, 1], iris[5], function(.x) fivenum(.x)[c(3, 1, 5)])
iris1x <- iris1$x
row.names(iris1x) <- iris1$Species
Dotchart3(iris1x[, 1], iris1x[, 2], iris1x[, 3])

}

\keyword{hplot}
