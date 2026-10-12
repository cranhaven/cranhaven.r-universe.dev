\name{ggdplot}
\alias{ggdplot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
	Nice plots of hierarchical clustering results via ggdendrogram
	}
\description{
This function provides two different plotting options for either a dendro object or an
object that can be coerced to class dendro, using the function ggdendrogram from the
package \code{ggdendro}
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
ggdplot(hc, lab = NULL, ptype = 1, title = NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{hc}{Either a dendro object or an object that can be coerced to class dendro using
  the \code{\link[ggdendro]{dendro_data}} function, i.e. objects of class dendrogram, hclust or tree}
%%     ~~Describe \code{hc} here~~
  \item{lab}{A character vector of labels for the leaves of the tree. By default labels
  in hc are used.}
%%     ~~Describe \code{lab} here~~
  \item{ptype}{A numeric indicator of the type of plot desired. If \code{ptype==1}, the plot is
  rotated by 90 degrees, the theme is blank, and the title is plotted. If \code{ptype!=1}, the
  plot is rotated by 90 degrees, and the background is a grayscale grid.}
  %%     ~~Describe \code{ptype} here~~
  \item{title}{A character label for the title of the plot. Only used if \code{ptype!=1}.}
%%     ~~Describe \code{title} here~~
  \item{...}{other parameters passed to \code{\link[ggplot2]{geom_text}}}
}
\details{Given either a dendro object or an object that can be coerced to class dendro, this is a
convenience function for plotting. For an object of type dendro, if \code{ptype==1}, the function
executes the equivalent of

\code{ggdendrogram(hcdata, rotate=TRUE, size=2) + labs(title="Dendrogram in ggplot2")}

If \code{ptype!=1}, the function executes the equivalent of

\code{ggdendrogram(hcdata, rotate = TRUE, theme_dendro = FALSE)}

Objects that are not of class dendro are coerced to class dendro prior to plotting.
%%  ~~ If necessary, more details than the description above ~~
}
\value{
A \code{\link[ggplot2]{ggplot}} object
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{
%\code{\link[ggdendro]{ggdendrogram}}
%}
\examples{

library(ggplot2)
hc <- hclust(dist(USArrests), "ave")
p<-ggdplot(hc, ptype=2)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%%\keyword{ ~kwd1 }
%%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
