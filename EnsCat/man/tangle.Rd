\name{tangle}
\alias{tangle}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Generate a tanglegram from two hierarchical clusterings of a data set
%%  ~~function to do ... ~~
}
\description{
This function generates a tanglegram of two different hierarchical clusterings of the same
dataset. This is essentially a convenience wrapper for the function \code{\link[dendextend]{tanglegram}} in
the package \code{dendextend}; see Galili (2015).
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
tangle(hc0, hc1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{hc0}{
  An object that can be coerced to a dendrogram, e.g., an object from \code{\link[stats]{hclust}}. This
  object will be plotted on the left side of the tanglegram.
%%     ~~Describe \code{hc0} here~~
}
  \item{hc1}{
  An object that can be coerced to a dendrogram, e.g., an object from \code{\link[stats]{hclust}}. This
  object will be plotted on the right side of the tanglegram.
%%     ~~Describe \code{hc1} here~~
}
}
\details{
This function is a convenience wrapper for the function \code{\link[dendextend]{tanglegram}} in the R package
\code{dendextend}; see \url{http://cran.at.r-project.org/web/packages/dendextend/}.
A tanglegram is used to visualize the similarities and differences between two different
hierarchical clusterings of the same dataset.
%%  ~~ If necessary, more details than the description above ~~
}
\value{An invisible \code{\link[dendextend]{dendlist}}, with two trees after being modified during the creation of the tanglegram
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
 	\url{https://cran.r-project.org/package=dendextend}, \url{https://github.com/talgalili/dendextend/},
 	\url{http://www.r-statistics.com/tag/dendextend/}, \url{http://bioinformatics.oxfordjournals.org/content/31/22/3718}
 	al Galili (2015). dendextend: an R package for visualizing, adjusting, and comparing
 	trees of hierarchical clustering. Bioinformatics. doi:10.1093/bioinformatics/btv428
%% ~put references to the literature/web site here ~
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link[dendextend]{tanglegram}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (hc0, hc1)
{
    hcd0 <- as.dendrogram(hc0)
    hcd1 <- as.dendrogram(hc1)
    hcd0 <- match_order_by_labels(hcd0, hcd1)
    dends_0_1 <- dendlist(hcd0, hcd1)
    t <- tanglegram(dends_0_1)
    t
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{dendrogram}
\keyword{tanglegram}% __ONLY ONE__ keyword per line
