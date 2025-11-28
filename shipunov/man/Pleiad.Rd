\name{Pleiad}
\alias{Pleiad}

\title{Correlation circles (correlation pleiads)}

\description{Plot correlation circles (correlation pleiads, correlograms)}

\usage{Pleiad(tbl, abs=FALSE, corr=FALSE, dist=FALSE, treshold=FALSE,
 circ=list(1, 1, 1), breaks=5, auto=TRUE, gr=6, lwd=NULL, lty=NULL,
 lcol=NULL,  abbr=-1, lbltext="internal", lblcex=1, off=1.09, hofft=0.07,
 hoff=1.02, legend=TRUE,  legtext=1, legpos="topright", leghoriz=FALSE,
 show.int=FALSE, dig.lab=1, neg.col=NULL, ...)}

\arguments{
\item{tbl}{Data: square, numeric, symmetric matrix with same row and column names}
\item{abs}{If TRUE, uses absolute values instead of real}
\item{corr}{If TRUE, uses absolute values instead of real and cuts from 0 to 1, this is good for correlation matrices}
\item{dist}{If TRUE, converts distance matrix to the data frame -- good for "dist" objects}
\item{treshold}{If this is (saying) =.5, selects for plotting (with lty=1) only those values which are >.5}
\item{circ}{Line type, width and color for the cirle; if first or third =0, no cicrle}
\item{breaks}{How to cut() values, if "cramer", then =c(0, .1, .3, .5, 1)}
\item{auto}{If FALSE, specify 'lwd', 'lty' and 'lcol'}
\item{gr}{Grayscale scheme starts from 6 breaks}
\item{lwd}{If auto=FALSE, specify here the vector concerted with breaks}
\item{lty}{If auto=FALSE, specify here the vector concerted with breaks}
\item{lcol}{If auto=FALSE, specify here the vector concerted with breaks; if length(lcol) == 1, all lines are of particular color}
\item{abbr}{If =-1, no abbreviation; if =0, no labels; other values run abbreviate(..., abbr)}
\item{lbltext}{If this is a vector starting from something else, will replace dimnames}
\item{lblcex}{Magnification of labels}
\item{off}{Radial offset of labels, be careful!}
\item{hofft}{Treshold determining which labels are rigtmost/leftmost, 'hofft=0' put all labels into this group}
\item{hoff}{Horizontal offset for rightmost/leftmost labels; 'hoff=1' removes offset}
\item{legend}{If FALSE, no legend}
\item{legtext}{If =1 then "weaker ... stronger"; if =2, shows cutting intervals; if =3, then 1:5; if >3, issues error}
\item{legpos}{Position of the legend, see help(legend)}
\item{leghoriz}{Make the legend horizontal?}
\item{show.int}{Show intervals in (...] form}
\item{dig.lab}{dig.lab for cut(), use to change notation}
\item{neg.col}{If not NULL and 'abs' or 'corr' are TRUE, colorize negative correlations using specified color}
\item{...}{Further arguments to points()}
}

\details{

Correlation circles (correlation pleiads, correlograms) based on the
works of Petr Terentjev's (Saint-Petersburg) school.

Please be sure to use 'corr=TRUE' or 'dist=TRUE' when working with
correlation matrices or 'dist' objects, respectively.

It is probably a good idea to order data entries with hierarchical
clustering to optimize the resulted graph (see examples).

Note that: (1) 'lty', 'lwd' and 'lcol' are not recycling; (2) plot has no
margins so consider second 'legend()' to add any text to the plot (see
examples); (3) it works best when number of items is relatively low (<
30); (4) it can visualize (colorize) negative correlations (see examples)
but this works bes with default (black) color; (5) 'dist=TRUE' will
convert dissimilarities into similarities by substracting from maximum.

Alternatives: those graph plotting packages which are able to plot
"correlogram".

}

\value{

Data frame (invisibly) with position of points, might help in plot
enchancing.

}

% \references{}

\author{Alexey Shipunov}

% \seealso{}

\examples{

l.c <- cor(datasets::longley, method="spearman", use="pairwise")
Pleiad(l.c, corr=TRUE, legtext=2, pch=21, cex=2, bg="white", breaks=3,
 gr=3, hoff=1, show.int=TRUE)
legend("topleft", legend="Macroeconomic correlations", text.font=2,
bty="n")

## colorize negative correlations and use hclust() to re-order
reorder <- hclust(dist(t(longley)))$order
Pleiad(l.c[reorder, reorder], corr=TRUE, neg.col="red")

dr.c <- cor(drosera[, -1], method="spearman", use="pairwise")
## simple example with most defaults
Pleiad(dr.c, corr=TRUE)
## complex example with user-specified colors etc.
Pleiad(dr.c, corr=TRUE, legtext=2, pch=19, cex=1.2, hoff=1.06,
 auto=FALSE, lwd=c(1, 1, 1, 2.5, 4), lty=rep(1, 5),
 lcol=colorRampPalette(c("grey", "#D8284F"))(5),
 circ=c(2, 1, 1))

## visualize distances
Pleiad(dist(t(atmospheres)), dist=TRUE, breaks=3, legtext=2, dig.lab=3)

}

\keyword{hplot}
