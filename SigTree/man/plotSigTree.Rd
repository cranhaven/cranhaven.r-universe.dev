\name{plotSigTree}
\alias{plotSigTree}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Function to plot the phylogenetic tree in R with branches (tips) colored according to significance of families of p-values (tips' p-values)
}
\description{
\code{plotSigTree} takes \code{tree} and \code{unsorted.pvalues} and computes p-values for each branch (family
 of tips) and colors the corresponding descendant branches.  It computes the p-values based on arguments 
 involving p-value adjustment (for multiple hypothesis testing) and either Hartung's, Stouffer's, or Fisher's 
 p-value combination method.  There are arguments that allow for the customization of the p-value cutoff 
 ranges as well as the colors to be used in the coloring of the branches.

}
\usage{
plotSigTree(tree, unsorted.pvalues, adjust=TRUE, side=1, 
	method="hommel", p.cutoffs=ifelse(rep(side==1, ifelse(side==1, 6, 3)),
	c(.01, .05, .1, .9, .95, .99), c(.01, .05, .1)),
	pal=ifelse(rep(side==1, ifelse(side==1, 1, length(p.cutoffs)+1)),
	"RdBu", rev(brewer.pal(length(p.cutoffs)+1,"Reds"))),
	test="Stouffer", branch.label=FALSE, tip.color=TRUE, edge.color=TRUE,
	tip.label.size=1, branch.label.size=1,  type="fan",
	use.edge.length=TRUE, edge.width=1, branch="edge", 
	root.edge=ifelse(type=="fan",FALSE,TRUE),
	branch.label.frame="none")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{tree}{
a phylogenetic tree of class \code{phylo}.
}
  \item{unsorted.pvalues}{
a data frame (or matrix) with tip labels in column 1 and p-values in column 2.  The tip labels must correspond to the tip labels in \code{tree}.
}
  \item{adjust}{
a logical argument that controls whether there is p-value adjustment performed (\code{TRUE}) or not (\code{FALSE}).
}
  \item{side}{
a numerical argument that takes values \code{1} and \code{2}, depending on whether the p-values in \code{unsorted.pvalues} are 1-sided or 2-sided, respectively.  Only used in p-value adjustment if \code{adjust = TRUE}.
}
  \item{method}{
one of the p-value adjustment methods (used for multiple-hypothesis testing) found in \code{p.adjust.methods} (\code{"holm"}, \code{"hochberg"}, \code{"hommel"}, \code{"BH"}, \code{"bonferroni"}, \code{"BY"}, \code{"fdr"}, and \code{"none"}).  See help for \code{p.adjust} for more details on these methods.  \code{method} is only used if \code{adjust = TRUE}.
}
  \item{p.cutoffs}{
a vector of increasing p-value cutoffs (excluding 0 and 1) to determine the ranges of p-values used in the coloring of the branches.
}
  \item{pal}{
one of the palettes from the RColorBrewer package (see \code{brewer.pal.info} for a list) or a vector of hexadecimal colors (or other valid \code{R} colors).  These are the colors that are used to color the branches.  The first color corresponds to the first range in p.cutoffs and so on.
}
  \item{test}{
a character string taking on \code{"Hartung"}, \code{"Stouffer"}, or \code{"Fisher"}.  This is the p-value combination method that will be used.  In most cases, "Stouffer" will be most appropriate, unless \code{adonis.tree} indicates significant evidence of dependence among p-values, in which case "Hartung" is preferred.
}
  \item{branch.label}{
a logical argument that controls whether the branches are labeled (\code{TRUE}) or not (\code{FALSE}). This results in either edges or nodes being labeled, depending on the \code{branch} argument.  The edge and node labeling is internal to class \code{phylo}, and are not necessarily sequential numbers.  When \code{branch="edge"} is used, the tip edges are not labeled (to avoid clutter).  Branch labels match those returned by \code{export.inherit}.
}
  \item{tip.color}{
a logical argument that controls whether the tips are colored (\code{TRUE}) or not (\code{FALSE}). 
}
  \item{edge.color}{
a logical argument that controls whether the edges are colored (\code{TRUE}) or not (\code{FALSE}). 
}
  \item{tip.label.size}{
a numerical argument that controls the (cex) size of the text of the tip labels.
}
  \item{branch.label.size}{
a numerical argument that controls the (cex) size of the text of the branch labels (see \code{branch.label} argument).
}
  \item{type}{
a character string that controls which type of plot will be produced.  Possible values are \code{"phylogram"}, \code{"cladogram"}, \code{"fan"}, \code{"unrooted"}, and \code{"radial"}.  See \code{plot.phylo}.
}
  \item{use.edge.length}{
a logical argument that uses the original edge lengths from \code{tree} (\code{TRUE}) or not (\code{FALSE}).  This has no effect if \code{tree} does not have edge lengths defined to begin with.  Can be affected by \code{root.edge}, depending on \code{type} (see \code{root.edge} below).
}

  \item{edge.width}{
  a numeric vector controlling width of plotted edges. This is passed to (\code{plot.phylo}).
}

  \item{branch}{
  a character controlling branch definition: \code{"edge"} and \code{"node"} are the only options.  This does not affect statistical methods, only the colors used in edge coloring. Prior to package version 1.2, only \code{branch="node"} was implemented.
}

  \item{root.edge}{
  a logical argument that controls whether the root edge is plotted (\code{TRUE}) or not (\code{FALSE}). Note that \code{root.edge=TRUE} forces \code{use.edge.length=FALSE} when \code{type} is \code{"phylogram"}, \code{"cladogram"}, \code{"fan"}, or \code{"unrooted"}.
  }

  \item{branch.label.frame}{
  a character controlling the frame around the branch labels (only used when \code{branch.label=TRUE}).  Only options \code{"none"}, \code{"circ"}, and \code{"rect"} are supported.
  }
}
\details{
The tip labels of \code{tree} (accessed via \code{tree$tip.label}) must have the same names (and the same length) as the tip labels in \code{unsorted.pvalues}, but may be in a different order.  The p-values in column 2 of \code{unsorted.pvalues} obviously must be in the [0, 1] range.  \code{p.cutoffs} takes values in the (0, 1) range.  The default value for \code{p.cutoffs} is \code{c(0.01, 0.05, 0.1, 0.9, 0.95, 0.99)} if \code{side} is \code{1} and \code{c(0.01, 0.05, 0.1)} if side is \code{2}.  Thus, the ranges (when side is \code{1}) are: [0, .01], (.01, .05], ..., (.99, 1].  These ranges correspond to the colors specified in \code{pal}.  P-values in the [0, .01] range correspond to the left-most color if \code{pal} is a palette (view this via \code{display.brewer.pal(x, pal)} - where \code{x} is the number of colors to be used) or the first value in the vector if \code{pal} is a vector of colors.  If \code{pal} is a vector of colors, then the length of \code{pal} should be one greater than the length of \code{p.cutoffs}.  In other words, its length must be the same as the number of p-value ranges.  An example of a color in hexadecimal format is \code{"#B2182B"}. The default value of \code{pal} is \code{"RdBu"} (a divergent palette of reds and blues, with reds corresponding to small p-values) if \code{side} is \code{1} and the reverse of \code{"Reds"} (a sequential palette) if \code{side} is {2}.   The sequential palettes in \code{RColorBrewer} go from light to dark, so \code{"Reds"} is reversed so that the dark red corresponds to small p-values.    It probably makes more sense to use a divergent palette when using 1-sided p-values and a sequential palette (reversed) when using 2-sided p-values.  To create a vector of reversed colors from a palette with \code{x} number of colors and \code{"PaletteName"} as the name of the palette, use \code{rev(brewer.pal(x, "PaletteName"))}.  \code{use.edge.length} may be useful to get a more uniformly-shaped tree.  \code{plotSigTree} assumes that each internal node has exactly two descendants.  It also assumes that each internal node has a lower number than each of its ancestors (excluding tips).

The \code{branch} argument controls whether edge coloring corresponds to the combined p-value of the tips below the edge (\code{"edge"}) or of the tips below the edge's leading (away from the tips) node (\code{"node"}).  Note that if \code{branch="node"} is used, then both edges leaving a node will necessarily be colored the same.

To access the tutorial document for this package (including this function), type in R: \code{vignette("SigTree")}


}
\value{
This function produces a phylogenetic tree plot.
}
\references{

Stevens J.R., Jones T.R., Lefevre M., Ganesan B., and Weimer B.C. (2017) "SigTree: A Microbial Community Analysis Tool to Identify and Visualize Significantly Responsive Branches in a Phylogenetic Tree."  Computational and Structural Biotechnology Journal 15:372-378.

Jones T.R. (2012) "SigTree: An Automated Meta-Analytic Approach to Find Significant Branches in a Phylogenetic Tree" (2012). MS Thesis, Utah State University, Department of Mathematics and Statistics.  http://digitalcommons.usu.edu/etd/1314

}
\author{
John R. Stevens and Todd R. Jones
}
\note{
Extensive discussion of methods developed for this package are available in Jones (2012).  In that reference, (and prior to package version number 1.1), this \code{plotSigTree} function was named \code{plot.color}; the name change was made to resolve S3 class issues.

For purposes of acknowledgments, it is worth noting here that the plotting done by \code{plotSigTree} relies internally on tools of the \code{ape} package (Paradis et al., 2004 Bioinformatics 20:289-290).  To accomodate edge-specific coloring (as with the \code{branch="edge"} option), some of these \code{ape} package tools were adapted and re-named in the \code{SigTree} package.  Specifically, see \code{?plotphylo2} and \code{?circularplot2}.

}

%% ~Make other sections like Warning with \section{Warning }{....} ~


\examples{
### To access the tutorial document for this package, type in R (not run here): 
# vignette('SigTree')

### Create tree, then data frame, then use plotSigTree to plot the tree
### Code for random tree and data frame
node.size <- 10
seed <- 109
# Create tree
set.seed(seed);
library(ape)
r.tree <- rtree(node.size)
# Create p-values data frame
set.seed(seed)
r.pval <- rbeta(node.size, .1, .1)
# Randomize the order of the tip labels
# (just to emphasize that labels need not be sorted)
set.seed(seed)
r.tip.label <- sample(r.tree$tip.label, size=length(r.tree$tip.label))
r.pvalues <- data.frame(label=r.tip.label, pval=r.pval)

# Check for dependence among p-values; lack of significance here
# indicates default test="Stouffer" is appropriate; 
# otherwise, test="Hartung" would be more appropriate.
adonis.tree(r.tree,r.pvalues)

# Plot tree in default 'fan' type, with branches labeled
plotSigTree(r.tree, r.pvalues, edge.width=4, branch.label=TRUE)

# Plot tree in 'phylogram' type, with branch labels circled
plotSigTree(r.tree, r.pvalues, edge.width=4, branch.label=TRUE,
  type='phylo', branch.label.frame='circ')
  
# Plot tree in 'phylogram' type, with branch labels circled,
# and assuming original p-values were for 2-sided test
plotSigTree(r.tree, r.pvalues, edge.width=4, branch.label=TRUE,
  type='phylo', branch.label.frame='circ', side=2)

# Plot tree in 'phylogram' type, with branch labels boxed;
# also give custom significance thresholds, and use
# a Purple-Orange palette (dark purple for low p-vals
# to dark orange for high p-vals)
plotSigTree(r.tree, r.pvalues, edge.width=4, branch.label=TRUE,
  type='phylo', branch.label.frame='rect',
  p.cutoffs=c(.01,.025,.975,.99), pal='PuOr')
  
}


\keyword{ tree }
\keyword{ dplot }
